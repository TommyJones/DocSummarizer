###############################################
# Functions for Document Summarization
###############################################


# Module installes required packages if needed and loads them
required.packages <- c("tm",
                       "openNLP",
                       "NLP",
                       "RWeka",
                       "Matrix",
                       "slam",
                       "igraph")

install <- try(required.packages[ ! required.packages %in% installed.packages() ], silent = TRUE)

if( length(install) > 0 & class(install) != "try-error" ) install.packages( install )

capture.output <- lapply(required.packages, function(x) require(x, character.only = TRUE))

rm(install, capture.output, required.packages)


SummaryFunction <- function(path.to.file, N=4, method="raw"){
    ###############################################
    # Takes the path to a text file and returns
    # the top N sentances summarizeing the doc
    # This is the main function
    ###############################################
    
    doc <- scan(path.to.file, what="character", sep="\n")
    
    # collapse document to single line
    doc <- paste(doc, collapse=" ")
    
    # extract sentences
    doc <- ParseSentences(doc = doc)
    
    # get dtm of sentences
    dtm <- MakeSentenceDtm(doc = doc)
    
    # keep only sentences that have at least 5 words and less than 21
    dtm <- dtm[ rowSums(dtm) %in% 5:20, ] 
    
    # get adjacency matrix
    g <- MakeSentenceAdjmat(dtm = dtm)
    
    # top N sentences based on eigenvector centrality
    top.n <- SentenceEigenRank(igraph.object = g, sentences = doc, N = N, method=method)
    
    # paste together for final result and output
    summary <- paste(top.n, collapse=" ")
    
    return(summary)
}



ParseSentences <- function(doc){
    ######################################################
    # Takes a single document, stored as an entry of a
    # character vector, and parses out the sentances. 
    ######################################################
    
    annotator <- Maxent_Sent_Token_Annotator(language = "en")
    
    doc <- as.String(doc)
    
    sentence.boundaries <- annotate(doc, annotator)
    
    result <- doc[ sentence.boundaries ]
    
    if( is.null(names(result)) ) names(result) <- paste("sen", 1:length(result), sep=".")
    
    return(result)
}

NgramTokenizer <- function(min, max) {
    require(RWeka)
    # Function creates a function to create ngrams from a document term matrix
    # For bigrams min=max=2. For bigrams and trigrams min=2, max=3
    # Example: 
    # Bigrams <- NgramTokenizer(2, 2)
    # myDTM <- DocumentTermMatrix(myCorp, control = list(tokenize = Bigrams))
    
    
    result <- function(x) {NGramTokenizer(x, Weka_control(min = min, max = max))}
    
    return(result)
}

MakeSentenceDtm <- function(doc, stem=TRUE, min.ngram=1, max.ngram=1){
    #################################################
    # input: character vector whose entries 
    # correspond to sentences in a document
    #
    # output: a sentance * stem dimensional matrix
    #   of class dgCMatrix from the Matrix package
    #################################################
    
    doc <- gsub("[^a-zA-Z]", " ", doc) # removes all non-alphabetic characters
    
    doc <- tolower(doc) #lowercase
    
    doc <- gsub(" +", " ", doc) # removes extra spaces
    
    corp <- Corpus(VectorSource(doc))
    
    corp <- tm_map(corp, removeWords, gsub("[^a-zA-Z]", " ", c(stopwords("english"), stopwords("SMART")) ) ) # remove stopwords
    
    corp <- tm_map(corp, stripWhitespace) # remove spaces again
    
    if(stem) corp <- tm_map(corp, stemDocument) # stem document
    
    if( min.ngram > 1 | max.ngram > 1 ){
        tokenize <- NgramTokenizer(min=min.ngram, max=max.ngram)
        
        dtm <- DocumentTermMatrix(corp, control=list(tokenize = tokenize))
    }else{
        dtm <- DocumentTermMatrix(corp)
    }
    
    dtm.sparse <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,  # converts to a sparse dtm
                               dims=c(dtm$nrow, dtm$ncol))
    
    rownames(dtm.sparse) <- Docs(dtm)
    colnames(dtm.sparse) <- Terms(dtm)
    
    return(dtm.sparse)
}


ExtractKeywords <- function(dtm, M=5){
    ########################################################################
    # inputs: 
    #       dtm = sparse dtm from Matrix package or standard R matrix as input
    #       M = number of keywords to return
    #
    # output: a vector of keywords
    ########################################################################
    
    
    
    dtm <- dtm > 0 # becomes a matrix of ones and zeros, basically "has word" or does not
    
    adj <- t(dtm) %*% dtm
    
    g <- graph.adjacency(adj, weighted=TRUE, mode="undirected") 
    
    ev <- evcent(graph=g)
    
    terms <- names(ev$vector)[ order(ev$vector, decreasing=TRUE) ][ 1:M ]
    
    return(terms)
}

MakeSentenceAdjmat <- function(dtm, method="cosine"){
    ########################################################################
    # inputs: 
    #       dtm = sparse dtm from Matrix package or standard R matrix as input
    #       method = one of "raw", "cosine", "keyword". 
    #           raw = adjacency matrix based on common word frequencies
    #           cosine = adjacency matrix based on cosine similarity (5 nearest neighbors)
    #           keyword = runs keyword extraction then makes adjacency based on common keywords
    #
    # output: an igraph object
    ########################################################################
    
    methods <- list(raw=function(mat) mat %*% t(mat),
                    cosine=function(mat){
                        mat <- t(apply(mat, 1, function(x){
                            x <- x / sqrt( sum( x * x ) )
                            return(x)
                        }))
                        return(mat %*% t(mat))
                    }, 
                    keyword=function(mat){ # this doesn't yet scale
                        keywords <- ExtractKeywords(dtm=mat, M=10)
                        key.mat <- dtm[ , keywords ]
                        key.mat <- key.mat[ rowSums(key.mat) > 0 , ] # drops sentences w/o any keywords
                        return(key.mat %*% t(key.mat))
                    })
    FUN <- methods[[ method ]]
    
    adj <- FUN(dtm)
    
    g <- graph.adjacency(adj, mode = "undirected", weighted=TRUE, diag = FALSE)
    
    return(g)
}

SentenceEigenRank <- function(igraph.object, sentences, N=5){
    ######################################################
    # input: and igraph object whose vertices correspond
    #   to the sentences of a document
    #
    # output: The top N sentences of a document ranked by
    #   eigenvector centrality
    ######################################################
    
    rank <- evcent(igraph.object)
    
    top.n <- names(rank$vector)[ order(rank$vector, decreasing=TRUE ) ][ 1:N ]
    
    result <- sentences[ top.n ]
    
    return(result)
}