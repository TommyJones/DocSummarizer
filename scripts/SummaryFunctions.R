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


SummarizeDoc <- function(doc, N=4, keywords=FALSE){
    ###############################################
    # This is the main function
    # 
    # inputs:
    #   doc = a document represneted as 1 X 1 
    #       character vector.
    #   N = number of sentences (keywords) you
    #       want returned as output.
    #   keywords = logical return keywords or 
    #       summary sentences of the document
    #
    # output:
    #   A 1 X 1 character vector of N summary 
    #   sentences of the document or, if keywords
    #   is TRUE, N keywords related to the doc.
    #
    # future functionality: ability to stem words
    #   or have n-gram keywords
    ###############################################
    
    # extract sentences
    doc <- ParseSentences(doc = doc)
    
    # get dtm of sentences
    dtm <- MakeSentenceDtm(doc = doc)
    
    # keep only sentences that have at between 5 and 20 words
    # this is arbitrary and could be adjusted/improved
    dtm <- dtm[ rowSums(dtm) %in% 5:20, ] 
    
    # dtm and adjacency matrix
    if( keywords ){ # transpose dtm and make entries 0,1 for keywords
        dtm <- t(dtm) 
        
        # get adjacency matrix
        g <- MakeSentenceAdjmat(dtm=dtm, method="raw")
        
    }else{ # don't transpose and use cosine similarity for sentences
        g <- MakeSentenceAdjmat(dtm=dtm, method="cosine")
    }
    
    # top N sentences (keywords) based on eigenvector centrality
    top.n <- evcent(graph=g)
    
    top.n <- names(top.n$vector)[ order(top.n$vector, decreasing=TRUE) ][ 1:N ]
    
    if( ! keywords ){ # if we don't want keywords
        # get full sentences
        top.n <- doc[ top.n ]
        
        # paste together for final result and output
        top.n <- paste(top.n, collapse=" ")
    }
    
    
    return(top.n)
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
    # for now, this function is unused but may be later
    # Function creates a function to create ngrams from a document term matrix
    # For bigrams min=max=2. For bigrams and trigrams min=2, max=3
    # Example: 
    # Bigrams <- NgramTokenizer(2, 2)
    # myDTM <- DocumentTermMatrix(myCorp, control = list(tokenize = Bigrams))
    
    
    result <- function(x) {NGramTokenizer(x, Weka_control(min = min, max = max))}
    
    return(result)
}

MakeSentenceDtm <- function(doc, stem=FALSE, min.ngram=1, max.ngram=1){
    #################################################
    # input: 
    #   doc = character vector whose entries 
    #       correspond to sentences in a document
    #   stem = logical, should document be stemmed?
    #   min.ngram = minimum word length for ngrams
    #   max.ngram = maximum word length for ngrams
    #
    # output: a sentance X terms dimensional matrix
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

MakeSentenceAdjmat <- function(dtm, method="cosine"){
    ########################################################################
    # inputs: 
    #       dtm = sparse dtm from Matrix package or standard R matrix as input
    #       method = one of "raw", "cosine", "keyword". 
    #           raw = adjacency matrix based on common words
    #           cosine = adjacency matrix based on cosine similarity (5 nearest neighbors)
    #
    # output: an igraph object
    ########################################################################
    
    # define methods for adjacency matrix
    methods <- list(cosine=NA, raw=NA)
    
    methods$cosine <- function(mat){
        
        mat <- t(apply(mat, 1, function(x){
            x <- x / sqrt( sum( x * x ) )
            return(x)
        }))
        
        mat <- mat %*% t(mat)
        
        return(mat)
    }
    
    methods$raw <- function(mat){
        mat <- mat > 0
        
        mat <- mat %*% t(mat)
        
        return(mat)
    }
    
    
    
    dist.fun <- methods[[ method ]]
    
    adj <- dist.fun(dtm)
    
    g <- graph.adjacency(adj, mode = "undirected", weighted=TRUE, diag = FALSE)
    
    return(g)
}

