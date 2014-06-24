###################################################
# Examples on how to use these functions
##################################################

rm(list=ls())

source("scripts/SummaryFunctions.R")

# load data
mydocs <- grep("Wikipedia", dir("data/"), value=TRUE)

mydocs <- sapply(mydocs, function(DOC){
    result <- scan(paste("data", DOC, sep="/"), sep="\n", what="character")
    
    result <- paste(result, collapse=" ")
    
    return(result)
})

# parse sentences
mysentences <- lapply(mydocs, function(doc){
    # extract sentences
    doc <- ParseSentences(doc = doc)
    
    return(doc)
})

# get dtms
mydtms <- lapply(mysentences, function(doc){
    # get dtm of sentences
    dtm <- MakeSentenceDtm(doc = doc, stem=FALSE, min.ngram=1, max.ngram=1)
    return(dtm)
})

##########################################
# Get some keywords from each document
##########################################

mykeywords <- lapply(mydtms, function(x){
    ExtractKeywords(dtm=x, M=7)
})

##########################################
# Summarize based on "raw" word counts
##########################################
summaries.raw <- mapply(function(dtm, doc){
    # keep only sentences that have at least 5 words and less than 21
    dtm <- dtm[ rowSums(dtm) %in% 5:20, ] 
    
    # get adjacency matrix
    g <- MakeSentenceAdjmat(dtm = dtm, method="raw")
    
    # top N sentences based on eigenvector centrality
    top.n <- SentenceEigenRank(igraph.object = g, sentences = doc, N = 5)
    
    # paste together for final result and output
    summary <- paste(top.n, collapse=" ")
    
    return(summary)
}, dtm=mydtms, doc=mysentences)

##########################################
# Summarize based on cosine similarity
##########################################
summaries.csim <- mapply(function(dtm, doc){
    # keep only sentences that have at least 5 words and less than 21
    dtm <- dtm[ rowSums(dtm) %in% 5:20, ] 
    
    # get adjacency matrix
    g <- MakeSentenceAdjmat(dtm = dtm, method="cosine")
    
    # top N sentences based on eigenvector centrality
    top.n <- SentenceEigenRank(igraph.object = g, sentences = doc, N = 5)
    
    # paste together for final result and output
    summary <- paste(top.n, collapse=" ")
    
    return(summary)
}, dtm=mydtms, doc=mysentences)

##########################################
# Summarize based on keywords
##########################################
summaries.key <- mapply(function(dtm, doc){
    # keep only sentences that have at least 5 words and less than 21
    dtm <- dtm[ rowSums(dtm) %in% 5:20, ] 
    
    # get adjacency matrix
    g <- MakeSentenceAdjmat(dtm = dtm, method="keyword")
    
    # top N sentences based on eigenvector centrality
    top.n <- SentenceEigenRank(igraph.object = g, sentences = doc, N = 5)
    
    # paste together for final result and output
    summary <- paste(top.n, collapse=" ")
    
    return(summary)
}, dtm=mydtms, doc=mysentences)

##########################################
# Write output
##########################################
write.table(summaries.raw, "output/summaries.raw.txt", sep="\t", row.names=T, col.names=F)
write.table(summaries.raw, "output/summaries.csim.txt", sep="\t", row.names=T, col.names=F)
write.table(summaries.raw, "output/summaries.key.txt", sep="\t", row.names=T, col.names=F)



