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


##########################################
# Get some keywords from each document
##########################################

keywords <- sapply(mydocs, function(x){
    SummarizeDoc(doc=x, N=7, keywords=TRUE)
})


##########################################
# Summarize The documents
##########################################

summaries <- sapply(mydocs, function(x){
    SummarizeDoc(doc=x, N=5, keywords=FALSE)
})



##########################################
# Write output
##########################################

write.table(keywords, "output/example.keywords.csv", sep=",", row.names=FALSE)
write.table(summaries, "output/example.summaries.txt", sep="\n", row.names=T, col.names=F)



