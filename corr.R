corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## Vector with the list of files in the selected directory
    listoffiles <-  list.files(directory, full.names = TRUE)
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## t is the list of files with complete cases on each
    t <- complete(directory = directory, id = 1:332)
    
    ## u is the list of files where the complete cases are greater than the threasold
    u <- t[which(t$nobs > threshold),1]
    if (length(u) > 0) {
    v <- listoffiles[u]
    
    ## merge only the files in the directory selected in the id variable
    numvector <- vector("numeric", length = length(u))
    
    for (i in 1:length(v)) {
        o <- read.csv(v[i])
        k <- o[which(complete.cases(o)),2:3]
        correlac <- cor(k)
        numvector[i] <- correlac[1,2]
    }} else {numvector <- vector("numeric", length = 0)}
    #append(numvector, 5, after = 2)
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    numvector
    
}