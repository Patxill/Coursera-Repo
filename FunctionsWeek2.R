pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## Vector with the list of files in the selected directory
    listoffiles <-  list.files(directory, full.names = TRUE)
    
    ## New data frame to contain the merged list of selected files
    mergedfiles <- data.frame()
    
    ## merge only the files in the directory selected in the id variable
    for (i in id) {
        mergedfiles <- rbind(mergedfiles, read.csv(listoffiles[i]))
    }
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## Now subset the mergedfiles to get only the column selected (pollutant) 
    dataSubset <- mergedfiles[,pollutant]
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    mean(dataSubset, na.rm = TRUE)
    
}

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## Vector with the list of files in the selected directory
    listoffiles <-  list.files(directory, full.names = TRUE)
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    completetable <- data.frame()
    for (i in id) {
        file <- read.csv(listoffiles[i])
        sulfate <- file[,2]
        nitrate <- file[,3]
        numbrcomplete <- complete.cases(sulfate,nitrate)
        summary <- table(numbrcomplete)
        completetable <- rbind(completetable, list(id = i, nobs = summary["TRUE"]))
    }
    completetable
    
}

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
    u <- t[which(t$nobs > threshold),]
    
    ## New data frame to contain the merged list of selected files
    mergedfiles <- data.frame()
    
    ## merge only the files in the directory selected in the id variable
    for (i in u$id) {
        mergedfiles <- rbind(mergedfiles, read.csv(listoffiles[i]))
    }
    k <- mergedfiles[which(complete.cases(mergedfiles)),2:4]
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    cor(k)
}