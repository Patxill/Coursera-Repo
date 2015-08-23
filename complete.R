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