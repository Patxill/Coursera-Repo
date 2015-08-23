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