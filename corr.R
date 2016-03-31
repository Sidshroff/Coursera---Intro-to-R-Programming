corr <- function(directory,threshold = 0){
  directory <- "C:/Users/Siddhi/Desktop/Personal/Coursera/R Programming/Programming Assignment 1/specdata" 
  files <- list.files(directory,full.names = TRUE)
  result <- vector("numeric")
  id = 1:332
  for( i in id){
    
    fileread <- read.csv(files[i]) 
    complete <- sum(complete.cases(fileread))
    if (complete > threshold){
      
      completeCases <- complete.cases(fileread)
      validSulfateData <- fileread[completeCases, 2]
      validNitrateData <- fileread[completeCases, 3]
      
      c<- cor(validSulfateData,validNitrateData)
      result <- append(result,c)
    }
   
}
result
}