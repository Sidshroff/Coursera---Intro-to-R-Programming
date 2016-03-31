complete <- function(directory,id =1:332){
  directory <- "C:/Users/Siddhi/Desktop/Personal/Coursera/R Programming/Programming Assignment 1/specdata" 
  
  complete_cases <- data.frame()
  files <- list.files(directory,full.names = TRUE)
  for( i in id){
    
    fileread <- read.csv(files[i]) 
    nobs <- sum(complete.cases(fileread))
    complete_cases <- rbind(complete_cases, data.frame(id = i,nobs))
    
  }
 complete_cases 
}