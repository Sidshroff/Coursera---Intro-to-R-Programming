pollutantmean <- function(directory,pollutant,id =1:332){
   directory <- "C:/Users/Siddhi/Desktop/Personal/Coursera/R Programming/Programming Assignment 1/specdata" 
  
   data <- numeric()
  files <- list.files(directory,full.names = TRUE)
  for( i in id){
    
    fileread <- read.csv(files[i])
    data <- rbind(data, fileread)}
    
    if(pollutant =="sulfate"){
      meansulfate = mean(data$sulfate, na.rm = TRUE)
      print(meansulfate)
    }
    else if(pollutant=="nitrate"){
      meannitrate = mean(data$nitrate, na.rm = TRUE)
      print(meannitrate)
    }
      
  
  
}
