rankall <- function(outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv",na.string = "Not Available", stringsAsFactors = FALSE)
  reloutcome <- data[,c(2,7,11,17,23)]
  if(!(outcome == "heart attack"| outcome == "heart failure" | outcome == "pneumonia")){
    stop("invalid outcome")
  }
  else{
    reloutcome <- rename(reloutcome, MRbyHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                         MRbyHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                         MRbyP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    if(outcome == "heart attack"){ 
      reqoutcome <- subset(reloutcome, !is.na(MRbyHA))
      ordered <- arrange(reqoutcome,State,MRbyHA)
      
    }
    else if(outcome == "heart failure"){
      reqoutcome <- subset(reloutcome, !is.na(MRbyHF))
      ordered <- arrange(reqoutcome,MRbyHF,Hospital.Name) 
      
    }
    else {
      reqoutcome <- subset(reloutcome, !is.na(MRbyP))
      ordered <- arrange(reqoutcome,MRbyP,Hospital.Name)
      
    }
    
    bystate <- split(ordered,ordered$State)
    ans <- sapply(bystate,function(x)x[num,c(1,2)])
    final <- as.data.frame(ans)
    final_1 <- t(final)
    names <- row.names(final_1)
    nam <- as.data.frame(names)
    final_1$State <- names
    
    if(num == "best"){
      num = 1
    }
    else if(num == "worst"){
      num = length(final_1[,1])
      
    }
  }
  len <- length(final_1[,1])
  if(num > len){
    print(NA)
  } else { 
    print(final_1[1:num,c(1,2)])}

}