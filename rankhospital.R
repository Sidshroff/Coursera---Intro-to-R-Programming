rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv",na.string = "Not Available", stringsAsFactors = FALSE)
  reloutcome <- data[,c(2,7,11,17,23)]
  uniquestates <- unique(reloutcome[,2])
  if(!(state %in% uniquestates)){
    stop("invalid state")
  }
  if(!(outcome == "heart attack"| outcome == "heart failure" | outcome == "pneumonia")){
    stop("invalid outcome")
  }
  if(state %in% uniquestates){
    reloutcome <- rename(reloutcome, MRbyHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                         MRbyHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                         MRbyP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    reqstatedata <- filter(reloutcome, State == state)
    if(outcome == "heart attack"){ 
      reqoutcome <- subset(reqstatedata, !is.na(MRbyHA))
      ordered <- arrange(reqoutcome,MRbyHA,Hospital.Name)
    }
    else if(outcome == "heart failure"){
      reqoutcome <- subset(reqstatedata, !is.na(MRbyHF))
      ordered <- arrange(reqoutcome,MRbyHF,Hospital.Name) 
      
    }
    else {
      reqoutcome <- subset(reqstatedata, !is.na(MRbyP))
      ordered <- arrange(reqoutcome,MRbyP,Hospital.Name)
      
    }
    if(num == "best"){
      num = 1
    }
    else if(num == "worst"){
      num = length(ordered[,1])
      
    }
  }
  len <- length(ordered[,1])
  if(num > len){
    print(NA)
  } else { print(ordered[num,1])}
}