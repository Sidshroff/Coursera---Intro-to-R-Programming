best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", na.string = "Not Available", stringsAsFactors = FALSE)
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
      answer <- reqoutcome$Hospital.Name[which.min(reqoutcome$MRbyHA)]
      }
    else if(outcome == "heart failure"){ 
      reqoutcome <- subset(reqstatedata, !is.na(MRbyHF))
      answer <- reqoutcome$Hospital.Name[which.min(reqoutcome$MRbyHF)]
      }
    else{ 
      reqoutcome <- subset(reqstatedata, !is.na(MRbyP))
      answer <- reqoutcome$Hospital.Name[which.min(reqoutcome$MRbyP)]
      }
    
    if(length(answer) > 1){
      answer_sort <- sort(answer)
      print(answer_sort[1])
    }
    
    else
    { print(answer)}
  }
} 