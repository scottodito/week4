## function best
## @param state   : 2 char abbreviated name of a state
## @param outcome : outcome (describes medical condition)
## @return        : a  character  vector  with the name
##                  of  the hospital that has the  best 
##                  (i.e. lowest)  30-day mortality for 
##                  the specified outcome.in that state

## usage 
## source("best.R")
## best("TX", "heart failure")

best <- function(state, outcome) {
  
  ## Read outcome data
  
  dataFile = "outcome-of-care-measures.csv";
  if(!file.exists(dataFile)) stop("no file found")
    
  dtafrm <- read.csv(dataFile)
  
  ## Check that state is valid
  if(!isTRUE(which(state%in%state.abb) > 0)){
    stop("invalid state")
  }
  
  ## guard :: is valid if it matches (“heart attack”, “heart failure”, or “pneumonia”)
  outcomeFactor <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% outcomeFactor) stop("invalid outcome");
  
  ## connect the dots
  outcome <- gsub(" ", ".", outcome)
  
  ## get the name of the col to return
  columnToQuery <-  grep(paste("^", "Hospital.30.Day.Death.", ".*", outcome, "$", sep=""),  names(dtafrm), value = TRUE, ignore.case = TRUE) 

  ##set the column as numeric
  dtafrm[,columnToQuery] <- suppressWarnings(as.numeric(as.character(dtafrm[,columnToQuery])))
  
  ##find and sort data by state and subset
  dtafrm <- dtafrm[ which(dtafrm$State == state),][,c("State","Hospital.Name",columnToQuery)]
  
  ##get the sorted frame
  winner <- with(dtafrm, dtafrm[order(dtafrm[,columnToQuery], dtafrm$State, decreasing = FALSE), ])
  
  ##return the string of the hospital name
  as.character(winner[1, "Hospital.Name"])
  
}