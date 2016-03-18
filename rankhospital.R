## function rankhospital
## @param state   : 2 char abbreviated name of a state
## @param outcome : outcome (describes medical condition)
## @param num     : number ranking
## @return        : a  character  vector  with the name
##                  of  the hospital that has the  best 
##                  (i.e. lowest)  30-day mortality for 
##                  the specified outcome.in that state

## usage 
## source("best.R")
## best("TX", "heart failure")
rankhospital <- function(state, outcome, num){
  
  ## guard :: is valid if it matches (“heart attack”, “heart failure”, or “pneumonia”)
  outcomeFactor <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% outcomeFactor) stop("invalid outcome");
  
  ## return the nth row of the frame
  sortedFrame <- getSortedFrame("outcome-of-care-measures.csv", c("State", "Hospital.Name"), state, outcome)
  
  ## clean NA values from condition
  sortedFrameCleaned <- sortedFrame[complete.cases(sortedFrame[,length(sortedFrame),]),];
  
  ## num can take "best", "worst", or integer for the rank
  if(length(num) < 1 || length(num) > 1 || num > length(sortedFrameCleaned)) NA
  
  ## check if num == "best" || "worst"
  if(identical(num , "best")){
    num = 1
  }else if(identical(num , "worst")){
    message(nrow(sortedFrameCleaned))
    num <- nrow(sortedFrameCleaned)
  }
  
  ## translate into int or NA
  num = as.integer(num)
  if(is.na(num)) stop("invalid num")
  
  as.character(sortedFrameCleaned[num,"Hospital.Name"])
  
}

## function : getSortedFrame
## @param   : datafile - name of file
## @param   : columns - the cols to return
## @param   : state to check
## @param   : outcome is human readable outcome

## @return  : a sorted frame with specified columns and outcome columns
## example :: getSortedFrame("outcome-of-care-measures.csv" , c("State","Hospital.Name",columnToQuery), state, outcome)

getSortedFrame <- function(dataFile, columns, state, outcome){
  
  ## Read outcome data
  
  if(!file.exists(dataFile)) stop("no file found")
  
  dtafrm <- read.csv(dataFile)
  
  ## Check that state is valid
  if(!isTRUE(which(state%in%state.abb) > 0)){
    stop("invalid state")
  }
  
  ## connect the dots
  outcome <- gsub(" ", ".", outcome)
  
  ## get the name of the col to return
  columnToQuery <-  grep(paste("^", "Hospital.30.Day.Death.", ".*", outcome, "$", sep=""),  names(dtafrm), value = TRUE, ignore.case = TRUE)
  
  ##set the column as numeric
  dtafrm[,columnToQuery] <- suppressWarnings(as.numeric(as.character(dtafrm[,columnToQuery])))
  
  ##find data by state and subset
  dtafrm <- dtafrm[ which(dtafrm$State == state),][,c(columns, columnToQuery)]
  
  ##get the sorted frame
  with(dtafrm, dtafrm[order(dtafrm[,columnToQuery], dtafrm$State, dtafrm$Hospital.Name, decreasing = FALSE), ])
  
}