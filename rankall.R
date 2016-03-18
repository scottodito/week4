## function rankall
## @param outcome   : describes medical condition we check
## @param num       : the number of ranking that we want
## @return          : a  data frame with the hospital names
##                  and state and ranking as outcome num
##                  (i.e. lowest)  30-day mortality for 
##                  the specified outcome.in that state

## usage 
## source("rankall.R")
## rankall("heart attack", 20)

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  dataFile <-"outcome-of-care-measures.csv" 
  if(!file.exists(dataFile)) stop("no file found")
  dtafrm <- read.csv(dataFile)
  
  message(head(dtafrm, 2))
  ## connect outcome with dot
  outcome <- gsub(" ", ".", outcome)
  ## Check that state is valid
  ## Check that state and outcome are valid
  outcomeColName <-  grep(paste("^", "Hospital.30.Day.Death.", ".*", outcome, "$", sep=""),  names(dtafrm), value = TRUE, ignore.case = TRUE)
  if(!length(outcomeColName)) stop("invalid outcome")
  
  
  ##subset with cols
  ##dtafrm[,c("Hospital.Name", "State", outcomeColName)]
  ##cast as numeric
  ##dtafrm[,outcomeColName] <- suppressWarnings(as.numeric(as.character(dtafrm[,outcomeColName])))
  ##remove na cases
  ##dtafrm <- dtafrm[complete.cases(dtafrm[, c("Hospital.Name", "State", outcomeColName)]),];
  
  dtafrm
  
  # isWorst = FALSE
  # ## check for string "best" || "worst"
  # if(identical(num , "best")){
  #   num = 1
  # }else if(identical(num , "worst")){
  #   isWorst = TRUE
  # }else{
  #   ## cast as int or na
  #   num = as.integer(num)
  #   if(is.na(num)) stop("invalid num")
  # }
  # 
  # ## For each state, find the hospital of the given rank
  # states <- split(dtafrm, dtafrm$State)
  # 
  # head(states)
  # 
  
  ##iterate and create df of states
  # returnFrame <- NULL;
  # for(state in states) {
  #   
  #   if(!isTRUE(which(state[1,"State"]%in%state.abb) > 0)){
  #     next()
  #   }
  #   
  #   ## message(state[num,"State"])
  #   ## message(state[num,outcomeColName])
  #   ## if it is worst we find the last row of the state
  #   if(isWorst) {
  #     row <- state[nrow(state),]
  #   }else{
  #     row <- state[num,]
  #   }
  #   rbind(returnFrame,row)->returnFrame
  #   
  # }
  # 
  # str(returnFrame)
  

  
  
  ## Return a data frame with the hospital names and the
  
  ## (abbreviated) state name
}
