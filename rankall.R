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
  
  ## connect outcome with dot
  outcome <- gsub(" ", ".", outcome)
  ## Check that state and outcome are valid
  outcomeColName <-  grep(paste("^", "Hospital.30.Day.Death.", ".*", outcome, "$", sep=""),  names(dtafrm), value = TRUE, ignore.case = TRUE)
  if(!length(outcomeColName)) stop("invalid outcome")

  ## cast as numeric
  dtafrm[,outcomeColName] <- suppressWarnings(as.numeric(as.character(dtafrm[,outcomeColName])))
  
  ## order the frame
  dtafrm <- with(dtafrm, dtafrm[order(dtafrm[,outcomeColName], dtafrm$Hospital.Name, decreasing = FALSE),])
  
  ## subset to the name and state
  dtafrm2 <- dtafrm[, c("Hospital.Name", "State")]
  
  isWorst = FALSE
  ## check for string "best" || "worst"
  if(identical(num , "best")){
    num = 1
  }else if(identical(num , "worst")){
    isWorst = TRUE
  }else{
    ## cast as int or na
    num = as.integer(num)
    if(is.na(num)) stop("invalid num")
  }

  ## For each state, find the hospital of the given rank
  states <- split(dtafrm2, dtafrm$State)
  
  ##iterate and create df of states
  returnFrame <- data.frame("hospital" = character(), "state" = character(), stringsAsFactors = FALSE);
  
  ##iterate the split DF
  for(state in states) {

    ## if it is worst we find the last row of the state
    if(isWorst) {
      num = nrow(state)
    }
    
    ## check that the state has this many hospitals to rank
    if(nrow(state) < num){
      row <- data.frame( "hospital"=NA, "state"=state[1, 'State'], row.names = state[1, 'State'] )
    } else {
      row <- data.frame(  "hospital"=state[num,"Hospital.Name"],  "state"=state[num,"State"], row.names = state[1, 'State']);
    }
    
    ## add back to the data frame
    returnFrame <- rbind(returnFrame,row)

  }

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  returnFrame

}