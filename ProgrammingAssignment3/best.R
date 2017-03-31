best <- function(state, outcome){
  outcome_table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out <- outcome == c("heart attack","heart failure","pneumonia")
  
  if (!(state %in% outcome_table[,7])) stop("invalid state")
  outcome_table <- outcome_table[outcome_table[,7]==state,]
  if (sum(out)!=1) stop("invalid outcome")
  
  outcome_table[,11] <- as.numeric(outcome_table[,11])
  outcome_table[,17] <- as.numeric(outcome_table[,17])
  outcome_table[,23] <- as.numeric(outcome_table[,23])
  
  if(out[1]==TRUE){
    m <- min(outcome_table[!is.na(outcome_table[,11]), 11])
    res <- sort(outcome_table[outcome_table[,11]==m,2])
  }
  if(out[2]==TRUE){
    m <- min(outcome_table[!is.na(outcome_table[,17]), 17])
    res <- sort(outcome_table[outcome_table[,17]==m,2])
  }
  if(out[3]==TRUE){
    m <- min(outcome_table[!is.na(outcome_table[,23]), 23])
    res <- sort(outcome_table[outcome_table[,23]==m,2])
  }
  return(res[1])
}