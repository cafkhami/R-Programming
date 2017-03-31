rankhospital <- function(state, outcome, rank = "best"){
  outcome_table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out <- outcome == c("heart attack","heart failure","pneumonia")
  
  if (!(state %in% outcome_table[,7])) stop("invalid state")
  outcome_table <- outcome_table[outcome_table[,7]==state,]
  if (sum(out)!=1) stop("invalid outcome")
  
  outcome_table[,11] <- as.numeric(outcome_table[,11])
  outcome_table[,17] <- as.numeric(outcome_table[,17])
  outcome_table[,23] <- as.numeric(outcome_table[,23])
  
  
  if(out[1]==TRUE){
    m <- outcome_table[order(outcome_table[,11],outcome_table[,2]),]
    if (rank == "worst") rank <- sum(!is.na(m[,11]))
  }
  if(out[2]==TRUE){
    m <- outcome_table[order(outcome_table[,17],outcome_table[,2]),]
    if (rank == "worst") rank <- sum(!is.na(m[,17]))
  }
  if(out[3]==TRUE){
    m <- outcome_table[order(outcome_table[,23],outcome_table[,2]),]
    if (rank == "worst") rank <- sum(!is.na(m[,23]))
  }
  if (rank == "best") rank <- 1
  if (rank > nrow(m)) return(NA)
  return(m[rank,2])
}