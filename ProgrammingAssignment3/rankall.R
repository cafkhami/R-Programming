rankall <- function(outcome, rank="best"){
  
  outcome_table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- sort(unique(outcome_table[,7]))
  out <- outcome == c("heart attack","heart failure","pneumonia")
  result <- data.frame()
  if (sum(out)!=1) stop("invalid outcome")
  
  outcome_table[,11] <- as.numeric(outcome_table[,11])
  outcome_table[,17] <- as.numeric(outcome_table[,17])
  outcome_table[,23] <- as.numeric(outcome_table[,23])
  
  if(out[1]==TRUE){
    m <- outcome_table[order(outcome_table[,11],outcome_table[,2]),]
    if (rank == "worst") rank <- sum(!is.na(m[,11]))
  }
}