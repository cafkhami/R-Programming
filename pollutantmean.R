pollutantmean <- function(directory, pollutant, id = 1:332){
  setwd("~/Coursera/Data Science/R Programming")
  setwd(directory)
  csvs <- dir()
  temp2 <- c()
  for(i in id){
    x <- read.csv(csvs[i])
    temp <- x[!is.na(x[,pollutant]),pollutant]
    temp2 <- c(temp2,temp)
  }
  return(mean(temp2))
}