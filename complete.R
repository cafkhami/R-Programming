complete <- function(directory, id = 1:332){
  setwd("~/Coursera/Data Science/R Programming")
  setwd(directory)
  dirs <- dir()
  x <- data.frame()
  
  for (i in id){
    f <- read.csv(dirs[i])
    s <- !is.na(f$sulfate)
    n <- !is.na(f$nitrate)
    
    nob <- sum(s & n)
    x <- rbind(x, c(i,nob))
  }
  colnames(x) <- c("id","nobs")
  return(x)
}