corr <- function(directory, threshold = 0){
  setwd("~/Coursera/Data Science/R Programming")
  setwd(directory)
  comp <- complete(directory)
  good <- comp$id[comp$nobs > threshold]
  dirs <- dir()
  corrs <- c()
  for(i in good){
    f <- read.csv(dirs[i])
    x <- f$sulfate
    y <- f$nitrate
    x1 <- x[!is.na(x) & !is.na(y)]
    y1 <- y[!is.na(x) & !is.na(y)]
    corrs <- c(corrs, cor(x1,y1))
  }
  return(corrs)
}