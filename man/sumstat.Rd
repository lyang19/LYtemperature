\name{sumstat}
\alias{sumstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summary Statistics
}
\description{
This function calcualtes mean, median, range and the standard deviation of each column of the input matrix/array/dataframe. It requires the input argument to be matrix/array/dataframe.
}
\usage{
sumstat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The matrix/array/dataframe that needs to be calculated. If class(x) does not match these three data types the function will display a warning message.
}
}

\value{
\item{sumst}{Sumst is a matrix of the column means, column median, column range and column standard deviation.}
}

\author{
Luoying Yang
}

\examples{

##Use the data set 'mtcars' to try the function

## The function is currently defined as
sumstat<-function(x){
  if (class(x)!='matrix'|class(x)!='array'|class(x)!='data.frame'){
  n<-ncol(x)
  sumst<-matrix(nrow = 4, ncol = n)
  for (i in 1:n) {
    sumst[1,i]<-mean(x[,i],na.rm = TRUE)
    sumst[2,i]<-median(x[,i],na.rm = TRUE)
    sumst[3,i]<-max(x[,i])-min(x[,i],na.rm = TRUE)
    sumst[4,i]<-sd(x[,i],na.rm = TRUE)
  }
  rownames(sumst)<-c('mean','median','range','standard deviation')
  return(sumst)
  }
  else
  {print('WARNING: Your input is invalid.')}
}
sumstat(mtcars)


\keyword{summarystatistics}
