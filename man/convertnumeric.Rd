\name{convertnumeric}
\alias{Convertnumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert into numeric values
}
\description{
This function convert entries in the dataset to be numeric values.It requires the input argument to be matrix/array/dataframe.
}
\usage{
convertnumeric(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The matrix/array/dataframe that needs to be converted. If class(x) does not match these three data types the function will display a warning message.
}
}

\value{
\item{x}{
the matrix/array/dataframe is the same as the input argument, the only difference is that the entries are all numeric.
}
}

\author{
Luoying Yang
}

\examples{
##Use the 'sleep' data set to try the function
str(sleep)
##
There are 2 columns of data that are factor. The function will convert them to be numeric
## The function is currently defined as
convertnumeric<-function (x)
{
if (class(x)=='matrix'|class(x)=='array'|class(x)=='data.frame')
  {n<-ncol(x)
  for (i in 1:n) {
    x[,i]<-as.character(x[,i])
    x[,i]<-as.numeric(x[,i])
  }

  return(x)
  }
  else{print('WARNING: Your input is invalid.')}
  }

sleepnew<-convertnumeric(sleep)
str(sleepnew)
##now they are all numeric.

##If your input is only a vector(not matrix/array/dataframe)
str(lh)
##The function will print a warning message
convertnumeric(lh)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{convertnumericvalue}

