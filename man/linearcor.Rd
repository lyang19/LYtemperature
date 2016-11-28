\name{linearcor}
\alias{linearcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Correlation
}
\description{
This function examines and plots the linear correlation between 2 variables.
}
\usage{
linearcor(x,y,dataset,xmin,xmax,xby)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{the independent variable in the model
}
\item{y}{the dependent variable in the model
}
\item{dataset}{the data set used in the model
}
\item{xmin}{the minimum value of the independent variable
}
\item{xmax}{the maximum value of the independent variable
}
\item{xby}{the step size in linear model
}
}
\details{
This function does not specify x label, y label and the plot title. These need to be added later.
}

\author{
Luoying Yang
}

\examples{
##Use the data set 'mtcars' to try the function

## The function is currently defined as
linearcor<-function(x,y,dataset,xmin,xmax,xby){
  plot(x,y)
  lm1<-lm(formula(x~y),data = dataset)
  summary(lm1)
  xs<-seq(from=xmin,to=xmax,by=xby)
  ys<-coef(lm1)[1]+coef(lm1)[2]*xs
  lines(xs,ys,lty=1,lwd=2)
  lines(loess.smooth(x=temperaturedata$Morning,temperaturedata$Breakfast), lty=2, lwd=2)
  legend("topleft", lty=1:2, lwd=2, c("linear", "loess"))
}
x<-mtcars$mpg
y<-mtcars$cyl
xmin<-min(mtcars$mpg)
xmax<-max(mtcars$mpg)
xmin<-as.character(xmin)
xmin<-as.numeric(xmin)
xmax<-as.character(xmax)
xmax<-as.numeric(xmax)
xby<-(xmax-min)/10
linearcor(x,y,xmin,xmax,xby)
}
