###Function that coverts data into numeric value
convertnumeric<-function(x){
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



###Function that used to calculate mean, median, range and standard deviation of a data set.

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

###Function that plot linear correlation

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

