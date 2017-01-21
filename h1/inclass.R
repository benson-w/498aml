# setwd('~/Current/Courses/LearningCourse/Pima')
wdat<-read.csv('pima-indians-diabetes.data.txt', header=FALSE)
library(klaR)
library(caret)

# negative sign leaves everything except for the 9th column
bigx<-wdat[,-c(9)] 

# only leaves 9th column
bigy<-wdat[,9]

# 
trscore<-array(dim=10)

tescore<-array(dim=10)
for (wi in 1:10)
{
  wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
  nbx<-bigx
  ntrbx<-nbx[wtd, ]
  ntrby<-bigy[wtd]
  
  # is y positive?
  trposflag<-ntrby>0
  
  # select positive 
  ptregs<-ntrbx[trposflag, ]
  # select negative 
  ntregs<-ntrbx[!trposflag,]
  
  ntebx<-nbx[-wtd, ]
  nteby<-bigy[-wtd]
  
  # na.rm true to detect unknown values
  # calculate mean and standard deviation
  ptrmean<-sapply(ptregs, mean, na.rm=TRUE)
  ntrmean<-sapply(ntregs, mean, na.rm=TRUE)
  ptrsd<-sapply(ptregs, sd, na.rm=TRUE)
  ntrsd<-sapply(ntregs, sd, na.rm=TRUE)
  
  # compute the distance from sample points to data, positive training samples
  ptroffsets<-t(t(ntrbx)-ptrmean)
  ptrscales<-t(t(ptroffsets)/ptrsd)
  ptrlogs<--(1/2)*rowSums(apply(ptrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ptrsd))
 
  # same thing but for negative samples
  ntroffsets<-t(t(ntrbx)-ntrmean)
  ntrscales<-t(t(ntroffsets)/ntrsd)
  ntrlogs<--(1/2)*rowSums(apply(ntrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ntrsd))
  
  # compare if one side is bigger than the other side
  lvwtr<-ptrlogs>ntrlogs
  gotrighttr<-lvwtr==ntrby
  trscore[wi]<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))
  pteoffsets<-t(t(ntebx)-ptrmean)
  ptescales<-t(t(pteoffsets)/ptrsd)
  ptelogs<--(1/2)*rowSums(apply(ptescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ptrsd))
  nteoffsets<-t(t(ntebx)-ntrmean)
  ntescales<-t(t(nteoffsets)/ntrsd)
  ntelogs<--(1/2)*rowSums(apply(ntescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ntrsd))
  lvwte<-ptelogs>ntelogs
  gotright<-lvwte==nteby
  tescore[wi]<-sum(gotright)/(sum(gotright)+sum(!gotright))
}