#  Stochastic CW-OSL process using KMC
rm(list = ls(all=T))
library(matrixStats)
mcruns<-100
n0<-500
mu<-.03
tmax<-100
times<-(1:tmax)
nMatrix<-matrix(NA,nrow=tmax-1,ncol=mcruns)
system.time(
  for (k in 1:mcruns)
  {n<-n0
  allt<-t<-0.5
  for(i in 1:n){
    P<-mu
    t<-t+rexp(1)/(P*n)
    n<-n-1
    allt<- rbind(allt,t)
  }
  depth.class <- cut(allt,times, include.lowest = TRUE)
  singlerun <- tapply(seq(from=n0,to=0,by=-1), depth.class, 
                      mean,na.rm = FALSE)
  singlerun<-as.vector(singlerun)
  nMatrix[,k]<-singlerun 
  })
par(mfrow=c(1,3))
matplot(nMatrix,typ="l",col="green",ylim=c(0,700),
        xlab="Time [s]",ylab="Remaining electrons")
timesMC<-seq(from=1,to=tmax-1,by=1)
avgn<-rowMeans(nMatrix,na.rm=TRUE)
avgCWOSL<-mu*avgn
sd<-rowSds(nMatrix,na.rm=TRUE)
cv<-100*sd/avgn
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,"black","red")
lines(timesMC,avgn,ylab="Remaining electrons n(t)",
      xlab="Time [s]")
curve(n0*exp(-P*x),from=0,to=150,add=TRUE,col="red",lwd=2)
legend("topright",bty="n",c("(a)  n(t)"," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
plot(timesMC,avgCWOSL,ylab="CWOSL",xlab="Time [s]",ylim=c(0,21))
legend("topright",bty="n",c("(b)  CW-OSL"," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
curve(n0*mu*exp(-mu*x),from=0,max(times),add=TRUE,
      col="red",lwd=2)
plot(timesMC,cv,ylab="CV[%]",xlab="Time [s]",ylim=c(0,40))
curve(100*sqrt((exp(mu*x)-1)/n0),0,max(times),add=TRUE,
      col="red",lwd=2)
legend("topleft",bty="n",c("(c)  CV[%]"," ","MC (M=100)",
                           "Analytical"),pch=pch,lty=lty,col=col)