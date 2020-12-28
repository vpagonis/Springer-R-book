# Vectorized MC code for first-order CW-OSL process
rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
mcruns<-300
n0<-500
mu<-.03
deltat<-1
tmax<-100
times<-seq(1,tmax,deltat)
nMatrix <-  matrix(NA, nrow = length(times), ncol = mcruns)
nMC<-rep(NA,length(times))
system.time(
  for (k in 1:mcruns){
    n<-n0             #initialize each of the M=100 MC runs
    for (t in 1:length(times)){
      vec<-rep(runif(n))    #create a vector vec,
      #containing n random numbers between 0 and 1
      P<-mu*deltat
      n<-length(vec[vec>P]) #if the random number in vec is >P,
      #then the corresponding electron survives 
      nMC[t]<-n } # store number of electrons n in the vector nMC
    nMatrix[,k]<-nMC # store single run in column k of nMatrix 
  })
#Find average of n(t), CW-OSL signal, and CV[%]
avgn<-rowMeans(nMatrix)
avgCWOSL<-mu*rowMeans(nMatrix)
sd<-rowSds(nMatrix)
cv<-100*sd/avgn
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,2,1)
plot(times,avgn,ylab="Remaining electrons n(t)",
     xlab="Time [s]",ylim=c(0,700),col=2)
curve(n0*exp(-mu*x),0,tmax,add=TRUE,col=1,lwd=2)
legend("topright",bty="n",c("(a)    n(t)"," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
plot(times,avgCWOSL,ylab="CWOSL",xlab="Time [s]",ylim=c(0,20),
     col=2)
legend("topright",bty="n",c("(b)    CW-OSL"," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
curve(n0*mu*exp(-mu*x),0,tmax,add=TRUE,col=1,lwd=2)
plot(times,cv,ylab="CV[%]",xlab="Time [s]",ylim=c(0,27),col=2)
curve(100*sqrt((exp(mu*x)-1)/n0),0,max(times),add=TRUE,
      col=1,lwd=2)
legend("topleft",bty="n",c("(c)    CV[%]"," ","MC (M=100)",
                           "Analytical"),pch=pch,lty=lty,col=col)