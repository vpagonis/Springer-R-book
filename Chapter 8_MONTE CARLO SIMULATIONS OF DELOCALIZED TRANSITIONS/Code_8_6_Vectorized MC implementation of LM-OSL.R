rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
mcruns<-100
n0<-500
tmax<-60
A<-0.2
deltat<-1
times<-seq(1,tmax,deltat)
nMatrix<-LMMatrix<-matrix(NA,nrow=length(times),ncol=mcruns)
nMC<-LM<-rep(NA,length(times))
system.time(
  for (j in 1:mcruns){
    n<-n0
    for (t in 1:length(times)){
      vec<-rep(runif(n))
      P<-deltat*t*A/tmax
      n<-length(vec[vec>P])
      nMC[t]<-n
      LM[t]<-n*P}
    nMatrix[,j]<-nMC
    LMMatrix[,j]<-LM
  })
#Find average of n(t),LM-OSL signal and CV[%]
avgn<-rowMeans(nMatrix)
avgLM<-rowMeans(LMMatrix)
sd<-rowSds(LMMatrix)
cv<-100*sd/avgLM
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,2,1)
plot(times,avgn,ylab="Remaining electrons n(t)",
     xlab="Time [s]",ylim=c(0,700),col=2)
curve(n0*exp(-A*x^2/(2*tmax)),0,tmax,add=TRUE,
      col=1,lwd=2)
legend("topright",bty="n",c("(a)    ",
                            " ","MC","Analytical"),
       pch=pch,lty=lty,col=col)
plot(times,avgLM,ylab="LMOSL",xlab="Time [s]",
     col=2,ylim=c(0,24))
legend("topright",bty="n",c("(b)    LM-OSL"," ","MC",
                            "Analytical"), pch=pch,lty=lty,col=col)
curve(A*n0*exp(-A*x^2/(2*tmax))*x/tmax,0,tmax,add=TRUE,
      col=1,lwd=2)
plot(times,cv,ylab="CV[%]",xlab="Time [s]",ylim=c(0,150),
     col=2)
curve(100*sqrt((exp(A*x^2/(2*tmax))-1)/n0),0,tmax,add=TRUE,
      col=1,lwd=2)
legend("topleft",bty="n",c("(c)    CV[%]"," ","MC (M=100)",
                           "Analytical"), pch=pch,lty=lty,col=col)