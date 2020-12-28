rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
mcruns<-100
n0<-500
s<-1e12
E<-1
kb<-8.617e-5
tmax<-150
deltat<-1
times<-seq(0,tmax,deltat)
nMatrix<-TLMatrix<-matrix(NA,nrow=length(times),ncol=mcruns)
nMC<-TL<-rep(NA,length(times))
system.time(
  for (j in 1:mcruns){
    n<-n0
    for (t in 1:length(times)){
      vec<-rep(runif(n))
      P<-s*exp(-E/(kb*(t+273)))*deltat
      n<-length(vec[vec>P])
      nMC[t]<-n
      TL[t]<-n*P}
    nMatrix[,j]<-nMC
    TLMatrix[,j]<-TL
  })
#Find average n(t),average CW-OSL signal and CV[%]
avgn<-rowMeans(nMatrix)
avgTL<-rowMeans(TLMatrix)
sd<-rowSds(TLMatrix)
cv<-100*sd/avgTL
## Calculate the analytical error of TL in first order peak
x1<-times+273
k<-function(u) {integrate(function(p){s*exp(-E/(kb*p))},
                          273,u)[[1]]}
y1<-lapply(x1,k)
x<-unlist(x1)
y<-unlist(y1)
errn<-sqrt(n0*(exp(-y)-exp(-2*y)))
nanalyt<-n0*exp(-y)
TLanalyt<-n0*s*exp(-E/(kb*x))*exp(-y)
# plots
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,2,1)
plot(times,avgn,ylab="Remaining electrons n(t)",
     col=2,xlab=expression("Temperature ["^"o"*"C]"),ylim=c(0,700))
lines(x-273,nanalyt,col=1)
legend("topleft",bty="n",c("(a)    n(t)"," ","MC",
                           "Lambert Eq."),pch=pch,lty=lty,col=col)
plot(times,avgTL,ylab="TL",
     col=2,xlab=expression("Temperature ["^"o"*"C]"),ylim=c(0,23))
lines(x-273,TLanalyt,col=1)
legend("topleft",bty="n",c("(b)    TL"," ","MC",
                           "Lambert Eq."),pch=pch,lty=lty,col=col)
plot(times,cv,ylab="CV[%]",ylim=c(0,150),
     col=2,xlab=expression("Temperature ["^"o"*"C]"))
lines(x-273,100*errn*s*exp(-E/(kb*x))/TLanalyt,col=1)
legend("topleft",bty="n",c("(c)    CV[%]"," ","MC",
                           "Analytical"), pch=pch,lty=lty,col=col)