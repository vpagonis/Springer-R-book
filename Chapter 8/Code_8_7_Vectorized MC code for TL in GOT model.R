# GOT MODEL- Monte Carlo code for TL 
rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
library(lamW)
mcruns<-100
n0<-500
N<-1000
s<-1e12
E<-1
R<-0.6
kb<-8.617e-5
tmax<-200
deltat<-1
times<-seq(1,tmax,deltat)
nMatrix<-TLMatrix<-matrix(NA,nrow=length(times),ncol=mcruns)
nMC<-TL<-rep(NA,length(times))
system.time(
  for (j in 1:mcruns){
    n<-n0
    for (t in 1:length(times)){
      vec<-rep(runif(n))
      P<-s*exp(-E/(kb*(t+273)))*n/((N-n)*R+n)
      n<-length(vec[vec>P])
      nMC[t]<-n
      TL[t]<-n*P}
    nMatrix[,j]<-nMC
    TLMatrix[,j]<-TL
  })
#Find average of n(t), average TL signal and CV[%]
avgn<-rowMeans(nMatrix)
avgTL<-rowMeans(TLMatrix)
sd<-rowSds(TLMatrix)
cv<-100*sd/avgTL
# plots
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,2,1)
k<-function(u) {integrate(function(p){exp(-E/(kb*p))},
                          300,u)[[1]]}
x1<-300:450
y1<-lapply(x1,k)
x<-unlist(x1)
y<-unlist(y1)
c<-(n0/N)*(1-R)/R
zTL<-(1/c)-log(c)+(s*n0/(c*N*R))*y
plot(times,avgn,ylab="Remaining electrons n(t)",
     col=2,xlab=expression("Temperature ["^"o"*"C]"),ylim=c(0,700))
lines(x-273,(N*R/(1-R))/(lambertW0(exp(zTL))),col=1)
legend("topright",bty="n",c("(a)    n(t)"," ","MC",
                            "Lambert Eq."),pch=pch,lty=lty,col=col)
plot(times,avgTL,ylim=c(0,14),ylab="TL",
     col=2,xlab=expression("Temperature ["^"o"*"C]"))
# plots
lines(x-273,(N*R/((1-R)^2))*s*exp(-E/(kb*x))/
        (lambertW0(exp(zTL))+lambertW0(exp(zTL))^2),col=1)
legend("topleft",bty="n",c("(b)    TL"," ","MC",
                           "Lambert Eq."), pch=pch,lty=lty,col=col)
plot(times,cv,ylab="CV[%]",ylim=c(0,120),
     col=2,xlab=expression("Temperature ["^"o"*"C]"))
legend("topleft",bty="n",c("(c)    CV[%]"," ","MC"))