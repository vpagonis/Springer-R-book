# Vectorized MC code for localized TL transitions (LT model)
rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
library(lamW)
mcruns<-100
n0<-500
s<-1e13
E<-1
kb<-8.617e-5
r<-1e2
tmax<-120
deltat<-1
times<-seq(1,tmax,deltat)  #heating rate=1 K/s
nMatrix<-TLMatrix<-matrix(NA,nrow=length(times),ncol=mcruns)
nMC<-TL<-rep(NA,length(times))
system.time(
  for (j in 1:mcruns){
    n<-n0
    for (t in 1:length(times)){
      vec<-rep(runif(n))
      P<-s*exp(-E/(kb*(t+273)))*n/(r+n)*deltat
      n<-length(vec[vec>P])
      nMC[t]<-n
      TL[t]<-nMC[t]*P}
    nMatrix[,j]<-nMC
    TLMatrix[,j]<-TL })
#Find average avgn, average TL signal,CV[%]
avgn<-rowMeans(nMatrix)
avgTL<-rowMeans(TLMatrix)
sd<-rowSds(TLMatrix)
cv<-100*sd/avgTL
# plots
par(mfrow=c(1,2))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,"black","red")
plot(times,avgTL,ylab="TL",
     xlab=expression("Temperature ["^"o"*"C]"))
# plot analytical solution
k<-function(u) {integrate(function(p){exp(-E/(kb*p))},
                          300,u)[[1]]}
y1<-lapply(times+273,k)
x<-unlist(273+times)
y<-unlist(y1)
zTL<-(r/n0)-log(n0/r)+(s*y)
lines(x-273,r*s*exp(-E/(kb*x))/(lambertW0(exp(zTL))
                                +lambertW0(exp(zTL))^2),type="l",col="red")
legend("topleft",bty="n",c("(a)  TL"," ","MC",
                           "Analyt."),pch=pch,lty=lty,col=col)
plot(times,cv,ylab="(b)  CV[%]",ylim=c(0,120),
     xlab=expression("Temperature ["^"o"*"C]"))
legend("topleft",bty="n",c("CV[%]"," ","MC"), 
       pch=pch,lty=lty,col=col)