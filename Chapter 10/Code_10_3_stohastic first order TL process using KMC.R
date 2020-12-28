# Stochastic first order TL process using KMC
rm(list = ls(all=T))
library(matrixStats)
mcruns<-100
n0<-500
tmax<-110
s<-1e13
E<-1
kb<-8.617e-5

times<-seq(1,tmax-1,1)
nMatrix<-matrix(NA,nrow=tmax-1,ncol=mcruns)

system.time(
  for (k in 1:mcruns)
  {n<-n0
  allt<-t<-30
  for(i in 1:n-1){
    P<-s*exp(-E/(8.617e-5*(t+273)))
    t<-t+rexp(1)/(P*n)
    n<-n-1
    allt<- rbind(allt,t)
  }
  depth.class <- cut(allt,seq(1:tmax), include.lowest = TRUE)
  singlerun <- tapply(seq(from=n0,to=0,by=-1), depth.class, 
                      mean,na.rm = FALSE)
  singlerun<-as.vector(singlerun)
  nMatrix[,k]<-singlerun
  })
# Calculate analytical solution
x1<-times+273
k<-function(u) {integrate(function(p){s*exp(-E/(kb*p))},
                          273,u)[[1]]}
y1<-lapply(x1,k)
x<-unlist(x1)
y<-unlist(y1)
errn<-sqrt(n0*(exp(-y)-exp(-2*y)))
nanalyt<-n0*exp(-y)
TLanalyt<-n0*s*exp(-E/(kb*x))*exp(-y)
# plot MC and analytical
par(mfrow=c(1,3))
matplot(nMatrix,typ="l",col="green",xlim=c(30,100),
        ylab="Remaining electrons",
        ylim=c(0,700),xlab=expression("Temperature ["^"o"*"C]"))
avgn<-rowMeans(nMatrix,na.rm = TRUE)
cv<-100*rowSds(nMatrix,na.rm=TRUE)/avgn
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,"black","red")
lines(seq(from=1,to=tmax-1,by=1),avgn,lwd=2,col="black",
      typ="p",pch=1)
legend("topright",bty="n",c("(a)  n(t)"," ","MC",
                            "Analyt."),pch=pch,lty=lty,col=col)
lines(x-273,nanalyt,col="red",lwd=2)
k<-function(u){s*exp(-E/(kb*(u+273)))}
y<-lapply(times,k)
TLMC<-as.numeric(y)*as.numeric(avgn)
plot(times,TLMC,col="black",ylab="TL",
     ylim=c(0,23),xlab=expression("Temperature ["^"o"*"C]"))
lines(x-273,TLanalyt,col="red",lwd=2)
legend("topleft",bty="n",c("(b)  TL"," ","MC",
                           "Analyt."), pch=pch,lty=lty,col=col)
plot(times,cv,ylab="CV[%]",ylim=c(0,150),col="blue",
     xlab=expression("Temperature ["^"o"*"C]"))
legend("topleft",bty="n",c("(c)  CV[%]"," ","MC"))