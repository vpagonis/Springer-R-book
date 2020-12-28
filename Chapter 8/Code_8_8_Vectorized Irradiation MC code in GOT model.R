#Vectorized Irradiation MC code in GOT model
rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
library(lamW)
mcruns<-100
deltat<-1
tmax<-1000
Dvalues<-seq(1,tmax,deltat)
nMatrix <-  matrix(NA, nrow = length(Dvalues), ncol = mcruns)
nMC<-rep(NA,length(Dvalues))
N<-100
n0<-1
R<-1.2
system.time(
  for (k in 1:mcruns){
    n<-n0               #initialize each of the M=100 MC runs
    for (t in Dvalues){
      vec<-rep(runif(n))    #create a vector vec,
      #containing n random numbers between 0 and 1
      P<-R*(N-n)*(1/n)/(R*(N-n)+n)*deltat
      dn<-length(vec[vec<P]) # if the random # in vec is <P,
      n<-n+dn #then increase the number of filled traps 
      nMC[t]<-n+dn } #store number of electrons n in vector nMC
    nMatrix[,k]<-nMC # store single MC run in column k of nMatrix 
  })
#Find average of n(t), CW-OSL signal, and CV[%]
avgn<-rowMeans(nMatrix)
sd<-rowSds(nMatrix)
cv<-100*sd/avgn
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
xlabs=expression("D [cm"^-3*"]")
plot(Dvalues,avgn,ylab="Remaining electrons n(t)",
     xlab=xlabs,ylim=c(0,140))
legend("topright",bty="n",c("(a)    n(t)"," ","MC",
                            "Lambert Eq."),pch=pch,lty=lty)
lines(Dvalues,N*(1+lambertW0((R-1)*exp(R-1-R*Dvalues/N))/(1-R)),
      col="red",lwd=3)
plot(Dvalues,sd,ylab=c(expression(sigma[n]*" ")," ","MC"), 
     xlab=xlabs, ylim=c(0,8),col="blue")
legend("topleft",bty="n",legend=c(expression("(b)"," ",
                                             sigma[n]*" ")),pch=pch,lty=lty,col="blue")
plot(Dvalues,cv,ylab="CV[%]", xlab=xlabs,ylim=c(0,60),col="red")
legend("topleft",bty="n",c("(c)  CV[%]"," ","MC"),pch=pch)