# Stochastic LM-OSL process using KMC
rm(list = ls(all=T))
library(matrixStats)
mcruns<-100
n0<-500
tmax<-100
A<-.1
nMatrix<-matrix(NA,nrow=tmax-1,ncol=mcruns)

times<-seq(1,tmax-1,1)
system.time(
  for (k in 1:mcruns)
  {n<-n0
  allt<-t<-1
  for(i in 1:n-1){
    P<-A*t/tmax
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
par(mfrow=c(1,3))
matplot(nMatrix,typ="l",col="green",ylim=c(0,700),
        xlab="Time [s]",ylab="Remaining electrons")
avgn<-rowMeans(nMatrix,na.rm = TRUE)
cv<-100*rowSds(nMatrix,na.rm=TRUE)/avgn
# plots
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,"black","red")
lines(seq(from=1,to=tmax-1,by=1),avgn,lwd=2,col="black")
curve(n0*exp(-A*x^2/(2*(tmax-1))),1,tmax,lwd=3,
      col="red",add=TRUE)
legend("topright",bty="n",c("(a)  n(t) for LM-OSL",
                            " ","MC","Analytical"),pch=pch,lty=lty,col=col)
avgLM<-A*(times/(tmax-1))*as.numeric(avgn)
plot(times,avgLM,ylab="LMOSL",xlab="Time [s]",ylim=c(0,15))
legend("topright",bty="n",c("(b)  LM-OSL"," ","MC",
                            "Analytical"), pch=pch,lty=lty,col=col)
curve(A*n0*exp(-A*x^2/(2*tmax))*x/tmax,0,tmax,add=TRUE,
      col="red",lwd=2)

plot(times,cv,ylab="CV[%]",xlab="Time [s]",ylim=c(0,120))
curve(100*sqrt((exp(A*x^2/(2*tmax))-1)/n0),0,tmax,add=TRUE,
      col="red",lwd=2)
legend("topleft",bty="n",c("(c)  CV[%]"," ","MC (M=100)",
                           "Analytical"), pch=pch,lty=lty,col=col)