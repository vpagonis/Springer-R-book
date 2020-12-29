#Pagonis SLT model: the anomalous heating rate effect
rm(list=ls())
library("deSolve")
PagonisHR <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dn<- -s1*n*exp(-E1/(kb*(273+hr*t)))+p*
      (An*(N-n)*(m-n)+s1*n*exp(-E1/(kb*(273+hr*t))))/
      ( s2*exp(-E2/(kb*(273+hr*t)))+p+B)
    dm<- -Am*m*(m-n)-B*
      (An*(N-n)*(m-n)+s1*n*exp(-E1/(kb*(273+hr*t))))/(
        s2*exp(-E2/(kb*(273+hr*t)))+p+B)  
    res <- c(dn,dm)
    list(res)  })}
hrVar<-function(hr){
  parms  <- c(p=p,s1=s1,s2=s2,An=An,Am=Am,
              B=B,N=N,E1=E1,E2=E2,kb=kb)
  y <- xstart <- c(n = 1e11, m=1e11)
  out <-  ode(xstart, times, PagonisHR, parms) }
p<-1e9
s1<-1e9
s2<-1e12
An<-1e-8
Am<-1e-6
B<-1e7
N<-1e13
E1<-0.8
E2<-0.5
kb<-8.617e-5
times  <- seq(0, 350,by=1)
Lc<-Lb<-temps<-matrix(NA,nrow=length(times),ncol=4)
areaLb<-areaLc<-vector(length=4)
for (i in 1:4){
  hr<-i
  a<-hrVar(hr)
  Lc[,i]<- Am*a[,"m"]*(a[,"m"]-a[,"n"])/hr
  Lb[,i]<-B*  ((An*(N-a[,"n"])*(a[,"m"]-a[,"n"])+s1*a[,"n"]*
                  exp(-E1/(kb*(273+hr*times))))/( s2*exp(-E2/(kb*
                                                                (273+hr*times)))+p+B))/hr
  temps[,i]<-273+hr*times
  areaLc[i]<-sum(Lc[,i],rm.NA=TRUE)*hr
  areaLb[i]<-sum(Lb[,i],rm.NA=TRUE)*hr
}
par(mfrow=c(1,3))
var<-c(NA,NA,NA,1:4)
matplot(cbind(temps),cbind(Lb),lty="solid",typ="o",pch=1:4,
        xlim=c(370,550),ylim=c(0,22e8),lwd=2, col=1:4 ,
        xlab=expression("Temperature [K]"), ylab="LB [a.u.])")
legend("topleft",bty="n",pch=var,lty="solid",col=var,
       legend=c("(a)",
                "Pagonis","SLT model: LB","1 K/s","2 K/s","3 K/s","4 K/s"))
matplot(cbind(temps),cbind(Lc),lty="solid",typ="o",pch=1:4,
        xlim=c(370,550),ylim=c(0,5e8),lwd=2,col=1:4,
        xlab=expression("Temperature [K]"), ylab="LC [a.u.])")
legend("topleft",bty="n",pch=var,lty="solid",col=var,
       legend=c("(b)"," ","LC emissions","1 K/s","2 K/s","3 K/s",
                "4 K/s"))
matplot(1:4,cbind(areaLc,areaLb,areaLb+areaLc),lty="solid",
        typ="o",lwd=3, pch=1:3,col=1:3,ylim=c(0,1.7e11),  
        xlab="Heating rate [K/s]",ylab="TL Areas: LB,  LC,  LB+LC")
legend("topleft",bty="n",pch=c(NA,NA,1:3),lty="solid",
       col=c(NA,NA,
             1:3),legend= c("(c)"," ","LC area","LB area","LC+LB area"))