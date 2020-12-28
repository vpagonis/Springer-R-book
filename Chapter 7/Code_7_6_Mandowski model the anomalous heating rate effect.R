#Mandowski model: the anomalous heating rate effect
rm(list=ls())
library("deSolve")
TLMandowski <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dH01<- -(s*exp(-E/(kb*(273+hr*t)))+C*(H00-E01-E10))*H01+A*H10
    dH10 <- s*exp(-E/(kb*(273+hr*t)))*H01-(A+B+sv*exp(-Ev/(kb*
                                                             (273+hr*t)))+C*(H00-E01-E10))*H10
    dH00<-sv*exp(-Ev/(kb*(273+hr*t)))*H10-C*(H00-E01-E10)*H00
    dE01<-C*(H00-E01-E10)*H01-s*exp(-E/(kb*(273+hr*t)))*E01+A*E10
    dE10<-C*(H00-E01-E10)*H10+s*exp(-E/(kb*(273+hr*t)))*E01-
      (A+sv*exp(-Ev/(kb*(273+hr*t))))*E10
    dE00<-B*H10+C*(H00-E01-E10)*H00+sv*exp(-Ev/(kb*(273+hr*t)))*E10                
    res <- c(dH01,dH10,dH00,dE01,dE10,dE00)
    list(res)  })}
hrVar<-function(hr){
  parms  <- c(E =1.05, s=1e10, sv=1e10, kb=8.617*10^-5,
              hr=hr, C=C,B=B,Ev=0.7,A=2*10^4)
  y <- xstart <- c(H01 = 10^17,   H10=0,H00=0,E10=0,E01=0,E00=0)
  out <-ode(xstart,times, TLMandowski, parms, method = "lsoda", 
            atol = 1,rtol=1) }
C<-1e-10
B<-1e4
times  <- seq(0, 300,by=.5)
Lc<-Lb<-temps<-matrix(NA,nrow=length(times),ncol=4)
areaLb<-areaLc<-vector(length=4)
for (i in 1:4){
  hr<-i
  a<-hrVar(hr)
  Lc[,i]<-(C*(a[,"H00"]-a[,"E01"]-a[,"E10"])*
             (a[,"H10"]+a[,"H01"]+a[,"H00"]))/hr
  Lb[,i]<-B*a[,"H10"]/hr
  temps[,i]<-hr*times
  areaLc[i]<-sum(Lc[,i])*hr*0.5
  areaLb[i]<-sum(Lb[,i])*hr*0.5
}
par(mfrow=c(1,3))
var<-c(NA,NA,NA,1:4)
matplot(cbind(temps),cbind(Lb),lty="solid",typ="o",pch=1:4,
        xlim=c(150,270),ylim=c(0,3e15),lwd=2, col=1:4 ,
        xlab=expression("Temperature ["^"o"*"C]"), ylab="LB [a.u.])")
legend("topleft",bty="n",pch=var,lty="solid",col=var,
       legend=c("(a)",
                "Mandowski","SLT model: LB","1 K/s","2 K/s","3 K/s","4 K/s"))
matplot(cbind(temps),cbind(Lc),lty="solid",typ="o",pch=1:4,
        xlim=c(170,300),ylim=c(0,7e14),lwd=2,col=1:4,
        xlab=expression("Temperature ["^"o"*"C]"), ylab="LC [a.u.])")
legend("topleft",bty="n",pch=var,lty="solid",col=var,
       legend=c("(b)"," ","LC peaks","1 K/s","2 K/s","3 K/s","4 K/s"))
matplot(1:4,cbind(areaLc,areaLb,areaLb+areaLc),lty="solid",
        typ="o",lwd=3, pch=1:3,col=1:3,ylim=c(0,1.7e17),  
        xlab="Heating rate [K/s]",ylab="TL Areas: LB,  LC,  LB+LC")
legend("topleft",bty="n",pch=c(NA,NA,1:3),lty="solid",col=c(NA,
                                                            NA,1:3),legend= c("(c)"," ","LC area","LB area","LC+LB area"))