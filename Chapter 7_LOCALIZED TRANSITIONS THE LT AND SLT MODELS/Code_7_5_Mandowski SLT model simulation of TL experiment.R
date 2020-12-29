# Mandowski SLT model: simulation of TL experiment
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
enVar<-function(en){
  parms  <- c(E =en, s=1e10, sv=1e10, kb=8.617*10^-5,
              hr=1, C=C,B=B,Ev=0.7,A=2*10^4)
  y <- xstart <- c(H01 = 10^17,   H10=0,H00=0,E10=0,E01=0,E00=0)
  out <-  ode(xstart, times, TLMandowski, parms, method = "lsoda", 
              atol = 1,rtol=1) }
C<-1e-10
B<-1e4
temps<-times  <- seq(0, 360,by=.5)
Lc<-Lb<-matrix(NA,nrow=length(times),ncol=4)
areaLb<-areaLc<-vector(length=4)
en<-.95
a<-enVar(en)
Lc<-(C*(a[,"H00"]-a[,"E01"]-a[,"E10"])*
       (a[,"H10"]+a[,"H01"]+a[,"H00"]))
Lb<-B*a[,"H10"]
areaLc<-sum(Lc)*0.5
areaLb<-sum(Lb)*0.5
par(mfrow=c(2,3))
xlabs=expression("Temperature ["^"o"*"C]")
plot(temps,Lb,typ="l",xlim=c(100,270),ylim=c(0,4e15),col="blue",
     pch=1,xlab=expression("Temperature ["^"o"*"C]"),lwd=5,
     ylab="LB [a.u.])")
legend("topleft",bty="n",col="red",
       legend=c("(a)","Mandowski","SLT model","LB TL peak"))
plot(temps,Lc,typ="o",xlim=c(100,270),ylim=c(0,.7e14),col="red",
     xlab=xlabs,ylab="LC [a.u.])")
legend("topleft",bty="n",col="red",
       legend=c("(b)"," ","LC TL peak"))
plot(temps,a[,"H00"],typ="o",xlim=c(100,270),ylim=c(0,2e15),
     xlab=xlabs,ylab=expression("H00 [cm"^-3*"]"),col="red",pch=1)
legend("top",bty="n",c("(c)"," ","H00"))
plot(temps,a[,"H01"],typ="o",xlim=c(100,270),ylim=c(0,2e17),
     xlab=xlabs,ylab=expression("H01 [cm"^-3*"]"),col="green")
lines(temps,a[,"E00"],typ="o",col="gray",pch=3)
legend("topleft",bty="n",c("(d)"," ","H01"))
text(200,1.2e17,"E00")
plot(temps,a[,"E01"],typ="o",xlim=c(100,270),ylim=c(0,2e15),
     xlab=xlabs,ylab=expression("E01 [cm"^-3*"]"),col="magenta")
legend("top",bty="n",c("(e)"," ","E01"))
plot(temps,a[,"H10"],typ="o",xlim=c(100,270),ylim=c(0,4e11),
     col="black",xlab=xlabs,ylab=expression("H10,  E10 [cm"^-3*"]"))
lines(temps,a[,"E10"],typ="o",col="green",)
legend("top",bty="n",c("(f)"," ","H10"))
text(220,1e11, "E10")