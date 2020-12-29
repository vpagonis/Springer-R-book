rm(list=ls())
library(lamW)
## Plot the analytical solution of LT, using Lambert W-function
x1<-300:450
kB<-8.617E-5
no<-1E8
r<-1e4
En<-1
s<-1E13
beta<-1
k<-function(u) {integrate(function(p){exp(-En/(kB*p))},
                          300,u)[[1]]}
y1<-lapply(x1,k)
x<-unlist(x1)
y<-unlist(y1)
zTL<-(r/no)-log(no/r)+(s*y)
plot(x,r*s*exp(-En/(kB*x))/(lambertW0(exp(zTL))
                            +lambertW0(exp(zTL))^2),type="l",col="red",
     lwd=3,lty=1,xlab="Temperature, K",ylab="TL in LT model [a.u.]")
zTL<-(r/no)-log(no/r)+(s*y)
r<-1e8
zTL<-(r/no)-log(no/r)+(s*y)
lines(x,r*s*exp(-En/(kB*x))/(lambertW0(exp(zTL))
                             +lambertW0(exp(zTL))^2),col="green",
      lwd=3,  lty=2,   xlab="Temperature, K",ylab="TL")
r<-1e9
zTL<-(r/no)-log(no/r)+(s*y)
lines(x,r*s*exp(-En/(kB*x))/(lambertW0(exp(zTL))
                             +lambertW0(exp(zTL))^2),col="blue",
      lwd=3,  lty=3,    xlab="Temperature, K",ylab="TL")
legend("topright",bty="n",lwd=2, lty=c(NA,NA,1,2,3), 
       legend = c("Retrapping ratio r", expression("(cm"^"-3"*")"),
                  expression("10"^"4"*""),expression("10"^"8"*""),
                  expression("10"^"9"*"")),
       col=c(NA,NA,"red","green","blue"))