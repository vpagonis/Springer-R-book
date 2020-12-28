
## Plot the CW-OSL solution of GOT, using Lambert W-function
rm(list=ls())
library(lamW)
t<-0:300
kB<-8.617E-5
no<-1E10
N<-1E10
R<-.01
c<-(no/N)*(1-R)/R
lambda<-.01
x<-unlist(t)
zCWOSL<-unlist((1/c)-log(c)+(lambda*no/(c*N*R))*t)
# plots
par(mfrow=c(1,2))
plot(x,(N*R/(1-R))/(lambertW0(exp(zCWOSL))),xlab="Time, s",
     ylab="Filled traps n(T)",ylim=c(0,N),col="red")
legend("topright",bty="n",c("(a)  n(t)"," ","using","W0(x)"))
plot(x,(N*R/((1-R)^2))*lambda/(lambertW0(exp(zCWOSL))
                               +lambertW0(exp(zCWOSL))^2),
     xlab="Time, s",ylab="CW-OSL signal")
legend("topright",bty="n",c("(b)",  "CW-OSL"))