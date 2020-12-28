rm(list=ls())
library(lamW)
##  Example of plot for Lambert W-function from x=0 to x=100
xs <- seq(0, 100, by=1)
ys <- lambertW0(xs)
## Plot the analytical solution of GOT, using Lambert W-function
x1<-300:450   # temperatures in K
kB<-8.617E-5
no<-1E8
N<-1E10
R<-.01
c<-(no/N)*(1-R)/R
En<-1 
s<-1E12
beta<-1
k<-function(u) {integrate(function(p){exp(-En/(kB*p))},
                          300,u)[[1]]}
y1<-lapply(x1,k)
x<-unlist(x1)
y<-unlist(y1)
zTL<-(1/c)-log(c)+(s*no/(c*N*R))*y
# plots
par(mfrow=c(1,2))
plot(xs, ys, type="l", col="red", lwd=3, ylim=c(0,5),
     xlab="x", ylab="Lambert W0(x)")
legend("topleft",bty="n",c("(a)"," ","Lambert",
                           "function W0(x)"))
plot(x,(N*R/((1-R)^2))*s*exp(-En/(kB*x))/(lambertW0(exp(zTL))
                                          +lambertW0(exp(zTL))^2),xlab="Temperature, K",col="blue",
     ylab="TL with W0(T)")
legend("topleft",bty="n",c("(b)"," ","TL", "using", "W0(x)"))