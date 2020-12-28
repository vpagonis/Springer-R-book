# plot analytical solution of GOT equation for LM-OSL with W(x)
rm(list=ls())
library(lamW)
## Plot the analytical solution of GOT, using Lambert W-function
t<-0:120
kB<-8.617E-5
no<-1E10
N<-1E10
R<-.46
c<-(no/N)*(1-R)/R
lambda<-.003
x<-unlist(t)
zLM<-unlist((1/c)-log(c)+(lambda*no/(c*N*R))*t^2/2)
# plots
par(mfrow=c(1,2))
plot(x,(N*R/(1-R))/(lambertW0(exp(zLM))),xlab="Time, s",
     ylab=expression("n(t)  [cm"^-3*"]"),ylim=c(0,N))
legend("topright",bty="n",c("(a)"," ","n(t)"))
plot(x,(N*R/((1-R)^2))*lambda*x/(lambertW0(exp(zLM))
                                 +lambertW0(exp(zLM))^2), xlab="Time, s",ylab="LM-OSL [a.u.]")
legend("topright",bty="n",c("(b)","LM-OSL","with W0"))