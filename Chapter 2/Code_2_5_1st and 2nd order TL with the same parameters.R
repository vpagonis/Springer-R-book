.width ='80%'>>=
  # Simulate a first and a second  order TL glow peak 
  # with the same kinetic parameters
  rm(list=ls())
library(package ="tgcd")
temps <- seq(300, 500, by=.2)
peak1 <- simPeak(temps, n0=1e10, Nn=1e10, ff=1e12, ae=1, hr=1,
                 typ="f",plot=FALSE)
peak2 <- simPeak(temps, n0=1e10, Nn=1e10,  ff=1e12, ae=1, hr=1,
                 typ="s",plot=FALSE)
n<-cbind(peak1$n, peak2$n)
par(mfrow=c(1,2))
matplot(temps, n, type="l", lwd=3,lty=c(1,2),
        xlab="Temperature (K)",
        ylab=expression("Filled traps (cm"^"-3"*")"))
legend("topright",bty="n", expression("(a)"))
legend("right",bty="n", lty=c(1,2),expression("b=1","b=2"),
       col=c("black","red"),lwd=2)
peaks<-cbind(peak1$tl, peak2$tl)
matplot(temps, peaks, type="l", lwd=3, lty=c(1,2),
        xlab="Temperature (K)",ylab="TL intensity [a.u.])")
legend("right",bty="n", lty=c(1,2),expression("b=1","b=2"),
       col=c("black","red"),lwd=2)
legend("topright",bty="n", expression("(b)"))

print.noquote("Parameters for first order TL peak")
print(c(peak1$sp[1],peak1$sp[2],peak1$sp[3]))
print(c(peak1$sp[4],peak1$sp[5],peak1$sp[6]))
cat("\nShape factor=",round(peak1$sp[7],3))

print.noquote("Parameters for second order TL peak")
print(c(peak2$sp[1],peak2$sp[2],peak2$sp[3]))
print(c(peak2$sp[4],peak2$sp[5],peak2$sp[6]))
cat("\nShape factor=",round(peak2$sp[7],3))