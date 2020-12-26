# Solution of the system of ODE's for the OTOR model
rm(list=ls())
library(deSolve)
TLOTOR <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dn1 <- - n1*s*exp(-E1/(kb*(273+hr*t)))+ nc*An*(N1-n1)          
    # n1=concentration of trapped electrons
    dnc <-  n1*s*exp(-E1/(kb*(273+hr*t)))-nc*An*(N1-n1)-m*Am*nc  
    # nc=concentration of conduction band electrons
    dm <- -m*Am*nc                                                 
    # m=concentration of recombination centers
    res <- c(dn1, dnc, dm)
    list(res)
  })}
## Parameters 
hr<-1   # heating rate in K/s
parms  <- c(E1 =1, s=10^12, kb=8.617*10^-5, hr=hr, 
            An = 10^-7,  N1 = 10^10,Am = 10^-7)
## vector of timesteps
times  <- seq(0, 250)
temps<-times*hr   
## Initial conditions for the system
y <- xstart <- c(n1 = 10^9, nc = 0, m = 10^9)
## Solve system of differential equations
out <-  lsoda(xstart, times, TLOTOR, parms) 
## Plotting
par(mfrow=c(1,2))
plot(temps,out[,"n1"],xlab=expression("Temperature ["^"o"*"C]"),
     ylab =expression("Filled traps n(T),cm"^-3*" "),ylim=c(0,1.2e9))
legend("left",bty="n",expression("OTOR","n(T)"))
legend("topleft",bty="n", expression("(a)"))
plot(temps,out[,"m"]*parms["Am"]*out[,"nc"],pch=1,col="red",
     xlab=expression("Temperature ["^"o"*"C]"),
     ylim=c(0,2.8e7),ylab="TL [a.u.] and nc (t)",xlim=c(0,250))
lines(temps,10*out[,"nc"],xlab=expression("Temperature [
"^"o"*"C]"),ylab="nc(t)",typ="o",pch=2,col="blue")
legend("topright",bty="n", expression("(b)"))
legend("topleft",bty="n", pch=c(1,2),expression("TL","ncx10"),
       col=c("red","blue"),lwd=1)