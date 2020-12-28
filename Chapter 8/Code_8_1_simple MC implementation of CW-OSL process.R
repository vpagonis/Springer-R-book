# Simulate CW-OSL process using the simplest MC code 
# Original Mathematica program by Vasilis Pagonis
# R version written by Johannes Friedrich, 2018
rm(list = ls(all=T))
options(warn=-1)
library(matrixStats)
# Define Parameters
mu <- 0.03 # probability of optical excitation per second
deltat <- 1
times <- seq(1, 100, deltat) # time sequence
n0  <- 500 # initial number of electrons at t=0
# Number of iterations of the Monte carlo process
mcruns <- 100   
nMatrix  <- matrix(NA, nrow = length(times), ncol = mcruns)
# The 3 main Monte Carlo loops follow
system.time(invisible(
  for (k in 1:mcruns)
  {  n <- n0
  for (t in 1:length(times)){
    for (j in 1:n){
      r <- runif(1)  # random number in (0,1)
      P <- mu*deltat
      if (r < P) n <- n - 1  # the electron has recombined  
    }
    nMatrix[t,k] <- n  }  }  ))
# Take average of iterations 
avgn <- rowMeans(nMatrix)
## plot MC and analytical solution n(t)
par(mfrow=c(1,3))
pch<-c(NA,NA,1,NA)
lty<-c(NA,NA,NA,"solid")
col<-c(NA,NA,"black","red")
matplot(x=times,nMatrix,xlab = "Time [s]",   
        ylab = "Remaining electrons",ylim=c(0,700))
legend("topright",bty="n",legend=c("(a)"," ",
                                   "n(t) ","MC","n0=500 M=100"))
plot(x = times,      y = avgn,type = "p",pch = 1,,ylim=c(0,700), 
     xlab = "Time [s]", ylab = "Average of Remaining electrons")
curve(n0*exp(-mu*x),0,max(t),add=TRUE,col="red",lwd=2)
legend("topright",bty="n",c("(b)   "," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
plot(x = times,   y = mu*avgn,type = "p",pch = 1,ylim=c(0,20), 
     xlab = "Time [s]",  ylab = "Average of CW-OSL signal")
legend("topright",bty="n",c("(c)      CW-OSL"," ","MC (M=100)",
                            "Analytical"),pch=pch,lty=lty,col=col)
curve(n0*mu*exp(-mu*x),from=0,max(t),add=TRUE,col="red",lwd=2)