# Simulate second-order glow peaks with various 
# initial electron trap concentrations (n0).
rm(list=ls())
library(tgcd)
temps <- seq(300, 500, by=1)
peak1 <- simPeak(temps,n0=0.2e10,Nn=1e10,ff=1e12, ae=1.0, hr=1, 
                 typ="s",plot=FALSE)
peak2 <- simPeak(temps,n0=0.4e10,Nn=1e10,ff=1e12, ae=1.0, hr=1, 
                 typ="s",plot=FALSE)
peak3 <- simPeak(temps,n0=0.6e10,Nn=1e10,ff=1e12, ae=1.0, hr=1, 
                 typ="s",plot=FALSE)
peaks<-cbind(peak1$tl, peak2$tl, peak3$tl)
matplot(temps, peaks, type="o",  pch=c(0,1,2),
        col=c("black","red","blue"),lwd=2,
        xlab="Temperature (K)", ylab="TL intensity [a.u.]")
legend("topright",bty="n", pch=c(NA,NA,NA,0,1,2),
       c(expression('Second order peaks',' ','n'[0]*'/N'), 
         "0.2","0.4","0.6"),col=c(NA,NA,NA,"black","red","blue"))