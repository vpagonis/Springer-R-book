# Plot the same TL glow curve for four different heating rates
rm(list=ls())
library(tgcd)
temps <- seq(300, 440, by=1)  # temperature in K
hRates<-c(1,2,3,4)
## function to calculate TL for different hehating rates####
findTL<-function(hRate)
{peak<-simPeak(temps,n0=0.2e10,Nn=1e10,ff=1e12,ae=1.0, hr=hRate, 
               typ="f",plot=FALSE)
peak$tl}
### Calculate TL with different heating rates
TLs<-sapply(hRates,findTL)
matplot(temps,TLs,type="o", lty=c(1,1,1,1),lwd=1,  pch=1:4,
        col=1:4,xlab="Temperature (K)", ylim=c(0,8e7),  
        ylab=expression(paste("TL/ ",beta," (counts/K)")))
legend("topright",bty="n","Heating rate method")
legend("topleft",bty="n", pch=c(1:4,NA),expression(
  "1 K/s","2","3","4"," "),col=c(1:4,NA),lwd=1)