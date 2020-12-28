# Apply the heating rate method to find E,s 
rm(list=ls())
library(tgcd)
library(scales)
kB<-8.617*1e-5 # Boltzmann constant in eV/K
temps <- seq(340, 420, by=2)
#### function to calculate TL for different heating rates####
findTL<-function(hRate)
{peak<-simPeak(temps,n0=0.2e10,Nn=1e10,ff=1e12,ae=1.0, hr=hRate, 
               typ="f",plot=FALSE)
peak$tl}
######## function to find Tmax ######
findTmax<-function(hRate)
{peak<-simPeak(temps,n0=0.2e10,Nn=1e10,ff=1e12,ae=1.0, hr=hRate, 
               typ="f",plot=FALSE)
temps[[match(max(peak$tl),peak$tl)]]}
########## calculate 1/kTmax and log(Tmax^2/beta)
hRates<-c(1,2,3,4)
maxTL<-sapply(hRates,findTmax)
TLs<-sapply(hRates,findTL)
####### plots
par(mfrow=c(1,2))
matplot(temps,TLs,type="o",lty="solid",lwd=1,pch=1:4,
        col=c(1:4),xlab="Temperature (K)",  ylim=c(0,8e7), 
        ylab=expression(paste("TL/ ",beta," (counts/K)")))
y<-log(maxTL^2/hRates)
x<-1/(kB*maxTL)
coefficients(lm(y~x))
energy<-coefficients(lm(y~x))[[2]]
intercept<-coefficients(lm(y~x))[[1]]
cat('\nFrequency factor s=',
    scientific(exp(-intercept)*energy/kB),' s^-1')
legend("topright",bty="n","(a)")
legend("topleft",bty="n", pch=c(1:4,NA),expression(
  "1 K/s","2","3","4"," "),col=c(1:4,NA),lwd=1)
plot(x,y,xlab="1/(kT)   (1/eV)",
     ylab=expression(paste('ln(Tmax'^'2'*')/',beta)),ylim=c(10.4,12))
abline(lm(y ~ x))
legend("topleft",bty="n",legend=c("(b)","Heating rate",
                                  "method"))