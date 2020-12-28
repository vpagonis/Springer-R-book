# Apply the initial rise method to find the activation energy E
# Load the data from txt file, which  contains pairs of
# data  in the form: (Temperature_in_C,TL_Intensity (any units)
rm(list=ls())
library(tgcd)
data("Kitis")
x<-Kitis$x001[,1]
y<-Kitis$x001[,2]
mydata<-data.frame(x,y)
kB<-8.617*1e-5 # Boltzmann constant in eV/K
initialPos<-270  #analyze data points from #270 to #320
finalPos<-320
x<- mydata[,1][initialPos:finalPos]
y<- mydata[,2][initialPos:finalPos]
rangeData<-cbind(x,y)
y<-log(y)
x<-1/(kB*x)
bestfit<-lm(y~x)
summary(bestfit)
coefficients(bestfit)
par(mfrow=c(1,2))
plot(mydata,col="blue",
     xlab=expression("Temperature [K]"),ylab = "TL (a.u.)")
lines(rangeData,col="red",lwd = 3)
legend("topright",bty="n","(a)")
plot(x, y, xlab = "1/(kT)  [1/eV]",ylab = "ln(TL)")
abline(lm(y~x))
legend("topright",bty="n",c("(b)"," ","Initial","Rise"))