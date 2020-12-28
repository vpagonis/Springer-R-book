rm(list=ls())
# Load the data of Anomalous fading (AF) and calculate the g-factor
par(mfrow=c(1,2))
mydata <- read.table("durnago0sok.txt")
T<-mydata[,1]
TL<-mydata[,2]/1e5
plot(T,TL,type="o",pch=1,col="red",xlim=c(50,450),ylim=c(0,1.6),
     xlab=expression("Temperature ["^"o"*"C]"),ylab ="TL (a.u.)")
mydata2 <- read.table("durango10daysok.txt")
T2<-mydata2[,1]
TL2<-mydata2[,2]/1e5
lines(T2,TL2,type="o",pch=2,col="blue")
legend("left",bty="n","(a)")
legend("topleft",bty="n",lwd=2, lty=c(NA,NA,1,2,3), 
       pch=c(NA,NA,1,2),
       legend=c(expression('Anomalous', 'fading ', 't=0 s',
                           't=10 days')),col=c(NA,NA,"red","blue"))
mydata3 <- read.table("DurangoAFdataok.txt")
t<-mydata3[,1]
RTL<-mydata3[,2]
#plot(t,RTL,xlab="ln(t/to)",ylab="Remnat TL [%]")
y<-RTL
x<-t
bestfit<-lm(y~x)
coefficients(bestfit)
plot(x, y, xlab=expression("ln(t/to)"),ylab = "RTL [%]")
abline(lm(y~x))
slope<-abs(coefficients(bestfit)[[2]])
g<-230.2*slope
paste0("g-factor=",round(g,digits=2)," % per decade")
legend("topright",bty="n", 
       legend=c(expression('Durango apatite', 
                           'g-factor=20.5% ',)))
legend("left",bty="n","(b)")