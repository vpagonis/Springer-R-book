# Simulate Loss of charge due to ground state tunneling (GST)
rm(list=ls())
z<-1.8
s<-3e15                  # frequency factor
rho1<-1e-6               # rho-prime values
rho2<-5e-6
rho3<-1e-5
years<-1000
elapsedt<-3.154e7*years
t<-seq(from=1,to=elapsedt,by=1e6)
gr1<-100*exp(-rho1*(log(z*s*t)**3.0))
gr2<-100*exp(-rho2*(log(z*s*t)**3.0))
gr3<-100*exp(-rho3*(log(z*s*t)**3.0))
plot(unlist(t)/(3.154e7),unlist(gr1),type="l",lty=1,lwd=2,
     ylim=c(0,130),pch=1,ylab="Remaining electrons [%]",
     xlab="Elapsed time t [years]")
lines(unlist(t)/(3.154e7),unlist(gr2),lwd=2,col="red",lty=2)
lines(unlist(t)/(3.154e7),unlist(gr3),lwd=3,col="green",lty=2)
legend("topright",bty="n",lwd=2, lty=c(NA,1,2,3), 
       legend = c("Acceptor Density"  ,expression("10"^"-6"*""),
                  expression("5x10"^"-6"*""),expression("10"^"-5"*"")),
       col=c(NA,"black","red","green")) 