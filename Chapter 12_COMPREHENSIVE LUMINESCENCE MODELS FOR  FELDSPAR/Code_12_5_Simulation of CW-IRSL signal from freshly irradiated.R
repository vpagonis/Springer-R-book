## CW-IRSL function for freshly irradiated feldspars (EST)
rm(list = ls(all=T))
source("Pagonis2020FSF.R")# Loads the functions
rho<-.013                 # Dimensionless acceptor density
dr<-.05                   # Step in dimensionless distance r'
rprimes<-seq(0,2.2,dr)    # Values of r'=0-2.2 in steps of dr'
A<-3                      # A=stun*sigma*I/B
Aeff<-A*exp(-rprimes*(rho**(-1/3.0)))  # Effective A
########################## Simulations ###############
par(mfrow=c(1,3))
#### Example : IRSL for unfaded sample
distr<<-3*rprimes^2*exp(-rprimes^3)  # unfaded distribution
distr1<-distr
timesCW<-seq(1,100)
IRsignal1<-stimIRSL()
distr2<-distr
########## End of Simulations, next Plot the results #######

plot(rprimes,distr1,ylim=c(0,1.9),typ="o",pch=1,col="black",
     ylab="Distribution of r'",xlab="Dimensionless distance r'")
lines(rprimes,distr2,ylim=c(0,1.9),typ="o",pch=2,col="blue",
      ylab="Distribution of r'",xlab="Dimensionless distance r'")
legend("topleft",bty="n",legend=c("(a)"," ",
                                  " Distribution of r'", " before and after IR"))
matplot(timesCW,t(sapply(timesCW,CWsignal)),typ="l",lty="solid",
        ylim=c(0,.004), lwd=2, xlab=expression("Times [s]"), 
        ylab="CW-IRSL [a.u.]")
legend("topleft",bty="n",legend=c("(b)","    ","CW-IRSL curves",
                                  "for each r'"))
plot(timesCW,IRsignal1,typ="p",lwd=2,pch=1,col="red",
     ylim=c(0,.17),xlab=expression("Times [s]"),
     ylab="CW-IRSL [a.u.]")
legend("topleft",bty="n",legend=c("(c)"," ","CW-IRSL signal",
                                  "Unfaded sample"," ","Sum of CW-IRSL","curves"))
lines(timesCW,3*rho*A*1.8*exp(-rho*(log(1+1.8*A*timesCW))**3.0)*
        (log(1 +1.8* A*timesCW) ** 2.0)/(1 + 1.8*A* timesCW),lwd=2,
      col="blue")