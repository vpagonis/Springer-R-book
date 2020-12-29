# Anomalous fading  over geological and laboratory times 
rm(list = ls(all=T))
rho<-1e-6                 # Dimensionless acceptor density
dr<-.01                   #Step in dimensionless distance r'
rprimes<-seq(0,2.2,dr)    #Values of r'=0-2.2 in steps of dr
s<-3e15
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective A

###################### Functions  ################

###################### Anomalous fading Functions
#### AFfortimeT
AFfortimeT<-function(timAF){distr<<-distr*exp(-(seff*timAF))}
###################### End of Functions

########################## Simulations ###############
par(mfrow=c(1,2))
#### Example : Anomalous fading
# Long term fading  0-10^4 years in nature
distr<<-3*rprimes^2*exp(-rprimes^3)
timesAF<-3.154e7*c(.1,.2,.5,1,2,5,10,20,50,100,200,500,1000,
                   2000,5000,1e4)
n<-dr*colSums(sapply(timesAF,AFfortimeT))
plot(timesAF/(3.154e7),100*n,typ="p",lwd=2,pch=1,col="red",
     ylim=c(70,110),xlab=expression("Time [years]"), 
     ylab="Remaining charge [%]")
legend("topleft",bty="n",legend=c("(a)"," ","AF in nature",
                                  "Analyt. Eq."),lwd=2,lty=c(NA,NA,NA,1), col=c(NA,NA,NA,"blue"))
lines(timesAF/3.154e7,y=100*exp(-rho*(log(1.8*s*timesAF)**3.0)),
      lwd=2,col="blue")
### Repeat for short term fading 0-10 days in lab
distr<<-3*rprimes^2*exp(-rprimes^3)  # unfaded distribution
timesAF<-3600*24*c(1e-4,1e-3,1e-2,.1,.2,.5,seq(1,10,.5))
n<-dr*colSums(sapply(timesAF,AFfortimeT))
plot(timesAF/(3600*24),100*n,typ="p",lwd=2,pch=1,col="red",
     ylim=c(70,110),xlab=expression("Time [days]"), 
     ylab="Remaining charge [%]")
legend("topleft",bty="n",legend=c("(b)"," ","AF in Lab",
                                  "Remaining charge","0-10 days"))
lines(timesAF/(3600*24),
      y=100*exp(-rho*(log(1.8*s*timesAF)**3.0)),lwd=2,col="blue")