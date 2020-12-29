## Simple TL function for freshly irradiated feldspars
rm(list = ls(all=T))
source("Pagonis2020FSF.R")# Loads the FSF functions
rho<-.013                 # Dimensionless acceptor density
s<-3.5e12                 # Frequency factors in s^-1
E<-1.45                   # Energy in eV
Tph<-300                  #Preheat temperature (C)
tph<-10                   #Preheat time (s)
dr<-.1                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr)  #Values of r'=0-2.2 in steps of dr
kb<-8.617e-5              #Boltzmann constant
beta<-1
Tpreheat<-273+320         # Preheat Temperature
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
########################## Simulations ###############
par(mfrow=c(1,3))
# #### Example #1: TL for unfaded sample
distr<<-3*rprimes^2*exp(-rprimes^3)
distr1<-distr
temps<-273+seq(1:400)     # Temperatures for TL
manyTL<-t(sapply(temps,TLsignal))
TL1<-stimTL()             # Evaluate TL signal
distr2<-distr             

########## End of Simulations, next Plot the results #######
plot(rprimes,distr1,ylim=c(0,1.9),typ="o",pch=1,col="black",
     ylab="Distribution of r'",xlab="Dimensionless distance r'")
lines(rprimes,distr2,typ="o",pch=2,col="red",
      ylab="Distribution of r'",xlab="Dimensionless distance r'")
legend("topleft",bty="n",legend=c("(a)"," ",
                                  "   Distribution of r'",  "   before and after", "TL"))
matplot(temps-273,manyTL,typ="l",lty="solid",
        xlim=c(175,450),ylim=c(0,.03),lwd=2,
        xlab=expression("Temperature ["^"o"*"C]"), ylab="TL [a.u.]")
legend("topleft",bty="n",legend=c("(b)","    ","TL curves ",
                                  "for each r'"))
plot(temps-273,TL1,typ="l", lwd=2,pch=1,col="black",
     xlim=c(175,450),ylim=c(0,.018),
     xlab=expression("Temperature ["^"o"*"C]"),ylab="TL [a.u.]")
legend("topleft",bty="n",legend=c("(c)"," ","Sum of all",
                                  "TL curves "))