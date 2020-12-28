## Examples of TL for thermally and optically treated samples
rm(list = ls(all=T))
source("Pagonis2020FSF.R")# Loads the FSF functions
rho<-.013                 # Dimensionless acceptor density
Po<-s<-3.5e12             # Frequency factors in s^-1
E<-1.45                   # Energy in eV
Tph<-300                  #Preheat temperature (C)
tph<-10                   #Preheat time (s)
dr<-.05                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr)  #Values of r'=0-2.2 in steps of dr
kb<-8.617e-5              #Boltzmann constant
beta<-1
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
A<-5
Aeff<-A*exp(-rprimes*(rho**(-1/3.0)))  # Effective A
########################## Simulations ###############
par(mfrow=c(1,2))
## Example #1: TL of unfaded sample
distr<<-3*rprimes^2*exp(-rprimes^3)  # unfaded distribution
distr1<-distr             # Store distr for plotting later
temps<-273+seq(1:500)     # Temperatures for TL
TL1<-stimTL()             # Evaluate TL signal
## Example #2: Heat to temperature Tpreheat, then measure TL
distr<<-3*rprimes^2*exp(-rprimes^3)  # unfaded distribution
Tpreheat<-273+320         # Preheat Temperature
heatTo(Tpreheat)          # Heat to 320 degC
distr2<-distr             # Store distr for plotting                                 
TL2<-stimTL()             # Store TL   
## Example #3: Heat for 30 s at Tpreheat=320C, then measure TL
distr<<-3*rprimes^2*exp(-rprimes^3)
heatTo(Tpreheat)          # Heat to 320 degC
heatAt(Tpreheat,30)       # Heat for 30 s at 320C
distr3<-distr                                                  
TL3<-stimTL()                  
## Example #4: CW-IRSL excitation for 10 s, then measure TL
distr<<-3*rprimes^2*exp(-rprimes^3)  # unfaded distribution
timesCW<-seq(1,10)
invisible(sapply(timesCW,CWfortimeT) ) # IR for 10 s
distr4<-distr                                                   
TL4<-stimTL()                   
########## End of Simulations, next Plot the results #######
plot(rprimes,distr1,ylim=c(0,1.9),typ="l",pch=1,col="black",
     lwd=2,ylab="Distribution of r'",
     xlab="Dimensionless distance r'")
legend("topleft",bty="n",legend=c("(a)","Various Examples",
                                  "of r' Distributions"))
text1 <- data.frame(x=c(.5,.8,1.,1.25),
                    y=c(1,.7,.55,.3), labels=c("1","2","3","4"))
text(text1) 
lines(rprimes,distr2,typ="l",col="red",lwd=2)
lines(rprimes,distr3,typ="l",col="blue",lwd=2)
lines(rprimes,distr4,typ="l",col="magenta",lwd=2)
plot(temps-273,TL1,xlim=c(200,450),ylim=c(0,.017),typ="l",
     col="black",lwd=2,
     xlab=expression("Temperature ["^"l"*"C]"), ylab="TL [a.u.]")
lines(temps-273,TL2,typ="l",col="red",lwd=2)
lines(temps-273,TL3,typ="l",col="blue",lwd=2)
lines(temps-273,TL4,typ="l",col="magenta",lwd=2)
legend("topleft",bty="n",legend=c("(b)"," ",
                                  "Corresponding TL")) 
text1 <- data.frame(x=c(270,310,330,350),
                    y=c(0.009,.006,.004,.002), labels=c("1","2","3","4"))
text(text1)