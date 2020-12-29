#Dose response of feldspars, irradiation in nature (TA-EST)
rm(list = ls(all=T))
rho<-3e-3                 # Dimensionless acceptor density
Po<-s<-2e15               # Frequency factors in s^-1
E<-1.3                    # Energy in eV
Do<-1600                  #Do in Gy
yr<-365*24*3600           #year is seconds
Ddot<-2.85/(1e3*yr)       #Low natural dose rate = 1 Gy/kA
dr<-.05                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr) #Values of r'=0-2.2 in steps of dr
kb<-8.617e-5              #Boltzmann constant
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
Tirr<-273-4
Peff<-(1/(1/s+1/seff))*exp(-E/(kb*Tirr))

########## Irradiation functions  ##########
#### irradfortimeT
irradandThermalfortimeT<-function(tirr){distr<<-distrUnfaded*
  (Ddot*rprimes/(Do*Peff+Ddot))*(1-exp(-(Ddot/Do+Peff)*tirr))}
###################### End of Functions ##########
par(mfrow=c(1,2))
distrUnfaded<-3*rprimes^2*exp(-rprimes^3)  # Unfaded sample
irrTimes<-10^seq(2,6,by=.2)*yr
distribs<-sapply(irrTimes,irradandThermalfortimeT)

########## plot distibutions
matplot(rprimes,distribs,typ="l",ylim=c(0,.45),lty="solid",
        xlab="Dimensionless Distance r'",
        ylab="Distribution of r'",lwd=2)
legend("topleft",bty="n",legend=c(expression("(a)", 
                                             "Irradiation in nature",
                                             "Distributions of r'","tirr=10"^3*"-10"^6*"y")))
plot(irrTimes/yr,colSums(distribs)*dr,typ="o",lwd=2,
     xlab="Time [y]",ylab="Trap filling ratio n(t)/N",
     ylim=c(0,.27))
legend("topleft",bty="n",legend=c(" ","(b)",
                                  "Trap filling n(t)/N"))