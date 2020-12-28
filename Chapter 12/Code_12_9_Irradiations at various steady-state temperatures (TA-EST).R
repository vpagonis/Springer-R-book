## Multiple feldspar irradiations at steady-state temperatures

rm(list = ls(all=T))
rho<-1e-2                 # Dimensionless acceptor density
Po<-s<-2e15               # Frequency factors in s^-1
E<-1.3                    # Energy in eV
Do<-1600                  #Do in Gy
yr<-365*24*3600           #year is seconds
Ddot<-2.85/(1e3*yr)       #Low natural dose rate = 1 Gy/kA
dr<-.05                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr) #Values of r'=0-2.2 in steps of dr
kb<-8.617e-5              #Boltzmann constant
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
# Peff<-(1/(1/s+1/seff))*exp(-E/(kb*Tirr))   Effective P

########## Irradiation functions  ##########
#### irradatsometemp
irradatsometemp<-function(Tirr){
  Peff<-(1/(1/s+1/seff))*exp(-E/(kb*Tirr))
  distr<<-distrUnfaded*
    (Ddot*rprimes/(Do*Peff+Ddot))*(1-exp(-(Ddot/Do+Peff)*tirr))}
###################### End of Functions ##########
tirr<-1e3*yr
Tirrs<-273+c(-4,0,4,8)     # Burial temperatures
distrUnfaded<-3*rprimes^2*exp(-rprimes^3)  # Unfaded sample
distribs<-sapply(Tirrs,irradatsometemp)

########## plot distibutions
cols=c("black","red","blue","magenta")
pchs=c(1,2,3,4)
matplot(rprimes,distribs,typ="o",pch=pchs,col=cols,
        ylim=c(0,.003),lty="solid",
        xlab="Dimensionless Distance r'", ylab="Distribution of r'",
        lwd=2)
legend("topleft",bty="n",legend=c("Irradiation in nature",
                                  "Various Burial Temperatures"," ","Distributions of distances"))
legend("topright",bty="n",legend=c(expression("-4 "^o*"C",
                                              "  0  "^o*"C"," +4  "^o*"C"," +8  "^o*"C")),pch=pchs,col=cols)