## Feldspar irradiation in nature  - Dose response 
rm(list = ls(all=T))
rho<-2e-6                 # Dimensionless acceptor density
s<-3e15               	# Frequency factors in s^-1
Do<-538                   #Do in Gy
yr<-365*24*3600           #year is seconds
Ddot<-2.85/(1e3*yr)       #Low natural dose rate = 2.85 Gy/Ka
dr<-.05                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr) #Values of r'=0-2.2 in steps of dr
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
tau<-1/seff
###################### Irradiation Function
#### irradfortimeT
irradfortimeT<-function(tirr){distr<<-unFaded*(Ddot*tau/
                                                 (Do+Ddot*tau))*  (1-exp(-(Do+Ddot*tau)/(Do*tau)*tirr))}
# function calculates new distribution at end of irradiation
###################### End of Function ##########
par(mfrow=c(1,2))
unFaded<-3*rprimes^2*exp(-rprimes^3)  # Unfaded sample
irrTimes<-10^seq(2,6,by=.2)*yr
distribs<-sapply(irrTimes,irradfortimeT)
########## plot distibutions
matplot(rprimes,distribs,typ="l",ylim=c(0,1.8),lty="solid",
        xlab="Dimensionless Distance r'",  ylab="Distribution of r'",
        lwd=2)
legend("topleft",bty="n",legend=c(expression("(a)", 
                                             "Irradiation in Nature","Distributions of r'",
                                             "tirr=100-10"^"6"*"y")))
plot(irrTimes/yr,colSums(distribs)*dr,typ="p",lwd=2,
     xlab="Time [y]",ylab="Trap filling ratio n(t)/N",ylim=c(0,1.1))
legend("topleft",bty="n",legend=c("(b)"," ",
                                  "Trap filling n(t)/N",  "Analyt. Eq."),lwd=2,
       lty=c(NA,NA,NA,1),pch=c(NA,NA,1), col=c(NA,NA,NA,"red"))
lines(irrTimes/yr,(1-exp(-Ddot*irrTimes/Do))*exp(-rho*
                                                   (log(Do*s/Ddot)**3.0)),lwd=2,col="red")