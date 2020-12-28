## Distribution of distances for feldspar irradiation in nature
rm(list = ls(all=T))
rho<-2e-6                 # Dimensionless acceptor density
Do<-538                   #Do in Gy
yr<-365*24*3600           #year is seconds
Ddot<-3/(1e3*yr)          #Low natural dose rate = 3 Gy/kA
dr<-.04                   #Step in dimensionless distance r'
rprimes<-seq(0.01,2.2,dr) #Values of r'=0-2.2 in steps of dr
s<-2e15                   # Frequency factors in s^-1
seff<-s*exp(-rprimes*(rho**(-1/3.0)))  # Effective s
tau<-1/seff
###################### Irradiation Function
#### irradfortimeT
irradfortimeT<-function(tirr){distr<<-unFaded*(Ddot*tau/
                                                 (Do+Ddot*tau))*  (1-exp(-(Do+Ddot*tau)/(Do*tau)*tirr))}
# function calculates new distribution at end of irradiation
###################### End of Functions
unFaded<-3*rprimes^2*exp(-rprimes^3)  # Unfaded sample
irrTimes<-c(6.67e4,1.67e5,1e6)*yr
distribs<-sapply(irrTimes,irradfortimeT)
########## plot distibutions
matplot(rprimes,distribs,xlab="Dimensionless Distance r'",
        typ="o",lty="solid", ylab="Distribution of r'",pch=c(2,3,4),
        lwd=2,col=c("blue","green","red"))
lines(rprimes,unFaded,col="black",pch=1,typ="p",lwd=2)
legend("topright",bty="n",legend=c(
  expression("Natural Irradiation",
             "Unfaded","tirr=6.7x10"^4*" y", "tirr=1.7x10"^5*" y", 
             "Field saturation")),  pch=c(NA,1,2,3,4),
  col=c(NA,"black","blue","green","red"),lwd=2)