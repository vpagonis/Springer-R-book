# Simultaneous irradiation and anomalous fading in nature
rm(list=ls())
s<-3e15                  # frequency factor
rho1<-1e-6               # rho-prime values 0.005-0.02
rho2<-2e-6
rho3<-3e-6
Do<-538
X<-3/(1000*365*3600*24)      #natural dose rate X=3 Gy/Ka
curve((1-exp(-x/Do))*exp(-rho1*(log(Do*s/X)**3.0)),1,3500, 200,
      lty=1,lwd=3,
      ylab=expression('L'[FADED]*'  [a.u.]'),xlab="Natural Dose [Gy]",
      col=1,ylim=c(0,1.2))
curve((1-exp(-x/Do))*exp(-rho2*(log(Do*s/X)**3.0)),1,3500,200,
      col=2,lty=2, lwd=3,   add=TRUE)
curve((1-exp(-x/Do))*exp(-rho3*(log(Do*s/X)**3.0)),1,3500,200,
      col=3,   lwd=3,   lty=3,add=TRUE)
legend("topright",bty="n",lwd=3, lty=c(NA,1,2,3),legend =
         c("Acceptor Density"  ,expression("10"^"-6"*""),
           expression("2x10"^"-6"*""),expression("3x10"^"-6"*"")),
       col=c(NA,1:3))
legend('topleft',bty="n",c("Simultaneous", 
                           "Irradiation and fading") ) 