rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library(expint)
## Least squares fit to TL using the KP-TL eqt (TLT model)-
mydata <- read.table("ph300s0.asc")
t<-as.numeric(gsub(",", ".", gsub("\\.", "", mydata[,1])))
y<-as.numeric(gsub(",", ".", gsub("\\.", "", mydata[,3])))
y<-y/max(y)
mydata<-data.frame(t,y)
plot(t,y,xlab="Temperature [\u00B0C]",ylab="Normalized TL",
     col="black",pch=1,xlim=c(200,450))
kb<-8.617e-5
z<-1.8
fit_data <-mydata
kB<-8.617E-5
En<-1.45
T<-t+273
fit <- minpack.lm::nlsLM(
  formula=y~imax* exp(-rho*( (log(1+z*s*kB*((T**2.0)/
                                              abs(En))*exp(-En/(kB*T))*(1-2*kB*T/En)))**3.0))*
    (En**2.0-6*(kB**2.0)*(T**2.0))*( (log(1+z*s*kB*((T**2.0)/
                                                      abs(En))*exp(-En/(kB*T))*(1-2*kB*T/En)))**2.0)/
    (En*kB*s*(T**2)*z-2*(kB**2.0)*s*z*(T**3.0)+exp(En/(kB*T))*En),
  data = fit_data,
  start = list(imax=1e12,s=1e11,rho=.009),upper=c(1e20,1e13,.02),
  lower=c(1e11,1e11,.008))
# Obtain parameters from best fit
imax_fit <- coef(fit)[1]
s_fit <- coef(fit)[2]
rho_fit <- coef(fit)[3]
En_fit <- En
## plot analytical solution
lines(
  x = t,imax_fit* exp(-rho_fit*( (log(1+z*s_fit*kB*((T**2.0)/
                                                      abs(En_fit))*exp(-En_fit/(kB*T))*(1-2*kB*T/En_fit)))**3.0))*
    (En_fit**2.0-6*(kB**2.0)*(T**2.0))*((log(1+z*s_fit*kB*((T**2.0)/
                                                             abs(En_fit))*exp(-En_fit/(kB*T))*(1-2*kB*T/En_fit)))**2.0)/
    (En_fit*kB*s_fit*(T**2)*z-2*(kB**2.0)*s_fit*z*(T**3.0)+
       exp(En_fit/(kB*T))*En_fit),col="red",lwd=2)
legend("topleft",bty="n", pch=c(NA,NA,1,NA),lwd=2,
       lty=c(NA,NA,NA,"solid"),
       c(expression('KST4 feldspar',' ',
                    'Experiment','KP-TL Eq.')),col=c(NA,NA,"black","red"))
## print results
cat("\nBest fit parameters"," ")
cat("\nImax=", formatC(imax_fit, format = "e", digits = 2),
    " s=",formatC(s_fit, format = "e", digits = 2)," (s^-1) ")
cat("\nrho=",round(rho_fit,digits=4),sep=" ","E=",En_fit," eV")