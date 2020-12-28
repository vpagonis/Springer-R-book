#Fit FL1 TR-IRSL data with analytical equation
rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
par(mfrow=c(1,2))
## fit ON data to analytical TR-IRSL equation ----
mydata <- read.table("FL1ONdata.txt")
t<-1e-6*mydata[,1]
y<-mydata[,2]
mydata<-data.frame(t,y)
plot(t*1e6,y,xlab="Time [s]",ylab="TR-IRSL [Normalized]",
     col="black",pch=1)
fit_data <-mydata
fit <- minpack.lm::nlsLM(
  formula=y~ imax*(1-exp (-rho*(log(1 + A*t)) ** 3.0))+bgd,
  data = fit_data,
  start = list(imax=3,A=1e6,rho=0.001,bgd=min(y)))
imax_fit <- coef(fit)[1]
A_fit <- coef(fit)[2]
rho_fit <- coef(fit)[3]
bgd_fit <- coef(fit)[4]
## plot analytical solution
t1<-seq(from=1e-7,to=5e-5,by=1e-7)
lines(
  x = t1*1e6,
  y =imax_fit*(1-exp (-rho_fit*(log(1 + A_fit*
                                      t1)) ** 3.0))+bgd_fit,   col = "red",lwd=2)
legend("topleft",bty="n","(a)")
legend("right",bty="n", pch=c(NA,NA,NA,1,NA),lwd=2,
       lty=c(NA,NA,NA,NA,"solid"),
       c(expression('TR-IRSL','Microcline',' ',
                    'Experiment','Analytical')),
       col=c(NA,NA,NA,"black","red"))
## print results
cat("\nParameters from Least squares fit")
cat("\nImax=",formatC(imax_fit,format="e",digits=2)," cts/s",
    sep="    ","A=",round(A_fit,digits=2)," (s^-1)")
cat("\nrho=",round(rho_fit,digits=4),sep=" ",
    "  bgd=",round(bgd_fit,digits=2)," cts/s")
## Use same parameters to fit the OFF data 
mydata <- read.table("FL1OFFdata.txt")
t<-1e-6*mydata[,1]
y<-mydata[,2]
mydata<-data.frame(t,y)
plot(t*1e6,y,xlab="Time [s]",ylab="TR-IRSL [Normalized]",
     col="black",pch=1)
## plot analytical solution
t1<-seq(from=5e-7,to=1e-4,by=1e-7)
lines(
  x = t1*1e6,
  y =.06+ imax_fit*(exp (-rho_fit*(log(1 + A_fit*t1)) ** 3.0)-
                      exp (-rho_fit*(log(1 + A_fit*(t1+5e-5))) ** 3.0))+bgd_fit,
  col = "red",lwd=2)
legend("topleft",bty="n","(b)")
legend("right",bty="n", pch=c(NA,NA,NA,1,NA),lwd=2,
       lty=c(NA,NA,NA,NA,"solid"),
       c(expression('TR-IRSL','OFF data',' ',
                    'Experiment','Analytical')),col=c(NA,NA,NA,"black","red"))