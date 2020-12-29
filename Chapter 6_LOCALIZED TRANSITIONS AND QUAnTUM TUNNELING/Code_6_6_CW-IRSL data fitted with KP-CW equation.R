#Fit CW-IRSL data with KP-CW equation
rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
## fit to analytical KP-CW equation for CW-IRSL (TLT model)-
mydata <- read.table("ph300s0IR.asc")
t<-as.numeric(gsub(",", ".", gsub("\\.", "", mydata[,1])))
y<-as.numeric(gsub(",", ".", gsub("\\.", "", mydata[,3])))
mydata<-data.frame(t,y)
plot(t,y,log="xy",
     xlab="Time [s]",ylab="CW-IRSL [counts/s]",col="black",pch=1)
fit_data <-mydata
fit <- minpack.lm::nlsLM(
  formula=y~ imax*exp (-rho*(log(1 + A*t)) ** 3.0)*
    (log(1 + A*t) ** 2.0)/(1 +  t*A)+bgd,
  data = fit_data,
  start = list(imax=3,A=1.1,rho=0.03,bgd=min(y)))
imax_fit <- coef(fit)[1]
A_fit <- coef(fit)[2]
rho_fit <- coef(fit)[3]
bgd_fit <- coef(fit)[4]
## plot analytical solution
lines(
  x = t,
  y =imax_fit*exp (-rho_fit*(log(1 + A_fit*t)) ** 3.0)*
    (log(1 + A_fit*t) ** 2.0)/(1 +  t*A_fit)+bgd_fit,
  col = "red",lwd=2,log="xy")
legend("topright",bty="n", pch=c(NA,NA,1,NA),lwd=2,
       lty=c(NA,NA,NA,"solid"),
       c(expression('KST4 feldspar',' ',
                    'Experiment','KP-CW equation')),col=c(NA,NA,"black","red"))
## print results
cat("Parameters from Least squares fit"," ")
cat("\nImax=",formatC(imax_fit,format="e",digits=2)," cts/s",
    sep="    ","A=",round(A_fit,digits=2)," (s^-1)")
cat("\nrho=",round(rho_fit,digits=4),sep=" ",
    "  bgd=",round(bgd_fit,digits=2)," cts/s")