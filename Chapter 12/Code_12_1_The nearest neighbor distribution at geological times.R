# The nearest neighbor distribution at geological times
rm(list=ls())
s<-3e15                 # frequency factor
rho<-1e-6               # rho-prime values 0.005-0.02
rc<-0.0                 # for freshly irradiated samples, rc=0
timesAF<-3.154e7*c(0,1e2,1e4,1e6)          # times in seconds
rprimes<-seq(from=rc,to=2.2,by=0.002)    # rprime=0-2.2

##### function to find distribution of distances ###
AFfortimeT<-function(tim){3*(rprimes**2.0)*exp(-(rprimes**3.0))*
    exp(-exp(-(rho**(-1/3))*rprimes)*s*tim)}
######

distribs<-sapply(timesAF,AFfortimeT)
# Plots
cols=c(NA,NA,NA,"black","red","green","blue")
matplot(rprimes,distribs,xlab="Dimensionless distance r'",
        ylab="Nearest neighbor Distribution g(r')",type="l",lwd=4)
legend("topright",bty="n", lty=c(NA,NA,NA,1,2,3,4), lwd=4,
       col=cols,legend = c("Elapsed", "time"  ," ","t=0 s",
                           expression("10"^"2"*" years"),
                           expression("10"^"4"*" years"),expression("10"^"6"*" years")))