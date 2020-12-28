rm(list=ls())
options(warn=-1)
library("minpack.lm")
library("lamW")
# Load the data
TLqzx<-c(174.13, 345.027, 931.847, 1603.74, 2524.39, 4031.12, 
         6372.18,9044.09,12217.5,15058.3,19981.5,25072.3,30082.3,40011.4)
TLqzy<-c(1.07478, 1.68389, 2.18591, 2.93875, 3.51271, 4.48122, 
         5.59377,6.3484,7.3184,8.39557,9.33133,10.4105,11.8478,13.6478)
plot(TLqzx,TLqzy,type="p",pch=1,col="red",
     xlab=expression("Dose [Gy]"),ylab ="ESR signal [a.u.]",
     ylim=c(0,20))
legend("topleft",bty = "n",
       legend = c("Quartz"," ","ESR dose response"))
## fit to Lambert equation  ----
t <-TLqzx
y<-TLqzy 
fit_data <-data.frame(  t ,y)
#plot(fit_data,ylim=c(0,max(y)))
fit <- minpack.lm::nlsLM(
  formula=y~N*(1+lambertW0((abs(R)-1)*exp(abs(R)-1-b*t))/
                 (1-abs(R))),
  data = fit_data,
  start = list(N= max(y),R=.9, b = .01)
)
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- coef(fit)[3]
## plot analytical solution
t<- seq(from=0,to=40000,by=100)
lines(  x = t,
        y=N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit*t))/(1-R_fit)),
        col = "blue")
## print results
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted Dc: ", 1/b_fit, " Gy")