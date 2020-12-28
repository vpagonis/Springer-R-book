rm(list=ls())
library("minpack.lm")
mydata <- read.table("chithamboTROSLqzdata.txt")
plot(mydata,pch=2,col="red",  ylab="TR-OSL [a.u.]",
     xlab=expression(paste("Time [",mu,"s]")))
legend("topright",bty="n",legend=c("Sedimentary","quartz"," ",
                                   "470 nm","LEDs"))
# fit ON data
t<-mydata[,1][1:20]
y<-mydata[,2][1:20]
fit_data <-data.frame(t ,y)
fit <- minpack.lm::nlsLM(
  formula = y ~ N * (1-exp(- t/b))+abs(bgd), data = fit_data,
  start = list(N= max(y),b = 40,bgd=10))
t1<-0:55
lines(x = t1,  y = coef(fit)[1]  * 
        (1-exp(- t1/coef(fit)[2]))+abs(coef(fit)[3]),col = "blue")
coef(fit)
# fit OFF data
t<-mydata[,1][21:length(mydata[,1])]-51.27
y<-mydata[,2][21:length(mydata[,1])]
fit_data <-data.frame(t ,y)
fit <- minpack.lm::nlsLM(
  formula = y ~ N * (exp(- t/b))+abs(bgd),data = fit_data,
  start = list(N= max(y),b = 40,bgd=10))
t1<-5:300
lines(x=t1+51.2,y=coef(fit)[1]*(exp(-(t1)/coef(fit)[2]))+
        abs(coef(fit)[3]),col = "blue")
coef(fit)