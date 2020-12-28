#Fit dose response data with Saturating Exponential
rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library("lamW")
## fit to saturation exponential ----
t = c( 0,50.7117,100.534,152.135,204.626,272.242)
y = c(0,33.144,42.205,43.1055,44.4157,43.7098)
fit_data <-data.frame(  t ,y)
plot(fit_data,ylim=c(0,max(y)),xlab="Dose [Gy]",
     ylab="OSL (L/T)")
fit <- minpack.lm::nlsLM(
  formula = y ~ N * (1-exp(-b* t)),
  data = fit_data,
  start = list(N= max(y),b = .01))
N_fit <- coef(fit)[1]
b_fit <- coef(fit)[2]
## plot analytical solution
t1<-0:300
lines(x = t1,  y = N_fit  * (1-exp(-b_fit* t1)),col = "blue")
legend("right",bty="n",legend=c("Quartz OSL"," ",
                                "Libyan quartz"))
## print results
cat("\nfitted N: ", N_fit)
cat("\nfitted Do: ", round(1/b_fit,2), "Gy")