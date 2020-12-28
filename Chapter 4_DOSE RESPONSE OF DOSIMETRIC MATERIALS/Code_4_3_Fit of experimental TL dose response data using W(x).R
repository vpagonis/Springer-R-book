rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library("lamW")
## fit to Lambert equation  ----
t = c(0.00394056,0.00451523,0.39225,0.412035,0.703062,0.741318, 
      0.742221,1.49553,1.49758,1.5158,2.98473,3.00304,3.02282, 
      5.99852, 6.05755)
y = c(2.45103,2.80847,3.97964,4.28586,5.30465,5.10007,5.66177,
      6.21706,7.49364,6.82965,8.50226,7.88934,8.19555,11.0809,11.7953)
fit_data <-data.frame(  t ,y)
plot(fit_data,ylim=c(0,max(y)),xlab="Dose [Gy]",
     ylab="OSL (L/T)",xlim=c(-.5,6.5))
fit <- minpack.lm::nlsLM(
  formula =y~N*(1+lambertW0((abs(R)-1)*exp(abs(R)-1-abs(b)*
                                             (t+abs(f))))/(1-abs(R))),
  data = fit_data,
  start = list(N= max(y),R=.9, b = .1,f=0.3))
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- abs(coef(fit)[3])
f_fit <- abs(coef(fit)[4])
## plot analytical solution
t<- seq(from=-0.5,to=7,by=.02)
lines(
  x = t,
  y=N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit*
                                       (t+f_fit)))/(1-R_fit)),
  col = "blue")
legend("right",bty="n",legend=c("Berger (1990)",
                                "Volcanic glass"," "))
## print results
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted Dc: ", round(1/b_fit,2), "Gy")
cat("\nfitted f: ", f_fit)