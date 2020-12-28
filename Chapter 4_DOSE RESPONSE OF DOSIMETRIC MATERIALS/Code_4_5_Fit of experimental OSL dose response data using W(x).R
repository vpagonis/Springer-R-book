rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library("lamW")
par(mfrow=c(1,2))
## fit to saturation exponential ----
t = c(-34.2466, 34.2466, 68.4932, 273.973, 1027.4,1986.3,3013.7,
      5000, 7979.45, 10000)
y = c(1.04664, 0.000978474, 2.76386, 7.24592,12.6008,14.5329,
      15.8956,  17.1905, 17.847, 18.0952)
fit_data <-data.frame(  t ,y)
plot(fit_data,ylim=c(0,max(y)),xlab="Dose [Gy]",
     ylab="OSL (L/T)")
fit <- minpack.lm::nlsLM(
  formula=y~N*(1+lambertW0((abs(R)-1)*
                             exp(abs(R)-1-b*t))/(1-abs(R))),
  data = fit_data,
  start = list(N= max(y),R=.9, b = .01))
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- coef(fit)[3]
## plot analytical solution
t<- seq(from=0,to=10000,by=100)
lines(  x = t,
        y=N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit* t))/
                   (1-R_fit)),  col = "blue")
legend("right",bty="n",legend=c("(a)"," ","Fine grain",
                                "Quartz"," "))
## print results
cat("\nFine grain"," ")
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted Dc: ",round( 1/b_fit,2), "Gy")
## fit to Lambert equation ----
t = c(0, 3.5583, 44.1822, 258.718, 1051.62, 2044.98, 3003.94,
      5024.61, 7046.32, 9992.29)
y = c(0, 0.93512, 2.61108, 4.99104, 6.36704, 6.42148, 
      6.43643, 6.46792, 6.77215, 6.97391)
fit_data <-data.frame(  t ,y)
plot(fit_data,ylim=c(0,max(y)),xlab="Dose[Gy]",ylab="OSL (L/T)")
fit <- minpack.lm::nlsLM(
  formula=y~N*(1+lambertW0((abs(R)-1)*
                             exp(abs(R)-1-b*t))/(1-abs(R))),
  data = fit_data,
  start = list(N= max(y),R=10, b = 1e-4))
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- coef(fit)[3]
## plot analytical solution
t<- seq(from=0,to=10000,by=100)
lines(  x = t,
        y=N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit*t))/(1-R_fit)),
        col = "blue")
legend("right",bty="n",legend=c("(b)"," ","Coarse grain",
                                "Quartz"," "))
## print results
cat("\nCoarse grain"," ")
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted Dc: ",round( 1/b_fit,2)," Gy")