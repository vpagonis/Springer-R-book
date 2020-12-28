rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library("lamW")
## fit g(D) to Lambert equation ----
t = c(0.0811131, 0.171804,0.450923,0.857988,1.88341,      
      4.44069,8.44947,17.2683,40.7152,83.2104,176.246,
      415.552,819.456,1674.74,3948.68,8983.32)
y = c(1.03326,0.983911,1.08074,1.10465,1.12844,1.28023,
      1.37731,1.50481,1.76636,1.79021,1.45428, 0.813382,      
      0.428722,0.208669,0.0799713,0.0305689)
fit_data <-data.frame(  t ,y)
fit <- minpack.lm::nlsLM(
  formula = y ~ N * (1-(lambertW0(abs(B)*exp(abs(B)- t/Dc))/
                          abs(B))**beta)/t,
  data = fit_data,
  start = list(N= max(y),B=1.2, Dc = 5,beta=0.1)
)
N_fit <- coef(fit)[1]
B_fit <- abs(coef(fit)[2])
Dc_fit <- coef(fit)[3]
beta_fit <- coef(fit)[4]
## print results
cat("\nfitted N: ", N_fit)
cat("\nfitted B: ", B_fit)
cat("\nfitted Dc: ", round(Dc_fit,2)," Gy")
cat("\nfitted beta: ", beta_fit)
## plot analytical solution
t<- seq(from=0.01,to=10000,by=.2)
plot(fit_data,log="x",ylab="Supralinearity Index f(D)",
     xlab="Irradiation Dose, Gy")
lines( x = t,
       y=N_fit*(1-(lambertW0(abs(B_fit)*exp(abs(B_fit)-t/Dc_fit))/
                     abs(B_fit))**beta_fit)/t,       col = "blue",log="x")
legend("topright",bty="n",legend=c("Edmund (2007)",
                                   "figure 5.11","Probe A"))