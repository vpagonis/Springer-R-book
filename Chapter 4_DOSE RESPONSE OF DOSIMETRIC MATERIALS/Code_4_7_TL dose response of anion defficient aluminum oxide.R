rm(list = ls(all=T))
options(warn=-1)
library("minpack.lm")
library("lamW")
## fit to Pagonis-Kitis-Chen PKC superlinearity equation ----
t=c(0.0537568,0.103385,0.156481,0.211929,0.260776, 
    0.321015,0.36483,0.414625,0.478926,0.535569,0.589476,
    0.638899,0.824344,0.951985,1.18991,1.63688,3.19332)
y=c(6694.89,15592.6,24767.6,39360.5,52176.2,78075.6,
    101463,131855,189548,227223,272406,376197,469268,
    634929,792121,1.16105e6,1.6992e6)
fit_data <-data.frame(  t ,y)
fit <- minpack.lm::nlsLM(
  formula=y~N*(1-(lambertW0(abs(B)*exp(abs(B)-lamda*t))/
                    abs(B))**beta),
  data = fit_data,
  start = list(N= max(y),B=5, lamda = 10,beta=0.1))
N_fit <- coef(fit)[1]
B_fit <- coef(fit)[2]
lamda_fit <- coef(fit)[3]
beta_fit <- coef(fit)[4]
## print results
cat("\nfitted N: ", N_fit)
cat("\nfitted B: ", B_fit)
cat("\nfitted Dc: ", round(1/lamda_fit,2)," Gy")
cat("\nfitted beta: ", beta_fit)
## plot analytical solution
t<- seq(from=0,to=4,by=.01)
plot(fit_data,log="xy",xlab="Dose [Gy]",ylab="TL")
legend("topleft",bty="n",legend=c("Nikiforov et al. (2014)",
                                  "Anion-deficient","Aluminum Oxide"))
lines( x = t,
       y=N_fit*(1-(lambertW0(abs(B_fit)*exp(abs(B_fit)-lamda_fit*
                                              t))/abs(B_fit))**beta_fit),
       col = "blue",log="xy")