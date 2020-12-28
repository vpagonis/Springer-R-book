rm(list=ls())
library("minpack.lm")
par(mfrow=c(1,2))
aluminatau470nmx<-unlist(read.table("aluminax1.txt"))
aluminatau470nmy<-unlist(read.table("aluminay1.txt"))
x<-aluminatau470nmx[8:20]+273
y<-aluminatau470nmy[8:20]
kB<-8.617*1e-5 # Boltzmann constant in eV/K
fit_data <-data.frame(x ,y)
plot(aluminatau470nmx+273,aluminatau470nmy,
     xlab=expression("Temperature [K]"),
     ylab="Integrated-TR-OSL [a.u.]")
legend("topright",bty="n","(a)")
legend("bottomleft",bty = "n", legend =
         c(expression('Al'[2]*'O'[3]*':C',' ',
                      'Thermal','quenching',' ','Fit to find C,W')))
fit <- minpack.lm::nlsLM(
  formula = y ~ N /(1+c*exp(-W/(kB*x))),data = fit_data,
  start = list(N= max(y),c=1e6, W =1))
x1<-seq(from=350,to=500,by=1)
lines(x=x1,y=coef(fit)[1]/(1+coef(fit)[2]*
                             exp(-coef(fit)[3]/(kB*x1))),      col = "blue")
coef(fit)
al2o3risoOSLvsTempx<-unlist(read.table("aluminax2.txt"))
al2o3risoOSLvsTempy<-unlist(read.table("aluminay2.txt"))
x<-al2o3risoOSLvsTempx[4:10]
y<-al2o3risoOSLvsTempy[4:10]
y<-log(y)
x<-1/(kB*(x+273.15))
bestfit<-lm(y~x)
coefficients(bestfit)
plot(x, y, xlab = "1/(kT)  [1/eV]",ylab = "ln(TR-OSL)")
legend("topright",bty = "n", legend =c('(b)',expression(' ',
                                                        'Fit to find E'[th]*' ')))
abline(lm(y~x))