# Dose Response of 110degC TL peak in quartz
rm(list=ls())
library("deSolve")
library("minpack.lm")
library("lamW")
source("Bailey01_Model.R")
setPars()         
setInis()
irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
heatAt(temp=20, tim=60)                  #Relaxation
heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
irradiate(temp=20, tim=20/1e-11, doseRate=1e-11)# Burial dose
heatAt(20,60)    #Store concentrations for natural sample
storeNat<-inis  # in variable storeNat
reDose<-c(1,3,5,10,20,60,100,200,300,400,500,700)
tlm<-vector(length=length(reDose)) 
tlx<-tly<-matrix(nrow=480,ncol=length(reDose)) 
for (i in seq(length(reDose))) { 
  inis<-storeNat 
  irradiate(temp=20,tim=reDose[i],doseRate=1)
  heatAt(temp=20,tim=60) 
  res<-stimTL(lowTemp=20,upTemp=500,hRate=5,nChannel=480) 
  heatTo(temp1=500,temp2=20,hRate=-5) 
  tlx[,i]<-res[,"tlx"] 
  tly[,i]<-res[,"tly"] 
  tlm[i]<-approx(x=res[,"tlx"],y=res[,"tly"],xout=330)$y } 
par(mfrow=c(1,2)) 
matplot(tlx,tly,type="l",lty="solid",ylab = "TL [a.u.]",
        xlab=expression("Temperature "^"o"*"C]"),ylim=c(0,1.4e6),
        xlim=c(30,380))
legend("topright",bty="n",legend=c("(a)"," ","TL glow curves"))
plot(reDose,tlm,type="p",xlab="Dose [Gy]",ylab="TL [a.u]",
     ylim=c(0,.95e6))  
legend("topright",bty="n",legend=c("(b)","Dose response",
                                   "Lambert fit"),pch=c(NA,1),lty=c(NA,NA,"solid"))
t <-reDose
y <-tlm
fit_data <-data.frame(  t ,y)
#plot(fit_data,ylim=c(0,max(y)))
fit <- minpack.lm::nlsLM(
  formula=y~N*(1+lambertW0((abs(R)-1)*exp(abs(R)-1-b*t))/
                 (1-abs(R))),
  data=fit_data,start = list(N=5* max(y),R=0.1, b = .002))
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- coef(fit)[3]
## plot analytical solution
curve(
  N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit*x))/(1-R_fit)),
  0,700,col = "blue",add=TRUE,lwd=2)
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted D0: ", 1/b_fit, " Gy")