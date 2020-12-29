# Dose Response of quartz OSL signal measured at 125degC  
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
reDose<-c(1,50,100,200,300,400,500,700)
oslm<-vector(length=length(reDose)) 
oslx<-osly<-matrix(nrow=100,ncol=length(reDose)) 
for (i in seq(length(reDose))) { 
  inis<-storeNat #Restore natural concentrations
  irradiate(temp=20,tim=reDose[i],doseRate=1)
  heatAt(temp=20,tim=60) 
  heatTo(20,125,hRate=5) 
  res<-stimOSL(temp=125, tim=100, pValue=2.0, nChannel=100) 
  oslx[,i]<-res[,"oslx"] 
  osly[,i]<-res[,"osly"] 
  oslm[i]<-res[,"osly"][1]}  
par(mfrow=c(1,2)) 
matplot(oslx,osly,type="l",lty="solid",xlim=c(0,20),
        xlab="Time [s]",ylab = "OSL [a.u.]",ylim=c(0,1.6e8))
legend("topright",bty="n",legend=c("(a)"," ","OSL curves"))
plot(reDose,oslm,type="p",xlab="Dose [Gy]",ylab="OSL [a.u]",
     ylim=c(0,2.6e8))  
legend("topright",bty="n",legend=c("(b)","OSL response",
                                   "Lambert fit"),pch=c(NA,1),lty=c(NA,NA,"solid"))  
t <-reDose
y <-oslm
fit_data <-data.frame(  t ,y)
fit <- minpack.lm::nlsLM(
  formula=y~N*(1+lambertW0((abs(R)-1)*exp(abs(R)-1-b*t))/
                 (1-abs(R))),  data = fit_data,
  start = list(N= max(y),R=0.1, b = .002))
N_fit <- coef(fit)[1]
R_fit <- abs(coef(fit)[2])
b_fit <- coef(fit)[3]
curve(
  N_fit*(1+lambertW0((R_fit-1)*exp(R_fit-1-b_fit*x))/(1-R_fit)),
  1,700,add=TRUE )
cat("\nfitted N: ", N_fit)
cat("\nfitted R: ", R_fit)
cat("\nfitted Dc: ", 1/b_fit, " Gy")