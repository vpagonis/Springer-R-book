# Use Pagonis model to simulate thermal quenching in quartz 
# TL measured in laboratory with different heating rates
rm(list=ls())
library("deSolve")
source("Pagonis_Model.R")
beta<-2*seq(1:6)
tlylow<-tlyhigh<-vector(length = length(beta))
tlx<-tly<-matrix(nrow=480,ncol=length(beta))
for (i in 1:6){
  setPars()         
  setInis()
  irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
  heatAt(temp=20, tim=60)                  #Relaxation
  heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
  heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
  stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
  irradiate(temp=20, tim=20/1e-11, doseRate=1e-11)# Burial dose
  heatAt(20,60)    
  irradiate(temp=20, tim=100, doseRate=1) 
  heatAt(temp=20, tim=60)
  TL<-stimTL(lowTemp=20,upTemp=500,hRate=beta[i],nChannel=480) 
  tlx[,i]<-TL[,"tlx"]
  tly[,i]<-TL[,"tly"]/i
  tlyhigh[i]<-sum(tly[,i][130:400])
  tlylow[i]<-sum(tly[,i][1:130])}
par(mfrow=c(1,2))
matplot(tlx,tly,typ="l",lty="solid",lwd=2,
        xlab=expression("Temperature ["^"o"*"C]"), ylab = "TL [a.u.]",
        ylim=c(0,350000),xlim=c(30,380))
legend("topleft",bty="n",legend=c("(a)","Heating 2-12 K/s",
                                  "Thermal quenching"))
plot(beta,tlylow/max(tlylow),pch=1,typ="o",ylim=c(0,1.2),
     xlab="Heating rate [K/s]", ylab = "Normalized TL Area")
lines(beta,colSums(tly)/max(colSums(tly)),pch=2,typ="o")
lines(beta,tlyhigh/max(tlyhigh),pch=3,typ="o")
legend("bottomleft",bty="n",legend=expression("(b)",
                                              "Area 0-130"^"o"*"C","Area 0-500"^"o"*"C","Area
130-500"^"o"*"C"),pch=c(NA,1,2,3))