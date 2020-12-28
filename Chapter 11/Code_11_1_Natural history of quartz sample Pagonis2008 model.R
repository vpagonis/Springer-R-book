# Use Pagonis model to simulate quartz sample history and 
# TL measured in lab after irradiating with 10 Gy at 1 Gy/s
rm(list=ls())
library("deSolve")
source("Pagonis_Model.R") 
setPars()         
setInis()
irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
heatAt(temp=20, tim=60)                  #Relaxation
heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
irradiate(temp=20, tim=20/1e-11, doseRate=1e-11)# Burial dose
heatAt(20,60)    
storeNat<-inis #Store concentrations for natural sample
TL<-stimTL(lowTemp=20,upTemp=500,hRate=5,nChannel=480) #TL
heatTo(temp1=500,temp2=20,hRate=-5) # Cool down  
tlx<-TL[,"tlx"] 
tly<-TL[,"tly"] 
par(mfrow=c(1,2))
plot(tlx,tly,type="l",ylim=c(0,16000),lwd=2,
     xlab=expression("Temperature "^"o"*"C]"), ylab = "TL [a.u.]") 
legend("topleft",bty="n",legend=c("(a)","Pagonis model",
                                  "Natural TL"))
# repeat irradiation and TL for lab irradiation
inis<-storeNat   # restore concentrations for natural sample
irradiate(temp=20, tim=10, doseRate=1) 
heatAt(temp=20, tim=60)
TL<-stimTL(lowTemp=20,upTemp=500,hRate=5,nChannel=480) 
heatTo(temp1=500,temp2=20,hRate=-5) 
tlx<-TL[,"tlx"] 
tly<-TL[,"tly"] 
plot(tlx,tly,type="l",xlab=expression("Temperature ["^"o"*"C]"), 
     ylab = "TL [a.u.]",ylim=c(0,400000),lwd=2)
legend("topright",bty="n",legend=c("(b)", " ","Regenerated TL"))