# The Zimmerman thermal transfer of holes in quartz
rm(list=ls())
library("deSolve") 
source("Bailey01_Model.R")
setInis()
setPars()
pars["N6"] <- 3e10   # change the concentration of holes in R1
irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
heatAt(temp=20, tim=60)                  #Relaxation
heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
irradiate(temp=20, tim=100/1e-11, doseRate=1e-11)#Burial 100 Gy
storeNat<-inis
# Set up parameters and vectors
testDose<-.1
nTemps<-15
initTemp<-100
finalTemp<-500
actTemp<-seq(initTemp,finalTemp,by=(finalTemp-initTemp)/
               (nTemps-1))
# Loop for activation temperatures
inis<-storeNat   # Reset concentrations for natural sample
res2<-heatTo(25,500,hRate=1)    # heat to activation temperature
par(mfrow=c(1,2))
xlabel<-expression("Temperature T ["^"o"*"C]")
ylabel<-expression("n6,  n7 [cm"^"-3"*"]")
plot(res2[,"time"],res2[,"n6"],xlab = xlabel
     ,ylab =ylabel,ylim=c(0,1.7*max(res2[,"n6"])),pch=1,col="red")
legend("topleft",bty="n",legend=c("(a)","Zimmerman hole",
                                  "transfer effect","R1,R2 -> L"))
par(new = TRUE)
plot(res2[,"time"],res2[,"n7"],xlab = xlabel
     ,ylab =ylabel,ylim=c(0,1.7*max(res2[,"n6"])),pch=2,col="blue")
plot(res2[,"time"],res2[,"n8"],xlab = xlabel
     ,ylab =expression("n8 [cm"^"-3"*"]"),
     ylim=c(.75*max(res2[,"n8"]),1.1*max(res2[,"n8"])))
legend("topleft",bty="n",legend=c("(b)"," ","Hole transfer",
                                  "into L"))