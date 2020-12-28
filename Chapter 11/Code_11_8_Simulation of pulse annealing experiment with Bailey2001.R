#Simulation of pulse annealing experiment with 
# Bailey2001 model
rm(list=ls())
library("deSolve") 
source("Bailey01_Model.R")
setInis()
setPars ()
pars["N6"] <- 3e9
### Start natural sample simulations
irradiate(temp=20, tim=1000, doseRate=1) 
heatAt(temp=20, tim=60) 
heatTo(temp1=20, temp2=350, hRate=5) 
heatTo(temp1=350, temp2=200, hRate=-5) 
stimOSL(temp=200, tim=100, pValue=2.0, nChannel=10)
irradiate(temp=20, tim=51/1e-11, doseRate=1e-11)  #Burial 51 Gy 
## End of natural history with natural irradiation
res<- heatAt(temp=20, tim=60)
storeNat<-inis
nTemps<-20
initTemp<-100
finalTemp<-350
actTemp<-seq(initTemp,finalTemp,by=(finalTemp-initTemp)/
               (nTemps-1))
tlm<-vector(length=nTemps) 
tlx<-tly<-matrix(nrow=1000,ncol=nTemps) 
tl<-osl<-rep(0,nTemps)
par(mfrow=c(1,2))
for (i in seq(nTemps)) { 
  heatTo(20,actTemp[i],hRate=1) 
  heatAt(actTemp[i],tim=10)
  heatTo(actTemp[i],20,hRate=-1) 
  heatTo(20,125,hRate=1)
  resOSL<-stimOSL(temp=125, tim=0.1, pValue=2.0, nChannel=10)
  osl[i]<-sum(resOSL[,"osly"])
  heatTo(125,20,hRate=-1)
  irradiate(temp=20,tim=0.1,doseRate=1)
  heatAt(20,tim=60)
  resTL<-stimTL(20,160,hRate=1,nChannel=110) 
  tl[i]<-max(resTL[,"tly"])} 
xlabel<-expression("Activation T ["^"o"*"C]")
plot(actTemp,osl/tl,xlab=xlabel,
     ylim=c(0,200000),ylab="Sensitivity corrected OSL/TL")
legend("topright",bty="n",legend=c("(a)"," ","Pulse annealing",
                                   "Natural sample"))
#
inis<-storeNat  # Restore concentrations of natural sample
heatTo(20,125,hRate=1)
stimOSL(temp=125, tim=200, pValue=2.0, nChannel=10)
heatTo(125,20,hRate=-1)
irradiate(temp=20,tim=56,doseRate=1)  #56 Gy in lab
for (i in seq(nTemps)) { 
  heatTo(20,actTemp[i],hRate=1) 
  heatAt(actTemp[i],tim=10)
  heatTo(actTemp[i],20,hRate=-1) 
  heatTo(20,125,hRate=1)
  resOSL<-stimOSL(temp=125, tim=0.1, pValue=2.0, nChannel=10)
  osl[i]<-sum(resOSL[,"osly"])
  heatTo(125,20,hRate=-1)
  irradiate(temp=20,tim=0.1,doseRate=1)
  heatAt(20,tim=60)
  resTL<-stimTL(20,160,hRate=1,nChannel=110) 
  tl[i]<-max(resTL[,"tly"])} 
plot(actTemp,osl/tl,xlab=xlabel,ylim=c(0,320000),
     ylab="Sensitivity corrected OSL/TL")
legend("topright",bty="n",legend=c("(b)"," ",
                                   "Irradiated sample"))