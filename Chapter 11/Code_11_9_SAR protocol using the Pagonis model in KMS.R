### TEMPLATE1 in Peng and Pagonis (2016)
# SAR protocol using the Pagonis model in KMS
library(deSolve) 
source("Pagonis_Model.R") 
setPars() 
setInis() 
irradiate(temp=20,tim=50000/3.17e-11,doseRate=3.17e-11) 
heatAt(temp=20,tim=60) 
stimOSL(temp=20,tim=6000,pValue=4.73e16,nChannel=6000) 
nCycle<-2
for (i in seq(nCycle)) { 
  irradiate(temp=20,tim=10/3.17e-11,doseRate=3.17e-11) 
  heatAt(temp=20,tim=60) 
  stimOSL(temp=20,tim=6000,pValue=4.73e16,nChannel=6000) 
} #end for. 
irradiate(temp=20,tim=20/3.17e-11,doseRate=3.17e-11) 
heatAt(temp=20,tim=60) 
### TEMPLATE4 in Peng and Pagonis (2016)
reDose<-c(1e-13,8,16,24,32,1e-13,8)
LxTx<-sLxTx<-vector(length=7) 
for (i in seq(7)) { 
  irradiate(temp=20,tim=reDose[i]/0.1,doseRate=0.1) 
  heatAt(temp=20,tim=60) 
  heatTo(temp1=20,temp2=260,hRate=5) 
  heatAt(temp=260,tim=10) 
  heatTo(temp1=260,temp2=125,hRate=-5) 
  Lxdat<-stimOSL(temp=125,tim=100,pValue=4.73e16,nChannel=1000)
  heatTo(temp1=125,temp2=20,hRate=-5) 
  irradiate(temp=20,tim=1/0.1,doseRate=0.1) 
  heatAt(temp=20,tim=60) 
  heatTo(temp1=20,temp2=220,hRate=5) 
  heatAt(temp=220,tim=10) 
  heatTo(temp1=220,temp2=125,hRate=-5) 
  Txdat<-stimOSL(temp=125,tim=100,pValue=4.73e16,nChannel=1000) 
  heatTo(temp1=125,temp2=20,hRate=-5) 
  LxTx[i]<-sum(Lxdat[1:5,"osly"])/sum(Txdat[1:5,"osly"]) 
} #end for. 
plot(reDose,LxTx,pch=c(3,2,2,2,2,1,4),col=c(1,rep(2,4),3,4),
     lwd=2)