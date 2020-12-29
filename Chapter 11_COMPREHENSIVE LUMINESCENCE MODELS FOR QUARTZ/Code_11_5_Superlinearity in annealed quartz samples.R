# Superlinear TL dose response in annealed quartz
rm(list = ls(all=T))
library("deSolve") 
source("Bailey01_Model.R")
reDose<-seq(0.1,2,by=1.9/7) 
setInis()
setPars() 
irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
heatAt(temp=20, tim=60)                  #Relaxation
heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
irradiate(temp=20, tim=20/1e-11, doseRate=1e-11)#Burial 100 Gy
heatTo(20,700,hRate=5) 
heatAt(700,3600)
heatTo(700,20,hRate=-5)
storeNat<-inis
tlm<-vector(length=8) 
tlx<-tly<-matrix(nrow=480,ncol=8) 
# natural dose rate =0.1 Gy
for (i in seq(8)) { 
  inis<-storeNat 
  irradiate(temp=20,tim=reDose[i],doseRate=1)
  heatAt(temp=20,tim=60) 
  res<-stimTL(lowTemp=20,upTemp=500,hRate=5,nChannel=480) 
  tlx[,i]<-res[,"tlx"] 
  tly[,i]<-res[,"tly"] 
  tlm[i]<-approx(x=res[,"tlx"],y=res[,"tly"],xout=330)$y } 
par(mfrow=c(1,3)) 
matplot(tlx,tly,type="l",ylim=c(0,470),xlim=c(20,380),
        lty="solid",lwd=2,xlab=expression("Temperature ["^"o"*"C]"),
        ylab = "TL [a.u.]") 
legend("topright",bty="n",legend=c("(a)","TL glow curves",
                                   "Annealed quartz"))
plot(reDose,tlm,type="o",xlab = "Dose [Gy]",ylab = "TL [a.u.]",
     ylim=c(0,1.4*max(tlm)),xlim=c(0,2.5))
legend("topleft",bty="n",legend=c("(b)"," ","Superlinear",
                                  "Dose response"))
x<- log(reDose)
y<- log(tlm)
rangeData<-cbind(x,y)
lm(y~x)$coefficients
plot(x, y, xlab = "ln(Dose)",ylab = "ln(TL)",ylim=c(0,7),
     xlim=c(-2,1.5))
legend("topleft",bty="n",legend=c("(c)"," ","Log-Log", 
                                  "slope=1.39"))
abline(lm(y~x))