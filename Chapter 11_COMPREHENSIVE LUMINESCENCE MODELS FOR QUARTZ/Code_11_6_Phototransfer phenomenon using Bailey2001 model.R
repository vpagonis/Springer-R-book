#Phototransfer simulation using Bailey2001 model
rm(list = ls(all=T))
library("deSolve") 
source("Bailey01_Model.R")
photo<-function(x,colr,ln,pchs){
  setInis()
  setPars() 
  pars["N6"] <- 3e10 # change the parameter N6 in Bailey model
  # N6=total concentration of holes in hole reservoir R1
  irradiate(temp=20, tim=1000, doseRate=1) #1000 Gy at 1 Gy/s
  heatAt(temp=20, tim=60)                  #Relaxation
  heatTo(temp1=20, temp2=350, hRate=5)     # Heat to 350 degC
  heatTo(temp1=350, temp2=200, hRate=-5)   # Cool down to RT
  stimOSL(temp=200, tim=100, pValue=2.0, nChannel=1000) #Bleach
  # Large Burial dose 100 Gy
  irradiate(temp=20, tim=200/1e-11, doseRate=1e-11)
  # end natural sample simulation, store concentrations
  # Next is the optical Bleach
  stimOSL(temp=20, tim=x, pValue=2.0, nChannel=1000) 
  heatAt(temp=20, tim=60)
  TL<-stimTL(lowTemp=20,upTemp=500,hRate=5,nChannel=150) #TL
  heatTo(temp1=500,temp2=20,hRate=-5) # Cool down  
  tlx<-TL[,"tlx"] 
  tly<-TL[,"tly"] 
  par(new=TRUE)
  plot(tlx,tly,type="o",xlab=expression("Temperature ["^"o"*"C]"),
       ylab = "TL [a.u.]",xlim=c(0,400),ylim=c(0,7e5),col=colr,lty=ln,
       lwd=1,pch=pchs)
}   #end function photo
for (i in 1:3)
{bleachTime<-c(1,3,6)
colr<-c("red","blue","black")
ln<-rep("solid",3)
pchs<-c(1,2,4)
photo(bleachTime[i],colr[i],ln[i],pchs[i])}
legend("topleft",bty="n","Phototransfer")
legend("topright",bty="n",lty=c(NA,ln),col=c(NA,colr),lwd=1,
       pch=c(NA,pchs), c("Bleaching times","1 s","3 s","6 s"))