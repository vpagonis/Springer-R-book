#Simulate TR-PL experiments with Nikiforov/Pagonis model
rm(list = ls(all=T))
library("deSolve")
PagonisAlumina <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dn<- -s*exp(-E/(kb*(273+T)))*n+alpha*(N-n)*nc
    dm1<- delta1*(M1-m1)*nc
    dm2<- delta2*(M2-m2)*nc
    dnc<- s*n*exp(-E/(kb*(273+T)))-delta1*(M1-m1)*nc-
      delta2*(M2-m2)*nc-Gamma*nF*nc-alpha*(N-n)*nc+w1*n1P
    dnF<- -Gamma*nF*nc+w1*n1P
    dn3P<-w2*n1P-C*exp(-W/(kb*(273+T)))*n3P-w3*n3P
    dn1P<-f+Gamma*nF*nc-w1*n1P-w2*n1P
    res <- c(dn,dm1,dm2,dnc,dnF,dn3P,dn1P)
    list(res)  })}

TempVar<-function(T){
  parms<- c(E=1.3, s=1e13,alpha=1e-14, delta1=1e-12, delta2=1e-14,
            Gamma=1e-11,N=1e13,M1=1e15,T=T,M2=1e14,C=1e13,W=1,w1=1,w2=3e3,
            w3=w3,f=1e10,kb=8.617e-5)
  y <- xstart <- c(n = 0, m1=1e14,m2=0,nc=0,nF=1e14,n3P=0,n1P=0)
  out <-  ode(xstart, times, PagonisAlumina, parms)  }
w3<-29  
times  <- seq(0, .2,by=.005)
Temps<-seq(70,220,10)
Lc<-temps<-matrix(NA,nrow=length(times),ncol=length(Temps))
areaLc<-vector(length=length(Temps))
for (i in 1:length(Temps)){
  T<-Temps[i]
  a<-TempVar(T)
  Lc[,i]<- w3*a[,"n3P"]
  areaLc[i]<-sum(Lc[,i],rm.NA=TRUE)}
par(mfrow=c(1,2))
plot(times,Lc[,6],typ="o",col="black",xlim=c(0,.2),
     ylim=c(0,1.7e10),pch=1, xlab="Time [s]", 
     ylab="TR-OSL Intensity [a.u.]")
lines(times,Lc[,12],typ="o",col="red",pch=2)
lines(times,Lc[,15],typ="o",col="blue",pch=3)
legend("topleft",bty="n",pch=c(NA,1,2,3),
       lty=c(NA,rep("solid",3)),col=c(NA,"black",
                                      "red","blue"),legend=c(expression("(a)  Al"[2]*"O"[3]*":C",
                                                                        "120"^o*"C","180"^o*"C","210"^o*"C")))
plot(Temps,areaLc,typ="o",col="blue",lwd=2,pch=1,ylim=c(0,6e11),
     xlab=expression("Stimulation Temperature"^o*"C"),
     ylab="TR-OSL Areas [a.u.]")
legend("topleft",bty="n",legend=c("(b)"," ","Thermal quenching",
                                  "TR-OSL Areas"))