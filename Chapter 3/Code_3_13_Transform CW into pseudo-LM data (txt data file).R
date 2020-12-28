##Read CWOSL data from .txt file and transform to pseudo-LMIRSL
rm(list=ls())
##produce x and y (time and count data for the data set)
par(mfrow=c(1,2))
CWdata <- read.table("KST4ph300IR.txt")
t<-CWdata[,1][1:100]
CW<-CWdata[,2][1:100]
u<-sqrt(2*t*max(t))
Iu<-u*CW/max(t)
plot(data.frame(t,CW),ylim=c(0,20000))
legend("topright",bty="n",legend=c("(a)"," ","CW-IRSL data",
                                   "KST4 feldspar"))
plot(u,Iu,xlab="u-parameter",typ="o",ylab="Pseudo-LM-IRSL",
     col="red",ylim=c(0,3200))
legend("topright",bty="n",legend=c("(b)"," ","Pseudo-LM-IRSL",
                                   "data"))