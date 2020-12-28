##Read CW-IRSL data from .txt file and transform to pseudo-LMOSL
rm(list=ls())
suppressMessages(library("Luminescence",warn.conflicts =FALSE))
##produce x and y (time and count data for the data set)
par(mfrow=c(1,2))
values <- read.table("KST4ph300IR.txt")
t<-values[,1][1:100]
CW<-values[,2][1:100]
data<-data.frame(t,CW)
plot(data,xlab="Time [s]",ylab="Original CW-IRSL [cts/s]",
     ylim=c(0,20000))
legend("topright",bty="n",legend=c("(a)"," ","CW-IRSL data",
                                   "KST4 feldspar"))
##transform values
data.transformed <- CW2pLM(data)
plot(data.transformed,xlab="u-parameter",typ="o",
     ylab="Pseudo-LMIRSL",col="red",ylim=c(0,3000))
legend("topright",bty="n",legend=c("(b)"," ","Pseudo-LM-OSL",
                                   "data"))