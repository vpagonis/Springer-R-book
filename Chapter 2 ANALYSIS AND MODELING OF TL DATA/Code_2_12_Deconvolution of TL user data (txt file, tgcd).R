# Deconvolve data with 2 peaks using the LAMBERT W function
rm(list=ls())
library("tgcd")
# Load the data
mydata = read.table("lbodata.txt")
startingPars <-
  cbind(c(105.0,5.0),c(1.1,1.4),c(460,550),c(0.01,.01)) #Im,E,Tm,R
invisible(capture.output(TL1 <- tgcd(mydata, npeak=2, 
                                     model="lw",inisPAR=startingPars, nstart=10, edit.inis=FALSE)))
print.noquote("Best fit parameters")
TL1$pars
cat("\nGeometrical shape factors"," ")
round(TL1$sp[,7],2)