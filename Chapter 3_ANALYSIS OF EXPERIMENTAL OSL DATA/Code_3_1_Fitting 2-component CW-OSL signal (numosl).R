### Fitting CW-OSL signal with package numOSL (2 components)
rm(list=ls())
library("numOSL")
data <- read.table("KST4ph300IR.txt")
data<-data.frame(data[2:500,1],data[2:500,2]) #data t=1-500 s
a<-decomp(data, ncomp=2)
print.noquote("Best fit parameters")
a$LMpars
cat("\nFOM=",a$FOM)