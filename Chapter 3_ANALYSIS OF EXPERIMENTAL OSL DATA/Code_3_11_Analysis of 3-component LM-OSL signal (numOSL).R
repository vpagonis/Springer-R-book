### Example: Analyze LM-OSL signal using package numOSL
rm(list=ls())
library("numOSL")
CaF2LMx <- read.table("CaF2LMx.txt")
CaF2LMy <- read.table("CaF2LMy.txt")
d<-data.frame(CaF2LMx,CaF2LMy)
a<-decomp(d,ncomp=3,typ="lm",log="",
          control.args=list(maxiter=10))
print.noquote("Best fit parameters")
a$LMpars
cat("\nFOM=",a$FOM)