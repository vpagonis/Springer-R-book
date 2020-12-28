# Deconvolution of Reference GLOCANIN glow curve #1 with MOK 
rm(list=ls())
library("tgcd")
data(Refglow)
# Load the data.
# Deconvolve data with 1 peak using the MOK expression
startingPars <-
  cbind(c(15.0),  c(1.0), c(520), c(0.1)) # Im, E, Tm, R
invisible(capture.output(TL1 <- tgcd(Refglow$x001, npeak=1, 
                                     model="m1",inisPAR=startingPars, nstart=10, edit.inis=FALSE)))
print.noquote("Best fit parameters")
TL1$pars