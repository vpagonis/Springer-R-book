# Deconvolution of Reference glow curve #1 (project GLOCANIN) 
rm(list=ls())
library("tgcd")
data(Refglow)	# Load the data
# Deconvolve data with 1 peak using the LAMBERT W function 
startingPars <-
  cbind(c(15.0),c(1.0),c(520), c(0.1))   # Im, E, Tm, R
invisible(capture.output(TL1 <- tgcd(Refglow$x001, npeak=1, 
                                     model="lw",inisPAR=startingPars,nstart=10,edit.inis=FALSE)))
print.noquote("Best fit parameters")
TL1$pars
cat("\nGeometrical shape factor=",
    round(TL1$sp[,7],2))
cat("\nFigure Of Merit FOM=",round(TL1$FOM,4))