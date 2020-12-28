# Deconvolve TL signal using 9 peaks (no background subtraction)
# a GOK model using user-supplied initial kinetic parameters.
rm(list=ls())
getwd()
library("tgcd")
data(Refglow)
knPars <-
  cbind(c(9824,21009,27792,50520,7153, 5496,6080,1641,2316), # Im
        c(1.24, 1.36, 2.10, 2.65, 1.43, 1.16, 2.48, 2.98, 2.25), # E
        c(387, 428, 462, 488, 493, 528, 559, 585, 602), # Tm
        c(1.02, 1.15, 1.99, 1.20, 1.28, 1.19, 1.40, 1.01, 1.18)) # b
invisible(capture.output(TL1 <- tgcd(Refglow$x009, npeak=9,
                                     model="g1",inisPAR=knPars, nstart=10, edit.inis=FALSE)))
print.noquote("Best fit parameters")
TL1$pars