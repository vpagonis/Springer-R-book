#Dose response of TL (RlumModel package
rm(list=ls())
suppressMessages(library(package = "RLumModel")) 
##set list with laboratory doses
Lab.dose <- seq(from = 2, to = 8, by = 2)
model.output <- lapply(Lab.dose, function(x){
  sequence <- list(
    IRR = c(20, x, 0.1),
    TL = c(20, 400, 1))
  TL_data <- model_LuminescenceSignals(
    sequence = sequence,
    model = "Bailey2001",
    plot = FALSE,
    verbose = FALSE)
  return(Luminescence::get_RLum(TL_data, recordType = "TL$", 
                                drop = FALSE))
})
model.output.merged <- merge_RLum(model.output)
plot_RLum(
  object = model.output.merged,
  xlab = "Temperature [\u00B0C]",
  ylab = "TL signal [a.u.]",main=" ",lwd=3,lty=1:4,
  legend.text = paste(Lab.dose, " Gy"),
  combine = TRUE)
legend("top",bty="n",legend=c("TL dose response","RLumModel"))