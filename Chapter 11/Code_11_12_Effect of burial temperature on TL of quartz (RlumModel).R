#Effect of burial temperature on TL of quartz (RlumModel)
rm(list=ls())
suppressMessages(library(package = "RLumModel")) 
##set list with burial temperatures
burial.temp <- seq(from = 0, to = 30, by = 10)
model.output <- lapply(burial.temp, function(x){
  sequence <- list(
    IRR = c(x, 20, 1e-11),
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
  legend.text = paste(burial.temp, "\u00B0C"),ylim=c(0,70000),
  combine = TRUE)
legend("top",bty="n",legend=c("Burial Temperature","RLumModel"))