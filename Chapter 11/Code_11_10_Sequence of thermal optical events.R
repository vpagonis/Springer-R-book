# Sequence of thermal/optical events (RLumModel)
rm(list=ls())
suppressMessages(library(package = "RLumModel"))
sequence <- list(
  IRR = c(temp = 20, dose = 10, dose_rate = 1),
  TL = c(temp_begin = 20, temp_end = 400, heating_rate = 1))
model.output <- model_LuminescenceSignals(   
  model = "Pagonis2008",   sequence = sequence, main=" ", 
  verbose = FALSE)