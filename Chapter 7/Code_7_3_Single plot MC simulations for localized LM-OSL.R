##===========================================================##
##===========================================================##
## MC simulations for localized LM-OSL
##===========================================================##
rm(list = ls(all=T))
library(RLumCarlo)
run_MC_LM_OSL_LOC(
  A = 0.1,
  times = 0:250,
  clusters = 100,
  n_filled = 50,
  r = 1e-7,
  method = "seq",
  output = "signal"
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo(legend = T)
legend("top",bty="n",c("LM-IRSL signal","LOC function",
                       "LT model"))