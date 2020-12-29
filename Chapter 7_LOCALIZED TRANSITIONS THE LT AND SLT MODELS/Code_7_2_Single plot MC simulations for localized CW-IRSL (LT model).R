##===========================================================##
## Example: MC simulations for localized CW_IRSL (LT model)
##===========================================================##
rm(list = ls(all=T))
library(RLumCarlo)
run_MC_CW_IRSL_LOC(
  A = 0.12,
  times = 0:100,
  clusters = 10,
  n_filled = 100,
  r = 1e-7,
  method = "seq",
  output = "signal"
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo(legend = T)
legend("top",bty="n",c("CW-IRSL signal","LOC function",
                       "LT model"))