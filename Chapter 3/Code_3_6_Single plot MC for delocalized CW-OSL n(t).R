##=================================================##
## MC simulations for delocalized CW-OSL
##=================================================##
rm(list = ls(all=T))
library(RLumCarlo)
run_MC_CW_OSL_DELOC(
  n_filled=100,N_e=100,A = 0.12,
  R = 0.1,
  times = 0:100,
  output="remaining_e"
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo(legend = F,xlab="Time (s)",
                 ylab="Filled Traps n(t)")
legend("topright",bty="n",legend=c("Remaining trapped electrons",
                                   "during CW-OSL experiment"))