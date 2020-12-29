##====================================================##
## Example 1: MC simulations of tunneling LM_IRSL
##====================================================##
rm(list = ls(all=T))
library(RLumCarlo)
run_MC_LM_OSL_TUN(
  A =3.0,
  rho = 1e-2,
  times = 0:200,
  clusters = 100,
  N_e = 20,
  r_c = 0.001,
  delta.r = 1e-1,
  method = "par",
  output = "signal"
) %>%
  # Plot results of the MC simulation
  plot_RLumCarlo(norm = F,legend=F)
legend("topright",bty="n",legend=c("LM-IRSL signal",
                                   "TUN function",""))