##=================================================##
## Example:Single Plot for  tunneling CW-IRSL
##=================================================##
rm(list = ls(all=T))
suppressMessages(library("RLumCarlo"))
run_MC_CW_IRSL_TUN(
  A = 5,
  rho = 5e-3,
  times = 0:500,
  r_c = 0.5,
  delta.r = 1e-2,
  method = "par",
  output = "signal"
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo(norm = F, legend = F)
legend("top",bty="n",legend=c("CW-IRSL","TUN function"))