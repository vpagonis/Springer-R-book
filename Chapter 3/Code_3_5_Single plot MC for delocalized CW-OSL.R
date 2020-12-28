<<Single Plot for delocalized CW-OSL,eval=TRUE,results='hold',fig.show='hold',collapse=TRUE,fig.width=6,out.width='80%',fig.height=4.5 >>=
  ##==================================================##
  ## MC simulations for delocalized CW-OSL
  ##==================================================##
  rm(list = ls(all=T))
library(RLumCarlo)
run_MC_CW_OSL_DELOC(
  A = 0.12,
  R = 0.1,
  times = 0:100
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo(legend = F,ylab="CW-OSL [a.u.]")
legend("top",bty="n", legend=c("CW-OSL", "with function DELOC",
                               "Package RLumCarlo")