##=================================================##
## Example 1: Single MC Plot  for delocalized TL
##=================================================##
rm(list = ls(all=T))
library(RLumCarlo)
run_MC_TL_DELOC(
  s = 3.5e12,
  E = 1.45,
  R = 0.1,
  times = 100:350
) %>%
  #Plot results of the MC simulation
  plot_RLumCarlo
legend("topleft",bty="n",c("TL signal", "using 
DELOC","function"))