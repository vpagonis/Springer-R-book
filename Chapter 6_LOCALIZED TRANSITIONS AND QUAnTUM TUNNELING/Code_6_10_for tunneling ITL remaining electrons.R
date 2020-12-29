# MC Model for remaining charges and ITL signal
rm(list = ls(all=T))
library(RLumCarlo)
par(mfrow=c(1,2))
results <- run_MC_ISO_TUN(
  E = 1.0,
  s = 1e12,
  T = 250,
  rho = 0.01,
  clusters=100,
  times = seq(0, 200),
  output = "remaining_e"
) %T>%
  plot_RLumCarlo(
    legend = FALSE,
    ylab = "Remaining electrons"  )
legend("topright",bty="n",legend=c("(a)"," ","n(t)",
                                   "TUN function"))

results <- run_MC_ISO_TUN(
  E = 1.2,
  s = 1e12,
  T = 250,
  rho = 0.01,
  times = seq(0, 200)
) %T>%
  plot_RLumCarlo(norm = FALSE, legend = FALSE)
legend("topright",bty="n",legend=c("(b)"," ","ITL signal"))