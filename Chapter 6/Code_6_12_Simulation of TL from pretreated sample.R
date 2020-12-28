##=============================================
## Simulate TL from pretreated sample
##=============================================
rm(list = ls(all=T))
library(RLumCarlo)
s <- 3.5e12
rho <- 0.015
E <- 1.45
r_c <- c(0,0.8,1.0)
times <- seq(100, 450) # time = temperature
results <- lapply(r_c, function(x) {
  run_MC_TL_TUN(
    s = s,
    E = E,
    rho = rho,
    r_c = x,
    times = times
  )})%>%
  plot_RLumCarlo(norm = FALSE, legend = TRUE)
legend("topleft",bty="n",legend=c(expression('Critical Radius',
                                             ' ','r'[c]*'=0, 0.8, 1.0')) )