rm(list = ls(all=T))
library(RLumCarlo)
times <- seq(0, 200)
run_MC_CW_IRSL_TUN(A = 3, rho = 0.003, times = times) %>%
  plot_RLumCarlo(norm = TRUE, lty=1,legend = TRUE)
run_MC_CW_IRSL_TUN(A = 6, rho = 0.003, times = times) %>%
  plot_RLumCarlo(norm = TRUE, lty=2,col="blue",add = TRUE)
legend("top",bty="n",legend=c(expression("CW-IRSL","TUN function",
                                         "A=3.0 s"^-1*" ","A=6.0 s"^-1*" ")),lty=c(NA,NA,1:2))