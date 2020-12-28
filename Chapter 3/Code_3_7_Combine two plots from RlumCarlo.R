##=====================================================##
## Example: COMBINE TWO PLOTS  for delocalized CW-OSL 
##=====================================================##
rm(list = ls(all=T))
library(RLumCarlo)
## set time vector
times <- seq(0, 50)
run_MC_CW_OSL_DELOC(A = 0.1, R = 0.01,n_filled=100,N_e=100,
                    times = times) %>%
  plot_RLumCarlo(norm = TRUE, col="blue",
                 legend = TRUE)
legend("top",bty="n",legend=c("CW-OSL signal", 
                              expression("with A=0.1, 0.2 s"^"-1"*"")))
run_MC_CW_OSL_DELOC(A = 0.2, R = 0.01,n_filled=100,N_e=100,
                    times = times) %>%
  plot_RLumCarlo(norm = TRUE, col="red", add = TRUE)