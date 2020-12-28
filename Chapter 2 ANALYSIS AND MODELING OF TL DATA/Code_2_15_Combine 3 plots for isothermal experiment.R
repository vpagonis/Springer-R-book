##=======================================================##
## COMBINE 3 PLOTS  FOR DELOCALIZED ITL
##=======================================================##
rm(list = ls(all=T))
suppressMessages(library(RLumCarlo))
## set time vector
times <- seq(0, 400)
run_MC_ISO_DELOC(T=220, E=1.4, s=1e12,R=1e-3, times = times) %>%
  plot_RLumCarlo(norm = TRUE, col="red",legend = F)
run_MC_ISO_DELOC(T=230, E=1.4, s=1e12,R=1e-3, times = times) %>%
  plot_RLumCarlo(norm = TRUE, col="green",add = TRUE)
run_MC_ISO_DELOC(T=240, E=1.4, s=1e12,R=1e-3, times = times) %>%
  plot_RLumCarlo(norm = TRUE, col="blue",add = TRUE, times= times) 
legend("top",bty="n",legend=c("Isothermal TL signal", 
                              expression("at 220, 230, 240"^"o"*"C"))) 