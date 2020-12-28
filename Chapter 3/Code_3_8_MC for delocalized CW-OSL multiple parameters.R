## Simulate CW-OSL DELOC with several parameter changes
##======================================================##
rm(list = ls(all=T))
library(RLumCarlo)
# define your parameters
A <- c(1,.3,.5,.1)
times <- seq(0,60,1)
R<-c(1e-7,1e-6,0.01,0.1) # sequence of different R values
clusters <- 200 # number of Monte Carlo simulations
N_e <- c(200, 500, 700, 400) # number of free electrons
n_filled <- c(200, 500, 100, 70) # number of filled traps
method <-"par"
output <- "signal"
col <- c(1,2,3,4) # different colors for the individual curves
plot_uncertainty <- c(T,F,T,F) # plot the uncertainty?
add_TF <- c(F,rep(T, (length(R)-1)))
for (u in 1:length(R)){
  results <-run_MC_CW_OSL_DELOC(A=A[u],times,clusters =clusters,
                                N_e = N_e[u],   n_filled = n_filled[u], 
                                R=R[u], method = method, output = output)
  plot_RLumCarlo(results,add=add_TF[u],legend = F, col=col[u])
}
legend("topright",ncol=4,bty="n",cex=.65,title = "CW-OSL
function DELOC" ,
       legend=c(paste0("A = ", A),
                paste0("n_filled = ", n_filled),
                paste0("N_e = ", N_e),
                paste0("R = ", R)), text.col=col)