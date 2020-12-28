##===============================================##
##===============================================##
## Plot multiple TL  curves with varying params
##===============================================##
# define your parameters
rm(list = ls(all=T))
library(RLumCarlo)
times=seq(150,350,1)
s=rep(3.5e12,4)
E=rep(1.45,4)
R<-c(0.7e-6,1e-6,0.01,0.1)
clusters=100
N_e =c(400, 500, 700, 400)
n_filled =c(400, 500, 300, 70)
method="par"
output ="signal"
col=c(1,2,3,4) # different colors for the individual curves
plot_uncertainty <- c(TRUE,TRUE,TRUE,TRUE) # plot  uncertainty?
add_TF <- c(FALSE,rep(TRUE, (length(R)-1)))
for (u in 1:length(R)){
  results <-run_MC_TL_DELOC(times=times, s=s[u],E=E[u], 
                            clusters =clusters, N_e = N_e[u],n_filled = n_filled[u], 
                            R=R[u], method = method, output = output)
  plot_RLumCarlo(results,add=add_TF[u],legend = FALSE, 
                 col=col[u], ylim=c(0,20))
}
legend("left",bty="n",c("TL","DELOC","many variables"))
legend("topright",bty="n",ncol=5,cex=0.55,title = "parameters" ,
       legend=c(paste0("E = ", E),paste0("s = ", s),
                paste0("n_filled = ", n_filled),
                paste0("N_e = ", N_e),
                paste0("R = ", R)), text.col=col)