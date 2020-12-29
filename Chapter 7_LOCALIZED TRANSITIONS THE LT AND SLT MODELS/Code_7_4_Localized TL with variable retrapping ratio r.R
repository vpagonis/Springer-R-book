##====================================================##
## Localized TL with variable retrapping ratio r (LT model)
##====================================================##
rm(list = ls(all=T))
library(RLumCarlo)
f<-function(rvar,vars,addTF){
  run_MC_TL_LOC(
    s = 1e12,
    E = 1,
    times = 0:300,
    r = rvar
  ) %>%
    #Plot results of the MC simulation
    plot_RLumCarlo(legend = F,plot_uncertainty=NULL,type="o",
                   pch=vars,col=vars,add=addTF, xlim=c(50,275))}
f(1,1,FALSE)
f(1e3,2,TRUE)
f(1e4,3,TRUE)
legend("topright",bty="n",pch=c(NA,NA,NA,1:3),
       col=c(NA,NA,NA,1:3),legend = c("Retrapping"  ,"Ratio r ",
                                      " ","1",  expression("10"^"3"*""),expression("10"^"4"*"")) )