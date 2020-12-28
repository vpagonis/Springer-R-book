# OTOR MODEL- IRRADIATION Lambert solution code
rm(list = ls(all=T))
options(warn=-1)
library(lamW)
library("deSolve")
## Plot  analytical solution of OTOR irradiation stage, using W
t<-0:100
N<-1E10
R<-.1
X<-1e10
k<-function(u){1+(1/(1-R))*lambertW0((R-1)*exp(-(R*X*u/N)+R-1))}
y1<-lapply(t,k)
x<-unlist(t)
y<-unlist(y1)
plot(x,y,xlab="Irradiation time t, s",ylab="Filled Traps n/N",
     pch=1)
# Numerically Solve GOT equation for IRRADIATION  using  deSolve 
n.0 <- 0  # initial concentration of filled traps
# Calculate numerical ODE solution
ODE <- function(t, state, parmameters){
  with(as.list(c(state, parameters)),{
    dn <-   (N-n) * X*R / ((N - n) * R + n )
    list(c(dn)) })}
parameters <- c(N=N, X = X, R =R)
state <- c(n = n.0)
num_ODE <- ode(y = state, times =t,func=ODE,parms=parameters)
lines(x = num_ODE[,1],y = num_ODE[,2]/N,xlab ="Time[s]",
      type="l",col = "red")
legend("right",bty="n",pch=c(NA,1),legend =c("W(t)", "ODE"), 
       col = c("red","black"), lwd = 1)