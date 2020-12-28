# OTOR MODEL- IRRADIATION of thermally unstable trap (OTOR)
rm(list = ls(all=T))
options(warn=-1)
library(lamW)
library("deSolve")
## Plot the numerical solution of OTOR irradiation stage, 
## at different irradiation temperatures
t<-0:100
N<-1E10
R<-.1
X<-1e10
E<-1
s<-1e12
kb<-8.617e-5
TISO<-20
p<-s*exp(-E/(kb*(273+TISO)))
# Numerically Solve GOT equation for IRRADIATION  using  deSolve 
n.0 <- 0  # initial concentration of filled traps
# Calculate numerical ODE solution
ODE <- function(t, state, parmameters){
  with(as.list(c(state, parameters)),{
    dn <-  ( (N-n) * X*R-(p*(n**2))) / ((N - n) * R + n )
    list(c(dn))  })}
parameters <- c(N=N, X = X, R =R, p=p)
state <- c(n = n.0)
num_ODE <- ode(y = state,times = t,func = ODE,parms=parameters)
plot(x = num_ODE[,1], y = num_ODE[,2]/N,pch=2,ylim=c(0,1.1),
     xlab = "Time[s]", ylab = "Trap filling ratio, n/N",type="b",
     col = "red")
# Change the irradiation temperature TISO
TISO<-100
p<-s*exp(-E/(kb*(273+TISO)))
parameters <- c(N=N, X = X, R =R, p=p)
num_ODE2 <- ode(y = state, times = t, func=ODE,parms=parameters)
lines(x = num_ODE2[,1],    y = num_ODE2[,2]/N, pch=3,
      col = "Black",type="b")
legend("right",bty="n",pch=c(2,3),expression("T=" ~ 20^o ~C ~"",
                                             "T=" ~ 100^o ~C ~""),col=c("red","black"),lwd=2)