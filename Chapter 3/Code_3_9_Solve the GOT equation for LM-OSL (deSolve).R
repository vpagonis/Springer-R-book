# Numerically solve the GOT equation for LM-OSL
rm(list=ls())
library("deSolve")
# Define Parameters
A_n <- 1e-10 # coefficient of retrapping
A_m <- 1e-8 # coefficient of recombination
Aopt <- 0.1 # Stimulation coefficient [1/s]
delta.t <- .2
t <- seq(0, 50, delta.t)
n.0 <- 1e10  # initial concentration of filled traps
N.traps <- 1e11   # total concentrations of available traps
# Calculate numerical ODE solution
ODE <- function(t, state, parmameters){
  with(as.list(c(state, parameters)),{
    dn <-  -Aopt *t*  n^2 * A_m / ((N.traps - n)* A_n+n* A_m)
    list(c(dn))
  })}
parameters <- c(N.traps =N.traps,Aopt = Aopt,A_m=A_m,A_n = A_n)
state <- c(n = n.0)
num_ODE <- ode(y = state, times = t, func = ODE, 
               parms = parameters)
# Plot remaining electrons and LM-OSL as a function of time
par(mfrow=c(1,2))
plot(x = num_ODE[,1], 
     y = num_ODE[,2],xlab = "Time[s]", ylab = "Filled traps",
     col = "red")
legend("topright",bty="n",c("(a)"," ","n(t)"))
plot(x = num_ODE[-1,1], y = abs(diff(num_ODE[,2])),
     xlab = "Time[s]", ylab="LM-OSL signal[a.u.]")
legend("topright",bty="n",c("(b)"," ","LM-OSL"))