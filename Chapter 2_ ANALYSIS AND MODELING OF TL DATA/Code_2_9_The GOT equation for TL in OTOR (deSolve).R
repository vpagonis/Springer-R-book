# Numerical solution of the GOT equation for TL
rm(list=ls())
library("deSolve")
# Define Parameters
A_n <- 1e-10 # coefficient of retrapping
A_m <- 1e-8 # coefficient of recombination
k_B <- 8.617e-5  # Boltzmann constant
E <- 1 # electron trap depth [eV]
s <- 1e12 # frequenc factor [1/s]
delta.t <- 1
t <- seq(0, 300, delta.t) 
# time = temperature, i.e. heating rate= 1 K/s
n.0 <- 1e10  # initial concentration of filled traps
N.traps <- 1e11   # total concentrations of available traps
# Calculate numerical ODE solution
ODE <- function(t, state, parmameters){
  with(as.list(c(state, parameters)),{
    dn <- -s*exp(-E/(k_B*(273+t))) * n^2 * A_m / ((N.traps - n) * 
                                                    A_n + n * A_m)
    list(c(dn))  })}
parameters <- c(N.traps = N.traps, s = s, E = E, k_B = k_B, 
                A_m = A_m, A_n = A_n)
state <- c(n = n.0)
num_ODE <- ode(y = state, times = t, func = ODE, 
               parms = parameters)
# Plot filled traps n(T) and TL as a function of temperature 
par(mfrow=c(1,2))
plot(x = num_ODE[,1], 
     y = num_ODE[,2],xlab=expression("Temperature ["^"o"*"C]"), 
     ylab =expression("Filled traps (cm"^"-3"*")"),col = "red")
legend("topright",bty="n",expression('(a)',' ','GOT','n(T)'))
plot(x = num_ODE[-1,1],  y = abs(diff(num_ODE[,2])),
     xlab=expression("Temperature ["^"o"*"C]"), 
     ylab = "TL signal [a.u.]")
legend("topright",bty="n",c("(b)"," ","TL"))