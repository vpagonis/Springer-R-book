# Numerically solve the ODE for TL: first order kinetics
rm(list=ls())
library(package ="deSolve")
# Define Parameters 
k_B <- 8.617e-5  # Boltzmann constant 
E <- 1 # electron trap depth [eV] 
s <- 1e12 # frequency factor [1/s] 
delta.t <- 1 
t <- seq(0, 200, delta.t) 
n.0 <- 1e10  
N.traps <- 1e11    
ODE <- function(t, state, parmameters){ 
  with(as.list(c(state, parameters)),{     
    dn <-  -s*exp(-E/(k_B*(273+t))) * n
    list(c(dn)) })} 
parameters <- c(N.traps = N.traps, s = s, E = E, k_B = k_B) 
state <- c(n = n.0) 
num_ODE <- ode(y = state, times = t, func = ODE, 
               parms = parameters) 
# Plot remaining electrons and TL as a function of temperature
par(mfrow=c(1,2)) 
plot(x =num_ODE[,1],      
     y = num_ODE[,2], xlab=expression("Temperature ["^"o"*"C]"), 
     ylab = expression("Filled traps n(T), cm"^-3*" "),   
     col = "red") 
legend("topright",bty="n",c("(a)","n(T)"))
plot(num_ODE[-1,1], y = abs(diff(num_ODE[,2])), 
     xlab=expression("Temperature ["^"o"*"C]"), 
     ylab = "TL signal [a.u.]",xlim=c(0,170))
legend("topleft",bty="n",c("(b)","TL"))
