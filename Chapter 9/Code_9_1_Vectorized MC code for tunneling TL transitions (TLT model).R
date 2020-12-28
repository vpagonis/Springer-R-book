# Vectorized MC code for tunneling transitions (TLT model)
# Original Mathematica program by Vasilis Pagonis
# R version written by Johannes Friedrich, 2018
rm(list = ls(all=T))
rho <- 5e-3
En<-1.43
s<-3.5e12
kB<-8.617e-5
deltat <- 1
times <- seq(0, 500, deltat) 
# In this example time =temperature, i.e. a heating rate=1 K/s
r <- seq(0, 2, 0.1)
clusters <- 20
n0<-100
signal<-array(0,dim=c(length(times),
                      ncol = length(r), clusters))
# Run MC simulation
system.time(invisible(for(c in 1:clusters)
{
  for(k in 1:length(r)){
    n <- n0
    for (t in 1:length(times)){
      P <- s*exp(-En/(kB*(t+273)))*exp(-rho^(-1/3) * r[k])
      vec<-rep(runif(n))
      n<-length(vec[vec>P*deltat])
      signal[t,k,c] <- n * P * 3 * r[k]^2 * exp(-r[k]^3) }}})
)
par(mfrow=c(1,2))
# plot an example : the result from the first cluster
matplot(signal[,,1],type = "l",lty="solid", 
        ylab = "Partial TL glow curves for constant(r')",
        ylim=c(0,3.2),xlab=expression("Temperature ["^"o"*"C]"),lwd = 2)
legend("topleft",bty="n",legend=c("(a)"," ","Partial TL",
                                  "glow curves"))
#add the signals from all clusters
sum_signal <- sapply(1:clusters, function(y){
  vapply(1:length(times), function(x){
    sum(signal[x,,y])
  }, FUN.VALUE = 1)  })
# add the signals from all r values
TL <- rowMeans(sum_signal)
# plot and normalize the TL signal
plot(  x = times, y = TL/max(TL),type = "l", lwd = 3,
       ylim=c(0,1.6), xlab=expression("Temperature ["^"o"*"C]"),
       ylab="Average TL signal")
legend("topleft",bty="n",legend=c("(b)"," ",
                                  "Sum of partial TL","glow curves"))
## plot analytical solution Kitis-Pagonis
z<-1.8
T<-times+273
TLanalyt<-exp(-rho*( (log(1+z*s*kB*((T**2.0)/
                                      abs(En))*exp(-En/(kB*T))*(1-2*kB*T/En)))**3.0))*
  (En**2.0-6*(kB**2.0)*(T**2.0))*( (log(1+z*s*kB*((T**2.0)/
                                                    abs(En))*exp(-En/(kB*T))*(1-2*kB*T/En)))**2.0)/
  (En*kB*s*(T**2)*z-2*(kB**2.0)*s*z*(T**3.0)+
     exp(En/(kB*T))*En)
lines(times,TLanalyt/max(TLanalyt),lty="dashed",col="red",
      lwd=3)