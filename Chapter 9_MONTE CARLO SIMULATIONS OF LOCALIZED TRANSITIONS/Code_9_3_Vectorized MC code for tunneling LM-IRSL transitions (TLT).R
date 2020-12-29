# Vectorized MC code for tunneling transitions (TLT model)
# Original Mathematica program by Vasilis Pagonis
# R version written by Johannes Friedrich, 2018
rm(list = ls(all=T))
rho <- 5e-3
A<-2                # IR exciation rate in s^-1
deltat <- 1
times <- seq(0, 400, deltat) 
r <- seq(0, 2.2, 0.1)
clusters <- 10
n0<-500
signal<-array(0,dim=c(length(times),
                      ncol = length(r), clusters))
# Run MC simulation
system.time(invisible(for(c in 1:clusters)
{  for(k in 1:length(r)){
  n <- n0
  for (t in 1:length(times)){
    P <- A*t/max(times)*exp(-rho^(-1/3) * r[k])
    vec<-rep(runif(n))
    n<-length(vec[vec>P*deltat])
    signal[t,k,c] <- n * P * 3 * r[k]^2 * exp(-r[k]^3) }}}))
par(mfrow=c(1,2))
# plot an example : the result from the first cluster
matplot(signal[,,1],type = "l",lty="solid", 
        ylab = "Partial LM-IRSL curves",
        ylim=c(0,7),xlab=expression("Time [s]"),lwd = 2)
legend("topleft",bty="n",legend=c("(a)"," ","Partial LM-IRSL",
                                  " curves"))
#add the signals from all clusters
sum_signal <- sapply(1:clusters, function(y){
  vapply(1:length(times), function(x){
    sum(signal[x,,y])
  }, FUN.VALUE = 1)  })
# add the signals from all r values
TL <- rowMeans(sum_signal)
# plot and normalize the TL signal
plot(  x = times, y = TL/max(TL),type = "l", lwd = 3,
       ylim=c(0,1.6), xlab=expression("Time [s]"),
       ylab="Average LM-IRSL signal")
legend("topleft",bty="n",legend=c("(b)"," ",
                                  "Sum of partial","LM-IRSL curves"))
## plot analytical solution Kitis-Pagonis
z<-1.8
LManalyt<-exp(-rho*( (log(1+z*A*times^2/(2*max(times))))**3.0))*
  times*((log(1+z*A*times^2/(2*max(times))))**2.0)/(1+z*A*times^2/
                                                      (2*max(times)))
lines(times,LManalyt/max(LManalyt),lty="dashed",col="red",lwd=3)