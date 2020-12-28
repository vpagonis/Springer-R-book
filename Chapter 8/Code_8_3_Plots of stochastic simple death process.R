rm(list = ls(all=T))
n0<-100
mu<-.03
par(mfrow=c(1,3))
curve(n0*exp(-mu*x),0,100,lwd=3,xlab="Time t, s",ylab="<n(t)>",
      ylim=c(0,140))
legend("top",bty="n",c("(a)"," ","Stochastic", "<n>"))
curve(sqrt(n0)*sqrt(exp(-mu*x)-exp(-2*mu*x)),0,100,lwd=3,
      xlab="Time t, s",ylab=expression(sigma),ylim=c(0,8))
legend("top",bty="n",c(expression("(b)"," ","Stochastic",sigma)))
curve(100*sqrt(exp(mu*x)-1)/n0,1,100,xlab="Time t, s",lwd=3,
      ylab="CV[%]",ylim=c(0,7))
legend("top",bty="n",c("(c)"," ","Stochastic","CV[%]"))