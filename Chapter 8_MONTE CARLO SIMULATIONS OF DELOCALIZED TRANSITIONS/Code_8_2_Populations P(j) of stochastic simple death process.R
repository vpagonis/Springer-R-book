# Populations P(j) of stochastic simple death process
rm(list = ls(all=T))
n0<-100
mu<-.03
x<-seq(1,100)
f<-function(u) {choose(n0,u)*exp(-mu*u*t)*
    ((1-exp(-mu*u*t))**(n0-u))}
times<-c(.01,1,20,40,110)
TF=c(FALSE,rep(TRUE,4))
for (i in 1:5){
  t<-times[i]
  area<-sum(unlist(lapply(x,f)))
  curve(choose(n0,round(x))*exp(-mu*x*t)*
          ((1-exp(-mu*x*t))**(n0-x))/area,
        1,100,ylim=c(0,.4),lwd=3,add=TF[i],col=i,lty=i,
        xlab="# of particles j",ylab="probability Pn(j,t)")}
legend("topleft",bty="n",c("t=.01 s","1 s ","20 s ","40 s ",
                           "110 s"), col=1:5,lty=1:5,lwd=3)
legend("top",bty="n",c("Populations P(j,t)"," ",
                       "Stochastic death process"))