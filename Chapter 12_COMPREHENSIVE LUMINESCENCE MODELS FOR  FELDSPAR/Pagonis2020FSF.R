###################### Anomalous fading Function
#### AFfortimeT: calculates new distribution at the end of AF period
AFfortimeT<-function(timAF){distr<<-distr*exp(-(seff*timAF))}

###################### Irradiation Function
#### irradfortimeT: calculates new distribution at end of irradiation
irradfortimeT<-function(tirr){distr<<-unFaded*(Ddot*tau/
 (Do+Ddot*tau))*  (1-exp(-(Do+Ddot*tau)/(Do*tau)*tirr))}

###################### Optical stimulation (IR) Functions ####
# stimCW:calculates new distribution at end of CW stimulation
CWfortimeT<-function(timCW){distr<<-distr*exp(-(Aeff*timCW))}

# CWsignal: calculates CW-IRSL signal 
CWsignal<-function(timCW){distr*Aeff*exp(-(Aeff*timCW))}

#stimIRSL: sets new distribution after IR, returns CW-IRSL signal
stimIRSL<-function(){
  signal<-dr*colSums(sapply(timesCW,CWsignal)) #  IRSL signal 
  CWfortimeT(max(timesCW))  # sets parameter distr after IR
  signal}

# Thermal stimulation functions ###############
# heatTo: calculates new distribution at end of heating to Tph
heatTo<-function(Tph){distr<<-distr*exp(-(seff*kb*Tph^2)/
                                          (beta*E)*exp(-E/(kb*Tph))*(1-2*kb*Tph/E))}
# heatAt: calculates new distribution at end of preheat for time tph at
# temperature Tph
heatAt<-function(Tph,tph){
  distr<<-distr*exp(-(seff*exp(-E/(kb*Tph))*tph))}

# TLsignal: calculates TL signal 
TLsignal<-function(temp){
  distr*seff*exp(-E/(kb*temp))*exp(-(seff*kb*temp^2)/
  (beta*E)*exp(-E/(kb*temp))*(1-2*kb*temp/E))}

# stimTL: sets new distribution after TL, returns TL signal
stimTL<-function(){
  signal<-dr*colSums(sapply(temps,TLsignal))  # finds TL signal
  heatTo(max(temps))            # sets parameter distr after TL
  signal}                       # function returns TL signal

# irradfortimeT: irradiation at temperature Tirr=fixed for time tirr
irradandThermalfortimeT<-function(tirr){distr<<-unFaded*
  (Ddot*rprimes/(Do*Peff+Ddot))*(1-exp(-(Ddot/Do+Peff)*tirr))}

# irradatsometemp:  irradiation at temperature Tirr for time tirr=fixed
irradatsometemp<-function(Tirr){
  Peff<-(1/(1/s+1/seff))*exp(-E/(kb*Tirr))
  distr<<-distrUnfaded*
    (Ddot*rprimes/(Do*Peff+Ddot))*(1-exp(-(Ddot/Do+Peff)*tirr))}

###################### End of Functions  ###################