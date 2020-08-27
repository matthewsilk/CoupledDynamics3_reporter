
#This function will provide a single number to feed into the concern model
reporter<-function(statuses,t2=t,pop_size,comms,type=c("C","NC","H","HC","D"),level=c("comm","pop"),mPps=0.2,Ps=0.5,Ph=1,Pr=1,Pd=1,Precd=1,delay=1,hosp_cap=20,eff=0.05,tp=tp,tn=tn,td=td,trd=trd){
  
  #work out statuses at the relevant time point (subject to the delay) and time point before
  inf<-statuses[[t2-delay]]
  infp<-statuses[[t2-(delay+1)]]
  
  #work out new developments in statuses
  diffs<-(inf-infp)==1
  
  #Use to calculate actual information on number of active cases and number of new cases
  NEWCASES<-aggregate(diffs,by=list(comms),sum)
  CASES<-aggregate(inf,by=list(comms),sum)
  
  #Works out prevalence at either community or population level
  if(level=="comm"){
    prevs<-aggregate(tp,by=list(comms),mean)[,2]
  }
  if(level=="pop"){
    prevs<-rep(mean(tp),10)
  }
  
  #Uses prevalence to work out probability of pre-symptomatic test. Scales with prevalence to a max of mPps
  Pps<-rep(prevs*mPps,length(comms)/length(unique(comms)))
  
  #"Test" everyone who's infected today
  ttp<-rbinom(pop_size,statuses[[t2-delay]]$I1,Pps)+rbinom(pop_size,statuses[[t2-delay]]$I2,Ps)+rbinom(pop_size,statuses[[t2-delay]]$I3,Ph)
  
  #Record new positives
  ttp2<-as.numeric(ttp==1&tp==0)
  
  #Update current active cases
  tp<-sign(tp+ttp)
  
  #Update current negatives to include newly recovereds and the use this to update active cases
  tn<-sign(tn+rbinom(pop_size,statuses[[t2-delay]]$R,Pr))
  tp[tn==1]<-0
  
  #Update new deaths and work out which are recorded as COVID deaths (trd)
  ttd<-rbinom(pop_size,statuses[[t2-delay]]$D,Pd)
  ttd2<-as.numeric(ttd==1&td==0)
  ttrd<-rbinom(pop_size,ttd2,Precd)
  trd<-sign(trd+ttrd)
  
  #Use this to update number of deaths and active cases
  td<-sign(td+ttd)
  tp[td==1]<-0
  
  #Work out number of hospitalisations in "current" and previous time-step
  hosps<-aggregate(statuses[[t2-delay]]$I3,by=list(comms),sum)
  hospsp<-aggregate(statuses[[t2-(delay+1)]]$I3,by=list(comms),sum)
  
  #Work out how much each individuals concern will increase by (either by community or population)
  #Returns td, tn, tp and trd (required for next iteration of this function) and then incs giving how much each persons concern will change
  if(level=="comm"){
    if(type=="C"){
      ns<-aggregate(tp,by=list(comms),sum)[,2]
      incs<-rep(eff*ns,pop_size/length(unique(comms)))
    }
    if(type=="NC"){
      ns<-aggregate(ttp2,by=list(comms),sum)[,2]
      incs<-rep(eff*ns,pop_size/length(unique(comms)))
    }
    if(type=="H"){
      incs<-rep(eff*hosps[,2],pop_size/length(unique(comms)))
    }
    if(type=="HC"){
      hc2<-hosp_cap/length(unique(comms))
      incs<-rep(eff*as.numeric(hosps[,2]>=hc2&hospsp[,2]<hc2),pop_size/length(unique(comms)))
    }
    if(type=="D"){
      ns<-aggregate(ttrd,by=list(comms),sum)[,2]
      incs<-rep(eff*ns,pop_size/length(unique(comms)))
    }
    
    out<-list(tp,tn,td,trd,incs)
    names(out)<-c("tp","tn","td","trd","incs")
    return(out)
    
  }
  
  if(level=="pop"){
    if(type=="C"){
      eff2<-eff/length(unique(comms))
      incs<-rep(eff2*sum(tp),pop_size)
    }
    if(type=="NC"){
      eff2<-eff/length(unique(comms))
      incs<-rep(eff2*sum(ttp2),pop_size)
    }
    if(type=="H"){
      eff2<-eff/length(unique(comms))
      incs<-rep(eff2*sum(hosps[,2]),pop_size)
    }
    if(type=="HC"){
      incs<-rep(eff*as.numeric(sum(hosps[,2])>=hosp_cap&sum(hospsp[,2])<hosp_cap),pop_size/length(unique(comms)))
    }
    if(type=="D"){
      eff2<-eff/length(unique(comms))
      incs<-rep(eff2*sum(ttrd),pop_size)
    }
    
    out<-list(tp,tn,td,trd,incs)
    names(out)<-c("tp","tn","td","trd","incs")
    return(out)
    
  }
  
}
