net<-seq(1,9,1)
start<-0.2
soc_con<-c(0.1,0.4)
aw_eff<-c(0.1,0.4)
re_eff<-c(-0.075,-0.025)
p_inf<-1
level<-c("comm","pop")
type=c("C","NC","H")
mPps<-0.2
Ps<-c(0.25,0.5,0.75,1)
Ph<-1
Pd<-1
Pr<-1
Prd<-c(0.75,1)
delay<-c(1,4,7)
hc<-20

eff<-c(0.001,0.005,0.01,0.025,0.05,0.1,0.2,0.5)
rep<-seq(1,10,1)

pars<-expand.grid(net,start,soc_con,aw_eff,re_eff,p_inf,level,type,mPps,Ps,Ph,Pd,Pr,Prd,delay,hc,eff,rep)
names(pars)<-c("net","start","SC","AW","RE","p_inf","level","type","mPps","Ps","Ph","Pd","Pr","Prd","delay","hc","eff","rep")

pars<-data.frame(seq(1,nrow(pars),1),pars)
names(pars)[1]<-"id"

pars<-pars[pars$SC==pars$AW,]

#----------------------------------------

net<-seq(1,9,1)
start<-0.2
soc_con<-c(0.1,0.4)
aw_eff<-c(0.1,0.4)
re_eff<-c(-0.075,-0.025)
p_inf<-1
level<-c("comm","pop")
type=c("HC","D")
mPps<-0.2
Ps<-c(0.25,0.5,0.75,1)
Ph<-1
Pd<-1
Pr<-1
Prd<-c(0.75,1)
delay<-c(1,4,7)
hc<-20

eff<-c(0.5,1,2)
rep<-seq(1,10,1)

pars2<-expand.grid(net,start,soc_con,aw_eff,re_eff,p_inf,level,type,mPps,Ps,Ph,Pd,Pr,Prd,delay,hc,eff,rep)
names(pars2)<-c("net","start","SC","AW","RE","p_inf","level","type","mPps","Ps","Ph","Pd","Pr","Prd","delay","hc","eff","rep")

pars2<-data.frame(seq(max(pars[,1])+1,max(pars[,1])+nrow(pars2),1),pars2)
names(pars2)[1]<-"id"

pars2<-pars2[pars2$SC==pars2$AW,]


#-----------------------------------------

parsF<-rbind(pars,pars2)
write.csv(parsF,"C:/Users/matth/Dropbox/NinaCollab_Covid/paper4_reportingscale/fullparams.csv")
