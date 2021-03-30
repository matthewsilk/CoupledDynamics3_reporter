fp<-read.csv("fullparams2.csv")
dim(fp)
head(fp)
########################################################

#setwd("D:/New folder/results_report/results")
#plot(fp$id)

setwd("D:/NovResults/biggeroutput/biggeroutput")
test<-readRDS("1000.RDS")

length(test)

dim(test[[1]])
dim(test[[2]])
dim(test[[3]])
dim(test[[4]])
dim(test[[5]])
dim(test[[6]])
dim(test[[7]])
dim(test[[8]])
dim(test[[9]])
dim(test[[10]])

plot(colMeans(test[[3]]),type="l",col="red")
lines(colMeans(test[[2]]),col="blue")

plot(colSums(test[[5]]),type="l",col="red")


inc_sub<-which((fp$type=="C"|fp$type=="NC")&fp$AW==0.1)

max(colMeans(test[[3]]))

peak<-numeric()
whenpeak<-numeric()
infections<-numeric()
deaths<-numeric()
totalcases<-numeric()
casespeak<-numeric()
whencasespeak<-numeric()
newcasespeak<-numeric()
whennewcasespeak<-numeric()
maxreporter<-numeric()
whenreporter<-numeric()

peak_comm<-matrix(nr=10,nc=length(inc_sub))
whenpeak_comm<-matrix(nr=10,nc=length(inc_sub))
infections_comm<-matrix(nr=10,nc=length(inc_sub))
deaths_comm<-matrix(nr=10,nc=length(inc_sub))
totalcases_comm<-matrix(nr=10,nc=length(inc_sub))
casespeak_comm<-matrix(nr=10,nc=length(inc_sub))
whencasespeak_comm<-matrix(nr=10,nc=length(inc_sub))
newcasespeak_comm<-matrix(nr=10,nc=length(inc_sub))
whennewcasespeak_comm<-matrix(nr=10,nc=length(inc_sub))
maxreporter_comm<-matrix(nr=10,nc=length(inc_sub))
whenreporter_comm<-matrix(nr=10,nc=length(inc_sub))
minconc_com<-matrix(nr=10,nc=length(inc_sub))
whenmico_com<-matrix(nr=10,nc=length(inc_sub))
maxconc_com<-matrix(nr=10,nc=length(inc_sub))
whenmaco_com<-matrix(nr=10,nc=length(inc_sub))
  


for(i in 1:length(inc_sub)){
  if (file.exists(paste0(fp$id[inc_sub[i]],".RDS"))){
    
    test<-readRDS(paste0(fp$id[inc_sub[i]],".RDS"))
    peak[i]<-max(colSums(test[[3]]))
    whenpeak[i]<-which.max(colSums(test[[3]]))
    infections[i]<-max(colSums(test[[5]]))+max(colSums(test[[6]]))
    deaths[i]<-max(colSums(test[[6]]))
    totalcases[i]<-sum(colSums(test[[8]]))
    casespeak[i]<-max(colSums(test[[7]]))
    whencasespeak[i]<-which.max(colSums(test[[7]]))
    newcasespeak[i]<-max(colSums(test[[8]]))
    whennewcasespeak[i]<-which.max(colSums(test[[8]]))
    maxreporter[i]<-max(colSums(test[[10]]))
    whenreporter[i]<-which.max(colSums(test[[10]]))
    
    peak_comm[,i]<-apply(test[[3]],1,max)
    whenpeak_comm[,i]<-apply(test[[3]],1,which.max)
    infections_comm[,i]<-apply(test[[5]],1,max)+apply(test[[6]],1,max)
    deaths_comm[,i]<-apply(test[[6]],1,max)
    totalcases_comm[,i]<-rowSums(test[[8]])
    casespeak_comm[,i]<-apply(test[[7]],1,max)
    whencasespeak_comm[,i]<-apply(test[[7]],1,which.max)
    newcasespeak_comm[,i]<-apply(test[[8]],1,max)
    whennewcasespeak_comm[,i]<-apply(test[[8]],1,which.max)
    maxreporter_comm[,i]<-apply(test[[10]],1,max)
    whenreporter_comm[,i]<-apply(test[[10]],1,which.max)
    minconc_com[,i]<-apply(test[[1]],1,min)
    whenmico_com[,i]<-apply(test[[1]],1,which.min)
    maxconc_com[,i]<-apply(test[[1]],1,max)
    whenmaco_com[,i]<-apply(test[[1]],1,which.max)
    
    if(i%%100==0){print(i)}
  }
}

fp2<-fp[inc_sub,]
fp2$type<-factor(fp2$type)

fp2a<-fp2[fp2$type=="C",]
fp2b<-fp2[fp2$type=="NC",]


fp2_hre<-fp2[fp2$RE==-0.075,]

boxplot(peak[fp2$type=="C"]~fp2a$Ps*fp2a$delay*fp2a$eff*fp2a$RE)


boxplot(peak[fp2$RE==-0.075]~fp2_hre$Ps*fp2_hre$delay*fp2_hre$eff*fp2_hre$type)



sd_peaks<-apply(whenpeak_comm,2,sd)

par(mfrow=c(1,2))
boxplot(sd_peaks[fp2$RE==-0.075&fp2$level=="comm"]~fp2_hre$Ps[fp2_hre$level=="comm"]*fp2_hre$delay[fp2_hre$level=="comm"]*fp2_hre$eff[fp2_hre$level=="comm"]*fp2_hre$type[fp2_hre$level=="comm"],ylim=c(0,120))
boxplot(sd_peaks[fp2$RE==-0.075&fp2$level=="pop"]~fp2_hre$Ps[fp2_hre$level=="pop"]*fp2_hre$delay[fp2_hre$level=="pop"]*fp2_hre$eff[fp2_hre$level=="pop"]*fp2_hre$type[fp2_hre$level=="pop"],ylim=c(0,120))

plot(peak~infections)
plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,1200))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"],x=infections[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(infections[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"]),col="firebrick",cex=3,pch=18)

############

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,250))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff>0.1&fp2$type=="C"]),col="firebrick",cex=3,pch=18)


#######################
#######################

#Try this style of plot but for other cases

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,1200))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"],x=infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

############

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

#######################
#######################

#Try this style of plot again

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]~infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,1200))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"],x=infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]),col="navy",cex=3,pch=18)
points(mean(infections[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"]),col="firebrick",cex=3,pch=18)

############

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300))
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="comm"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"&fp2$level=="pop"]),col="firebrick",cex=3,pch=18)

########

par(mfrow=c(1,2))
plot(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"],x=log(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"])+0.1,col=adjustcolor("dodgerblue",0.1),pch=16,ylim=c(0,120),xlim=c(-7.5,0))

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"],x=log(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"])-0.1,col=adjustcolor("salmon",0.1),pch=16,ylim=c(0,120))

points(y=aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"]),mean)[,2],x=log(aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="pop"]),mean)[,1])-0.1,col="firebrick",pch=18,cex=2)

points(y=aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"]),mean)[,2],x=log(aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="C"&fp2$level=="comm"]),mean)[,1])+0.1,col="navy",pch=18,cex=2)

########

plot(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"],x=log(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"])+0.1,col=adjustcolor("dodgerblue",0.1),pch=16,ylim=c(0,120),xlim=c(-7.5,0))

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"],x=log(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"])-0.1,col=adjustcolor("salmon",0.1),pch=16,ylim=c(0,120))

points(y=aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"]),mean)[,2],x=log(aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="pop"]),mean)[,1])-0.1,col="firebrick",pch=18,cex=2)

points(y=aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"]),mean)[,2],x=log(aggregate(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"],by=list(fp2$eff[fp2$RE==-0.075&fp2$delay==1&fp2$type=="NC"&fp2$level=="comm"]),mean)[,1])+0.1,col="navy",pch=18,cex=2)


######################
######################

reg_slopes<-numeric()

for(i in 1:ncol(peak_comm)){
  tmod<-lm(whenpeak_comm[,i]~peak_comm[,i])
  reg_slopes[i]<-coef(tmod)[2]
}

par(mfrow=c(1,2))
plot(reg_slopes[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~whenpeak[fp2$RE==-0.075&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("salmon",0.1),pch=16)
lines(x=c(-100,1000),y=c(0,0))
plot(reg_slopes[fp2$RE==-0.025&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"]~whenpeak[fp2$RE==-0.025&fp2$delay==1&fp2$eff>0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16)
lines(x=c(-100,1000),y=c(0,0))

boxplot(reg_slopes[fp2$RE==-0.075&fp2$eff>0.1&fp2$type=="C"]~fp2$level[fp2$RE==-0.075&fp2$eff>0.1&fp2$type=="C"]*fp2$delay[fp2$RE==-0.075&fp2$eff>0.1&fp2$type=="C"])
lines(x=c(-100,1000),y=c(0,0))


boxplot(reg_slopes[fp2$type=="C"&fp2$eff==0.001&fp2$delay==7&infections>250]~fp2$RE[fp2$type=="C"&fp2$eff==0.001&fp2$delay==7&infections>250],ylim=c(-5,5),notch=T)
lines(x=c(-100,1000),y=c(0,0))
boxplot(reg_slopes[fp2$type=="C"&fp2$eff>0.1&fp2$delay==1&infections>250]~fp2$RE[fp2$type=="C"&fp2$eff>0.1&fp2$delay==1&infections>250],ylim=c(-10,10),notch=T)
lines(x=c(-100,1000),y=c(0,0))

boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250],ylim=c(-3,3),notch=T)
lines(x=c(-100,1000),y=c(0,0))
boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff>0.1&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff>0.1&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff>0.1&infections>250],ylim=c(-10,10),notch=T)
lines(x=c(-100,1000),y=c(0,0))

#############

boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&infections>250],ylim=c(-3,3),notch=T)
lines(x=c(-100,1000),y=c(0,0))
boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&infections>250],ylim=c(-10,10),notch=T)
lines(x=c(-100,1000),y=c(0,0))

###################


boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff==0.001&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff==0.001&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff==0.001&infections>250],ylim=c(-3,3),notch=T)
lines(x=c(-100,1000),y=c(0,0))
boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff>0.1&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff>0.1&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="NC"&fp2$eff>0.1&infections>250],ylim=c(-10,10),notch=T)
lines(x=c(-100,1000),y=c(0,0))

######

plot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff>0.1&infections>250]~sd_peaks[fp2$RE==-0.075&fp2$type=="C"&fp2$eff>0.1&infections>250])



########################################
########################################

par(mfrow=c(1,2))
plot(casespeak,newcasespeak,pch=16,col=adjustcolor("grey",0.05),xlab="Peak in reported active cases",ylab="Peak in reported new cases")

x1=seq(1,500,1)
lines(x=x1,y=x1/2,col="dodgerblue2",lwd=2)
lines(x=x1,y=x1/5,col="dodgerblue3",lwd=2)
lines(x=x1,y=x1/10,col="dodgerblue4",lwd=2)

plot(whencasespeak,whennewcasespeak,pch=16,col=adjustcolor("grey",0.05),xlab="Timing of active case peak",ylab="Timing of new case peak")

lines(x=x1,y=x1,col="firebrick",lwd=2)

sum(whencasespeak>whennewcasespeak)/length(whencasespeak)

sum(whencasespeak==whennewcasespeak)/length(whencasespeak)


plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

#####################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

##############################
##############################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

#####################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

################################
###############################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.1&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

#####################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.5&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)


summary(lm(casespeak~newcasespeak-1))

##############

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.1&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.1&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.1&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.1&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

#####################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]~peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"],col=adjustcolor("dodgerblue",0.1),pch=16,xlim=c(0,300),ylim=c(0,70),xlab="Height of epidemic peak",ylab="Variation in community peaks")
points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"],col=adjustcolor("salmon",0.1),pch=16)

points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"]),col="navy",cex=3,pch=18)
points(mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"]),mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.5&fp2$type=="NC"]),col="firebrick",cex=3,pch=18)

##########################################
##########################################
##########################################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue",0.1),xlim=c(0,300),ylim=c(0,50),xlab="Height of epidemic peak",ylab="Variation in community peaks",pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue2",0.1),pch=21)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=21)


points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),col="navy",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),col="navy",pch=16,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),col="firebrick",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),col="firebrick",pch=16,cex=3)


################################
################################

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue",0.1),xlim=c(0,300),ylim=c(0,50),xlab="Height of epidemic peak",ylab="Variation in community peaks",pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue2",0.1),pch=21)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=21)


points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),col="navy",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),col="navy",pch=16,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),col="firebrick",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.2&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),col="firebrick",pch=16,cex=3)

###############################
###############################


plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue",0.1),xlim=c(0,400),ylim=c(0,20),xlab="Height of epidemic peak",ylab="Variation in community peaks",pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue2",0.1),pch=21)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=21)


points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),col="navy",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),col="navy",pch=16,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.25]),col="firebrick",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="pop"&fp2$Ps==0.75]),col="firebrick",pch=16,cex=3)


############

plot(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]~peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue",0.1),xlim=c(0,400),ylim=c(0,20),xlab="Height of epidemic peak",ylab="Variation in community peaks",pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],col=adjustcolor("navy",0.1),bg=adjustcolor("dodgerblue2",0.1),pch=21)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=22)

points(y=sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],x=peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75],col=adjustcolor("firebrick",0.1),bg=adjustcolor("salmon",0.1),pch=21)


points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),col="navy",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==1&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),col="navy",pch=16,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.25]),col="firebrick",pch=15,cex=3)

points(y=mean(sd_peaks[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),x=mean(peak[fp2$RE==-0.075&fp2$delay==7&fp2$eff==0.05&fp2$type=="C"&fp2$level=="comm"&fp2$Ps==0.75]),col="firebrick",pch=16,cex=3)

fp2_box<-fp2[fp2$type=="C"&fp2$level=="pop"&fp2$RE==-0.075&fp2$eff%in%c(0.01,0.05,0.2,0.5),]

peak_box<-peak[fp2$type=="C"&fp2$level=="pop"&fp2$RE==-0.075&fp2$eff%in%c(0.01,0.05,0.2,0.5)]

sd_peaks_box<-sd_peaks[fp2$type=="C"&fp2$level=="pop"&fp2$RE==-0.075&fp2$eff%in%c(0.01,0.05,0.2,0.5)]

cols1<-c("sienna3","indianred4","mediumorchid4","royalblue4")
box_cols1<-adjustcolor(cols1,0.3)
box_cols2<-adjustcolor(cols1,0.6)
box_cols3<-adjustcolor(cols1,0.9)

box_colsL<-list(box_cols1,box_cols2,box_cols3)

box_cols<-rep(NA,3*4*4)
c<-1
for(a in 1:4){
  for(i in 1:4){
    for(j in 1:3){
      box_cols[c]<-box_colsL[[j]][i]
      c<-c+1
    }
  }
}

par(mfrow=c(1,1))
boxplot(peak_box~fp2_box$delay*fp2_box$Ps*fp2_box$eff,range=0,lty=1,col=box_cols,las=1,ylab="Infection peak",xaxt="n",xlab="",cex.lab=1.5)
polygon(x=c(0,12.5,12.5,0),y=c(-20,-20,500,500),col="gray95",border=NA)
polygon(x=c(24.5,36.5,36.5,24.5),y=c(-20,-20,500,500),col="gray95",border=NA)
boxplot(peak_box~fp2_box$delay*fp2_box$Ps*fp2_box$eff,range=0,lty=1,col=box_cols,add=TRUE,las=1,xaxt="n")

mtext("Effect = 0.01 ",side=1,line=1,at=6.5,cex=1.5)
mtext("Effect = 0.05 ",side=1,line=1,at=18.5,cex=1.5)
mtext("Effect = 0.2 ",side=1,line=1,at=30.5,cex=1.5)
mtext("Effect = 0.5 ",side=1,line=1,at=42.5,cex=1.5)

text(x=50,y=380,"25% symptomatics tested daily",adj=c(1,0.5))
text(x=50,y=360,"50% symptomatics tested daily",adj=c(1,0.5))
text(x=50,y=340,"75% symptomatics tested daily",adj=c(1,0.5))
text(x=50,y=320,"100% symptomatics tested daily",adj=c(1,0.5))

points(x=38,y=380,pch=22,col="black",bg=cols1[1],cex=3,lwd=2)
points(x=38,y=360,pch=22,col="black",bg=cols1[2],cex=3,lwd=2)
points(x=38,y=340,pch=22,col="black",bg=cols1[3],cex=3,lwd=2)
points(x=38,y=320,pch=22,col="black",bg=cols1[4],cex=3,lwd=2)

############################################

par(mfrow=c(1,1))
boxplot(sqrt(sd_peaks_box)~fp2_box$delay*fp2_box$Ps*fp2_box$eff,range=0,lty=1,col=box_cols,las=1,ylab="Variation in timings of peaks",xaxt="n",xlab="",cex.lab=1.5,ylim=c(0,12))
polygon(x=c(0,12.5,12.5,0),y=c(-20,-20,500,500),col="gray95",border=NA)
polygon(x=c(24.5,36.5,36.5,24.5),y=c(-20,-20,500,500),col="gray95",border=NA)
boxplot(sqrt(sd_peaks_box)~fp2_box$delay*fp2_box$Ps*fp2_box$eff,range=0,lty=1,col=box_cols,add=TRUE,las=1,xaxt="n")

mtext("Effect = 0.01 ",side=1,line=1,at=6.5,cex=1.5)
mtext("Effect = 0.05 ",side=1,line=1,at=18.5,cex=1.5)
mtext("Effect = 0.2 ",side=1,line=1,at=30.5,cex=1.5)
mtext("Effect = 0.5 ",side=1,line=1,at=42.5,cex=1.5)

text(x=0,y=12,"25% symptomatics tested daily",adj=c(0,0.5))
text(x=0,y=11.25,"50% symptomatics tested daily",adj=c(0,0.5))
text(x=0,y=10.5,"75% symptomatics tested daily",adj=c(0,0.5))
text(x=0,y=9.75,"100% symptomatics tested daily",adj=c(0,0.5))

points(x=12,y=12,pch=22,col="black",bg=cols1[1],cex=3,lwd=2)
points(x=12,y=11.25,pch=22,col="black",bg=cols1[2],cex=3,lwd=2)
points(x=12,y=10.5,pch=22,col="black",bg=cols1[3],cex=3,lwd=2)
points(x=12,y=9.75,pch=22,col="black",bg=cols1[4],cex=3,lwd=2)


##########################################
#########################################
##########################################


fp2_box2<-fp2[fp2$type=="C"&&fp2$RE==-0.075&fp2$eff%in%c(0.05,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]

peak_box2<-peak[fp2$type=="C"&&fp2$RE==-0.075&fp2$eff%in%c(0.05,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]


cols2=c("deepskyblue4","darkslategray")

box_cols1<-adjustcolor(cols2,0.3)
box_cols2<-adjustcolor(cols2,0.6)
box_cols3<-adjustcolor(cols2,0.9)

box_colsL<-list(box_cols1,box_cols2,box_cols3)

box_cols2<-rep(NA,2*3*2)
c<-1
for(a in 1:2){
  for(i in 1:3){
    for(j in 1:2){
      box_cols2[c]<-box_colsL[[i]][j]
      c<-c+1
    }
  }
}

boxplot(peak_box2~fp2_box2$level*fp2_box2$delay*fp2_box2$eff,lty=1,range=0,at=c(1,1.5,2.5,3,4,4.5,6,6.5,7.5,8,9,9.5),las=1,ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4)
polygon(x=c(5.25,13,13,5.25),y=c(-20,-20,500,500),col="gray95",border=NA)
boxplot(peak_box2~fp2_box2$level*fp2_box2$delay*fp2_box2$eff,lty=1,range=0,at=c(1,1.5,2.5,3,4,4.5,6,6.5,7.5,8,9,9.5),add=TRUE,las=1,xaxt="n",xlab="",col=box_cols2,boxwex=0.4)

mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Delay = 7",side=1,line=1,at=4.25)

mtext("Delay = 1",side=1,line=1,at=6.25)
mtext("Delay = 4",side=1,line=1,at=7.75)
mtext("Delay = 7",side=1,line=1,at=9.25)

mtext("Response = 0.05",side=1,line=3,at=2.75,cex=1.5)
mtext("Response = 0.2",side=1,line=3,at=7.75,cex=1.5)


?boxplot

hist(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==4&fp2_box2$eff==0.2]-peak_box2[fp2_box2$level=="pop"&fp2_box2$delay==4&fp2_box2$eff==0.2])

sum(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==7&fp2_box2$eff==0.2]>peak_box2[fp2_box2$level=="pop"&fp2_box2$delay==7&fp2_box2$eff==0.2])/length(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==7&fp2_box2$eff==0.2])

head(fp2_box2[fp2_box2$level=="comm"&fp2_box2$delay==4&fp2_box2$eff==0.2,])
head(fp2_box2[fp2_box2$level=="pop"&fp2_box2$delay==4&fp2_box2$eff==0.2,])

median(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==1&fp2_box2$eff==0.2]-peak_box2[fp2_box2$level=="pop"&fp2_box2$delay==1&fp2_box2$eff==0.2])
sd(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==4&fp2_box2$eff==0.2]-peak_box2[fp2_box2$level=="pop"&fp2_box2$delay==4&fp2_box2$eff==0.2])


sum(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==7&fp2_box2$eff==0.05]>peak_box2[fp2_box2$level=="pop"&fp2_box2$delay==7&fp2_box2$eff==0.05])/length(peak_box2[fp2_box2$level=="comm"&fp2_box2$delay==7&fp2_box2$eff==0.05])

##################################


fp2_box3<-fp2[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.05,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]

peak_box3<-peak[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.05,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]


boxplot(peak_box3~fp2_box3$level*fp2_box3$delay*fp2_box3$eff,lty=1,range=0,at=c(1,1.5,2.5,3,4,4.5,6,6.5,7.5,8,9,9.5),las=1,ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4)
polygon(x=c(5.25,13,13,5.25),y=c(-20,-20,500,500),col="gray95",border=NA)
boxplot(peak_box3~fp2_box3$level*fp2_box3$delay*fp2_box3$eff,lty=1,range=0,at=c(1,1.5,2.5,3,4,4.5,6,6.5,7.5,8,9,9.5),add=TRUE,las=1,xaxt="n",xlab="",col=box_cols2,boxwex=0.4)

mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Delay = 7",side=1,line=1,at=4.25)

mtext("Delay = 1",side=1,line=1,at=6.25)
mtext("Delay = 4",side=1,line=1,at=7.75)
mtext("Delay = 7",side=1,line=1,at=9.25)

mtext("Response = 0.05",side=1,line=3,at=2.75,cex=1.5)
mtext("Response = 0.2",side=1,line=3,at=7.75,cex=1.5)



###########################################
###########################################


fp2_box4<-fp2[fp2$type=="C"&fp2$RE==-0.075&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]
peak_box4<-peak[fp2$type=="C"&fp2$RE==-0.075&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]
peak_box4<-peak_box4[(fp2_box4$eff%in%0.2+fp2_box4$level%in%"pop")!=2]
fp2_box4<-fp2_box4[(fp2_box4$eff%in%0.2+fp2_box4$level%in%"pop")!=2,]
peak_box4<-peak_box4[(fp2_box4$eff%in%0.1+fp2_box4$level%in%"comm")!=2]
fp2_box4<-fp2_box4[(fp2_box4$eff%in%0.1+fp2_box4$level%in%"comm")!=2,]

fp2_box5<-fp2[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]
peak_box5<-peak[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]
peak_box5<-peak_box5[(fp2_box5$eff%in%0.2+fp2_box5$level%in%"pop")!=2]
fp2_box5<-fp2_box5[(fp2_box5$eff%in%0.2+fp2_box5$level%in%"pop")!=2,]
peak_box5<-peak_box5[(fp2_box5$eff%in%0.1+fp2_box5$level%in%"comm")!=2]
fp2_box5<-fp2_box5[(fp2_box5$eff%in%0.1+fp2_box5$level%in%"comm")!=2,]

par(mfrow=c(1,2))

boxplot(peak_box4~fp2_box4$level*fp2_box4$delay,lty=1,range=0,las=1,at=c(1,1.5,3,3.5,5,5.5),ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4,col=box_cols2,ylim=c(0,275))
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=3.25)
mtext("Delay = 7",side=1,line=1,at=5.25)

boxplot(peak_box5~fp2_box5$level*fp2_box5$delay,lty=1,range=0,las=1,at=c(1,1.5,3,3.5,5,5.5),ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4,col=box_cols2,ylim=c(0,275))
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=3.25)
mtext("Delay = 7",side=1,line=1,at=5.25)

###########################################
###########################################

fp2_box6<-fp2[fp2$type=="C"&fp2$RE==-0.075&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]
peak_box6<-peak[fp2$type=="C"&fp2$RE==-0.075&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]
peak_box6<-peak_box6[(fp2_box6$eff%in%0.1+fp2_box6$level%in%"pop")!=2]
fp2_box6<-fp2_box6[(fp2_box6$eff%in%0.1+fp2_box6$level%in%"pop")!=2,]
peak_box6<-peak_box6[(fp2_box6$eff%in%0.2+fp2_box6$level%in%"comm")!=2]
fp2_box6<-fp2_box6[(fp2_box6$eff%in%0.2+fp2_box6$level%in%"comm")!=2,]

fp2_box7<-fp2[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25,]
peak_box7<-peak[fp2$type=="C"&fp2$RE==-0.025&fp2$eff%in%c(0.1,0.2)&fp2$delay%in%c(1,4,7)&fp2$Ps==0.25]
peak_box7<-peak_box7[(fp2_box7$eff%in%0.1+fp2_box7$level%in%"pop")!=2]
fp2_box7<-fp2_box7[(fp2_box7$eff%in%0.1+fp2_box7$level%in%"pop")!=2,]
peak_box7<-peak_box7[(fp2_box7$eff%in%0.2+fp2_box7$level%in%"comm")!=2]
fp2_box7<-fp2_box7[(fp2_box7$eff%in%0.2+fp2_box7$level%in%"comm")!=2,]

par(mfrow=c(1,2))

boxplot(peak_box6~fp2_box6$level*fp2_box6$delay,lty=1,range=0,las=1,at=c(1,1.5,3,3.5,5,5.5),ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4,col=box_cols2,ylim=c(0,275))
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=3.25)
mtext("Delay = 7",side=1,line=1,at=5.25)

boxplot(peak_box7~fp2_box7$level*fp2_box7$delay,lty=1,range=0,las=1,at=c(1,1.5,3,3.5,5,5.5),ylab="Infection peak",cex.lab=1.5,xaxt="n",xlab="",boxwex=0.4,col=box_cols2,ylim=c(0,275))
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=3.25)
mtext("Delay = 7",side=1,line=1,at=5.25)


#########################################################
#########################################################
#########################################################

library(viridis)

plotdf <- data.frame(peak,infections)
t_x <- densCols(peak,infections,colramp=colorRampPalette(c("black", "white")))
plotdf$dens <- col2rgb(t_x)[1,] + 1L
cols <-  viridis(max(plotdf$dens))
plotdf$col <- cols[plotdf$dens]

par(mfrow=c(1,1))
plot(infections~peak,col=plotdf$col,pch=15,cex.lab=1.5,ylab="Total infections",xlab="Height of peak",las=1)

#########################


plotdf <- data.frame(casespeak,newcasespeak)
t_x <- densCols(casespeak,newcasespeak,colramp=colorRampPalette(c("black", "white")))
plotdf$dens <- col2rgb(t_x)[1,] + 1L
cols <-  viridis(max(plotdf$dens))
plotdf$col <- cols[plotdf$dens]

par(mfrow=c(1,2))
plot(casespeak,newcasespeak,pch=16,col=adjustcolor(plotdf$col,0.1),xlab="Peak in reported active cases",ylab="Peak in reported new cases")

x1=seq(1,500,1)
lines(x=x1,y=x1/2,col="dodgerblue2",lwd=2)
lines(x=x1,y=x1/5,col="dodgerblue3",lwd=2)
lines(x=x1,y=x1/10,col="dodgerblue4",lwd=2)

plotdf <- data.frame(whencasespeak,whennewcasespeak)
t_x <- densCols(whencasespeak,whennewcasespeak,colramp=colorRampPalette(c("black", "white")))
plotdf$dens <- col2rgb(t_x)[1,] + 1L
cols <-  viridis(max(plotdf$dens))
plotdf$col <- cols[plotdf$dens]

plot(whencasespeak,whennewcasespeak,pch=16,col=adjustcolor(plotdf$col,0.1),xlab="Timing of active case peak",ylab="Timing of new case peak")

lines(x=x1,y=x1,col="firebrick",lwd=2)

######################################
#########################################


par(mfrow=c(1,3))

boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250],ylim=c(-2.5,2.5),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Negligible response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)
boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250],ylim=c(-8,4),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Intermediate response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)
boxplot(reg_slopes[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.075&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250],ylim=c(-20,40),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Strong response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)

##################################

boxplot(reg_slopes[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.001&fp2$Ps==0.25&infections>250],ylim=c(-4,4),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Negligible response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)
boxplot(reg_slopes[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.1&fp2$Ps==0.25&infections>250],ylim=c(-15,15),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Intermediate response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)
boxplot(reg_slopes[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250]~fp2$level[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250]*fp2$delay[fp2$RE==-0.025&fp2$type=="C"&fp2$eff==0.5&fp2$Ps==0.25&infections>250],ylim=c(-30,40),notch=T,lty=1,range=0,las=1,col=box_cols2,at=c(1,1.5,2.5,3,4,4.5),boxwex=0.4,ylab="Change in community severity over time",xaxt="n",cex.lab=1.5,xlab="")
lines(x=c(-100,1000),y=c(0,0),lwd=2)
mtext("Delay = 1",side=1,line=1,at=1.25)
mtext("Delay = 4",side=1,line=1,at=2.75)
mtext("Strong response",side=1,line=3,at=2.75,cex=1.5)
mtext("Delay = 7",side=1,line=1,at=4.25)



##################################

x<-seq(1,14,1)
y1<-1-(1-0.25)^x
y2<-1-(1-0.5)^x
y3<-1-(1-0.75)^x

plot(x,y1,type="l",ylim=c(0,1))
lines(x,y2,col="grey")
lines(x,y3,col="light grey")


y1<-1-(1-0.1)^x
y2<-1-(1-0.05)^x
y3<-1-(1-0.01)^x

plot(x,y1,type="l",ylim=c(0,1))
lines(x,y2,col="grey")
lines(x,y3,col="light grey")
























##########################################
##########################################

###OLD CODE###

boxplot(peak~fp$eff*fp$delay*fp$RE*fp$SC)
boxplot(infections~fp$eff*fp$delay*fp$RE*fp$SC)

cols<-c("red","blue","grey","green","orange","pink","purple","yellow")

boxplot(peak[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]~fp$delay[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]*fp$Ps[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]*fp$eff[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025],col=rep(cols,each=12))

boxplot(infections[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]~fp$delay[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]*fp$Ps[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025]*fp$eff[fp$type=="C"&fp$SC==0.1&fp$RE==-0.025],col=rep(cols,each=12))

#mod1<-lm(peak~fp$level+fp$type+as.factor(fp$delay)+as.factor(fp$SC)+as.factor(fp$RE)+as.factor(fp$eff)+as.factor(fp$Ps))

table(fp$start)

fp2<-fp[fp$SC==0.1,]
peak2<-peak[fp$SC==0.1]
infections2<-infections[fp$SC==0.1]

#mod1<-lm(peak2~fp2$level+fp2$type+as.factor(fp2$delay)+as.factor(fp2$RE)+as.factor(fp2$eff)+as.factor(fp2$Ps))

fp2_C<-fp2[fp2$type=="C"|fp2$type=="NC",]
peak2_C<-peak2[fp2$type=="C"|fp2$type=="NC"]
infections2_C<-infections2[fp2$type=="C"|fp2$type=="NC"]

fp2_C$type<-factor(fp2_C$type)

fp2_H<-fp2[fp2$type=="H",]
peak2_H<-peak2[fp2$type=="H"]
infections2_H<-infections2[fp2$type=="H"]

fp2_HC<-fp2[fp2$type=="HC",]
peak2_HC<-peak2[fp2$type=="HC"]
infections2_HC<-peak2[fp2$type=="HC"]

fp2_D<-fp2[fp2$type=="D",]
peak2_D<-peak2[fp2$type=="D"]
infections2_D<-infections2[fp2$type=="D"]
fp2_D$type<-factor(fp2_D$type)


boxplot(peak2_C~fp2_C$level*fp2_C$Ps*fp2_C$delay*fp2_C$eff*fp2_C$type*fp2_C$RE,range=0,lty=1)
boxplot(infections2_C~fp2_C$level*fp2_C$Ps*fp2_C$delay*fp2_C$eff*fp2_C$type*fp2_C$RE,range=0,lty=1)

boxplot(peak2_D~fp2_D$level*fp2_D$Prd*fp2_D$delay*fp2_D$eff*fp2_D$RE,range=0,lty=1)
boxplot(infections2_D~fp2_D$level*fp2_D$Prd*fp2_D$delay*fp2_D$eff*fp2_D$RE,range=0,lty=1)

boxplot(peak2_HC~fp2_HC$level*fp2_HC$delay*fp2_HC$eff*fp2_HC$RE,range=0,lty=1)
boxplot(infections2_HC~fp2_HC$level*fp2_HC$delay*fp2_HC$eff*fp2_HC$RE,range=0,lty=1)

boxplot(peak2_H~fp2_H$levelfp2_H$delay*fp2_H$eff*fp2_H$RE,range=0,lty=1)
boxplot(infections2_H~fp2_H$level*fp2_H$delay*fp2_H$eff*fp2_H$RE,range=0,lty=1)

#######################

cols<-rep(c("firebrick1","dodgerblue"),100)
borders<-rep(rep(c("mediumpurple1","mediumpurple2","mediumpurple3","mediumpurple4"),each=2),50)

boxplot(peak2_C[fp2_C$RE==-0.075&fp2_C$type=="C"]~fp2_C$level[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$Ps[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$delay[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$eff[fp2_C$RE==-0.075&fp2_C$type=="C"],range=0,lty=1,col=cols,border=borders)

polygon(x=c(0.5,24.5,24.5,0.5),y=c(-20,-20,50,50),col="light grey",border=NA)

polygon(x=c(48.5,72.5,72.5,48.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

polygon(x=c(96.5,120.5,120.5,96.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

polygon(x=c(144.5,168.5,168.5,144.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

boxplot(peak2_C[fp2_C$RE==-0.075&fp2_C$type=="C"]~fp2_C$level[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$Ps[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$delay[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$eff[fp2_C$RE==-0.075&fp2_C$type=="C"],range=0,lty=1,col=cols,add=TRUE,border=borders)

#######################

cols<-rep(c("firebrick1","dodgerblue"),100)
borders<-rep(rep(c("mediumpurple1","mediumpurple2","mediumpurple3","mediumpurple4"),each=2),50)

boxplot(infections2_C[fp2_C$RE==-0.075&fp2_C$type=="C"]~fp2_C$level[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$Ps[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$delay[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$eff[fp2_C$RE==-0.075&fp2_C$type=="C"],range=0,lty=1,col=cols,border=borders)

polygon(x=c(0.5,24.5,24.5,0.5),y=c(-20,-20,50,50),col="light grey",border=NA)

polygon(x=c(48.5,72.5,72.5,48.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

polygon(x=c(96.5,120.5,120.5,96.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

polygon(x=c(144.5,168.5,168.5,144.5),y=c(-20,-20,50,50),col=" light grey",border=NA)

boxplot(infections2_C[fp2_C$RE==-0.075&fp2_C$type=="C"]~fp2_C$level[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$Ps[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$delay[fp2_C$RE==-0.075&fp2_C$type=="C"]*fp2_C$eff[fp2_C$RE==-0.075&fp2_C$type=="C"],range=0,lty=1,col=cols,add=TRUE,border=borders)


#################################
################

head(fp)

dim(test[[1]])
dim(test[[2]])
dim(test[[3]])
dim(test[[4]])
dim(test[[5]])
dim(test[[6]])
dim(test[[7]])
dim(test[[8]])
dim(test[[9]])
dim(test[[10]])


plot(colSums(test[[3]]),type="l",lwd=2,ylim=c(0,100))
lines(colSums(test[[7]]),col="blue",lwd=2)
lines(colSums(test[[8]]),col="red",lwd=2)

plot(colMeans(test[[1]]),type="l")
