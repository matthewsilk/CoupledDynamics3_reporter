## exemple of use
source("tools.R")

## get parameters and clean
fp<-read.csv("fullparams2.csv")

folderNewExpe="strongerRespLowerTest"
exist=sapply(fp$id,simExist,resfold=folderNewExpe)
print(paste(sum(!exist),"simulations haven't been run"))
fp=fp[sapply(fp$id,simExist,resfold=folderNewExpe),] #remove the 15 inexisting simulations (why do we have some?), I guess this is my fault and it shouldn't be hard to 
fp$id=sapply(fp$id,updateid,folderNewExpe) #update id, this way the good path is stored in `fp` and we don't need to pass the folder of the experiments to the other functions
####

folderOldExp="firstExp/biggeroutput/"
fp2<-read.csv("fullparams.csv")
exist=sapply(fp2$id,simExist,resfold=folderOldExp)
print(paste(sum(!exist),"simulations haven't been run"))
fp2=fp2[sapply(fp2$id,simExist,resfold=folderOldExp),] #remove the 15 inexisting simulations (why do we have some?), I guess this is my fault and it shouldn't be hard to 
fp2$id=sapply(fp2$id,updateid,folderOldExp) #update id, this way the good path is stored in `fp2` and we don't need to pass the folder of the experiments to the other functions

fp=rbind(fp,fp2) #merging both

## the simplest test to check one simulation:
test=getData(fp$id[1]) #get one simu from a specific folder
explore=c("concern","deaths","cases","infs")
par(mfrow=c(1,length(explore)))
for(v in explore){
    res=test[[v]]
    plot(1,1,ylim=range(res),xlim=c(1,ncol(res)),xlab="time",ylab=v,type="n",main=paste(v,"for each community"))
    lapply(1:nrow(res),function(c)lines(res[c,],col=c))
}

par(mfrow=c(1,1))


#Explore multiples simulations given one variable (here the type of information reported)

#get a subset of the parameter
fpb=getSubset(fp,quote(eff>.1&RE==-0.075&delay==1&level=="comm"))
#get summary of trajectory for all simulations for this subset, stored given the variable 'type'
traject=getDataTrajByVar(fpb,"cases",var="type") 
#plot each summary
plotTrajByVar(traject,cls=typecol[unique(fpb$type)])

#get specifics metrics from these simulation:
data=getMetrics(fpb,"cases",c("peak","whenpeak"))

#definine some colorscale for this variable 
typecol=2:5
names(typecol)=levels(fpb$type)

#plot the two variable and color given type
plot(data,col=adjustcolor(typecol[fpb$type],.3),pch=16)
#plot both
par(mfrow=c(1,2))
plot(data,col=adjustcolor(typecol[fpb$type],.3),pch=16)
plotTrajByVar(traject,cls=typecol[unique(fpb$type)],legend=T)

#show only for DEATH vs CASE reporting
#one when to do is to paste the column computed previously to the datafram fpb and subset it using getSubset, another way would be to subset the initial table and recompute the metrics using get metrics on the new subset
fpbS=getSubset(cbind(fpb,data),quote(type %in% c("C","D")))
traject=getDataTrajByVar(fpbS,"infs",var="type") 
par(mfrow=c(1,2))
plot(fpbS$whenpeak,fpbS$peak,col=adjustcolor(typecol[fpbS$type],.1),pch=16)
plotTrajByVar(traject,cls=typecol[unique(fpbS$type)],legend=T)


#To quickly explore impact of type of report for different delays and reporting level, results are stored in a folder "img"
imgfold="img"
for( l in unique(fp$level)){
    for( d in unique(fp$delay)){
        fpb=getSubset(fp,bquote(eff>.1&RE==-0.075&delay==.(d)&level==.(l)))
        fpb=cbind(fpb,peak=getMetrics(fpb,"cases",c("peak","whenpeak"))) #computing this could be done once and for all at the begining as you did, though it will take lot of time, thus it's just a quesiotn of tradeoff I guess
        traject=getDataTrajByVar(fpb,"cases",var="type") 
        png(file.path(imgfold,paste("reporting",l,"delay",d,"TYPEonCASE.png",sep="_")),width=800,height=600,pointsize=20)
        par(mfrow=c(1,2))
        plot(fpb$whenpeak,fpb$peak,col=adjustcolor(typecol[fpb$type],.1),pch=16,main=paste0("Delay=",d,", level=",l))
        legend("topright",col=typecol,legend=paste("type:",names(typecol)),pch=16)
        plotTrajByVar(traject,cls=typecol[unique(fpb$type)])
        legend("topright",col=typecol,legend=paste("type:",names(typecol)),lwd=1)
        dev.off()
    }
}

#to simplify this I created a function to plot two metrics and the summary of the trajecties given one variable:
fpb=getSubset(fp,bquote(eff==1&RE==-0.075&level=="pop"&type=="C"))
plotQuick2plots(fpb,measures="infs",metrics=c("peak","sd_peak"),var="delay") 

fpb=getSubset(fp,bquote(eff==1&RE==-0.075&level=="pop"&delay==7))
plotQuick2plots(fpb,measures="infs",metrics=c("peak","sd_peak"),var="type")

# a title can be passed:
plotQuick2plots(fpb,measures="infs",metrics=c("peak","sd_peak"),var="type",main="eff=1,RE=-0.075,level=pop,delay=7")

#checkinf other measures with other meatrics
plotQuick2plots(fpb,measures="deaths",metrics=c("whenpeak","sd_peak"),var="type",main="eff=1,RE=-0.075,level=pop,delay=7")



#now lets compare impact of delay for lot of different combination of other variables, results stile stored in folder "img"
for( m in c("infs","cases","deaths")){
    for( sc in unique(fp$SC)){
        aw=sc
    #for( aw in unique(fp$AW)){
    for( l in unique(fp$level)){
        for( r in unique(fp$RE)){
            for( e in unique(fp$eff)){
            for( t in unique(fp$type)){
            for( p in unique(fp$Ps)){
                fpb=getSubset(fp,bquote(AW==.(aw)&SC==.(sc)&Ps==.(p)&eff==.(e)&RE==.(r)&type==.(t)&level==.(l)))
                title=paste0("Type=",t,", AW=",aw,", SC=",sc,", Ps=",p,", eff=",e,", level=",l,",RE=",r)
                tryCatch({
                png(file.path(imgfold,paste("AW",aw,"SC",sc,"reporting",l,"type",t,"Ps",p,"eff",e,"RE",r,"effSUP01effectOfDELAYon",m,"count.png",sep="_")),width=800,height=600,pointsize=20)
                plotQuick2plots(fpb,measures=m,metrics=c("peak","sd_peak"),main=title,var="delay",alpha=.5)
                dev.off()
                },error=function(e){print(paste(e,"PROBLEM WITH:",title));dev.off()})
            }
            }
            }
        }
    }
    #}
    }
}

#same than before but instead of comparing different type of delay we compare different type of report
for( m in c("infs","cases","deaths")){
    for( sc in unique(fp$SC)){
        aw=sc
    #for( aw in unique(fp$AW)){
    for( l in unique(fp$level)){
        for( r in unique(fp$RE)){
            for( d in unique(fp$delay)){
            for( e in unique(fp$eff)){
            for( p in unique(fp$Ps)){
                fpb=getSubset(fp,bquote(AW==.(aw)&SC==.(sc)&Ps==.(p)&eff==.(e)&RE==.(r)&delay==.(d)&level==.(l)))
                title=paste0("Delay=",d,", AW=",aw,", SC=",sc,", Ps=",p,", eff=",e,", level=",l,",RE=",r)
                tryCatch({
                png(file.path(imgfold,paste("AW",aw,"SC",sc,"reporting",l,"delay",d,"Ps",p,"eff",e,"RE",r,"effSUP01effectOfDELAYon",m,"count.png",sep="_")),width=800,height=600,pointsize=20)
                plotQuick2plots(fpb,measures=m,metrics=c("peak","sd_peak"),main=title,var="type",alpha=.5)
                dev.off()
                },error=function(e){
                    print(paste(e,"PROBLEM WITH:",title))
                    dev.off()
                } )
            }
            }
            }
        }
    #}
    }
    }
}


