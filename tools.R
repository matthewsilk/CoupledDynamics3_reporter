
#@param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#@param measure which measure do we want (in "concern"   "exps"      "infs"      "hosps"     "recovers"  "deaths"    "cases" "newcases" "repdeaths" "reports" )
#@param metrics  
getDataTrajByVar <- function(param,measure="cases",metric="colSums",var,var2=NULL){
    allsim=sapply(param$id,function(i)getAllInf(i,measure,metric)) #get all 

    ##concacaten
    allvar=lapply(unique(param[[var]]),function(v){
                  subset=allsim[param[[var]]==v]
                  sapply(1:maxx,function(t)tryCatch(quantile(sapply(subset,"[",t),na.rm=T),error=function(e)rep(NA,5)))
    })

    maxx=max(lengths(allsim))
    rangey=range(allsim)
    cls=alpha(1:length(unique(param[[var]])),.5)
    names(cls)=as.character(unique(param[[var]]))
    plot(1,1,type="n",ylim=rangey,xlim=c(1,maxx))
    lapply(seq_along(allvar),function(l)lines(allvar[[l]][3,],col=cls[l]))
    lapply(seq_along(allvar),function(l)lines(allvar[[l]][2,],col=cls[l],lty=2))
    lapply(seq_along(allvar),function(l)lines(allvar[[l]][4,],col=cls[l],lty=2))
    return(allvar)
}
#@param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#@param measure which measure do we want (in "concern"   "exps"      "infs"      "hosps"     "recovers"  "deaths"    "cases" "newcases" "repdeaths" "reports" )
#@param metrics  
getUniqueMetrics <- function(param,measure="cases",metric="max"){
    sapply(param$id,function(i)get(metric)(getAllInf(i,measure,metric))) 
}

#' Return simulation output from a given expe id
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
getData <- function(id,resfold=""){
    if(resfold=="")
        return(readRDS(paste0(id,".RDS")))
    else
        return(readRDS(paste0(file.path(resfold,id),".RDS")))
}

#' Return list of true false if files exist
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
simExist <- function(id,resfold=""){
    file.exists(paste0(file.path(resfold,id),".RDS"))
}

#' change the ids to store the whole path of the simu
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
updateid <- function(id,resfold="")file.path(resfold,id)

getAllInf <- function(id,measure="recovers",metrics="identity"){
    get(metrics)(getData(id)[[measure]])
}


fp<-read.csv("fullparams2.csv")
exist=sapply(fp$id,simExist,resfold="strongerRespLowerTest")
print(paste(sum(!exist),"simulations haven't been run"))
fp=fp[sapply(fp$id,simExist,resfold="strongerRespLowerTest"),] #remove the 15 inexisting simulations (why do we have some?), I guess this is my fault and it shouldn't be hard to 


test=getData(fp$id[1],"strongerRespLowerTest") #get one simu from a specific folder

fp$id=sapply(fp$id,updateid,"strongerRespLowerTest") #update id, we don't need to pass it to other function anymor

par(mfrow=c(2,2))
for(tpe in unique(fp$type)){
    net1=fp[fp$type==tpe,]
    net1=cbind(net1,peakcase=getUniqueMetrics(net1,"cases","max"))
    net1=cbind(net1,whenpeak=getUniqueMetrics(net1,"cases","which.max"))
    boxplot(net1$peakcase ~ net1$delay * net1$Pr)
}

m=getDATA(net1,var="delay")





