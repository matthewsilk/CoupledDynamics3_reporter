
#' @param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#' @param measure which measure do we want (in "concern"   "exps"      "infs"      "hosps"     "recovers"  "deaths"    "cases" "newcases" "repdeaths" "reports" )
#' @param metrics by default: colSums which will sum the value of the measures for all cuminities
#' @param maxt longest simulation time expected, if NULL this will correspond to the time of longest simulations within the one selected.
#' @param probs the quantile we wants from the simulations 
#' @return a list of of size \code{length(var)} storing matrices of \code{dim=c(maxt,length(probs))}
# note: not sure this was the wised way to do, I guess should be splitted by `var` at previous level
getDataTrajByVar <- function(param,measure="infs",maxt=NULL,metric="colSums",var,probs=c(.25,.50,.75)){
    allsim=sapply(param$id,function(i)getAllInf(i,measure,metric)) #get all 
    if(is.null(maxt))maxt=max(lengths(allsim)) #get the longest simulation time
    ##concacatenate all simulation using "var"
    allvar=lapply(unique(param[[var]]),function(v){
                  subset=allsim[param[[var]]==v]
                  sapply(1:maxt,function(t)tryCatch(quantile(sapply(subset,"[",t),probs=probs,na.rm=T),error=function(e)rep(NA,length(probs))))
    })
    names(allvar)=unique(param[[var]])
    return(allvar)
}

#' functions that take  matrices of \code{dim=c(t,c)} (where t is the length of a simulation and c the number of community in the simulation) and that return some metrics out of thes matrices
whenpeak <- function(d)which.max(colSums(d))
peak <- function(d)max(colSums(d))
sd_peak <- function(d)sd(apply(d,1,which.max)) #variation in community peak



#' @param df a dataframe
#' @param exp an expression as returned by expression or bquote or quote  
#' @return a subset of the dataframe \code{df} respecting the expression \code{exp}
getSubset <- function(df,exp)return(subset(df,eval(exp)))


#' @param summaries a list of matrics stories summaries of trajectories
#' @param 
#' @param 
plotTrajByVar <- function(summaries,cls=NULL,maxt=NULL,posl="topright",add=F,legend=F){
    if(is.null(cls)){
        cls=adjustcolor(1:length(summaries),.5)
        names(cls)=names(summaries)
    }
    rangey=range(summaries,na.rm=T)
    if(!add)plot(1,1,type="n",xlab="time",ylab="",ylim=rangey,xlim=c(1,ncol(summaries[[1]]))) 
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][2,],col=cls[l]))
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][1,],col=cls[l],lty=2))
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][3,],col=cls[l],lty=2))
    if(legend)legend(posl,legend=unique(names(summaries)),col=cls,lwd=1)
}

#' @param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#' @param measures one or more measure stored in a simulation
#' @param metrics one or more functions that can be applied to retrieve information from the simulations
#' @return a table storing for each simulation the metrics computed on the measures wanted 
getMetrics <- function(param,measures="cases",metrics="max"){
    res=sapply(param$id,function(i)getAllInf(i,measures,metrics))
    if(!is.null(dim(res)))return(t(res))
    return(res)
}

#' Return simulation output from a given expe id
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
#' @return a list of matrices that store the ouptu of a simulation 
getData <- function(id,resfold=""){
    if(resfold=="")
        return(readRDS(paste0(id,".RDS")))
    else
        return(readRDS(paste0(file.path(resfold,id),".RDS")))
}

#' Return list of true false if files exist
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
#' @return a boolean which is TRUE if the result of the simulation  \code{id} exists, FALSE if not
simExist <- function(id,resfold=""){
    file.exists(paste0(file.path(resfold,id),".RDS"))
}

#' change the ids to store the whole path of the simu
#' @param id the id of the experiment
#' @param resfold a folder where results are stored
#' @return a string with a full path to a simulation ouptut (could it be merge with simExist? a string if exist, FALSE-NA if not?)
updateid <- function(id,resfold="")file.path(resfold,id)

#' Get a certain element of the list of result and apply function to it
#' @param id the id of the experiment
#' @param measures one or more measure stored in a simulation
#' @param metrics one or more functions that can be applied to retrieve information from the simulations
#' @return the shape of the results depends on the values returns by the functions metrics. If all function in metrics return a unique elments thus \code{getAllInf} return a matrice of \code{dim=c(length(measures),length(metrics))
getAllInf <- function(id,measures="recovers",metrics="identity"){
    sapply(metrics,function(m)sapply(getData(id)[measures],m))
}


plotQuick2plots <- function(df,measures="infs",metrics=c("peak","sd_peak"),var="type",palcol=NULL,main=NULL,alpha=.1){
    varval=unique(df[[var]])

    traject=getDataTrajByVar(df,measures,var=var) 
    data=getMetrics(fpb,measures,metrics)
   
    par(mfrow=c(1,2),oma=c(0,0,1,0))
    if(is.null(palcol))cols=2:(length(varval)+1)
    else cols=pal(length(varval))
    names(cols)=as.character(varval)

    plot(data,col=adjustcolor(cols[as.character(df[[var]])],alpha=alpha),pch=16,cex=.5)
    legend("topright",col=cols,legend=paste(var,names(cols)),pch=16)

    plotTrajByVar(traject,cls=cols)
    legend("topright",col=cols,legend=paste(var,names(cols)),lwd=1)

    if(is.null(main))main=paste(paste0(metrics,collapse=" and "),"of",measures,"for different",var)
    mtext(main,3,-2,outer=T,cex=1.2)
    par(mfrow=c(1,1),oma=rep(0,4))
}
