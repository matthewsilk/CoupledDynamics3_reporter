
#@param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#@param measure which measure do we want (in "concern"   "exps"      "infs"      "hosps"     "recovers"  "deaths"    "cases" "newcases" "repdeaths" "reports" )
#@param metrics  
#@param metrics  
#@param maxt longest simulation time expected, if NULL this will correspond to the time of longest simulations within the one selected.
getDataTrajByVar <- function(param,measure="cases",maxt=NULL,metric="colSums",var,probs=c(.25,.50,.75)){
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

whenpeak <- function(d)which.max(colSums(d))
peak <- function(d)max(colSums(d))
sd_peak <- function(d)sd(apply(d,1,which.max)) #variation in community peak



#@param df a datafram
#@param exp an expression as returned by expression or bquote or quote  
getSubset <- function(df,exp)return(subset(df,eval(exp)))


#@param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#@param measure which measure do we want (in "concern"   "exps"      "infs"      "hosps"     "recovers"  "deaths"    "cases" "newcases" "repdeaths" "reports" )
#@param metrics  
plotTrajByVar <- function(summaries,cls=NULL,maxt=NULL,posl="topright",add=F,legend=F){
    if(is.null(cls)){
        cls=adjustcolor(1:length(summaries),.5)
        names(cls)=names(summaries)
    }
    rangey=range(summaries)
    if(!add)plot(1,1,type="n",ylim=rangey,xlim=c(1,ncol(summaries[[1]]))) 
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][2,],col=cls[l]))
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][1,],col=cls[l],lty=2))
    lapply(seq_along(summaries),function(l)lines(summaries[[l]][3,],col=cls[l],lty=2))
    if(legend)legend(posl,legend=unique(names(summaries)),col=cls,lwd=1)
}

#@param param a dataframe with at least the path to the output of the simultion a a column attributing the value of  _var_ for each simulationj
#' @param measures one or more measure stored in a simulation
#' @param metrics one or more functions that can be applied to retrieve information from the simulations
getUniqueMetrics <- function(param,measures="cases",metrics="max"){
    sapply(param$id,function(i)getAllInf(i,measures,metrics))
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


