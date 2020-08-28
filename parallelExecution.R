old <- Sys.time() 
source("parallelTools.R")

listsubproc=generateListSubproc("hostandprocs.csv")

for(ind in 1:518400){
folder_res="results/" #folder where all results will be store, should be the same than the one used in ScriptForPaper4.R
name=paste0(ind) #name of the simulation (should be unique)

outputname=file.path(folder_res,paste0(name,".RDS")) #file where result of the simulation will be sotred (!!!should correspond to the name given in ScriptForPaper4.R)

print("=====================================================")
print(paste0("expe:",ind))


while(length(which(sapply(listsubproc,"[[","free")))<1){
    listsubproc=checkHost(listsubproc)
    if(length(which(sapply(listsubproc,"[[","free")))<1){
        Sys.sleep(1)
        print(paste("no free slot among our", length(sapply(listsubproc,"[[","free")),"list of available CPU"))
        print(paste(length(list.files(folder_res,pattern="*.RDS")),"simulations have finished so far"))
    }
}

freehost=min(which(sapply(listsubproc,"[[","free")))
listsubproc[[freehost]]$file=outputname
listsubproc[[freehost]]$free=FALSE

cmd=paste0('ssh ',listsubproc[[freehost]]$host,' "cd ',getwd(),'; Rscript ScriptForPaper4.R ',ind,' > logs/',listsubproc[[freehost]]$host,"_",name,'.log "')
system(cmd,ignore.stderr=T,wait=F)

print(paste("expe",name,"launched on",listsubproc[[freehost]]$host,"waiting for",outputname,"to finish"))
########################################
########################################

} 
  
new <- Sys.time() - old # calculate difference
print(new) # print in nice format




