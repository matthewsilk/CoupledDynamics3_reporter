old <- Sys.time() 
source("parallelTools.R")

listsubproc=generateListSubproc("hostandprocs.csv")
folder_res="results" #folder where all results will be store, should be the same than the one used in ScriptForPaper4.R
p=read.csv("fullparams.csv")
ids=p$id
rm(p)

for(ind in 1:518400){

    i=ids[ind]
    name=paste0(i) #name of the simulation (should be unique)
    outputname=file.path(folder_res,paste0(name,".RDS")) #file where result of the simulation will be sotred (!!!should correspond to the name given in ScriptForPaper4.R)



    print("=====================================================")
    print(outputname)
    print(paste0("expe line:",ind, "id:",i))


    while(length(which(sapply(listsubproc,"[[","free")))<1){
        listsubproc=checkHost(listsubproc)
        if(length(which(sapply(listsubproc,"[[","free")))<1){
            Sys.sleep(1)
            print(paste("no free slot among our", length(sapply(listsubproc,"[[","free")),"list of available CPU"))
            print(paste(length(list.files(folder_res,pattern="*.RDS")),"simulations have finished so far"))
        }
    }

    if(!file.exists(outputname)){
        freehost=min(which(sapply(listsubproc,"[[","free")))
        listsubproc[[freehost]]$file=outputname
        listsubproc[[freehost]]$free=FALSE

        cmd=paste0('ssh ',listsubproc[[freehost]]$host,' "cd ',getwd(),'; Rscript ScriptForPaper4.R ',ind,' > logs/',listsubproc[[freehost]]$host,"_",name,'.log "')
        system(cmd,ignore.stderr=T,wait=F)

        print(paste("expe",name,"launched on",listsubproc[[freehost]]$host,"waiting for",outputname,"to finish"))
        ########################################
        ########################################

    } 
    else{
        print(paste(outputname,"already run"))
    }
}

new <- Sys.time() - old # calculate difference
print(new) # print in nice format




