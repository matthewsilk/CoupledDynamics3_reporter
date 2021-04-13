old <- Sys.time() 
source("parallelTools.R")

args=commandArgs(trailingOnly = TRUE) #pass name of the file with arguments
expname=args[1] #first argument is the line to be read in the file storing the parameters.
paramfile=args[2] #first argument is the line to be read in the file storing the parameters.
hostfile=args[3] #first argument is the line to be read in the file storing the parameters.

listsubproc=generateListSubproc(hostfile)
folder_res=expname #folder where all results will be store, should be the same than the one used in ScriptForPaper4.R, should pass that by arguments to both this script and ScriptForPaper4.R script
dir.create(folder_res, showWarnings = FALSE)
dir.create(paste0('logs_',expname,'/'), showWarnings = FALSE)
p=read.csv(paramfile)
ids=p$id
rm(p)

for(ind in 1:length(ids)){

    i=ids[ind]
    name=paste0(i) #name of the simulation (should be unique)
    outputname=file.path(folder_res,paste0(name,".RDS")) #file where result of the simulation will be sotred (!!!should correspond to the name given in ScriptForPaper4.R)



    print("=====================================================")
    print(outputname)
    print(paste0("expe line:",ind, ", id:",i))


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

        #Main line that launch the simulations on all computers
        cmd=paste0('ssh ',listsubproc[[freehost]]$host,' "cd ',getwd(),'; Rscript uniqueRun.R ',ind,' ',expname, ' ',paramfile,' 1> logs_',expname,'/',listsubproc[[freehost]]$host,"_",name,'.log',' 2> logs_',expname,'/',listsubproc[[freehost]]$host,"_",name,'.err "')
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




