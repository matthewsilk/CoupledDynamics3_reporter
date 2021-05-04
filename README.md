# CoupledDynamics3_reporter 

## Single Run
To run one unique simulation with a given set of parameters:

```bash
Rscript ScriptForPaper4.R 3 expname paramfile
```

This will run the simulation with the parameter described at the line 3 of the file `paramfile`, (something like :)

```
"","id","net","start","SC","AW","RE","p_inf","level","type","mPps","Ps","Ph","Pd","Pr","Prd","delay","hc","eff","rep"
"3",3,3,0.2,0.1,0.1,-0.075,1,"comm","C",0.2,0.25,1,1,1,0.75,1,20,0.001,1
```

And store the result of this experiment in the folder `expname/` in an RDS file : `expname/3.RDS`

## Multiple Runs

To run all experiment you can use the original loop in `SequentialLoop.R` or use `parallelExecution.R`. 

To use the  second option you will need to do :

```bash
Rscript parallelExecution.R expname paramfile hostfile > logAllruns #this logfile can be very big! 
```

Where: 
* `expname` the name of the experiment, output will be stored in a folder `./expname/`, logs for all run in `./logs_expname/`
* `paramfile` a csv file with the parameters used for the run name of the experiment, with a column "id" used to name the simulations' output
* `hostfile` a csv file with name of the host and number of experiment ber host

This  will to loop over all lines of the file `paramfile`  and send that to list of host defined in the file `hostfile`, all results will be stored in the folder `expname/`.

## Analysis results

script to analyse and visualise the output of the simulations are:

* `analysis_script2.R` (Matt's original script)
* `testools.R` (simon's script)
* `tools.R` (functions used by simon's scripts)

