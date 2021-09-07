# CoupledDynamics3_reporter parallel version

To run one single simulation suing a given set of parameters store in `paramfile`:

```bash
Rscript ScriptForPaper4.R N expname paramfile
```

Where: 
* N a line of the file given in `expname`
* `paramfile` a csv file with the parameters used for the run name of the experiment, with a column "id" used to name the simulations' output
* `expname` the name of the experiment, output will be stored in a folder `./expname/`, logs for all run in `./logs_expname/`

As an example:


```bash
Rscript ScriptForPaper4.R 3 test fullparams.csv
```
Will run the simulation with the parameter described at the line 3 of `fullparams.csv`, which are:

```
"","id","net","start","SC","AW","RE","p_inf","level","type","mPps","Ps","Ph","Pd","Pr","Prd","delay","hc","eff","rep"
"3",3,3,0.2,0.1,0.1,-0.075,1,"comm","C",0.2,0.25,1,1,1,0.75,1,20,0.001,1
```

And store the result of this experiment in the folder `test/` in an RDS file : `test/3.RDS`



You can then use `parallelExecution.R` to loop over all lines of the file `paramfile`  and send that to list of host defined in the file `hostfile`, all results will be stored in the folder `expname/`.

```bash
Rscript parallelExecution.R expname paramfile hostfile > logAllruns #this logfile can be very big! 
```
Where: 
* `expname` the name of the experiment, output will be stored in a folder `./expname/`, logs for all run in `./logs_expname/`
* `paramfile` a csv file with the parameters used for the run name of the experiment, with a column "id" used to name the simulations' output
* `hostfile` a csv file with name of the host and number of experiment ber host



