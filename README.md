# CoupledDynamics3_reporter

To run on simulation with a givent set of parameters:

```bash
Rsccript ScriptForPaper4.R 3
```

this will run the simulation with the parameter described at the line 3 of `fullparams.csv`:

```
"","id","net","start","SC","AW","RE","p_inf","level","type","mPps","Ps","Ph","Pd","Pr","Prd","delay","hc","eff","rep"
"3",3,3,0.2,0.1,0.1,-0.075,1,"comm","C",0.2,0.25,1,1,1,0.75,1,20,0.001,1
```

You can then use `parallelExecution.R` to loop over all lines of the file and send that to list of host defined in the file `hostandprocs.csv`
