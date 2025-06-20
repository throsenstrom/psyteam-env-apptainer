# Apptainer recipe for PsyTEAM

Public recipe files for Apptainer (formerly Singularity) containers to be used on CSC HPC platforms by [PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) at University of Helsinki, Finland.

Currently, using rocker/ml from the [Rocker Project](https://rocker-project.org/) as a base when building.

We need to install `tlverse` from the container (e.g., in cPouta) because personal GitHub account is needed, as per their [instructions](https://tlverse.org/tlverse-handbook/setup.html#installtlverse). 
So, the associated packages will not shown in the definition file. 
Similarly, non-public offline scripts will be added to folder `/usr/psyteam_cscsd_scripts`, whereas the scripts from this repo are in `/opt`.

To run R using the container, navigate to the folder containing the sif-file and execute in shell

```
apptainer exec \
   --scratch /run,/var/lib/rstudio-server \
   --workdir $(mktemp -d) \
   --bind ~/.local/share/rstudio/log \
   psyteam-env.sif \
   rserver --www-address=127.0.0.1 --server-user=$(whoami)
```

Then use your web browser to navigate to `http://127.0.0.1:8787` from where you'll find access via RStudio Server.
If multiple users are running the container on a same virtual machine, they need to setup different localhost addresses. 
E.g. use 127.0.0.2 and so on, both in the Apptainer call and the web browser.

The bind argument line is needed only **when running RStudio for the first time**. It creates the needed folder.
An alternative is to create it manually before running Apptainer by first running:

```
mkdir -p ~/.local/share/rstudio/log
```

It seems that the Rstudio Server tries to log on to that folder and fails only if it doesn't exists in your SD Desktop. So create it once.

Building the container is only possible in a machine with online access, so don't try it in sensitive-data environments. You'll have a copy there.
Developers of the environments can build containers by executing in shell of another VM, e.g., 

```
git clone https://github.com/throsenstrom/psyteam-env-apptainer/
cd psyteam-env-apptainer/
apptainer build psyteam-env.sif psyteam-env.def
```
	
NB! Files at a preliminary testing and building phase!
---
