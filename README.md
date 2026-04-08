# Apptainer recipe for PsyTEAM

Public recipe files for Apptainer (formerly Singularity) containers to be used on CSC HPC platforms by [PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) at University of Helsinki, Finland.

Currently, using rocker/tidyverse from the [Rocker Project](https://rocker-project.org/) as a base when building. Current final container in the SD Desktop is the `psyteam-env-v2-5.sif` file at the folder `/media/volume/kontit/`.
The folder additionally contains a sif-file for a CPU version of an LMM container (the GPU version is in another folder) but that isn't needed for running R.

## Using R from a container

To run R in our SD Desktop using the container, navigate to the folder `/media/volume/kontit` and execute in shell:

```
source exec_R.sh
```

Then use your web browser to navigate to `http://127.0.0.1:8787` from where you'll find access via RStudio Server. Make a folder with your name under `/media/volume/kontit/` for your R project files and analyses. 
In practice, you'll want to make your own `exec_R_myname.sh` file by copying and renaming the original `exec_R.sh` and changing last digit of the rserver ip address parameter to a digit not listed in the file `varatut_rstudio_ipt`. 
Running with this prevents conflicts between simultaneous users of SD Desktop. It is also possible to directly execute the commands in `exec_R.sh` in the shell, excluding the first line, possibly modifying them. They are:

```
#!/bin/bash

export TMPDIR=/media/volume/tmp
apptainer exec \
   --scratch /run,/var/lib/rstudio-server \
   --workdir $(mktemp -d) \
   --bind ~/.local/share/rstudio/log,/media/volume:/mnt \
   psyteam-env-v2-5.sif \
   rserver --www-address=127.0.0.1 --server-user=$(whoami)
```

If multiple users are running the container on a same virtual machine, they need to setup different localhost addresses. 
E.g. use 127.0.0.2 and so on, both in the Apptainer call (or exec-file) and the web browser. 
We also want to export the temporary directory to the external volume in the above script because the system file of the SD Desktop is only ~80Gb and will rapidly get clogged by some R packages and routines.

The bind argument creates a log-folder needed by RStudio server. 
In addition, it binds our data volume to the folder `/mnt` of the container, so you can access your files from RStudio server. 
When running RStudio server for the very first time you may need to create the log-folder to be bound by running:

```
mkdir ~/.local/share/rstudio/log
```

IMPORTANT! Do work in the mounted volume because it has size 1Tb whereas the system volume is only 80Gb. 
Also, please empty your folder `~/.local/share/rstudio/log` if it gets big to preserve space at the very limited system volume. 
You may follow disk space at different volumes by opening System Monitor from apps or by running in terminal 

```
df -h
```

## Running long R sessions while logged out

Sometimes it is necessary to leave R to carry out a computation for several hours, days, or even weeks, while doing other things. 
In those cases, it is recommendable to run an R script from a terminal instead of using RStudio server.
This can be done with a very similar execute command as in above, but with a call to `Rscript` command appended at the end instead of the `rserver` command. 
For example:

```
export TMPDIR=/media/volume/tmp
apptainer exec \
   --scratch /run,/var/lib/rstudio-server \
   --workdir $(mktemp -d) \
   --bind ~/.local/share/rstudio/log,/media/volume:/mnt \
   psyteam-env-v2-5.sif \
   Rscript "/mnt/work/myname/myscript.R"
```

Naturally, you must build the entire computational task in to the file `myscript.R` and place it appropriately 
(folder `/mnt/work/myname` in example but do substitute other obvious names for the file and folder).

## About the container(s)

The definition (.def) files in this repository mostly describe the recipes for building the containers from scratch. However, the final RStudio container at the SD Desktop was not built directly from the definition file. 
Instead, a sandbox was built, some additional installations carried out, and a singularity image file (.sif) was built from the sandbox. 
Namely, we need to install `tlverse` from a sandbox (e.g., in cPouta) because personal GitHub account is needed, as per their [instructions](https://tlverse.org/tlverse-handbook/setup.html#installtlverse). 
So, the associated packages will not shown in the definition file. 
Non-public offline scripts were added to the folder `/usr/psyteam_cscsd_scripts` of the first container version (`psyteam-env-plus.sif`), also available in SD Desktop. 
Scripts from this repo are under `/opt` in both the old and newer containers.

Descriptions about the large language models and related CPU and GPU containers will be made available later.

Building the containers is only possible in a machine with online access, so don't try it in sensitive-data environments. You'll have a copy there.
Developers of the environments can build containers by executing in shell of another VM, e.g., 

```
git clone https://github.com/throsenstrom/psyteam-env-apptainer/
cd psyteam-env-apptainer/
apptainer build psyteam-env.sif psyteam-env.def
```

---
