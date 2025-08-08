# Apptainer recipe for PsyTEAM

Public recipe files for Apptainer (formerly Singularity) containers to be used on CSC HPC platforms by [PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) at University of Helsinki, Finland.

Currently, using rocker/ml from the [Rocker Project](https://rocker-project.org/) as a base when building.

We need to install `tlverse` from the container (e.g., in cPouta) because personal GitHub account is needed, as per their [instructions](https://tlverse.org/tlverse-handbook/setup.html#installtlverse). 
So, the associated packages will not shown in the definition file. 
Similarly, non-public offline scripts will be added to folder `/usr/psyteam_cscsd_scripts`, whereas the scripts from this repo are in `/opt`. 
The final image containing these additional tools is called `psyteam-env-plus.sif`, available at our SD Desktop.

To run R in our SD Desktop using the container, navigate to the folder `/shared-directory/kontit` containing the sif-file and execute in shell

```
apptainer exec \
   --scratch /run,/var/lib/rstudio-server \
   --workdir $(mktemp -d) \
   --bind ~/.local/share/rstudio/log,/media/volume:/mnt \
   psyteam-env-plus.sif \
   rserver --www-address=127.0.0.1 --server-user=$(whoami)
```

Then use your web browser to navigate to `http://127.0.0.1:8787` from where you'll find access via RStudio Server.
If multiple users are running the container on a same virtual machine, they need to setup different localhost addresses. 
E.g. use 127.0.0.2 and so on, both in the Apptainer call and the web browser. 
The call text and the sif file can be found from SD Desktop folder `/shared-directory/kontit`. 
If you are sole user at the time, you can simply call this script in the folder using terminal and running

```
source exec_R.sh
```

The bind argument creates a log-folder needed by RStudio server. 
In addition, it binds our data volume to the folder `/mnt` of the container. 
Please, create a folder with your under the data volume for your own work, similarly as we had in Kapseli.
When running RStudio server for the very first time you may need to create the log-folder to be bound by running:

```
mkdir ~/.local/share/rstudio/log
```

IMPORTANT! Do work in the mounted volume because it has size 1Tb whereas the system volume is only 80Gb. 
Also, please empty your folder `~/.local/share/rstudio/log` if it gets big to preserve space at the very limited system volume.
You may follow disk space at different volumes by running in terminal 

```
df -h
```

Building the container is only possible in a machine with online access, so don't try it in sensitive-data environments. You'll have a copy there.
Developers of the environments can build containers by executing in shell of another VM, e.g., 

```
git clone https://github.com/throsenstrom/psyteam-env-apptainer/
cd psyteam-env-apptainer/
apptainer build psyteam-env.sif psyteam-env.def
```

---
