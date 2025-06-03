# Apptainer recipe for PsyTEAM

Public recipe files for Apptainer (formerly Singularity) containers to be used on CSC HPC platforms by [PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) at University of Helsinki, Finland.

Currently, using rocker/ml from the [Rocker Project](https://rocker-project.org/) as a base when building.

We need to install `tlverse` from the container (e.g., in cPouta) because personal GitHub account is needed, as per their [instructions](https://tlverse.org/tlverse-handbook/setup.html#installtlverse). 
So, the associated packages will not shown in the definition file. Similarly, non-public offline scripts will be added as a tarball to `\opt` folder.

To run R using the container, navigate to the folder containing the sif-file and execute in shell

```
apptainer exec \
   --scratch /run,/var/lib/rstudio-server \
   --workdir $(mktemp -d) \
   psyteam-env.sif \
   rserver --www-address=127.0.0.1 --server-user=$(whoami)
```

Then use your web browser to navigate to `http://localhost:8787` from where you'll find access via RStudio Server.
If multiple users are running the container on a same virtual machine, they need to setup different ports.

Building the container is only possible in a machine with online access, so don't try it in sensitive-data environments. You'll have a copy there.
Developers of the environment can build containers by executing in shell, e.g., 

```
git clone https://github.com/throsenstrom/psyteam-env-apptainer/
cd psyteam-env-apptainer/
apptainer build psyteam-env.sif psyteam-env.def
```

NB! Files at a preliminary testing and building phase!
---
