# Apptainer recipe for PsyTEAM

Public recipe files for Apptainer (formerly Singularity) containers to be used on CSC HPC platforms by [PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) at University of Helsinki, Finland.

Currently, using rocker/ml from the [Rocker Project](https://rocker-project.org/) as a base when building.

We need to install `tlverse` from the container (e.g., in cPouta) because personal GitHub account is needed, as per their [instructions](https://tlverse.org/tlverse-handbook/setup.html#installtlverse). 
So, the associated packages will not shown in the definition file. Similarly, non-public offline scripts will be added as a tarball to `\opt` folder.

NB! Files at a preliminary testing and building phase!
---
