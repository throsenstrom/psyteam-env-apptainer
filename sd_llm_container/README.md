# Language Model container for SD-Desktop

## Building the container

To build the container, type:
```
sudo singularity build sd_llm_container_cpu.sif sd_llm_container_cpu.def
```

To push the container to Allas, first set up the Allas access. Then run:
```
a-put --sdx --override sd_llm_container_cpu.sif -b sd_llm_container \
	  -m "LLM container for the SD desktop."
```

Then the container will be available through the Data Gateway.
For more information, see
[CSC documentation: Utilizing Apptainer containers in SD Desktop](https://docs.csc.fi/data/sensitive-data/sd-desktop-singularity/)

## Using `make` 

The attached Makefile includes the targets for building the container
(`make all`) and pushing it to Allas (`make push`).

## Running the container

To run Jupyter inside the container, execute:
```
singularity run sd_llm_container_cpu.sif jupyter notebook
```
