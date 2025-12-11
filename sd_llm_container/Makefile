all: sd_llm_container_cpu.sif

sd_llm_container_cpu.sif: sd_llm_container_cpu.def
	singularity build $@ $<

push: sd_llm_container_cpu.sif
	a-put --sdx --override $< -b sd_llm_container \
	      -m "LLM container for the SD desktop."
