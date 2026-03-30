all: sd_llm_container_gpu.sif

sd_llm_container_cpu.sif: sd_llm_container_gpu.def
	singularity build $@ $<

push: sd_llm_container_gpu.sif
	a-put --sdx --override $< -b 2013945_kontit_Findatadeskiin \
	      -m "LLM GPU container for the SD desktop."
