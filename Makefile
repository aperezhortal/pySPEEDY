# Main makefile for Speedy.f90

.PHONY:clean
all: 
	python registry/model_state_def.py
	$(MAKE) -C source

.PHONY:clean
test:	
	./test.sh

.PHONY:clean
clean:
	$(MAKE) -C source clean
	@rm -f pyspeedy/_speedy*.so

