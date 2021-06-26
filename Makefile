# Main makefile for Speedy.f90

.PHONY:clean
all: 
	$(MAKE) -C source
	
.PHONY:clean
clean:
	$(MAKE) -C source clean
	@rm -f speedy_f90/_speedy*.so

