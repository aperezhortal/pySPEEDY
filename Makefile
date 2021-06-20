# Main makefile for Speedy.f90

.PHONY:clean
all: 
	$(MAKE) -C source
	
.PHONY:clean
clean:
	$(MAKE) -C source clean

