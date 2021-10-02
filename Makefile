# Main makefile for Speedy.f90

.PHONY:clean
all: 
	$(MAKE) -C source

.PHONY:clean
test:	
	pytest -s -v pyspeedy

.PHONY:clean
clean:   
	$(MAKE) -C source clean
	@rm -f pyspeedy/*.so

