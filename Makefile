# Main makefile for Speedy.f90

.PHONY:clean
all: 
	$(MAKE) -C source

.PHONY:clean
test:	
	./test.sh

.PHONY:clean
clean:
	$(MAKE) -C source clean
	@rm -f pyspeedy/_speedy*.so

