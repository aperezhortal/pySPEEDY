# Main makefile for Speedy.f90

.PHONY:clean
all: 
	$(MAKE) -C speedy.f90
	@cp speedy.f90/libspeedy.a ./

.PHONY:clean
test:	
	pytest -s -v pyspeedy

.PHONY:clean
clean:   
	$(MAKE) -C speedy.f90 clean
	@rm -f pyspeedy/*.so
	@rm libspeedy.a

