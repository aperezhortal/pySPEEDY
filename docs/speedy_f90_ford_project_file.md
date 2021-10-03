project: SPEEDY.f90
summary: An intermediate complexity atmospheric general circulation model
src_dir: ../speedy.f90/
output_dir: ./_build/html/speedy_f90
fpp_extensions: fpp
author: Sam Hatfield
predocmark: <
docmark_alt:
predocmark_alt: >
display: public
         protected
         private
source: true
graph: true
search: true
macro: TEST
       LOGIC=.true.
extra_filetypes: sh #

The SPEEDY.f90 is a modern Fortran implementation of the intermediate complexity atmospheric
general circulation
[SPEEDY model](http://users.ictp.it/~kucharsk/speedy_description/km_ver41_appendixA.pdf)
developed by Fred Kucharski, Franco Molteni, and Martin P. King.

This model was originally developed by Sam Hatfield.
This page, documents the version of the code used in the [pySPEEDY package](../index.html)
that was considerable refactored to add a python interface and make it thread-safe.

The original Fortran version is available at https://github.com/samhatfield/speedy.f90


