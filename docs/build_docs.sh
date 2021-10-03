#!/bin/bash

ford speedy_f90_ford_project_file.md
mkdir -p _build/html
sphinx-build -v -v -M html "." "_build"
