#!/bin/bash

ford speedy_f90_ford_project_file.md
mkdir -p build/html
cp -rf source/speedy_f90 build/html/speedy_f90
sphinx-build -M html "source" "build"
