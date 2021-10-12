#!/bin/bash

ford speedy_f90_ford_project_file.md
mkdir -p _build/html
python -m sphinx -T -E -b html -d _build/doctrees -D language=en . _build/html

