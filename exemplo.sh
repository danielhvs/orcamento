#!/bin/bash
echo "Exemplo: ./exemplo.sh Jun_19"
lein run $1 && libreoffice arquivo1.csv arquivo2.csv
