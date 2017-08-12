#!/usr/bin/Rscript

# Activate required libraries
library(ggplot2)
library(rjson) 

# Import data from JSON (http://json.org/) file
transactions <- fromJSON(file= "sample.txt")

summary(transactions)

# Quick statistics
statistics <- within(transactions, {infAdic <- factor(infAdic)})
