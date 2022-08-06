library(tidyverse)
library(mlbench)
library(gmodels)
library(class)

f <- file.choose() 
DL <- read.csv(f) 
DL_tree <- DL
DL_tree <- data.frame(DL_tree)
str(DL_tree)
