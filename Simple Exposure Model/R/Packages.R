# Required Packages for EPA's Aggregate Exposure Model
# AE, ORAU, 2020.

requiredpackages<-  c("rio","stringr","plyr","dplyr","readxl")
neededpackages<-setdiff(requiredpackages,rownames(installed.packages()))
install.packages(neededpackages)
suppressWarnings(lapply(requiredpackages, library, character.only = TRUE))
rm(requiredpackages,neededpackages)
