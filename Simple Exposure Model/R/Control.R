rm(list=ls())

source("./Functions.R")
source("./Packages.R")
filename <- "PFOA Typical.xlsx"


read.individuals(filename)

data        <- get.GMGSD(filename)
minsamples  <- prompt.samplesize(data)
data        <- lapply(data,sorter,minsamples)

file.marker("PFOA Example",".xlsx")


