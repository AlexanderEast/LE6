rm(list=ls())



# Empty Function
LEM.Food.Project <- function(filename){
return()
}


# Internal part of function 1
filename <- "PFOA Typical.xlsx"    
source("./Functions.R")
source("./Packages.R")
    
data          <- get.GMGSD(filename)
minsamples    <- prompt.samplesize(data)
data          <- lapply(data,sorter,minsamples)
`Food Groups` <- make.foodgroups()
Cover         <- make.cover()

final <- create.project.file()

