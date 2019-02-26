###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Load Metrics object
load(paste0(code.dir, "/Data/Metrics.RData"))

###Load metrics for example patient
load(paste0(code.dir, "/DataApplication/Output/MeanCV.RData"))
load(paste0(code.dir, "/DataApplication/Output/PLR.RData"))
load(paste0(code.dir, "/DataApplication/Output/SpaceCV.RData"))
load(paste0(code.dir, "/DataApplication/Output/STCV.RData"))

###Check reproducibility
metrics <- Metrics[1, ] # our example patient is in row 1
all.equal(metrics$MeanCV, MeanCV)
all.equal(metrics$PLR, PLR)
all.equal(metrics$SpaceCV, SpaceCV)
all.equal(metrics$STCV, STCV)
