###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Load visual field data for example patient
load(paste0(code.dir, "/Data/VFSeries.RData"))
blind_spot <- c(26, 35)
VFSeries <- VFSeries[order(VFSeries$Location), ] # sort by location
VFSeries <- VFSeries[order(VFSeries$Visit), ] # sort by visit
VFSeries <- VFSeries[!VFSeries$Location %in% blind_spot, ] # remove blind spot locations
Nu <- length(unique(VFSeries$Visit))
M <- length(unique(VFSeries$Location))
day <- unique(VFSeries$Time)

###Format data into wide format and scale to be on similar to how ST CV and Space CV were computed
YWide  <- matrix(VFSeries$DLS, nrow = M, ncol = Nu) / 10

###Calculate PLR
Slope_Pvalues <- apply(YWide, 1, function(x) {reg <- lm(x ~ day); summary(reg)$coef[2, 4]})
PLR <- min(Slope_Pvalues)

###Save MeanCV
save(PLR, file = paste0(code.dir, "/DataApplication/Output/PLR.RData"))
