###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Load womblR package
library(womblR)

###Define blind side
blind_spot <- c(26, 35)

###Format data for STBDwDM MCMC sampler (uses VFSeries from womblR package)
VFSeries <- VFSeries[order(VFSeries$Visit), ] #sort by visit
VFSeries <- VFSeries[!VFSeries$Location %in% blind_spot, ] #remove blind spot locations
Y <- VFSeries$DLS #assig observed outcome data
Time <- unique(VFSeries$Time) / 365 #time since first visit
Nu <- length(Time)

###Create original adjacency matrix
W <- HFAII_Queen[ -blind_spot, -blind_spot] #Visual field adjacency matrix

###Load Garway-Heath angles for dissimiliarity metric
DM <- GarwayHeath[-blind_spot] #Uses Garway-Heath angles object "GarwayHeath"

###Compute bounds for phi prior
TimeDist <- abs(outer(Time, Time, "-"))
TimeDistVec <- TimeDist[lower.tri(TimeDist)]
minDiff <- min(TimeDistVec)
maxDiff <- max(TimeDistVec)
PhiUpper <- -log(0.01) / minDiff #shortest diff goes down to 1%
PhiLower <- -log(0.95) / maxDiff #longest diff goes up to 95%

###Initial values
Starting <- list(Delta = c(3, 0, 0), T = diag(3), Phi = 0.5)

###Hyperparameters
Hypers <- list(Delta = list(MuDelta = c(3, 0, 0), OmegaDelta = diag(c(1000, 1000, 1))),
               T = list(Xi = 4, Psi = diag(3)),
               Phi = list(APhi = PhiLower, BPhi = PhiUpper))

###Metropolis tuners
Tuning <- list(Theta2 = rep(1, Nu), Theta3 = rep(1, Nu), Phi = 1)

###MCMC inputs
MCMC <- list(NBurn = 10000, NSims = 250000, NThin = 25, NPilot = 20)

###Fit spatiotemporal boundary dection with dissimilarity metric (Seed = 54 by default)
reg.STBDwDM <- STBDwDM(Y = Y, DM = DM, W = W, Time = Time, Starting = Starting, Hypers = Hypers, Tuning = Tuning, MCMC = MCMC)

###Coefficient of variation function
cv <- function(x) sd(x) / mean(x)

###Obtain the posterior distribution of the CV of alpha
Alpha <- reg.STBDwDM$alpha
CVAlpha <- apply(Alpha, 1, cv)

###Calculate ST CV
STCV <- mean(CVAlpha)

###Save ST CV
save(STCV, file = paste0(code.dir, "/DataApplication/Output/STCV.RData"))
