rm(list=ls())

###Set the location of example_code directory
code.dir <- "."

###Load womblR package
library(womblR)

###Load other libraries
library(Matrix) #load for bdiag function
library(mvtnorm) #load for rmvnorm function

###################################################################
### Set data objects required for simulation.
###################################################################

###Exponential temporal correlation structure (wraps womblR internal SIGMA function)
Sigma <- function(phi, day) {
	temp <- day / 365
	womblR:::SIGMA(phi, 0, abs(outer(temp, temp, "-")), length(day))
}

###Glaucoma objects from womblR package
blind_spot <- c(26, 35) # define blind spot
W <- HFAII_Queen[ -blind_spot, -blind_spot] # visual field adjacency matrix (from womblR package)
DM <- GarwayHeath[-blind_spot] # Garway-Heath angles (from womblR package)

###Set data objects
Rho <- 0.99
M  <- 52
EyeM <- diag(M)
ScaleDM <- 100
ScaleY <- 10
WeightsInd <- 0 # continuous

###Obtain the unique locations of adjacencies, AdjacentEdgesBoolean
AdjacentEdgesBoolean <- (W == 1) & (!lower.tri(W))
# AdjacentEdgesBoolean <- matrix(which(AdjacentEdgesBoolean) - 1, ncol = 1) # indicates the index of the adjacencies in the adjacency matrix

###Obtain the scaled dissimilarity metric object, Z
Dist <- function(x, y) pmin(abs(x - y), (360 - pmax(x, y) + pmin(x, y))) #arc length of optic nerve
DM_Grid <- expand.grid(DM, DM)
Z_Vector <- Dist(DM_Grid[ , 1], DM_Grid[ , 2])
Z_Matrix <- matrix(Z_Vector, nrow = dim(W)[1], ncol = dim(W)[1], byrow = TRUE)
Z <- matrix(Z_Matrix[AdjacentEdgesBoolean] / ScaleDM, ncol = 1) # vector of the scaled dissimilarity metric

###################################################################
### Specify the simulation settings.
###################################################################

###Delta: fixed at a posterior mean of the example patient
delta <- matrix(c(2.44634552191589982328424, 0.07015085810136499622214, 0.97418292944508000363868), nrow = 3, ncol = 1)

###T: fixed at a posterior mean of the example patient
TFull <- matrix(0, nrow = 3, ncol = 3)
TFull[lower.tri(TFull, diag=TRUE)] <- c(0.820415457249000001915817,  0.004111988364394999866114, -0.02815687181061940144722,  0.380455175360999986455823, -0.19165688755208701299360,  0.84042396003099995027696)
TFull[upper.tri(TFull)] <- TFull[lower.tri(TFull)]
TDiag <- TFull # diagnoal matrix with no cross-covariance
TDiag[upper.tri(TDiag)] <- TDiag[lower.tri(TDiag)] <- 0

###Phi: fixed at a posterior mean of the example patient
phi <- 0.1633080144659999899392 # small value (i.e. temporal correlation)
phiI <- 100 # very large (i.e. no temporal correlation --> (I)ndependence)

###Number of visits
NuVec <- c(7, 21) #median and maximum setting

###Number of simulations
NThetas <- 100 # number of Theta vectors sampled
NDatas <- 10 # number of datasets sampled/Theta vector
NTotal <- NThetas * NDatas

###Begin Simulation
set.seed(54)

###Days of the VF vists (first 9 days are fixed at those from the average patient we used to set delta, T and phi, then we randomly sampled to get the remaining visit days out to 21 visit days)
day <- c(0, 126, 238, 406, 504, 588, 756, 868, 938) # vistis of the example patient
WaitingTime <- rpois(NuVec[2] - length(day), mean(diff(day))) # sample the remaining visits
day <- c(day, cumsum(WaitingTime) + day[9])

###Assemble simulation Settings:
Settings <- list(
list(phi = phiI, T = TDiag, Nu = NuVec[[1]]),
list(phi = phiI, T = TFull, Nu = NuVec[[1]]),
list(phi = phi, T = TDiag, Nu = NuVec[[1]]),
list(phi = phi, T = TFull, Nu = NuVec[[1]]),
list(phi = phiI, T = TDiag, Nu = NuVec[[2]]),
list(phi = phiI, T = TFull, Nu = NuVec[[2]]),
list(phi = phi, T = TDiag, Nu = NuVec[[2]]),
list(phi = phi, T = TFull, Nu = NuVec[[2]]))
NSettings <- length(Settings)

###Create Object For Simulation Summary
SimSummary <- matrix(nrow = NTotal * NSettings, ncol = 9)
SimSummary[, 1] <- c(rep(paste(LETTERS[1:4], "Median", sep = "_"), each = NTotal), rep(paste(LETTERS[1:4], "Maximum", sep = "_"), each = NTotal))
colnames(SimSummary) <- c("Setting", "TrueCV", "MeanCV", "VarCV", "2.5%CV", "97.5%CV", "Bias", "MSE", "EC")
SettingNames <- unique(SimSummary[, 1])

###################################################################
### Generate data for simulation based on defined settings.
###################################################################

###Begin simulation
for (i in 1:NSettings) {

  	###Set simulation settings
	settings <- Settings[[i]]
	settingName <- SettingNames[i]
	Nu <- settings$Nu
	Day <- day[1:Nu]
	T <- settings$T
	phi <- settings$phi

	###Sample thetas and assign level 1 parameters
	Thetas <- mvtnorm::rmvnorm(NThetas, kronecker(delta, rep(1, Nu)), kronecker(T, Sigma(phi, Day)))

	Mu <- Thetas[, 1:Nu]
	Tau2 <- exp(Thetas[, (Nu + 1):(2 * Nu)]) ^ 2
	Alpha <- exp(Thetas[, (2 * Nu + 1):(3 * Nu)])

	###Save True CV
	SimSummary[(1:NTotal) + (i - 1) * NTotal, 2] <- rep(apply(Alpha, 1, f <- function(x) sd(x) / mean(x)), each = NDatas)

	###Simulate data
	Yout <- matrix(nrow = Nu * M, ncol = NTotal)
	for (j in 1:NThetas) {

	  ##Set parameters
		mu <- Mu[j, ]
		tau2 <- Tau2[j, ]
		alpha <- Alpha[j, ]
		WAlphas <- womblR:::WAlphaCube(alpha, Z, W, M, Nu, WeightsInd)
		JointCovs <- womblR:::JointCovarianceCube(WAlphas, tau2, EyeM, Rho, M, Nu)
		JointCovsList <- list()
		for (k in 1:Nu) JointCovsList[[k]] <- JointCovs[ , , k]

		#Joint moments
		Q <- matrix(Matrix::bdiag(JointCovsList), nrow = Nu * M)
		JointMean <- kronecker(mu, rep(1, M))

		#Simulate data
		YStar <- mvtnorm::rmvnorm(NDatas, JointMean, Q)
		Yout[, (1:NDatas) + (j - 1) * NDatas] <- apply(YStar, 1, f <- function(x) pmax(0, x * ScaleY))

		###Output iteration
		cat(paste0("Setting: ", settingName, "; Simulation: ", j * NDatas, "\n"))

	#End sims over thetas
	}

	###Save simulated data for each setting
	save(Yout, file = paste0(code.dir, "/Simulation/Data/",settingName, ".RData"))

}

###Save SimSummary Object (Contains TrueCV values)
SimSummary <- data.frame(SimSummary)
save(SimSummary, file = paste0(code.dir, "/Simulation/Summary/SimSummary.RData"))
