###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Source scripts used to fit space only model to create SpaceCV metric
source(paste0(code.dir, "/SpaceOnlyModel/adjacency.R")) # Original Adjacency Matrix
source(paste0(code.dir, "/SpaceOnlyModel/opticnervedegree.R")) # Optic Nerve Degree (object called degree)
source(paste0(code.dir, "/SpaceOnlyModel/ComputeDissimilarityMetric.R")) # Dissimilarity Metric Functions
source(paste0(code.dir, "/SpaceOnlyModel/W.alpha.R")) # W(alpha) Function
source(paste0(code.dir, "/SpaceOnlyModel/SpaceOnly.R")) # Lee and Mitchell 2011 Space Model

###Define blind spot
blind_spot <- c(26, 35)

###Create original adjacency matrix (same as HFAII_Queen in womblR package)
Woriginal <- adj.mat(Queen)
Wreduced <- Woriginal[-blind_spot, -blind_spot]

###Load dissimilarity metric
scale_degree <- 100
Zoriginal <- obtainDissimilarityMetric(degree / scale_degree, Woriginal)
Zreduced <- obtainDissimilarityMetric(degree[-blind_spot] / scale_degree, Wreduced)

###Get phi upper bound
b_alpha <- log(0.50) / -min(Zreduced[Zreduced>0]) #original definition used by Lee and Mitchell

###Load visual field data for example patient
load(paste0(code.dir, "/Data/VFSeries.RData"))
VFSeries <- VFSeries[order(VFSeries$Location), ] # sort by location
VFSeries <- VFSeries[order(VFSeries$Visit), ] # sort by visit
VFSeries <- VFSeries[!VFSeries$Location %in% blind_spot, ] # remove blind spot locations

###Set Data Objects
Nu <- length(unique(VFSeries$Visit))
M <- length(unique(VFSeries$Location))
Ymat <- matrix(VFSeries$DLS, nrow = M, ncol = Nu)

###MCMC Sampler Inputs
NSims <- 250000
NBurn <- 10000
NThin <- 25

###Must Loop over visits (since this is a space only model)
Reg<-list()
for (i in 1:Nu) {

	###Set Data at Visit i
	Yreduced <- matrix(Ymat[,i], nrow=M, ncol=1)

	###Fit OPOV (i.e. Duncan Lee Model)
	Reg[[i]]<-SpaceOnly(Y = Yreduced, Z = Zreduced, W = Wreduced, Inits = list(mu = 2.5, tau2 = 1, alpha = 1/2),
						Hypers = list(mu = 10e4, tau2 = c(0.001, 0.001), alpha = b_alpha),
						delta = 1, NSims = NSims, NBurn = NBurn, NThin = NThin)

	cat(paste0("Model: ", i," of ", Nu,"\n"))

}

###Coefficient of variation function
cv <- function(x) sd(x) / mean(x)

###Obtain the posterior distribution of the CV of alpha
Alpha <- NULL
for (i in 1:Nu) Alpha<-cbind(Alpha, Reg[[i]]$alpha)
CVAlpha<-apply(Alpha, 1, cv)

###Calculate SpaceCV
SpaceCV <- mean(CVAlpha)

###Save SpaceCV
save(SpaceCV, file = paste0(code.dir, "/DataApplication/Output/SpaceCV.RData"))
