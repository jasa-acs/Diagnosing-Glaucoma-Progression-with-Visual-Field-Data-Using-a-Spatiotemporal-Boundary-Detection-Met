####Lee and Mitchell 2011 Space Only Model 
SpaceOnly<-function(
	Y=Yreduced, #m x 1 matrix of sensitivity outcome
	Z=Zreduced, #a x 1 matrix of dissimilarity differences, where a is number of adjacnecy edges 
	W=Wreduced, #m x m adjacnecy matrix
	Inits=list(mu=2.5,tau2=1,alpha=1/2), #Initial values of parameters 
	Hypers=list(mu=10e4,tau2=c(3,1),alpha=10), #Prior hyperparameters 
	delta=1, #Metropolis tuning parameter
	rho=0.99, #Fixed propriety parameter
	Scale=10, #Scale for the outcome
	NBurn=10000, #Number of burn-in samples 
	NSims=250000, #Number of simulations after burn-in
	NThin=25, #Interval for which samples are saved after burn-in, must be a factor of NSims
	Proposal="normal", #"normal" or "uniform"
	Verbose=FALSE) {

###Set Seed
set.seed(54)

###Load Libraries
suppressMessages(library(mvtnorm)) #multivariate normal
suppressMessages(library(pscl)) #inverse gamma
suppressMessages(library(msm)) #truncated normal
suppressMessages(library(coda)) #mcmc

###Format Data
YScaled<-Y/Scale #scale
m<-length(YScaled)

###Math Objects
Onem<-matrix(rep(1,m),nrow=m,ncol=1)
Eyem<-diag(m)

###Fixed Adjacency Matrix
Wfixed<-W

###Compute Adjacent Edge Locations Boolean
AdjacentEdgesBoolean<-obtainBooleanEdges(Wfixed)

###Create Global Objects For W.alpha Function
assign("Z",Z,envir=globalenv())
assign("Wfixed",Wfixed,envir=globalenv())
assign("AdjacentEdgesBoolean", AdjacentEdgesBoolean,envir=globalenv())

###Hyperparameters
ATau2<-Hypers$tau2[1]
BTau2<-Hypers$tau2[2]
SigmaMu2<-Hypers$mu[1]
BAlpha<-Hypers$alpha

###Initial Values

	##Parameters 
	mu<-Inits$mu
	tau2<-Inits$tau2
	alpha<-Inits$alpha

	##Covariance Objects
	WAlpha<-W.alpha(alpha)
	DwAlpha<-diag(apply(WAlpha,1,sum))
	WStar<-DwAlpha-WAlpha
	Q<-rho*WStar+(1-rho)*Eyem
	QInverse<-chol2inv(chol(Q))

###Alpha Metropolis Objects
delta<-delta
AcceptanceAlphaCounter<-0
PilotAdaptationCounter<-1

###Tobit Information
TobitBoolean<-Y==0
NumberOfZeros<-sum(TobitBoolean)
LocationOfZeros<-which(TobitBoolean) 
YStarNonZero<-YScaled[!TobitBoolean]

###MCMC Objects
NSims<-NSims
NBurn<-NBurn
NThin<-NThin
NTotal<-NBurn+NSims
WhichKeep<-NBurn+(1:(NSims/NThin))*NThin
NKeep<-length(WhichKeep)
OutProgress<-(1:10)*NTotal/10
PilotAdapt<-(1:10)*NBurn/10

###Output Matrix
RawSamples<-matrix(nrow=3,ncol=NKeep)

###Time MCMC Sampler
BeginTime<-Sys.time()

###Begin Gibbs Sampler
for (s in 1:NTotal) {
	
	##Tobit Step
	if (NumberOfZeros==0) YStar<-YScaled
	if (NumberOfZeros!=0) {
		
		#Initiate Tobit Step
		YStar<-YScaled
		
		#Loop over Latent Observations (since they are dependent, can't vectorize)
		for (i in 1:NumberOfZeros) {
			
			#Subset over Tobit Observations
			ZeroLocationIndex<-LocationOfZeros[i]
			
			#Compute Conditional Mean and Variance
			ConditionalVariance<-tau2/(rho*(WAlpha[ZeroLocationIndex,]%*%Onem)+(1-rho))
			ConditionalMean<-ConditionalVariance*((rho*(WAlpha[ZeroLocationIndex,]%*%YStar)+(1-rho)*mu)/tau2)
			
			#Sample Latent Variable From Full Conditional
			YStar[ZeroLocationIndex]<-rtnorm(1,ConditionalMean,sqrt(ConditionalVariance),upper=0)
			
		}
	}
	
	##Full Conditional For Mu
	MuVariance<-1/(t(Onem)%*%Q%*%Onem/tau2+1/SigmaMu2)
	MuMean<-MuVariance*(t(Onem)%*%Q%*%YStar/tau2)
	mu<-rnorm(1,MuMean,sqrt(MuVariance))
	
	##Full Conditional For Tau2
	ShapeTau2<-ATau2+m/2
	Residuals<-(YStar-mu*Onem)
	ScaleTau2<-BTau2+t(Residuals)%*%Q%*%Residuals/2
	tau2<-rigamma(1,ShapeTau2,ScaleTau2)

	##Reflecting Uniform Proposal
	if (Proposal=="uniform") {
	##Metropolis Step For Alpha
	
		#Sample Proposal Value Of Alpha
		AlphaProposal<-runif(1,alpha-delta,alpha+delta)	
		if (AlphaProposal<=0) AlphaProposal<-abs(AlphaProposal)
		if (AlphaProposal>BAlpha) AlphaProposal<-(2*BAlpha-AlphaProposal)
		
		#Compute W(alpha) and Covariance Proposal
		WAlphaProposal<-W.alpha(AlphaProposal)
		DwAlphaProposal<-diag(apply(WAlphaProposal,1,sum))
		WStarProposal<-DwAlphaProposal-WAlphaProposal
		QProposal<-rho*WStarProposal+(1-rho)*Eyem
		
		#Compute Log Acceptance Ratio
		LogR<-(1/2)*log(det(QProposal)/det(Q))-(1/(2*tau2))*t(Residuals)%*%(QProposal-Q)%*%Residuals
	
		#Metropolis Update
		if (log(runif(1))<LogR) {
			
			#Update Alpha
			alpha<-AlphaProposal
			AcceptanceAlphaCounter<-AcceptanceAlphaCounter+1
			
			#Update Covariance and W
			WAlpha<-WAlphaProposal
			Q<-QProposal
			QInverse<-chol2inv(chol(Q))

		}
	}	

	##Normal Transformation Proposal
	if (Proposal=="normal") {
	##Metropolis Step For Alpha
	
		#Transform Current State
		Theta<-log(alpha/(BAlpha-alpha))
	
		#Sample Transformed Proposal
		ThetaProposal<-rnorm(1,Theta,delta)	
		
		#Compute Alpha Proposal
		AlphaProposal<-(BAlpha*exp(ThetaProposal))/(1+exp(ThetaProposal))
		
		#Compute W(alpha) and Covariance Proposal
		WAlphaProposal<-W.alpha(AlphaProposal)
		DwAlphaProposal<-diag(apply(WAlphaProposal,1,sum))
		WStarProposal<-DwAlphaProposal-WAlphaProposal
		QProposal<-rho*WStarProposal+(1-rho)*Eyem
		
		#Compute Log Acceptance Ratio
		LogR<-(1/2)*log(det(QProposal)/det(Q))-(1/(2*tau2))*t(Residuals)%*%(QProposal-Q)%*%Residuals+(ThetaProposal-Theta)+2*log((1+exp(Theta))/(1+exp(ThetaProposal)))	
	
		#Metropolis Update
		if (log(runif(1))<LogR) {
			
			#Update Alpha
			alpha<-AlphaProposal
			AcceptanceAlphaCounter<-AcceptanceAlphaCounter+1
			
			#Update Covariance and W
			WAlpha<-WAlphaProposal
			Q<-QProposal
			QInverse<-chol2inv(chol(Q))

		}
	}	

	##Manage Acceptance Rates (Pilot Adaptation)
	if (s %in% PilotAdapt) {

		###Metropolis Tuning Parameter
		AcceptancePct<-round(AcceptanceAlphaCounter/PilotAdaptationCounter,digits=2)
		if (AcceptancePct>=0.90) delta<-delta+delta*0.3
		if (AcceptancePct>=0.75) delta<-delta+delta*0.2
		if (AcceptancePct>=0.45) delta<-delta+delta*0.1
		if (AcceptancePct<=0.25) delta<-delta-delta*0.1
		if (AcceptancePct<=0.15) delta<-delta-delta*0.2
		if (AcceptancePct<=0.10) delta<-delta-delta*0.3	
		AcceptanceAlphaCounter<-PilotAdaptationCounter<-0

	}
	PilotAdaptationCounter<-PilotAdaptationCounter+1

	###Store Samples
	if (s%in%WhichKeep) RawSamples[, which(s==WhichKeep)] <- c(mu,tau2,alpha)		

	##Output Verbose
	if (Verbose) {
		cat(paste("Completed Percentage: ",round((s/NTotal)*100,digits=0),"%\n",sep=""))
		cat(paste("Mu: ",round(mu,digits=3),"\n",sep=""))
		cat(paste("Tau2: ",round(tau2,digits=3),"\n",sep=""))
		cat(paste("Alpha: ",round(alpha,digits=3),", Acceptance Rate: ",round(AcceptanceAlphaCounter/PilotAdaptationCounter,digits=2),"\n",sep=""))
		cat(rep("-",75),"\n",sep="")
	}

	##Output Progress
	if (!Verbose) {
		if (s%in%OutProgress) cat(round(100*(s/NTotal)), "%..  ", sep="")	
		if (s==1) {
			cat(rep("-",75),"\n",sep="")
			cat("0%..  ", sep="")
		}
	}
	
	##Output MCMC Sampler Run Time
	if (s==NTotal) {
		EndTime<-Sys.time()
		RunTime<-EndTime-BeginTime
		cat(paste("Run Time: ",round(RunTime,digits=2)," ",attr(RunTime,"unit"),"\n",sep=""))
		cat(rep("-",75),"\n",sep="")
	}

###End MCMC Sampler
}

###Read in Posterior MCMC Samples
Mu<-RawSamples[1,]
Alpha<-RawSamples[3,]
Tau2<-RawSamples[2,]

###Create MCMC Objects
MuMCMC<-as.mcmc(Mu)
AlphaMCMC<-as.mcmc(Alpha)
Tau2MCMC<-as.mcmc(Tau2)

###Summarize Metropolis Output
Metropolis<-list(round(AcceptanceAlphaCounter/PilotAdaptationCounter,digits=2),delta)
names(Metropolis)<-c("accpetance","delta")

###Summarize Output and Return
FinalObject<-list(MuMCMC,Tau2MCMC,AlphaMCMC,Metropolis)
names(FinalObject)<-c("mu","tau2","alpha","metropolis")
return(FinalObject)

####End Space Only Function
}
	
	
	
	

