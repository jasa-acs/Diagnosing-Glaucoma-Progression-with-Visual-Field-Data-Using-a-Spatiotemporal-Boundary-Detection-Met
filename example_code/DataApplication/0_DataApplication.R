###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Load womblR package
library(womblR)

##############################################
### Load Metrics object
##############################################

load(paste0(code.dir, "/Data/Metrics.RData"))
head(Metrics)

##############################################
### Load Alpha object
##############################################

###Load Alpha object
load(paste0(code.dir, "/Data/Alpha.RData"))
head(Alpha)

##############################################
### Load Glaucoma objects
##############################################

###Glaucoma objects from womblR package
blind_spot <- c(26, 35)
W <- HFAII_Queen[ -blind_spot, -blind_spot] # visual field adjacency matrix
DM <- GarwayHeath[-blind_spot] # Garway-Heath angles

##############################################
### Reproduce Table 1.
##############################################

###Scale metrics
Metrics[, 4:7] <- scale(Metrics[, 4:7])

###Regress each metric against progression
regMeanCV <- glm(Prog ~ MeanCV, family = "binomial", data = Metrics)
regPLR <- glm(Prog ~ PLR, family = "binomial", data = Metrics)
regSpaceCV <- glm(Prog ~ SpaceCV, family = "binomial", data = Metrics)
regSTCV <- glm(Prog ~ STCV, family = "binomial", data = Metrics)

###Load xtable library
library(xtable)

###Output Table 1 material
xtable(regMeanCV, digits = c(0, 2, 2, 2, 3))
xtable(regPLR, digits = c(0, 2, 2, 2, 3))
xtable(regSpaceCV, digits = c(0, 2, 2, 2, 3))
xtable(regSTCV, digits = c(0, 2, 2, 2, 3))

##############################################
### Reproduce Figure 2.
##############################################

###Note the VFSeries object is loaded automatically from the womblR package.

###Glaucoma objects from womblR package
blind_spot <- c(26, 35)
W <- HFAII_Queen[ -blind_spot, -blind_spot] # visual field adjacency matrix
DM <- GarwayHeath[-blind_spot] # Garway-Heath angles

###Create figure using PlotAdjacency function from womblR
pdf(paste0(code.dir, "/DataApplication/Results/Figure2.pdf"), height = 5.5, width = 5.5)
PlotAdjacency(W = W,
              DM = DM,
              zlim = c(0, 180),
              Visit = NA,
              edgewidth = 3.75,
              cornerwidth = 0.33,
              lwd.border = 3.75,
              main = "Garway-Heath dissimilarity metric\n across the visual field")
dev.off()


##############################################
### Reproduce Figure 3.
##############################################

###Get Unique Patient IDs
Eyes <- unique(Alpha[, 1:2])

###Create figure 
pdf(paste0(code.dir, "/DataApplication/Results/Figure3.pdf"), height = 4, width = 8)

par(mfcol = c(1, 2))

###Stable plot
plot(1, 1, type = "n", xlim = c(min(Alpha$day), max(Alpha$day)), xlab = "Days from baseline visit", ylim = c(min(Alpha$alpha), max(Alpha$alpha)), ylab = expression("Posterior mean of " ~ alpha[t]), main = "Stable")
for (l in 1:dim(Eyes)[1]) {

	###Set patient information
	patid <- Eyes[l, 1]
	eye <- Eyes[l, 2]
	PatientData <- Alpha[(Alpha$id == patid) & (Alpha$left == eye),]
	if (!is.na(PatientData$prog[1])) if (PatientData$prog[1] == 0) lines(PatientData$day, PatientData$alpha)
	
}

###Progressing plot
plot(1, 1, type = "n", xlim = c(min(Alpha$day), max(Alpha$day)), xlab = "Days from baseline visit", ylim = c(min(Alpha$alpha), max(Alpha$alpha)), ylab = expression("Posterior mean of " ~ alpha[t]), main = "Progressing")
for (l in 1:dim(Eyes)[1]) {

	###Set patient information
	patid <- Eyes[l, 1]
	eye <- Eyes[l, 2]
	PatientData <- Alpha[(Alpha$id == patid) & (Alpha$left == eye),]
	if (!is.na(PatientData$prog[1])) if (PatientData$prog[1] == 1) lines(PatientData$day, PatientData$alpha)
	
}

dev.off()

##############################################
### Reproduce Figure 4A.
##############################################

###Load pROC
library(pROC)

###Format Data
Metrics$PLR[is.na(Metrics$PLR) == 1] <- 1.00
Metrics <- na.omit(Metrics)

###Regressions
regMeanCVPLR <- glm(Prog ~ MeanCV * PLR, family = "binomial", data = Metrics)
regMeanCVPLRSpaceCV <- glm(Prog ~ MeanCV * PLR + MeanCV * SpaceCV + PLR * SpaceCV, family = "binomial", data = Metrics)
regMeanCVPLRSTCV <- glm(Prog ~ MeanCV * PLR + MeanCV * STCV + PLR * STCV, family = "binomial", data = Metrics)

pdf(paste0(code.dir, "/DataApplication/Results/Figure4A.pdf"), height = 6, width = 6)
plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "", asp = 1, cex.axis = 1, cex.lab = 1.3)
# abline(a = 0, b = 1, lty = 2, col = 1)
abline(v = 0.15, col = 1, lty = 2)

###ROC curve for MeanCV and PLR
rocMeanCVPLR <- pROC::roc(Metrics$Prog, predict(regMeanCVPLR, type = "response"))
lines(1 - rocMeanCVPLR$spec, rocMeanCVPLR$sens, col = 2, lwd = 3)

###ROC curve for MeanCV and PLR + Space CV
rocMeanCVPLRSpaceCV <- pROC::roc(Metrics$Prog, predict(regMeanCVPLRSpaceCV, type = "response"))
lines(1 - rocMeanCVPLRSpaceCV$spec, rocMeanCVPLRSpaceCV$sens, col = 3, lwd = 3)

###ROC curve for MeanCV and PLR + ST CV
rocMeanCVPLRSTCV <- pROC::roc(Metrics$Prog, predict(regMeanCVPLRSTCV, type = "response"))
lines(1 - rocMeanCVPLRSTCV$spec, rocMeanCVPLRSTCV$sens, col = 4, lwd = 3)

###Legend
legend(0.39, 0.15, 
       col=c(2, 3, 4), 
       legend=c("Mean CV & PLR", "+ Space CV", "+ ST CV"),
       lwd=c(3, 3, 3), bty = "n")
text(0.999, 0.095, "0.68    0.21") 
text(0.999, 0.045, "0.70    0.22") 
text(0.999, -0.01, "0.74    0.29") 
text(0.93, 0.145, "AUC", font = 2) 
text(1.05, 0.145, "pAUC", font = 2) 

dev.off()

##############################################
### Reproduce Figure 4B.
##############################################

###Load Predicted Probabilities
load(paste0(code.dir, "/Data/PredProbs.RData"))

###Clinical Assessment of Progression 
Prog <- c(rep(1, 50), rep(0, 141))

###Calculate AUC and pAUC For All Metrics
AUC <- function(x) as.numeric(pROC::auc(pROC::roc(Prog, x)))  
CI.AUC <- function(x) as.numeric(pROC::ci.auc(pROC::roc(Prog, x)))[c(1, 3)]
pAUC <- function(x)  as.numeric(pROC::auc(pROC::roc(Prog ~ x, partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp"), partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp"))
CI.pAUC <- function(x)  as.numeric(pROC::ci.auc(pROC::roc(Prog ~ x, partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp"), partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp"))[c(1, 3)]
  
###Get summaries
pAUCOut <- list()
for (i in 1:length(PredProbs)) {
	
	###Set Metric
	metric <- PredProbs[[i]]
		
	###Calculate pAUC
	temp <- cbind(apply(metric, 2, pAUC), t(apply(metric, 2, CI.pAUC)))
	pAUCOut[[i]] <- temp / 0.15
		
	##Iteration
	cat("Metric: ", i, " of ", length(PredProbs), "\n")
	
}
names(pAUCOut) <- c("MeanCVPLR", "MeanCVPLRSpaceCV", "MeanCVPLRSTCV")

###Initiate figure
pdf(paste0(code.dir, "/DataApplication/Results/Figure4B.pdf"), height = 6, width = 6)

plot(1, 1, ylim = c(0, 0.35), xlim = c(0, 4.5), type = "n", yaxt = "n", xaxt = "n", xlab = "Years from baseline visit", ylab = "pAUC", main = "b)", cex.axis = 1.3, cex.lab = 1.3)
axis(1, at = seq(0, 4.5, 1))
axis(2, at = seq(0, 0.35, 0.05))
suppressWarnings(lines(loess.smooth(as.numeric(names(pAUCOut$"MeanCVPLR"[, 1])) / 365, as.numeric(pAUCOut$"MeanCVPLR"[, 1]), span = 0.6), col = 2, lwd = 3))
suppressWarnings( lines(loess.smooth(as.numeric(names(pAUCOut$"MeanCVPLRSpaceCV"[, 1])) / 365, as.numeric(pAUCOut$"MeanCVPLRSpaceCV"[, 1]), span = 0.6), col = 3, lwd = 3))
suppressWarnings( lines(loess.smooth(as.numeric(names(pAUCOut$"MeanCVPLRSTCV"[, 1])) / 365, as.numeric(pAUCOut$"MeanCVPLRSTCV"[, 1]), span = 0.6), col = 4, lwd = 3, lty = 1))
abline(h = 0.15 / 2, col = 1, lty = 2)
legend("bottomright", legend = c("Mean CV & PLR", "+ Space CV", "+ ST CV"), lty = c(1, 1, 1), col = c(2, 3, 4), ncol = 1, lwd = 3, cex = 1, bty = "n")

dev.off()

##############################################
### Reproduce Figure 4C.
##############################################

###Create boxplots
Names <- c("Mean CV & PLR", "Mean CV & PLR + Space CV", "Mean CV & PLR + ST CV")
pdf(paste0(code.dir, "/DataApplication/Results/Figure4C.pdf"), height = 10, width = 8)

par(mfcol = c(2, 1))
for (i in c(1, 3)) {
	metric <- PredProbs[[i]][, -c(1, 2)]
	colnames(metric) <- c(as.numeric(colnames(metric[,-9])) / 365, "End")
	boxplot(metric, xlim = c(0.5, ncol(metric) + 0.5), boxfill = rgb(1, 1, 1, alpha = 1), border = rgb(1, 1, 1, alpha = 1), ylab = "Probability of Progression", xlab = "Years from baseline visit", main = Names[i], ylim = c(0, 1), cex.lab = 1.3, cex.axis = 1)
	boxplot(metric[which(Prog == 0), ], xaxt = "n", add = TRUE, boxfill = "blue", boxwex = 0.25, at = 1:ncol(metric) - 0.15, cex.axis = 1)	
	boxplot(metric[which(Prog == 1), ], xaxt = "n", add = TRUE, boxfill = "red", boxwex = 0.25, at = 1:ncol(metric) + 0.15, cex.axis = 1)
	legend("topright", legend = c("Stable", "Progressing"), pch = c(15, 15), col = c("blue", "red"), ncol = 2, bty = "n", pt.cex = 3, cex = 1.5)	
}

dev.off()

##############################################
### Reproduce Table 2.
##############################################

###Collect regression objects
regs <- list(regMeanCVPLR, regMeanCVPLRSpaceCV, regMeanCVPLRSTCV)

###Calculate AUC and pAUC
rocs <- rocsPartial <- list()
for (i in 1:length(regs)) {
	reg <- regs[[i]]
	rocs[[i]] <- pROC::roc(Metrics$Prog, predict(reg))
	rocsPartial[[i]] <- pROC::roc(Metrics$Prog, predict(reg), partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp")	
}

###Calculate AUC and pAUC p-value
p_AUC1 <- roc.test(rocs[[1]], rocs[[2]], method = "bootstrap", alternative = "less")$p.value
p_AUC2 <- roc.test(rocs[[1]], rocs[[3]], method = "bootstrap", alternative = "less")$p.value
p_pAUC1 <- roc.test(rocsPartial[[1]], rocsPartial[[2]], method = "bootstrap", alternative = "less")$p.value
p_pAUC2 <- roc.test(rocsPartial[[1]], rocsPartial[[3]], method = "bootstrap", alternative = "less")$p.value

###Addon p-value
pvalues <- c(anova(regMeanCVPLR, regMeanCVPLRSpaceCV, test = "Chisq")$"Pr(>Chi)"[2], anova(regMeanCVPLR, regMeanCVPLRSTCV, test = "Chisq")$"Pr(>Chi)"[2])

###Summarize and create table 
Table2 <- cbind(cbind(round(unlist(lapply(regs, AIC)), 2), round(unlist(lapply(rocs, pROC::auc)), 2), round(unlist(lapply(rocsPartial, f <- function(x) pROC::auc(x, partial.auc = c(0.85, 1), partial.auc.correct = FALSE, partial.auc.focus = "sp"))) / 0.15, 2), c(rep("", 1), round(pvalues[1], 3), round(pvalues[2], 3)), c(rep("", 1), round(p_AUC1, 3), round(p_AUC2, 3)), c(rep("", 1), round(p_pAUC1, 3), round(p_pAUC2, 3))))
colnames(Table2)<-c("AIC","AUC","pAUC","p-value1", "p-value2", "p-value3")
rownames(Table2)<-c("Mean CV & PLR", "Mean CV & PLR + Space CV", "Mean CV & PLR + ST CV")
DigitsMat <- matrix(c(rep(0, 4), rep(2, 4), rep(2, 4), rep(2, 4), rep(3, 4), rep(2, 4), rep(2, 4), rep(2, 4), rep(3, 4)), nrow = 4, ncol = 9)
print(xtable(Table2), sanitize.text.function = function(x) {x})

###Note that this table does not exactly match the Table 2 in the paper. This is because this table is created using the SCALED metrics, while the paper uses the unscaled version. This does not effect the reproduction, however as the resuls are nearly identical.

##############################################
### Reproduce Figure 1 of the Appendix.
##############################################

###Set Metrics
MeanCV <- Metrics$MeanCV
PLR <- Metrics$PLR
SpaceCV <- Metrics$SpaceCV
STCV <- Metrics$STCV

###Pearson correlation tests
cor.test(MeanCV, SpaceCV)
cor.test(STCV, MeanCV)
cor.test(SpaceCV, STCV)
cor.test(MeanCV, PLR)
cor.test(STCV, PLR)
cor.test(SpaceCV, PLR)

###Create figure
pdf(paste0(code.dir, "/DataApplication/Results/Figure1_Appendix.pdf"), height = 6, width = 8.5)

par(mfrow = c(2, 3))

##MeanCV vs SpaceCV
plot(1, 1, type = "n", xlab = "Mean CV", ylab = "Space CV", xlim = c(-1.5, 4), ylim = c(-2, 3.2)) # initialize plot
points(MeanCV, SpaceCV) # add points that are progressing
abline(lm(SpaceCV ~ MeanCV), col = "black", lty = 2) # add regression line
text(3.5, 3.2, expression(paste(rho~"= 0.026", sep = ""))) # add pearson correlation
text(3.6, 2.9, "(0.718)") # add pearson correlation p-value

##MeanCV vs STCV
plot(1, 1, type = "n", xlab = "ST CV", ylab = "Mean CV", ylim = c(-1.5, 4), xlim = c(-1.5, 4))
points(STCV, MeanCV)
abline(lm(MeanCV ~ STCV), col = "black", lty = 2)
text(3.5, 4.0, expression(paste(rho~"= 0.062", sep = "")))
text(3.6, 3.7, "(0.389)")

##SpaceCV vs STCV
plot(1, 1, type = "n", xlab = "Space CV", ylab = "ST CV", ylim = c(-1.5, 4.3), xlim = c(-2, 3.3))
points(SpaceCV, STCV)
abline(lm(STCV ~ SpaceCV), col = "black", lty = 2)
text(2.8, 4.3, expression(paste(rho~"= 0.477", sep = "")))
text(2.8, 3.95, "(<0.001)")

##PLR vs MeanCV
plot(1, 1, type = "n", xlab = "Mean CV", ylab = "PLR", xlim = c(-1.5, 4), ylim = c(-0.8, 5.5)) # initialize plot
points(MeanCV, PLR) # add points that are progressing
abline(lm(PLR ~ MeanCV), col = "black", lty = 2) # add regression line
text(3.5, 5.5, expression(paste(rho~"= 0.090", sep = ""))) # add pearson correlation
text(3.6, 5.1, "(0.216)") # add pearson correlation p-value

##PLR vs STCV
plot(1, 1, type = "n", xlab = "ST CV", ylab = "PLR", ylim = c(-0.8, 5.5), xlim = c(-1.5, 4)) # initialize plot
points(STCV, PLR) # add points that are progressing
abline(lm(PLR ~ STCV), col = "black", lty = 2) # add regression line
text(3.5, 5.5, expression(paste(rho~"= 0.109", sep = ""))) # add pearson correlation
text(3.6, 5.1, "(0.134)") # add pearson correlation p-value

##PLR vs SpaceCV
plot(1, 1, type = "n", xlab = "Space CV", ylab = "PLR", ylim = c(-0.8, 5.5), xlim = c(-2, 3.3)) # initialize plot
points(SpaceCV, PLR) # add points that are progressing
abline(lm(PLR ~ SpaceCV), col = "black", lty = 2) # add regression line
text(2.8, 5.5, expression(paste(rho~"= 0.024", sep = ""))) # add pearson correlation
text(2.9, 5.1, "(0.739)") # add pearson correlation p-value

dev.off()
