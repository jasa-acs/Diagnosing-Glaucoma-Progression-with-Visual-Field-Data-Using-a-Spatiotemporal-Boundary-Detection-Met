###Start with a clean workspace
rm(list = ls())

###Set the location of example_code directory
code.dir <- "."

###Check to see if there is a current enough version of R installed: Requires at least version 3.1!!!
if (as.numeric(R.Version()$major) < 3) stop("The package requires at least R version 3.1.0: must install newer version of R before installing womblR")
if (as.numeric(R.Version()$minor) < 1) stop("The package requires at least R version 3.1.0: must install newer version of R before installing womblR")

###Make sure the necessary packages are installed:
list.of.packages <- c("msm", "mvtnorm", "Matrix", "Rcpp", "RcppArmadillo", "coda", "classInt", "pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

###Make sure the packages are up to date!
lapply(list.of.packages, require, character.only = TRUE) # load the libraries
mvtnormVersion <- sessionInfo()$otherPkgs$mvtnorm$Version
msmVersion <- sessionInfo()$otherPkgs$msm$Version
RcppVersion <- sessionInfo()$otherPkgs$Rcpp$Version
RcppArmadilloVersion <- sessionInfo()$otherPkgs$RcppArmadillo$Version
#Some helpful checks for the proper version of packages!!!
if (as.numeric(unlist(strsplit(mvtnormVersion, ".", fixed = TRUE))[1]) < 1) stop('Package "mvtnorm" needs to be at least version 1.0-0')
if (as.numeric(unlist(strsplit(msmVersion, ".", fixed = TRUE))[1]) < 1) stop('Package "msm" needs to be at least version 1.0.0')
if (as.numeric(unlist(strsplit(RcppVersion, ".", fixed = TRUE))[2]) < 12) stop('Package "Rcpp" needs to be at least version 0.12.9')
if (as.numeric(unlist(strsplit(RcppArmadilloVersion, ".", fixed = TRUE))[2]) < 7) stop('Package "RcppArmadillo" needs to be at least version 0.7.500.0.0')

# NOTE: Since the package contains C++ code it must be compiled.
# (The package RcppArmadillo requires a current version of gcc.
# If you are having issues with compilation, see the RcppArmadillo website,
# https://cran.r-project.org/web/packages/RcppArmadillo/index.html).

###Install womblR
install.packages("womblR")

###Load womblR package
library(womblR)

###Check that the package loaded by looking at the help file
help(womblR)
