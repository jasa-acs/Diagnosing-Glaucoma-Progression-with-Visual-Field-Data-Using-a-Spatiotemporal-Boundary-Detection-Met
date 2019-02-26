# Diagnosing-Glaucoma-Progression-with-Visual-Field-Data-Using-a-Spatiotemporal-Boundary-Detection-Met

# Author Contributions Checklist Form

## Data

### Abstract 
Our data come from the Vein Pulsation Study trial in Glaucoma and the Lions Eye trial registry, Perth, Western Australia; a clinical trial that recruited patients with primary open angle glaucoma from New Zealand and Australia with the primary objective of the trial being to detect glaucoma progression using visual fields.

### Availability 
The full data cannot be released at this time because they are involved with an active grant. However, there is a possibility that the full original dataset will be made available at the end of the grant.

Thanks to our collaborators who collected the data, we are able to release the full set of VF series from one patient in our study to demonstrate the reproducibility of the newly developed methods. The VF series that is being released includes nine visits. The data object can be accessed publicly in the R package _womblR_, using the command data(VFSeries) and you can use help(VFSeries) for more detailed information.

### Description
We have permission to use the data from the principal investigator on the study, [Professor William H. Morgan](BillMorgan@lei.org.au). There is no licensing information that corresponds to the data, as there is no identifying information. The data come from the Vein Pulsation Study Trial in Glaucoma and the Lions Eye Institute trial registry, Perth, Western Australia ([here](https://anzctr.org.au/Trial/Registration/TrialReview.aspx?ACTRN=12608000274370)). The VF series for the eye that is being released has nine visits, resulting in a data frame of 486 rows and 4 columns (i.e., 4 variables). The variables are as follows:
1. Visit: The visual field visit number (1, 2, ..., 9).
2. DLS: The observed outcome variable, differential light sensitivity (DLS).
3. Time: The time of the visual field test (in days from baseline visit).
4. Location: The location on the visual field of a Humphrey Field Analyzer-II (Carl Zeiss Meditec Inc., Dublin, CA) (1, 2, ..., 54).

## Code

### Abstract
The directory ‘example_code’ provided with the submission contains scripts that directly reproduce the results from the manuscript. A detailed description of each individual script is included in a README document in the ‘example_code’ directory. Meanwhile, code for the MCMC sampler detailed in the manuscript is included in the open source R package _womblR_ available on CRAN. The package includes a vignette that details its functionality for analyzing visual field data.

### Description 
As of this form the current version of the R package _womblR_ is version 1.0.3 (Licensed under GPL 3.0). Supporting software requirements for womblR are detailed on its CRAN website [page](https://cran.r-project.org/web/packages/womblR/index.html). In particular, to reproduce the results from the manuscript will require the following packages installed: _coda_, _classInt_, _devtools_, _Matrix_, _msm_ (>= 1.0.0), _mvtnorm_ (>=1.0-0), _pROC_, _Rcpp_ (>=0.12.9), and _RcppArmadillo_ (0.7.500.0.0).

**NOTE:** Any non-Windows or OS X users will have to compile the package. To compile a package that contains C++ code, and in particular using RcppArmadillo, requires a recent version of gcc (see the RcppArmadillo [webpage](https://cran.r- project.org/web/packages/RcppArmadillo/index.html) for details). If a new enough version of gcc is not present the package will not compile and the analyses detailed within will not be possible.
 
## Instructions for Use

The analysis was implemented using R version 3.3.3. All analyses were completed with the following R packages, _womblR_ (version 1.0.2), _pROC_ (version 1.12.1), _Matrix_ (version 1.2-14), _mvtnorm_ (version 1.0-8).

### Reproducibility
1. Install _womblR_ package:
  * Follow the instructions of the script Installation.R that is located in the ‘example_code’ directory.
  * Explore the package [vignette](https://cran.r-project.org/web/packages/womblR/vignettes/womblR-example.html) for an introduction to the package.

2. Analysis of Visual Field Data and Glaucoma Progression Risk
  * The code provided allows the user to reproduce Tables 1 and 2 and Figures 2, 3,
and 4 from the main text, and Figure 1 from the Appendix. Furthermore, the user has reproducible code to calculate the “Mean CV”, PLR, “Space CV” and “ST CV” metrics defined in the manuscript in Section 5. In order to reproduce these results, the user must implement the scripts in the ‘DataApplication’ directory in numerical order, starting with 0_DataApplication.R.

3. Simulation Study
  * From the reproducible code provided, the user also has the ability to generate
code for the simulation study described in Section 6.1 in the manuscript. (0_DataGeneration.R in ‘Simulation’). The user can apply the code used in ‘DataApplication’ to calculate “Space CV’ and “ST CV” to the simulated data in order to reproduce Table 3.

**NOTE:** There are two ways to setting the directory paths. All of the paths are referenced relative to the base directory, so the easiest method is to set your working directory to the base directory (i.e., the ‘example_code’ directory). An alternative method is to manually set the character string ‘code.dir’ at the top of each script. The string should not end in a ‘/’.

### Replication
The method introduced in the manuscript will hopefully have a large impact and be applied to various data settings with complex spatiotemporal structures. To make the method accessible, it has been implemented in the womblR package with a vignette introducing the package in the context of VF data. Furthermore, there are detailed help files for each of the functions in the package and a package [manual](https://cran.r-project.org/web/packages/womblR/womblR.pdf).
