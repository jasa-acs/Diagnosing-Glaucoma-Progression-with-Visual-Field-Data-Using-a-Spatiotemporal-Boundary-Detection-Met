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
The first six rows of the dataset are presented below:

| ---------------|------------------|------------------|------------------|

| Visit           | DLS        | Time        | Location         |

| ---------------|------------------|------------------|------------------|
| 1 | 25           | 0   | 1 |
| 2 | 23    | 126   | 1 |
| 3 | 23  | 238   | 1    |
| 4 | 23  | 406  | 1    |
| 5 | 24    | 504   | 1    |
| 6 | 21    | 588   | 1 | 
| ---------------|------------------|------------------|------------------|

## Code

### Abstract
The directory ‘example_code’ provided with the submission contains scripts that directly reproduce the results from the manuscript. A detailed description of each individual script is included in a README document in the ‘example_code’ directory. Meanwhile, code for the MCMC sampler detailed in the manuscript is included in the open source R package _womblR_ available on CRAN. The package includes a vignette that details its functionality for analyzing visual field data.

### Description 
As of this form the current version of the R package _womblR_ is version 1.0.3 (Licensed under GPL 3.0). Supporting software requirements for womblR are detailed on its CRAN website [page](https://cran.r-project.org/web/packages/womblR/index.html). In particular, to reproduce the results from the manuscript will require the following packages installed: _coda_, _classInt_, _devtools_, _Matrix_, _msm_ (>= 1.0.0), _mvtnorm_ (>=1.0-0), _pROC_, _Rcpp_ (>=0.12.9), and _RcppArmadillo_ (0.7.500.0.0).


### Reproducibility 
Tables and Figures in the main text can be reproduced using the supplementary R scripts.
