# The 'example_code' directory contains five subdirectories or files, 'Data', 'DataApplication', Installation.R, 'Simulation', and 'SpaceOnlyModel':

1. 'Data': contains the data objects Alpha.RData, Metrics.RData and VFSeries.RData we use throughout the reproducibility exercise.
* Alpha.RData is a data object that contains the posterior mean estimates of  for each patient at all their visits.
* Metrics.RData is a data object that contains the "Mean CV", PLR, "Space CV" and "ST CV" metrics for all patient eyes along with their progression status.
* PredPobs.RData is a data object that contains the predicted probabilities of progression at half year intervals up to 4.5 years and also at the end of the study for metrics, "Mean CV & PLR", "Mean CV & PLR + Space CV", and "Mean CV & PLR + ST CV". This object is a list with three components, each component being a matrix corresponding to a metric (each row is a patient and each column is a time during the study). 
* VFSeries.RData is the VF series for the example patient described in the Data section above and is the same data object contained in the womblR R package.

2. 'DataApplication' contains scripts to replicate results from the data application section of the manuscript.
* 0_DataApplication.R: This script loads Metrics.RData and reproduces Tables 1 and 2, Figures 2, 3, and 4 and Figure 1 of the Appendix.
* 1_STCV.R: This script shows how to replicate the proposed methodology to calculate “ST CV” for the example patient. The script loads the womblR package and fits the spatiotemporal boundary detection on the example patient to calculate "ST CV".  (This script takes approximately 20 minutes to run).
* 2_SpaceCV.R: This script shows how to replicate the Lee and Mitchel (2011) spatial boundary detection model to calculate "Space CV" for the example patient. This model sources the code in the local 'SpaceOnlyModel' directory to calculate "Space CV" (This script takes approximately 35 minutes to run).
* 3_MeanCV.R: This script shows how to calculate "Mean CV" for the example patient. 
* 4_PLR.R: This script shows how to calculate PLR of the example patient.
* 5_TestReproducibility.R: This script demonstrates that the "Mean CV", PLR, "Space CV", and "ST CV" calculated in the three previous scripts are equal to the reproducible values in Metrics.RData.
* 'Output': This subdirectory will be populated with objects STCV.RData, SpaceCV.RData, MeanCV.RData and PLR.RData after running scripts 1_STCV.R, 2_SpaceCV.R, 3_MeanCV.R, and 4_PLR.RData, respectively. 
* 'Results': This subdirectory will be populated with objects Figure1_Appendix.pdf, Figure2_Appendix.pdf, Figure2A.pdf, Figure2B.pdf, and Figure3.pdf after running 0_DataApplication.R.

3. Installation.R: This script is used to install the womblR package and all of its dependencies. This should be the first script that is used in the reproducibility analyses, as all the other scripts require the womblR package. 

4. 'Simulation' contains a script for simulating data for the simulation study. 
* 0_DataGeneration.R: This script generates simulated data for the simulation study based on the average patient outlined in the manuscript. 
* 'Data': This subdirectory will be populated with eight data objects, SettingX.RData, each corresponding to a simulation setting. Each column of SettingX.RData corresponds to a simulated dataset, so that the dataset contained within each object has 1,000 columns.
* 'Summary': This subdirectory will be populated with a data object SimSummary.RData. This data object is used to aid in the creation of Table 2. It contains the true CV for each simulated dataset.

5. 'SpaceOnlyModel' contains scripts for running the spatial boundary detection model of Lee and Mitchell (2011) needed to create "Space CV".
* SpaceOnly.R: This is the function that implements the spatial boundary detection model. The other scripts are support functions. 