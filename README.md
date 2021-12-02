# DML-Simulations
Files to reproduce analyses running the agent-based model in the Des Moines Lobe. This analysis ran 100,000 monarch agents in the Des Moines Lobe of Iowa for three management scenarios, to simulate the number of eggs laid under these different scenarios. This analysis will be featured in a paper currently (12-1-2021) in review.

Files:

**Working Notes - Des Moines Lobe Milkweed Augmentation Simulations.docx**:  my personal working notes on developing and running these simulations

**DML Model Runs and Parameters for GitHub.xlsx**:  list of model runs, parameters, etc. 

**Workflow for Running Monarch Model using Slurm on ISU HPC machines.docx**:  my workflow for running the model on Iowa State University High Performance Computing services

Shapefiles for Scenarios 1, 2, and 3:

**Spatial_Join_DML4_Counties5_UTMLL.shp**

**Spatial_Join_DML4_Counties5_UTMS2LL.shp**

**Spatial_Join_DML4_Counties5_UTMS3LL.shp**

The shapefiles for are ~400 GB each. Contact me if you need copies. 

Simulation output files (e.g., **CumEggsPerZone.2021.Mar.20.10_57_04.txt**) are ~5.6 TB. Contact me if you need copies. 

**DML-HPC-3.4.R**: example R code to combine simulation results from different instances (this particular code is for Scenario 3, run 4). These are listed as R jobs in the spreadsheet of model runs. 

**DMLSims.R**:  This long code file includes code to summarize the simulation results, which were then also put into an Excel file (next file). The end of the file includes code for a spatial analysis relating egg density and polygon characteristics. I keep most development code to keep track of my thinking, so this file has some code that was not used in final analyses. 

**HabArea and EggsLaid1-3.xlsx**:  summary of the model output - area of each landcover type in the DML and each county, eggs laid per each landcover type per county, and summary figures



