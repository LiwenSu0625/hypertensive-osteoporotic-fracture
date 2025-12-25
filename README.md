A Longitudinal Dataset of Hypertensive Osteoporotic Fracture Patients Treatments and Long-term Outcomes

## Project Overview
This repository contains R code used for data preprocessing, statistical analysis, and figure generation in the study:
"A longitudinal dataset of hypertensive osteoporotic fracture patients: treatments and long-term outcomes."
All results reported in the manuscript are fully reproducible using the scripts and data described below.

The study utilizes clinical data from tertiary hospitals to assess the impact of hypertension on mortality and refracture risk in osteoporotic fracture patients, including multivariable Cox proportional hazards regression, propensity score matching, and subgroup analyses.

The results summarized in sub.xlsx and sub_refra.xlsx can also be fully reproduced using the R code provided in this repository. They are used to reproduce:
Supplementary Figure S4 (mortality)
Supplementary Figure S5 (refracture)

## Software Requirements
R Version: 4.3.3 or higher
Operating System: Windows/Linux/macOS

## Required R Packages
readxl, dplyr, reshape2, ggplot2, ggpubr, corrplot, survival, survminer, irr, vcd, pROC, gmodels, mice, MatchIt, rms, forestploter, grid.

## View Outputs
Table 1: Line 192
Table 2: Line 752
Table 3: Line 862
Table S1: Line 1346

Figure 2: Line 714
Figure 3: Line 1103

Figure S1: Line 780
Figure S2: Line 816
Figure S3: Line 1256
Figure S4: Line 1647
Figure S5: Line 1922

## Note on Reproducibility
To ensure successful execution, it is recommended to run the code sequentially from top to bottom.  
Several variables used in later analyses depend on objects created in earlier steps; therefore, skipping sections may lead to errors.

## Contact
If you have questions regarding the code or analysis pipeline, please contact 879875806@qq.com.