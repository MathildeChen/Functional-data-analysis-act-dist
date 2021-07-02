# Cross-sectional and prospective association of socio-demographic, behavioural, and health-related factors with objectively-assessed physical activity and sedentary time in older adults

Code for functional and multivariate linear regression to analyse the association between socio-demographic, behavioural, and health-related factors with a functional (activity intensity distribution function) or a scalar (time spent in a specific activity intensity range) outcome, respectively.

Data were analysed using R 3.6.1 (http://www.r-project.org), analyses required downloading of the following packages:
- GGIR for accelerometer data processing (version 2.0-0, https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html)
- ks for kernel smoothing (version 1.11.7, https://cran.r-project.org/web/packages/ks/ks.pdf)
- REFUND for function-on-scalar regressions (version 0.1-21, https://cran.r-project.org/web/packages/refund/refund.pdf)
- pracma for trapezoidal integration of functional coefficients (version 2.2.9, https://cran.r-project.org/web/packages/pracma/pracma.pdf)

## Step 1 - Data  
Data from Whitehall II accelerometer-substudy. 

Data should include:
- the functional outcome for functional data analysis: individual activity intensity distribution function (a matrix with N lines and P columns, N corresponding to the number of subjects, P the number of points of the functional outcome)
- the scalar outcome for multivariate linear: individual daily duration of different activity behaviors (sedentary behaviour, ligh-intensity physical activity, moderate-to-vigorous physical activity)
- the scalar exposures: mean daily waking time, socio-demographics factors, behavioural factors, health related factors, the interaction terms, if necessary 

One dataset for each wave of data collection, excluding participants with missing covariates. One dataset restricted to participants with all measurements at each wave. 

## Step 2 - Load functions
Specific functions to fit the models, extract coefficients and p values, and to plot the associations (heatmap for function-on-scalar regressions, table of coefficients for multivariate linear regressions)

## Step 3 - Model fitting
Function-on-scalar regressions (conducted on the full study population, then stratified by sex)
Multivariate linear regressions (conducted on the full study population, then stratified by sex)

## Step 4 - Tables & Figures
Ploting the association between exposures and functional outcome using heatmaps.

[![DOI](https://zenodo.org/badge/382270985.svg)](https://zenodo.org/badge/latestdoi/382270985)
