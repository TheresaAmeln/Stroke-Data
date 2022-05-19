# Stroke-Data
R-Code for the Bachelor Thesis about Stroke Data in Austria

In order to work with my R-files, you need the stroke data from Gesundheit Österreich GmbH. 
The R-Code is not optimized, I'm sure, you will find more efficient solutions. If so, don't be shy to contact me, I'm eager to learn :)

In the file 'filtering_data_exploration.R' the data is filtered for NA's and only the used variables are kept. Afterwards, the data is explored by creating plots for the different variables.

In the file 'modeling.R' the data is modeled with the function 'mHMM()' from the R-package 'mHMMbayes'.

In the file 'model_evaluation.R' the mHMM is evaluated by creating an AIC plot, trace plots, density plots, and transition plots.
