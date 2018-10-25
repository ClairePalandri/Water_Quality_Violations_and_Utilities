# Water_Quality_Violations_and_Utilities
Here we analyze trends in drinking water quality violations.
The Exploratory_Analysis_of_the_Data.R file takes in complete dataset and filters the values and keeps in the ones used in the analysis. 
The Scenario_Complete_Pooling.R file runs the Stan program on the above output. 
The Stan Program is a completely pooled model. 
Note:- The priors are common on all of them so make sure that the order of magnitude of the variables is similar if they are not standardized prior to running. 
