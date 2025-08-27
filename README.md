# Uncertainty-directed and random exploration in Parkinson’s disease

### Cite as:
Meder, B., Sterf, M. Wu, C.M, & Guggenmos, M. (2025). Uncertainty-directed and random exploration in Parkinson’s disease. _ 

## Preprint:
https://

## Datasets:    
- `data/behavioralData.csv`: behavioral data of all 281 participants   
- `data/modelFit.csv`: parameter estimates of all models for all participants   
- `data/roughKernel.json`: all 40 environments, from which a new environment was chosen in each round without replacement. Also used for computational analyses.

## Scripts:   
- `gridsearch_parkinson_behavioral_analyses.qmd`: Quarto document with R code used for the statistical analyses and plots of the behavioral data
- `gridsearch_parkinson_computational_analyses.qmd`: Quarto document with R code used for the computational analyses
- `dataProcessing.R`: import and pre-process behavioral data and parameter estimates (added for reference, since we already include the generated outputs instead of the inputs)
- `statisticalTests.R` contains wrapper functions for all statistical tests used for the analyses
- `simulateModels.R`: simulate the GP-UCB model with different parameter combinations and plot expected rewards    
   

