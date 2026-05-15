# Suicide Rates in Chile (2020): Economic and Meteorological Predictors
 
A statistical consulting project, implemented in R, investigating the impact of economic indicators and meteorological factors on completed suicides in Chile in 2020, across four age groups.
 
## Research questions
 
1. Do economic factors (unemployment rate, inflation rate) affect the number of completed suicides?
2. Do meteorological conditions (temperature, atmospheric pressure, precipitation, humidity) affect the number of completed suicides?
3. Do these associations differ across age groups, and for meteorological factors, across geographical regions?
   
## Dataset
 
- **Primary data**: 1,602 completed suicide cases from Chile's National Statistics Institute ([INE](https://www.ine.gob.cl/))
- **Meteorological data**: Daily measurements from the Dirección Meteorológica de Chile, matched to each suicide's municipality
- **Economic data**: Monthly unemployment and inflation rates from the Central Bank of Chile
  
## Methods
 
- **Missing data**: Multiple imputation using the random forest method (`mice` package in R), applied mainly to the precipitation variable (~23% missing)
- **Models**: Poisson regression (GLM and GLMM) with and without age group interaction terms
- **Meteorological model**: Generalized linear mixed model with region as a random effect, meteorological variables standardized using z-scores within each region
- **Economic model**: GLM with monthly suicide counts; interaction terms between age group and economic indicators
- **Diagnostics**: Pearson and deviance chi-square goodness-of-fit tests

## Age groups
 
- Adolescents(≤ 25): 305 
- Young adults(26–40): 455 
- Middle-aged adults(41–60): 512 
- Elderly adults(> 60): 332 
 
## Key findings
 
- **Meteorological factors**: No significant associations found between temperature, atmospheric pressure, precipitation, or humidity and daily suicide counts, across all age groups
- **Economic factors**: Both unemployment and inflation rates had significant effects on monthly suicide counts
  - Higher unemployment: fewer suicides overall, but the effect varied by age group:
    - Adolescents: higher unemployment associated with *fewer* suicides
    - Young and middle-aged adults: higher unemployment associated with *more* suicides
  - Higher inflation: more suicides overall (significant only in the model without age group interaction)
- **Model fit**: The economic model including age group interactions showed good fit (Pearson p = 0.39), the model without age group showed significant lack of fit and overdispersion
  
## Repo structure
 
- `README.md`
- `analysis.R`: full R script with data preprocessing, imputation, modelling, and diagnostics
- `Statistical_Consulting_Report.pdf`: full written report
- `Dataset_CS_2020_copia.xlsx`: raw dataset (source: INE Chile)
- `HypothesesByClient.docx`: literature-based hypotheses provided by the client
## Requirements
 
R packages used: `tidyverse`, `mice`, `lme4`, `ggplot2`, `naniar`, `lubridate`
