# Racial Bias in Clinical and Population Health Algorithms

Data and replication materials for Coots et al. (2025) "Racial Bias in Clinical and Population Health Algorithms: A Critical Review of Current Debates".

To reproduce our analyses on diabetes risk models, run the `code/diabetes/figure_s_and_analysis.R` script. R data objects for the analyses are located in the data/ repository, so it is not necessary to run any data preprocessing scripts to generate the figures.

The data used for our lung cancer analysis are not publicly available, so we are not able to provide these data for replication. However, all code used to produce our results for lung cancer is located in `code/lung_cancer`.

All figures that appear in our paper may be found in `figures`.

___

For further detail, please see the structure of this repository outlined below:
- `code/`: Contains all code used for our analyses.
  - `diabetes/`: Contains all code used for our diabetes analysis.
    - `make_data_diabetes.R`: Data prepocessing script that produces an R data object for generating risk predictions. Pulls NHANES tables directly from the CDC website.
    - `diabetes_risk_models.R`: Script that generates diabetes risk predictions.
    - `figure_2_and_analysis.R`: Script that generates all subplots of Figure 2 and accompanying statistics.
  - `lung_cancer/`: Contains all code used for our lung cancer analysis.
    - `make_data_lc.R`: Data prepocessing script that produces an R data object using data from the National Lung Screening Trial (NLST) for generating risk predictions.
    - `lc_risk_predictions.R`: Script that generates lung cancer risk predictions.
    - `make_census_weights.R`: Script that generates survey weights for the NLST data using census data.
    - `figure_1_and_analysis.R`: Script that generates both subplots of Figure 1 and accompanying statistics.
  - `data/`: Contains raw and processed data used in our analyses.
  - `figures/`: Contains PDF files of all figures in our paper.

