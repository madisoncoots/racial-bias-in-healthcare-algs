# Racial Bias in Clinical and Population Health Algorithms

Data and replication materials for Coots et al. (2025) "Racial Bias in Clinical and Population Health Algorithms: A Critical Review of Current Debates".

To reproduce our analyses on diabetes risk models, run the `code/diabetes/figure_s_and_analysis.R` script. R data objects for the analyses are located in the data/ repository, so it is not necessary to run any data preprocessing scripts to generate the figures.

The data used for our lung cancer analysis are not publicly available, so we are not able to provide these data for replication. However, all code used to produce our results for lung cancer is located in `code/lung_cancer`.

All figures that appear in our paper may be found in `figures`.

___

For further detail, please see the structure of this repository outlined below:

```bash
code/                   
├── diabetes/           # Contains all code used for our diabetes analysis
│   ├── make_data_diabetes.R        # Data preprocessing script for NHANES tables
│   ├── diabetes_risk_models.R      # Generates diabetes risk predictions
│   └── figure_2_and_analysis.R     # Generates subplots of Figure 2 and accompanying statistics
│
├── lung_cancer/        # Contains all code used for our lung cancer analysis
│   ├── make_data_lc.R              # Preprocessing script for NLST data
│   ├── lc_risk_predictions.R       # Generates lung cancer risk predictions
│   ├── make_census_weights.R       # Generates survey weights for NLST data
│   └── figure_1_and_analysis.R     # Generates subplots of Figure 1 and accompanying statistics
│
├── colors.R            # Script that specifies group colors for figures
│
data/                   # Contains raw and processed data used in our analyses
figures/                # Contains PDF files of all figures
```



