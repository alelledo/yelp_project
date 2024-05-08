<p align="center">
  <img src="https://static.vecteezy.com/system/resources/previews/027/127/559/non_2x/yelp-logo-yelp-icon-transparent-free-png.png" width = 200 alt="Logo">
</p>
<p align="center"><em>by Vecteezy</em></p>
# Customer Insights on Restaurant Choice Analysis

## Introduction
This project aims to uncover valuable customer insights regarding their preferences for restaurants. Leveraging a dataset from Yelp containing over 4000 restaurant observations, we delve into various metrics to understand what influences customer choices.

## Dependencies
- R (version 3.6.3)
- R packages: dplyr, ggplot2, tidyr, caret, MASS

## Installation
1. Clone the repository: `git clone <repository_url>`
2. Navigate to the project directory: `cd restaurant-insights`
3. Install R packages: `install.packages(c("dplyr", "ggplot2", "tidyr", "caret", "MASS"))`

   
## Initial Data Cleanup
- **Artifact Removal**: Removed a redundant identifier column ('X') from the dataset.
- **Duplicate Removal**: Dropped approximately 1500 observations identified as potential duplicates.
- **Standardization**: Standardized non-standard 'NA' coding to the correct format.
- **Variable Class Adjustment**: Converted variables to appropriate classes (e.g., from character to factor).
- **Geolocation Variable Handling**: Dropped specific geolocation variables due to their interference with modeling.
- **Missing Value Imputation**: Imputed missing values based on certain variables with no missing values.
- **Variable Reduction**: Combined parking-related variables and reduced levels of certain variables.
- **Outlier Handling**: Identified and retained valid outliers in 'review_count'.

## Modeling Customer Characteristics for Restaurants
- **Model Selection**: Utilized a binary logit model to predict the likelihood of a restaurant being frequently visited.
- **Variable Selection**: Selected relevant independent variables based on literature review and correlation with the dependent variable.
- **Model Evaluation**: Conducted various tests to evaluate model performance, including AIC, BIC, hit rates, and lift curve analysis.

## Interpretation of Results
- **Estimation**: Interpreted coefficients and odds ratios to understand the impact of independent variables on the likelihood of a restaurant being frequently visited.
- **Marginal Effects**: Analyzed marginal effects to observe changes in probability with respect to changes in independent variables.
- **Star Rating Analysis**: Investigated factors driving restaurant customer ratings using ordered logit and multinomial logit models.
- **Nested Model**: Explored a nested multinomial model to partially relax the independence of irrelevant alternatives assumption.

## Conclusion
- **Key Findings**: Highlighted both expected and unexpected results, such as the mixed effects of price on star ratings.
- **Limitations**: Acknowledged limitations including the correlational nature of the results and the potential for regional differences.

## Next Steps
- **Causal Analysis**: Consider conducting causal analysis to establish causal relationships.
- **Regional Analysis**: Explore potential regional differences by analyzing data at the state level.
- **Time Analysis**: Incorporate time-related variables for a more comprehensive analysis.
