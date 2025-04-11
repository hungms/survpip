# survpip: Survival Analysis and Visualization Pipelines

<!-- badges: start -->
<!-- badges: end -->

## Overview

`survpip` is an R package that provides a seamless workflow for survival analysis and visualization. It streamlines the process of creating publication-ready survival plots, forest plots, and multivariate Cox proportional hazards models in clinical and biological research.

## Installation

You can install the development version of survpip from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("hungms/r-packages/survpip")
```

## Features

- **Kaplan-Meier Survival Analysis**: Create publication-ready survival plots with integrated risk tables
- **Cox Proportional Hazards Models**: Easy-to-use functions for uni- and multivariate Cox regression
- **Forest Plots**: Versatile forest plot generation for hazard ratios with customizable columns and labels
- **Optimal Cutoff Selection**: Automatically identify optimal cutpoints for continuous variables in survival analysis

## Usage Examples

### Kaplan-Meier Survival Analysis

```r
library(survpip)

# Find optimal cutoff for a continuous variable and create groups
data <- find_coxph_cutoff(df = lung, 
                         column = "age", 
                         time = "time", 
                         event = "status")

# Plot survival curves
plot_survival(data, 
              column = "age_coxph", 
              time = "time", 
              event = "status",
              title = "Survival by Age Group")
```

### Multivariate Analysis and Forest Plots

```r
# Create multivariate table
mv_table <- make_multivariate_table(df = lung,
                                   variables = c("sex", "ph.ecog"),
                                   covariates = c("age"),
                                   time = "time",
                                   event = "status")

# Create standard forest plot
plot_forest(mv_table, 
           title = "Hazard Ratios for Lung Cancer Survival")

# Customize columns and labels
plot_forest(mv_table,
           title = "Hazard Ratios",
           columns = c("Category", "n", "HR (multivariate)"),
           column_labels = c("Category" = "Variables",
                            "n" = "Sample Size",
                            "HR (multivariate)" = "Hazard Ratio (95% CI)"))
```

## Citation

If you use `survpip` in your research, please cite:

```
Hung M.S. (2023). survpip: Survival Analysis and Visualization Pipelines. R package.
```

## License

This package is licensed under the MIT License.