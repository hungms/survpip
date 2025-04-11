# survpip NEWS

## v0.1.0

### New Features

* Initial release of the survpip package
* Added `find_coxph_cutoff()` function to identify optimal cutpoints for continuous variables in survival analysis
* Added `plot_survival()` function to create publication-ready Kaplan-Meier plots with risk tables
* Added `make_multivariate_table()` function to generate formatted tables from Cox regression analyses
* Added `plot_forest()` function with customizable column selection and labeling
* Added `plot_forest_gene()` function for specialized gene-based forest plots

### Enhancements in v0.1.0

* Added column customization to `plot_forest()`:
  * New `columns` parameter to select which columns to display
  * New `column_labels` parameter to customize column headers
  * Dynamic handling of column numbers in forest plot lines
  * Improved error handling for line formatting

## Future Development

* Add support for competing risks analysis
* Enhance visualization themes and options
* Add functions for time-dependent covariates
* Implement bootstrapping methods for model validation 