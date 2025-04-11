.onLoad <- function(libname, pkgname){
  ### load default packages
  packages <- c(
    "plotr",
    "rlang",
    "ggplot2",
    "dplyr",
    "magrittr",
    "stringr",
    "cowplot",
    "viridis",
    "ggrepel",
    "patchwork",
    "survival",
    "npsurv",
    "survminer",
    "tidytidbits",
    "survivalAnalysis",
    "finalfit",
    "gridExtra",
    "gt",
    "forestplot")
  invisible(lapply(packages, library, character.only = TRUE))

  ### start up settings
  options(dplyr.summarise.inform = FALSE)
  set.seed(123)
  errorMessage <- NULL}
