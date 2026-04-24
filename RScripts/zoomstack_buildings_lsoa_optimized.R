#' Optimized zoomstack buildings processing using duckspatial
#'
#' This script provides an optimized version of zoomstack_buildings_lsoa
#' broken into four separate functions for verylow, low, medium, and high detail levels.
#' Uses duckspatial for faster spatial operations.
#'
#' @keywords internal

# Install duckspatial if needed:
# remotes::install_github("duckdb/duckdb-r")
# install.packages("duckspatial")

library(sf)
library(dplyr)
library(purrr)








