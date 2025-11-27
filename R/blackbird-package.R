#'
#' @title blackbird
#' @description Hydraulic modelling and pre-processing utilities
#' @docType _PACKAGE
#' @name blackbird
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @useDynLib blackbird
#' @details This package has functions to calculate a basic backwater profile for open channels
#' using the standard step method, floodplain mapping using the Height Above Nearest Drainage (HAND)
#' method, geospatial pre-processing tools, and approaches for combining the standard step method with
#' the HAND approach.
#'
#' Functions in this package are generally sorted into:
#' 1. pre-processing tools for geospatial operations (catchment delineation, HAND raster, etc.)
#' 2. HAND-related 0D flood calculations
#' 3. 1D hydraulic calculations using a standard step approach with mixed stream node representation
#' (cross-sections, structures, and HAND catchments).
#' 4. Additional utility functions and class definitions to support the above.
#'
#' @seealso \href{https://github.com/rchlumsk}{rchlumsk Github page} for more R packages.
NULL
