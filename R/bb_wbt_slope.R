#' Creates slope raster file
#'
#' Creates the slope raster (in radians) from an input DEM file
#'
#' @param ff_dem File name of dem with sinks removed.
#' @param outputfile File name for slope raster file to be created.
#' @param return_raster Should a raster object be returned?
#' @param slope_units units of the calculated slope (one of 'radians' (default), 'degrees', or 'percent')
#'
#' @return If \code{return_raster = TRUE} (the default), the slope raster
#'  will be returned as a raster object, in addition to being written to
#' \option{outputfile}. If \code{return_raster = FALSE}, the output file will still be created
#' but a \code{TRUE} value is returned.
#'
#' @examples
#'
#'
#' @importFrom raster raster
#' @importFrom whitebox wbt_slope
#' @export bb_wbt_slope
bb_wbt_slope <- function(ff_dem, outputfile, return_raster = TRUE, slope_units="radians") {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(ff_dem)) {
    stop("Error: input dem file does not exist")
  }
  message("bb_wbt: Creating slope raster")

  whitebox::wbt_slope(ff_dem, outputfile, units=slope_units)
  if (return_raster) {
    return(raster(outputfile))
  } else {
    return(TRUE)
  }
}
