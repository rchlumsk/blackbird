#' Creates aspect raster file
#'
#' Creates the aspect raster from an input DEM file
#'
#' @param ff_dem File name of dem with sinks removed.
#' @param outputfile File name for aspect raster file to be created.
#' @param return_raster Should a raster object be returned?
#'
#' @return If \code{return_raster = TRUE} (the default), the aspect raster
#'  will be returned as a raster object, in addition to being written to
#' \option{outputfile}. If \code{return_raster = FALSE}, the output file will still be created
#' but a \code{TRUE} value is returned.
#'
#' @examples
#'
#'
#' @importFrom raster raster
#' @importFrom whitebox wbt_aspect
#' @export bb_wbt_aspect
bb_wbt_aspect <- function(ff_dem, outputfile, return_raster = TRUE) {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(ff_dem)) {
    stop("Error: input dem file does not exist")
  }
  message("bb_wbt: Creating aspect raster")

  whitebox::wbt_aspect(ff_dem, outputfile)
  if (return_raster) {
    return(raster(outputfile))
  } else {
    return(TRUE)
  }
}
