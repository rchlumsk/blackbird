#' Breaches DEM at roads (BurnStreamsAtRoads tool)
#'
#' Processes the DEM to burn stream at roads, leaving the other portions of the DEM unaffected.
#'
#' @param ff_dem File name of dem with sinks removed
#' @param ff_streams File name of the streams vector file
#' @param ff_roads File name of the roads vector file
#' @param road_width  maximum road embankment width (map units)
#' @param outputfile File name for output dem file
#' @param return_raster Should a raster object be returned?
#'
#' @return If \code{return_raster = TRUE} (the default), the processed raster
#'  will be returned as a raster object, in addition to being written to
#' \option{outputfile}. If \code{return_raster = FALSE}, the output file will still be created
#' but a \code{TRUE} value is returned.
#'
#' @examples
#'
#'
#' @importFrom raster raster
#' @importFrom whitebox wbt_burn_streams_at_roads wbt_init
#' @export bb_wbt_breach_roads
bb_wbt_breach_roads <- function(ff_dem, ff_streams, ff_roads, road_width=20, outputfile, return_raster = TRUE) {
  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (!file.exists(ff_dem)) {
    stop("Error: input dem file does not exist")
  }
  if (!file.exists(ff_streams)) {
    stop("Error: input dem file does not exist")
  }
  if (!file.exists(ff_roads)) {
    stop("Error: input dem file does not exist")
  }
  message("bb_wbt: Processing breach roads raster")

  whitebox::wbt_burn_streams_at_roads(ff_dem, ff_streams, ff_roads, width=road_width, output=outputfile)
  if (return_raster) {
    return(raster(outputfile))
  } else {
    return(TRUE)
  }
}
