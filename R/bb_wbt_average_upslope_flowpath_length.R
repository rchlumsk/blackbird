#' Average Upslope Flowpath Length
#'
#' @param inputraster File path for a conditioned DEM to use in average flowpath
#' @param outputfile Output file to write the resulting clipped raster to
#' @param compress_rasters whether to compress the resulting raster, passed to \code{wbt_clip_raster_to_polygon}
#'
#' @return Returns average upslope flow path (in metres)
#'
#' @examples
#' print("TO DO")
#'
#'
#' @seealso \code{\link{whitebox::wbt_average_upslope_flowpath_length}} and
#' \code{\link{whitebox::wbt_max_upslope_flowpath_length}}
#'
#' @importFrom raster raster writeRaster crs
#' @importFrom whitebox wbt_average_upslope_flowpath_length
#'
#' @export bb_wbt_average_upslope_flowpath_length
bb_wbt_average_upslope_flowpath_length <- function(inputraster=NULL,
                                            compress_rasters=FALSE,
                                            return_raster=FALSE) {

  stopifnot(!is.null(inputraster))

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  if (is.character(inputraster)) {
    if (!file.exists(inputraster)) {
      stop("Error: file containing snapped pour points does not exist")
    }
  } else {
    # raster input directly, write to temporary file
    tf <- tempfile(fileext=".tif")
    writeRaster(inputraster, tf)
    inputraster <- tf
  }

  tempoutputfile <- tempfile(fileext = ".tif")

  # message("bb_wbt: Clipping raster to polygon")
  crs_rr <- raster::crs(raster(inputraster))

  # not sure which to use or how to interpret
  whitebox::wbt_max_upslope_flowpath_length(dem=inputraster,
                             output=tempoutputfile,
                             compress_rasters = compress_rasters,
                             verbose_mode = FALSE)

  # whitebox::wbt_average_upslope_flowpath_length(dem=inputraster,
  #                                                   output=tempoutputfile,
  #                                                   compress_rasters = compress_rasters,
  #                                                   verbose_mode = FALSE)

  rr <- raster(tempoutputfile)
  crs(rr) <- crs_rr
  # raster::writeRaster(x=rr, filename=outputfile,overwrite=TRUE)

  # get average flow path (not sure how to interpret numbers)
  average_flowpath <- max(rr[!is.na(rr)])*1e6 # in metres

  return(average_flowpath)
}
