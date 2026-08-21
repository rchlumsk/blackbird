#' @title Process slope raster to blackbird folder
#'
#' @description
#' Calculate the slope raster from fine resolution DEM
#'
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param smoothraster boolean whether to smooth the slope raster with the \code{terra::focal} function
#' @parm ww window weight to use in the \code{terra::focal} function; higher values will increase smoothing (default: 5)
#' @param return_raster whether to return the processed dem as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the dem if return_raster is TRUE}
#
#' @details
#'
#'
#' @examples
#' # IOU examples
#' @importFrom terra focal rast writeRaster
#' @importFrom whitebox wbt_init
#' @export bb_preprocess_slope
bb_preprocess_slope <- function(workingfolder=NULL, smoothraster=FALSE, ww=5, return_raster=FALSE, overwrite=TRUE) {

  # attempting to find dem from folder path
  # assume that DEM has been pre-processed and skip checks here
  dem <- bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE)

  if (!file.exists(dem)) {
    stop("dem file path does not exist; must first preprocess dem with bb_preprocess_dem function")
  }

  if (is.null(workingfolder) & !return_raster) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}

  bb_wbt_check_whitebox()
  exe_location <- whitebox::wbt_init()
  # message("bb_wbt: Creating slope raster")
  slope_file <- bb_get_sloperaster(workingfolder=workingfolder,returnobject=FALSE)
  bb_wbt_slope(ff_dem = dem, outputfile = slope_file)

  # smooth raster if specified
  if (smoothraster) {
    slope <- rast(slope_file)
    smoothed <- focal(slope, w=ww, fun="mean", na.policy="omit")
    writeRaster(smoothed, slope_file, overwrite=TRUE)
    rm(smoothed)
    rm(slope)
  }

  if (return_raster) {
    slope <- rast(slope_file)
    return(slope)
  } else {
    return(TRUE)
  }
}
