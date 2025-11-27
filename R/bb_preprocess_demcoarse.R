#' @title Saves coarse DEM to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a Digital Elevation Model (DEM), then saves it.
#'
#' @note the coarse dem is assumed to be the enhanced flow direction dem, provided for delineating catchments.
#'
#' @param dem A Digital Elevation Model (DEM) as raster or file path
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param return_raster whether to return the processed dem as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the dem if return_raster is TRUE}
#
#' @details
#'
#'
#' @examples
#'
#' dem_file <- system.file("extdata", "DEM_NB_10m_clipped.tif", package="blackbird")
#'
#' dem_coarse <- bb_preprocess_demcoarse(dem_file, return_raster=TRUE)
#'
#'
#' @importFrom raster raster writeRaster crs
#' @export bb_preprocess_demcoarse
bb_preprocess_demcoarse <- function(dem=NULL, workingfolder=NULL, return_raster=FALSE, overwrite=TRUE) {

  if (is.null(dem)) {stop("dem is required")}
  if (is.null(workingfolder) & !return_raster) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}

  ## check dem validity as raster
  if (is.character(dem)) {

    if (!file.exists(dem)) {
      stop("dem file path does not exist")
    } else{
      dem <- raster(dem)
    }
  } else if ("SpatRaster" %notin% class(dem)) {
      stop("dem must be a SpatRaster object or a file path to one")
  }

  # check projection
  if (is.null(crs(dem)) | is.na(crs(dem)) ) {
    stop(sprintf("invalid crs for dem, cannot be processed:\n%s", crs(dem)))
  }

  # check if square
  if (res(dem)[1] != res(dem)[2]) {
    stop("dem raster is not square; please resample with raster::resample or just terra::aggregate to obtain a square resolution dem")
  }

  # write to workingfolder if provided
  if (!is.null(workingfolder)) {

    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }

    dem_file <- bb_get_demcoarseraster(workingfolder = workingfolder, returnobject = FALSE)
    writeRaster(dem, dem_file, overwrite=overwrite)
  }

  if (return_raster) {
    dem <- raster(dem_file)
    return(dem)
  } else {
    return(TRUE)
  }
}
