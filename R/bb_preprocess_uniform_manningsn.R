#' @title Saves Mannings n raster to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a raster of Mannings n values, then saves it.
#'
#' @param manningsn a raster (or file path to one) of Mannings n roughness values
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param return_raster whether to return the processed raster as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return \item{manningsnraster}{returns manningsnraster if return_raster==\code{TRUE}, else returns just \code{TRUE}}
#
#' @examples
#' dem_file <- system.file("extdata", "DEM_NB_10m_clipped.tif", package="blackbird")
#' nraster <- bb_preprocess_uniform_manningsn(dem, manningsn_value=0.065, return_raster=TRUE)
#'
#' @importFrom raster raster writeRaster crs overlay
#' @export bb_preprocess_uniform_manningsn
bb_preprocess_uniform_manningsn <- function(dem=NULL, manningsn_value=0.05, workingfolder=NULL, return_raster=FALSE, overwrite=TRUE) {

  if (is.null(dem)) {stop("dem is required")}
  if (is.null(workingfolder) & !return_raster) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}

  ## check stream nodes
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


  ## create manningsn raster from dem
  mannningsn <- overlay(dem,fun=function(r1) {ifelse(!is.na(r1),manningsn_value,NA)})

  # xxx to do
  # version of this function where a buffer around rivershp gets one mannings n value, and rest gets the overbank value


  # write to workingfolder if provided
  if (!is.null(workingfolder)) {

    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }

    manningsn_file <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = FALSE)
    writeRaster(manningsn, manningsn_file, overwrite=overwrite)
  }

  if (return_raster) {
    manningsn <- raster(manningsn_file)
    return(manningsn)
  } else {
    return(TRUE)
  }
}
