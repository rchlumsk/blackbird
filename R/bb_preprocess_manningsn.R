#' @title Saves Mannings n raster to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a raster of Mannings n values, then saves it.
#'
#' @param manningsn a raster (or file path to one) of Mannings n roughness values
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param method raster resampling method passed to \code{raster::resample}
#' @param maxvalue_check boolean to check and throw error if roughness exceeds a max value of 1.0 (can be overridden with FALSE)
#' @param return_raster whether to return the processed raster as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @details
#' The \code{method} parameter determines how the raster is resampled, if needed, to match the exact extents and resolution
#' of the dem raster (passed to \code{\link{terra::resample}}. The default option is "near" (nearest neighbour approach),
#' which is best if using a model configuration
#' where the number of unique roughness values is important in the hydraulic calculations (such as if
#' \code{bbopt$catchment_conveyance_method == "roughzone_conveyance"}). Otherwise, one can use 'bilinear'.
#'
#' @return \item{manningsnraster}{returns manningsnraster if return_raster==\code{TRUE}, else returns just \code{TRUE}}
#
#' @examples
#' nraster <- system.file("extdata", "nraster_0.06_NB.tif", package="blackbird")
#' bb_preprocess_manningsn(nraster)
#'
#' @importFrom terra rast writeRaster crs resample as.matrix
#' @importFrom sf st_crs
#' @export bb_preprocess_manningsn
bb_preprocess_manningsn <- function(manningsn=NULL, workingfolder=NULL, method="near", maxvalue_check=TRUE, return_raster=FALSE, overwrite=TRUE) {

  if (is.null(manningsn)) {stop("manningsn is required")}
  if (is.null(workingfolder) & !return_raster) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}

  if (is.character(manningsn)) {
    if (!file.exists(manningsn)) {
      stop("manningsn file path does not exist")
    } else{
      manningsn <- terra::rast(manningsn)
    }
  } else if ("SpatRaster" %notin% class(manningsn)) {
      stop("manningsn must be a SpatRaster object (from the terra package) or a file path to one")
  }

  # check projection
  if (is.null(crs(manningsn)) | is.na(crs(manningsn)) ) {
    stop(sprintf("invalid crs for manningsn, cannot be processed:\n%s", crs(manningsn)))
  }

  # check that dem processed first
  if (!file.exists(bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE))) {
    stop("dem must be pre-processed first")
  }

  # check values in manningsn
  rng <- range(manningsn[!is.na(manningsn)])
  if (any(rng<=0)) {
    stop("Values in manningsn must be >=0, please revise values")
  }
  if (maxvalue_check & max(rng)>1) {
    stop(sprintf("Max roughness in manningsn layer provided is %.2f, which seems excessive!\nPlease revise if needed, or override maxvalue with maxval_check=FALSE.",rng[2]))
  }

  ## align extents to other rasters
  dem <- bb_get_demraster(workingfolder = workingfolder, returnobject = TRUE)

  # match projection to dem
  if (!compareCRS(manningsn,dem)) {
    # reproject manningsn to dem crs
    manningsn <- terra::project(manningsn,dem)
  }

  resampled <- FALSE

  # check if square
  if (res(manningsn)[1] != res(manningsn)[2]) {
    message("manningsn raster is not square; resampling to resolution of supplied DEM")
    manningsn <- terra::resample(manningsn,dem,method=method)
    resampled <- TRUE
  }

  # check if same resolution as DEM
  if (res(dem)[1] != res(manningsn)[1] & res(dem)[2] != res(manningsn)[2]) {
    message("resampling manningsn raster to the same resolution of supplied DEM")
    manningsn <- terra::resample(manningsn,dem,method=method)
    resampled <- TRUE
  }

  # check extents, adjust manningsn extents to that of dem
  if (!resampled & terra::ext(dem) != terra::ext(manningsn)) {
    # manningsn <- manningsn %>%
    #   raster::extend(x=., y=extent(dem), value=NA)  %>%
    #   crop(x=., y=extent(dem))
    manningsn <- bb_match_raster_extents(manningsn, dem)
  }

  # check that data exists everywhere that dem exists
  dem_check <- terra::as.matrix(dem)
  manningsn_check <- terra::as.matrix(manningsn)
  manningsn_check <- manningsn_check[which(!is.na(dem_check))]
  if (any(which(is.na(manningsn_check)))) {
    stop("Must ensure that landcover raster completely covers all valid DEM regions.")
  }

  # write to workingfolder if provided
  if (!is.null(workingfolder)) {
    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }
    manningsn_file <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = FALSE)
    writeRaster(manningsn, manningsn_file, overwrite=overwrite)
  }

  if (return_raster) {
    manningsn <- rast(manningsn_file)
    return(manningsn)
  } else {
    return(TRUE)
  }
}
