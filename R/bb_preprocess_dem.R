#' @title Saves DEM to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a Digital Elevation Model (DEM), then saves it.
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
#' dem <- bb_preprocess_dem(dem_file, return_raster=TRUE)
#'
#'
#' @importFrom terra rast writeRaster crs res
#' @export bb_preprocess_dem
bb_preprocess_dem <- function(dem=NULL, workingfolder=NULL,
                              removesinks_method="breach_leastcost",removesinks_dist=NULL,
                              calc_dinf=FALSE,
                              fixnegative=TRUE,
                              return_raster=FALSE, overwrite=TRUE) {

  if (is.null(dem)) {stop("dem is required")}
  if (is.null(workingfolder)) {"workingfolder is required"}

  ## check dem validity as raster
  if (is.character(dem)) {

    if (!file.exists(dem)) {
      stop("dem file path does not exist")
    } else{
      dem <- rast(dem)
    }
  } else if ("SpatRaster" %notin% class(dem)) {
      stop("dem must be a SpatRaster object (from the terra package) or a file path to one")
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
  if (!dir.exists(workingfolder)) {
    dir.create(workingfolder)
  }

  if (is.null(removesinks_dist)) {
    # default, looks for sinks up to 5x dem resolution
    removesinks_dist <- res(dem)[1]*5
  }

  if (min(dem[!is.na(dem)]) < 0) {
    if (fixnegative) {
      warning(sprintf("DEM has a minimum value of %.2f, adjusting DEM vertically to have min(dem)=0",
                      min(dem[!is.na(dem)])))
      dem <- dem + abs(min(dem[!is.na(dem)]))
    } else {
      stop(sprintf("DEM has a minimum value of %.2f < 0, cannot support negative values in DEM.",
                   min(dem[!is.na(dem)])))
    }
  }

  dem_file <- bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(dem, dem_file, overwrite=overwrite)

  # run the conditioning here
  demcond_file <- bb_get_demcondraster(workingfolder = workingfolder, returnobject = FALSE)

  # remove sinks
  if (removesinks_method == "skip") {
    file.copy(dem_file, demcond_file)
  } else {
    bb_wbt_removesinks(dem_file, demcond_file,
                       method = removesinks_method, dist=removesinks_dist)
  }
  message(sprintf("DEM conditioned with sink removal method %s",removesinks_method))

  # get flow accumulations and direction
  flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject = FALSE)
  bb_wbt_flow_accumulation(demcond_file, flow_acc_file, return_raster = FALSE)
  flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  bb_wbt_flow_direction(demcond_file, flow_dir_file, return_raster = FALSE)

  if (calc_dinf) {
    flow_dir_file <- bb_get_flowdirdinfraster(workingfolder,returnobject=FALSE)
    bb_wbt_flow_direction(demcond_file, flow_dir_file, return_raster = FALSE, method="dinf")
  }

  if (return_raster) {
    dem <- rast(dem_file)
    return(dem)
  } else {
    return(TRUE)
  }
}
