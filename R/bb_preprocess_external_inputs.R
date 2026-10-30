#' @title Saves externally developed files for use in Blackbird
#'
#' @description
#' Performs numerous checks on DEM, conditioned DEM, flow acc and flow direction layer, river
#' centrelines, and saves the files.
#'
#' @param dem A Digital Elevation Model (DEM) as raster or file path
#' @param demcond conditioned DEM raster or file path
#' @param flowacc flow accumulation DEM raster or file path
#' @param flowdir flow direction raster or file path
#' @param rivershp river centrelines shapefile sf or file path
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly}
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra rast writeRaster crs res
#' @export bb_preprocess_dem
bb_preprocess_external_inputs <- function(dem=NULL,
                                          demcond=NULL,
                                          flowacc=NULL,
                                          flowdir=NULL,
                                          rivershp=NULL,
                                          workingfolder=NULL,
                                          overwrite=TRUE) {

  if (is.null(dem)) {stop("dem is required")}
  if (is.null(demcond)) {stop("demcond is required")}
  if (is.null(flowacc)) {stop("flowacc is required")}
  if (is.null(flowdir)) {stop("flowdir is required")}
  if (is.null(rivershp)) {stop("rivershp is required")}

  if (is.null(workingfolder)) {"workingfolder is required"}

  ## check input validity
  dem <- bb_check_raster(dem)
  demcond <- bb_check_raster(demcond)
  flowacc <- bb_check_raster(flowacc)
  flowdir <- bb_check_raster(flowdir)
  rivershp <- bb_check_shapefile(rivershp)

  ## check attributes of shapefile
  if (any(c("reachID", "downID", "geometry")) %notin% colnames(rivershp)) {
    stop("rivershp must have the attributes of reachID, downID, and geometry")
  }

  # check projections
  bb_check_projections(list(dem,demcond,flowacc,flowdir))
  if (is.null(crs(rivershp)) | is.na(crs(rivershp)) | crs(rivershp,proj=TRUE) != crs(dem,proj=TRUE)) {
    stop("rivershp has a missing or mismatching coordinate reference system ")
  }

  # check other properties
  bb_check_resolution(list(dem,demcond,flowacc,flowdir))
  bb_check_extents(list(dem,demcond,flowacc,flowdir))

  # check if (approximately) square
  if (round(res(dem)[1],3) != round(res(dem)[2],3)) {
    stop("dem raster is not square; please resample with raster::resample or just terra::aggregate to obtain a square resolution dem")
  }

  # check min DEM
  if (min(dem[!is.na(dem)]) < 0) {
      stop(sprintf("DEM has a minimum value of %.2f < 0, cannot support negative values in DEM.",
                   min(dem[!is.na(dem)])))
  }

  # write to workingfolder if provided
  if (!dir.exists(workingfolder)) {
    dir.create(workingfolder)
  }

  dem_file <- bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(dem, dem_file, overwrite=overwrite)

  demcond_file <- bb_get_demcondraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(demcond, demcond_file, overwrite=overwrite)

  flowacc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(flowacc, flowacc_file, overwrite=overwrite)

  flowdir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(flowdir, flowdir_file, overwrite=overwrite)

  rivershp_file <- bb_get_rivershp(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(rivershp, rivershp_file, overwrite=overwrite)

  print("All files written to workingfolder successfully!")
  return(TRUE)
}
