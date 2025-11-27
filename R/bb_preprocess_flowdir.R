#' @title Saves flow direction grid to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a flow direction raster, then saves it.
#'
#' @param flowdir A flow direction grid as raster or file path
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param return_raster whether to return the processed flowdir as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the flowdir if return_raster is TRUE}
#
#' @details
#'
#'
#' @examples
#' # xx to do
#'
#' @importFrom raster raster writeRaster crs
#' @export bb_preprocess_flowdir
bb_preprocess_flowdir <- function(flowdir=NULL, workingfolder=NULL, return_raster=FALSE, overwrite=TRUE) {

  if (is.null(flowdir)) {stop("flowdir is required")}
  if (is.null(workingfolder) & !return_raster) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}

  ## check flowdir validity as raster
  if (is.character(flowdir)) {

    if (!file.exists(flowdir)) {
      stop("flowdir file path does not exist")
    } else{
      flowdir <- raster(flowdir)
    }
  } else if ("SpatRaster" %notin% class(flowdir)) {
      stop("flowdir must be a SpatRaster object or a file path to one")
  }

  # check projection
  if (is.null(crs(flowdir)) | is.na(crs(flowdir)) ) {
    stop(sprintf("invalid crs for flowdir, cannot be processed:\n%s", crs(flowdir)))
  }

  # check if square
  if (res(flowdir)[1] != res(flowdir)[2]) {
    stop("flowdir raster is not square; please resample with raster::resample or just terra::aggregate to obtain a square resolution flowdir")
  }

  # write to workingfolder if provided
  if (!is.null(workingfolder)) {

    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }

    flowdir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)
    writeRaster(flowdir, flowdir_file, overwrite=overwrite)
  }

  if (return_raster) {
    flowdir <- raster(flowdir_file)
    return(flowdir)
  } else {
    return(TRUE)
  }
}
