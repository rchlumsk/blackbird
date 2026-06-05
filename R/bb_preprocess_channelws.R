#' @title Processes a raster/sf or the channel water surface
#'
#' @description
#' Performs numerous checks and processing steps on a channel water surface raster, then saves it.
#'
#' @param input raster or polygon, or file path of the channel water surface location
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the dem if return_raster is TRUE}
#
#' @details
#' Accepts a raster, sf or file path, and writes the appropriate raster and polygon versions as provided
#'
#' Does not handle conversions between raster and poolygon, these have to be provided separately
#'
#'
#' @examples
#'
#' @importFrom terra rast writeRaster crs res
#' @importFrom sf read_sf write_sf
#' @export bb_preprocess_channelws
bb_preprocess_channelws <- function(input=NULL, workingfolder=NULL,
                                    overwrite=TRUE) {

  if (is.null(input)) {stop("input is required")}
  if (is.null(workingfolder)) {"workingfolder is required"}

  if (is.character(input)) {

    if (!file.exists(input)) {
      stop("input file path does not exist")
    } else{
      if (grep(".tif",input)==1) {
        rr <- rast(input)
      } else if (grep(".shp",input)==1) {
        ss <- read_sf(input)
      } else {
        stop("invalid file type provided as input")
      }

    }
  } else if ("SpatRaster" %notin% class(input) & "sf" %noin% class(input)) {
      stop("dem must be a SpatRaster object (from the terra package), sf object, or a file path to one")
  }

  # else {
  #   if ("sf" %in% class(input)) {
  #     ss <- input
  #   } else {
  #     rr <- input
  #   }

  # check projection
  if (is.null(crs(input)) | is.na(crs(input)) ) {
    stop(sprintf("invalid crs for input, cannot be processed:\n%s", crs(input)))
  }

  # write to workingfolder if provided
  if (!dir.exists(workingfolder)) {
    dir.create(workingfolder)
  }

  if (!file.exists(bb_get_demraster(workingfolder, returnobject = FALSE))) {
    stop("dem is required to be processed first")
  }

  if ("sf" %in% class(input)) {
    sf::st_make_valid(input) %>%
      st_write(dsn=bb_get_channelwsshp(workingfolder,returnobject=FALSE))

  } else {
    # check values of channel raster, ensure it is binary
    vv <- unique(as.vector(input[!is.na(input)]))
    if (any(vv %notin% c(0,1)) ) {
      stop("input raster should have binary 1 or 0 raster values")
    }

    ## resample to dem resolution
    dem <- bb_get_demraster(workingfolder)
    rr <- terra::resample(input,dem, method='near') # near for categorical classification

    ## write to file
    channelWs_file <- bb_get_channelwsraster(workingfolder, returnobject = FALSE)
    writeRaster(rr, channelWs_file)
  }

  return(TRUE)
}
