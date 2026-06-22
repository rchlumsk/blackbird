#' @title Masks the HAND raster with values of zero in channel area
#'
#' @description
#' Writes a new masked hand raster based on the channel raster.
#'
#' @param bbopt blackbird options object
#' @param return_raster whether to return the processed flowdir as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the masked hand raster if return_raster is TRUE}
#
#' @examples
#' # xx to do
#'
#' @importFrom terra writeRaster
#' @export bb_preprocess_handmasked
bb_preprocess_handmasked <- function(bbopt=NULL, return_raster=FALSE, overwrite=TRUE) {

  if (is.null(bbopt)) {stop("bbopt is required")}

  handrr <- bb_get_handraster(bbopt$workingfolder)
  channelrr <- bb_get_channelwsraster(workingfolder)
  outputfile <- bb_get_handmaskedraster(bbopt$workingfolder, returnobject=FALSE)

  mchannelrr <- as.matrix(channelrr)
  mhandrr <- as.matrix(handrr)
  if (!(length(mhandrr) == c(length(mchannelrr)))) {
    stop("Input rasters are of different dimensions between hand and channel ws raster, check inputs")
  }

  # Mask DEM to channel==1 as one-off if using channel_ws
  ind <- which(mchannelrr == 1)
  mhandrr[ind] <- 0
  values(handrr) <- mhandrr

  # write raster to file
  terra::writeRaster(handrr, outputfile, overwrite=overwrite)

  warning("masked hand raster is produced but not explicitly used in Blackbird code. Consider manually overwriting bb_hand.tiff with bb_handmasked.tif if you wish to use it.")

  if (return_raster) {
    return(handrr)
  } else {
    return(TRUE)
  }
}
