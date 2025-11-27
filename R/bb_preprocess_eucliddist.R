#' @title Calculates euclidean distance raster
#'
#' @description
#' Calculates euclidean distance to stream as a raster
#'
#' @details
#' This raster is used in the processing of geospatial layers in flat regions where
#' standard delineation routines fail.
#'
#' The rivershp as a shapefile is rasterized, and a raster of euclidean distance
#' is computed using the \link{\code{terra::distance}} function, to the same resolution
#' and extents of the supplied dem. The dem may be any raster with the desired output
#' extents and resolution.
#'
#'
#' @param dem A Digital Elevation Model (DEM) as raster or file path
#' @param rivershp shapefile of river network
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the euclidean distance raster if return_raster is TRUE}
#
#' @details
#' Suggest a fill method for removing sinks from the euclidean raster - breaching seems to create
#' a lot of issues.
#'
#'
#' @examples
#' # IOU
#'
#'
#' @importFrom terra rast writeRaster crs res distance rasterize
#' @export bb_preprocess_euclideandist
bb_preprocess_euclideandist <- function(workingfolder=NULL,
                                        removesinks_method="fill_wl",removesinks_dist=NULL,
                                        overwrite=TRUE) {

  if (is.null(workingfolder)) {"workingfolder is required"}

  dem=NULL
  if (file.exists(bb_get_demraster(workingfolder,returnobject = FALSE))) {
    dem <- bb_get_demraster(workingfolder)
  } else {
    stop("dem does not exist, must process first")
  }

  if (file.exists(bb_get_rivershp(workingfolder,returnobject = FALSE))) {
    rivershp <- bb_get_rivershp(workingfolder)
  } else {
    stop("rivershp does not exist, must process first")
  }

  if (is.null(removesinks_dist)) {
    demres <- res(dem)[1]
    removesinks_dist <- demres*3 # later remove hardcoding xxx, use bbopt?
  }

  rthalweg <- rasterize(rivershp,y=dem,field="reachID")
  rthalweg <- rthalweg/rthalweg

  # main raster
  rr <- terra::distance(rthalweg)
  # clip to dem mask
  pp <- terra::as.polygons(dem) %>% aggregate(dissolve = TRUE)
  rr <- bb_wbt_clip_raster_to_polygon(rr,pp,return_raster = TRUE)
  rm(pp)
  writeRaster(rr, bb_get_euclideandistraster(workingfolder,returnobject=FALSE), overwrite=overwrite)

  # remove sinks
  condfile <- bb_get_euclideandistcondraster(workingfolder = workingfolder, returnobject = FALSE)
  if (removesinks_method == "skip") {
    file.copy(bb_get_euclideandistraster(workingfolder,returnobject=FALSE), condfile, overwrite = overwrite)
  } else {
    bb_wbt_removesinks(bb_get_euclideandistraster(workingfolder,returnobject=FALSE), condfile,
                       method = removesinks_method, dist=removesinks_dist)
  }

  # compute accumulation as inverse
  # rr2 <- 1/rr
  # rr2[is.infinite(rr2)] <- max(rr2[is.finite(rr2)])
  # writeRaster(rr2,bb_get_euclideandistflowaccraster(workingfolder,returnobject = FALSE),overwrite=overwrite)

  # get flow accumulations and direction
  bb_wbt_flow_accumulation(condfile, bb_get_euclideandistflowaccraster(workingfolder,returnobject = FALSE), return_raster = FALSE)
  bb_wbt_flow_direction(condfile, bb_get_euclideandistflowdirraster(workingfolder,returnobject = FALSE), return_raster = FALSE)

  return(TRUE)
}
