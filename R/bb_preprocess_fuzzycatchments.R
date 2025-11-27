#' @title Computes overlapping streamnode catchment boundaries from downslope tracing
#'
#' @description
#' Updates the streamnode catchment boundaries as overlapping boundaries, adjusting
#' to account for plausible drainage cells from the downslope dinf tracing.
#'
#' @param bbopt blackbird options object
#' @return {\code{TRUE} if processed properly, or the dem if return_raster is TRUE}
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra rast writeRaster crs res
#' @importFrom raster raster extract rasterize
#' @export bb_preprocess_fuzzycatchments
bb_preprocess_fuzzycatchments <- function(bbopt=NULL) {

  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  workingfolder <- bbopt$workingfolder

  dem <- bb_get_demraster(workingfolder)
  dem2 <- dem

  pourpoints <- bb_get_snappedpourpointshand(workingfolder)

  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder)

  catchmentraster <- bb_get_catchmentsfromstreamnodesraster(workingfolder)
  crv <- as.vector(catchmentraster)

  allids <- bb_get_allids(workingfolder,returnobject=TRUE)
  validcells <- as.vector(bb_get_validcells(workingfolder,returnobject=TRUE)[,1])

  # sample rasters for pourpoints
  pourpoints$elev <- raster::extract(dem,pourpoints)[,2]
  pourpoints$streamnodeID <- raster::extract(catchmentraster,pourpoints)[,2]

  # list to store catchment raster cells
  crlist <- rep(list(NULL), nrow(catchments_streamnodes))

  for (i in 1:nrow(catchments_streamnodes)) {

    # get all rpps
    ppid <- pourpoints[pourpoints$streamnodeID == catchments_streamnodes$pointid[i],]$pointid
    ppid <- ppid[!is.na(ppid)]

    # get matching rows from allids where any pour points IDs found
    mr <- contains_element(allids,ppid)

    # get cellIDs of catchmentraster that correspond to mr, filter for ones already in catchment
    newind <- unique(c(validcells[mr],which(crv==catchments_streamnodes$pointid[i])))

    # set catchmentraster to streamnodeID at cells where these drains found
    cr <- catchmentraster
    cr[!is.na(cr)] <- NA
    cr[newind] <- catchments_streamnodes$pointid[i]

    # checks for making sure all cells are contiguous? or just trust to the downslope alg

    crlist[[i]] <- cr
  }

  # xxx consider adding in whitebox::wbt_remove_polygon_holes() to fill holes there
  # xxx to do

  # save raster stack of catchment rasters
  rr <- rast(crlist)
  terra::writeRaster(rr,
                     # "MadRiverProject/bb_catchmentraster_stack.tiff",
                     bb_get_catchmentstack(workingfolder,returnobject=FALSE),
                     overwrite=TRUE)


  # check that no cells in catchmentraster have noID where stack does
  cr <- as.vector(rr)
  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder)

  for (i in 1:nrow(catchments_streamnodes)) {
    vv <- as.vector(rr[[i]])
    ind <- which(vv==catchments_streamnodes$pointid[i])
    if (any(is.na(cr[ind]))) {
      print(sprintf("missing cells (%i) for streamnode %i with pointid of %i",
                    length(which(is.na(cr[ind]))), i,catchments_streamnodes$pointid[i]))
    }
  }

  return(TRUE)
}
