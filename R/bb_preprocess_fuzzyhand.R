#' @title Updates the HAND raster based on fuzzy dinf downslope tracing
#'
#' @description
#' Updates the existing HAND raster by taking the minimum HAND value of cells that each
#' cell is connected to through downslope dinf tracing.
#'
#' @param bbopt blackbird options object
#' @return {\code{TRUE} if processed properly}
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra rast writeRaster crs res
#' @importFrom raster raster extract rasterize
#' @export bb_preprocess_fuzzyhand
bb_preprocess_fuzzyhand <- function(bbopt=NULL) {

  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  workingfolder <- bbopt$workingfolder

  dem <- bb_get_demraster(workingfolder)
  dem2 <- dem
  dem <- as.vector(dem)

  pourpoints <- bb_get_snappedpourpointshand(workingfolder)
  rpp <- raster::rasterize(pourpoints,y=dem2, field="pointid")
  rpp <- as.vector(rpp)
  rpp[is.na(rpp)] <- -1 # set NA to-1 for easier processing
  rpp <- as.integer(rpp)

  # allids <- read.csv("MadRiverProject/allids_allcells.csv")
  # validcells <- as.vector(unlist(read.csv("MadRiverProject/validcells.csv")))

  allids <- bb_get_allids(workingfolder,returnobject=TRUE)
  validcells <- as.vector(bb_get_validcells(workingfolder,returnobject=TRUE)[,1])

  # sample rasters for pourpoints
  pourpoints$elev <- raster::extract(dem2,pourpoints)[,2]
  # pourpoints$streamnodeID <- raster::extract(catchment_raster,pourpoints)[,2]
  pourpoints <- sf::st_drop_geometry(pourpoints)

  hand <- bb_get_handraster(workingfolder)
  # hand <- as.vector(hand)

  # get all cells with multiple IDs (can maybe skip rpp, since single cell ones will already be rpp)
  mmid <- which(allids[,2]!=-1)


  # rebuild HAND raster

  # indexing!
  # mmid refers to place in validcells
  # allids same length as validcells
  # value in validcells refers to full dimension of dem/hand raster
  # value in allids refers to pourpoint ID


  for (i in 1:length(mmid)) { # only iterating those cells with multiple drain points

    # extract allids
    ppid <- allids[mmid[i],]
    ppid <- as.vector(ppid[ppid!=-1])
    ppid <- ppid[order(ppid)]

    # compute HAND for all ids
    elev <- dem[validcells[mmid[i]]] # elevation at cell

    handvals <- elev - pourpoints[pourpoints$pointid %in% ppid,]$elev

    if (any(handvals < hand[validcells[mmid[i]]])) {
      hand[validcells[mmid[i]]] <- min(handvals)
    }

  }

  # save as fuzzy hand raster
  writeRaster(hand,
              bb_get_fuzzyhandraster(workingfolder,returnobject=FALSE),
              overwrite=TRUE)

  return(TRUE)
}
