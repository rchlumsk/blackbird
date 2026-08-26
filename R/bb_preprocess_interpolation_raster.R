#' @title Calculates the interpolation raster
#'
#' @description
#' Calculates the interpolation raster required for interp-based postprocessing methods
#'
#' @param dem Digital Elevation Model (DEM) as a raster object
#' @param rivershp river shapefile as sf object
#' @param removesinks_method method to use in the sink removal algorithm for dem condioning (or 'skip' to use DEM as is)
#' @param use_channelws boolean whether to use the channel water surface polygon to determine the thalweg value
#' @param channelws_percentile if using the use_channelws method, then this is the percentile to use in each. Value of zero uses the min value
#' @param overwrite if \code{TRUE}, will overwrite any written files
#' @param return_raster whether to return raster (default \code{TRUE})
#'
#' @return \item{hand}{returns the HAND in raster format}
#' \item{misc}{other spatial objects may be written to file, such as the
#' intermediate drainage basins, the zdrainage layer, sampled points along the rivershp, etc.}
#
#' @details
#' This produces the raster that has continuous values from [0..1] indicating the interpolation to be applied between the downstream
#' streamnode depth (value of 1.0) and the upstream streamnode depth (value of 0). This supersedes the postprocessing
#' interpolation calculation that takes the pourpoints and hand pourpoint ID values, and calcultes the interpolation
#' based on that. Here, this calculation is done once as a preprocessing step to simplify the post-processing
#' interpolation substantially.
#'
#' @examples
#'
#'
#' @importFrom dplyr filter select
#' @importFrom sf st_drop_geometry
#' @importFrom terra classify writeRaster
#' @export bb_preprocess_interpolation_raster
bb_preprocess_interpolation_raster <- function(
                               bbopt=NULL,
                               bbgeom=NULL,
                               overwrite=TRUE, return_raster=TRUE) {

  if (is.null(bbopt) | "bb_options" %notin% class(bbopt)) {
    stop("bb_options class object bbopt is required")
  }

  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)
  hand <- bb_get_handraster(workingfolder)
  handid <- bb_get_handpourpointIDraster(workingfolder,returnobject = TRUE)
  spp <- bb_get_snappedpourpointshand(workingfolder,returnobject = TRUE)
  spp$depth <- NA
  sdf <- bbgeom$get_streamnodeList_as_dataframe()

  if (nrow(sdf) != nrow(catchments_streamnodes)) {
    stop("Mismatch in sdf from bbgeom and catchments_streamnodes, cannot use")
  }
  catchments_streamnodes$upnodeID1 <- sdf$upnodeID1
  catchments_streamnodes$upnodeID2 <- sdf$upnodeID2
  catchments_streamnodes$ds_reach_length <- sdf$ds_reach_length
  catchments_streamnodes$us_reach_length1 <- sdf$us_reach_length1

  ## build depths in each catchment
  for (i in 1:nrow(catchments_streamnodes)) {

    if (catchments_streamnodes$upnodeID1[i] == -1) {
      # headwater node, nothing to interpolate on one side of node
      # just skip interpolation and map depth across all spp nodes within catchment
      sppi <- spp
      sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
      sppi <- sppi %>% filter(within==1)
      sppi$depth <- 1.0 # replace with value corresponding to the current node, nothing to interpolate

    } else if (length(which(catchments_streamnodes$downid == catchments_streamnodes$pointid[i]))>1) {
      # junction catchment with multiple sets of reaches within it
      sppi <- spp
      sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
      sppi <- sppi %>% filter(within==1)
      sppi$depth <- catchments_streamnodes$depth[i]

      # sorting in descending order not needed here

      ## trying to do more here - for now just leave as constant depth
      # determine depth at junction, weighted average of depth and length
      # convention: depth 1 at downstream end, depth 2 at one upstream reach segment, depth 3 at the other
      depth1 <- 1.0 # catchments_streamnodes[i,]$depth
      depth2 <- 0.0 # catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth
      depth3 <- 0.0 # catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$depth
      L1 <- catchments_streamnodes[i,]$us_reach_length1
      L2 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$ds_reach_length
      L3 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID2[i],]$ds_reach_length
      depth_junction <- (depth1*L1+depth2*L2+depth3*L3)/sum(L1,L2,L3)

      if (is.finite(depth_junction)) {
        # simply apply the junction depth at all non-current reachID nodes, if any
        sppi[sppi$reachID != catchments_streamnodes$reachID[i],]$depth <- depth_junction
      }

    } else {
      # convention - basin 1 is downstream of basin i (not longer used), basin 2 = basin i, and basin 3 is upstream of basin i

      # upstream and downstream depths
      depth2 <- 1.0 # catchments_streamnodes$depth[i]
      # depth1 <- catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$downid[i],]$depth
      depth3 <- 0.0 # catchments_streamnodes[catchments_streamnodes$pointid == catchments_streamnodes$upnodeID1[i],]$depth

      # get upstream reach lengths
      L2 <- catchments_streamnodes$us_reach_length1[i]

      # filter to current catchment
      sppi <- spp
      sppi$within <- as.numeric(sf::st_within(spp, catchments_streamnodes[i,]))
      sppi <- sppi %>% filter(within==1)

      if (nrow(sppi) == 0) {
        stop(sprintf("No points found in catchment pointid=%i",catchments_streamnodes$pointid[i]))
      } else if (nrow(sppi)==1) {
        # exception if only one point in catchment - just apply depth as is
        sppi$depth <- 1.0 # catchments_streamnodes$depth[i]
      } else {
        # ensure points are in descending order
        if (sppi$downID[1] != sppi$pointid[2]) {
          sppi <- sppi[order(-sppi$pointid),]
          if (sppi$downID[1] != sppi$pointid[2]) {
            stop("issue in ordering of pointid, downid does not follow convention with pointid")
          }
        }

        # calculate chainage, assume most DS HAND point is on or very close to the streamnode
        sppi$chainage <- seq(L2,0,length.out=nrow(sppi))
        sppi$depth <- depth2 + (depth3-depth2)/L2*sppi$chainage
      }
    }
    # get depths back to main spp
    spp[spp$pointid %in% sppi$pointid,]$depth <- sppi$depth
    rm(sppi)
  }

  ## create depth raster from spp by reclassifying handids as depth
  rcl_tbl <- spp %>%
    select(pointid, depth) %>%
    sf::st_drop_geometry() %>%
    as.data.frame()

  interprr <- terra::classify(x=handid,rcl=rcl_tbl)
  interprr[!is.na(interprr) & interprr>1] <- 1.0 # catch any stray values from missing pointid

  outputfile <- bb_get_interpraster(bbopt$workingfolder, returnobject=FALSE)
  writeRaster(interprr, outputfile, overwrite=overwrite)

  if (return_raster) {
    return(interprr)
  } else {
    return(TRUE)
  }
}
