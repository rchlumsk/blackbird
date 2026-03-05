#' @title Computes streamnode connecttions at boundaries and minimum HAND values
#'
#' @description
#' Determines which streamnode boundaries overlap, and what the minimum HAND values are at each boundary.
#'
#' @param bbopt blackbird options object
#' @return {sbconndf which can be passed to \code{\link{bb_write_model_files_cpp}}}
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra extract
#' @improtFrom sf st_touches st_intersection st_as_sf st_buffer
#' @export bb_preprocess_snconndf
bb_preprocess_snconndf <- function(bbopt=NULL) {

  if (is.null(bbopt)) {
    stop("bbopt is required")
  }

  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)
  hand <- bb_get_handraster(bbopt$workingfolder)
  handid <- bb_get_handpourpointIDraster(bbopt$workingfolder)
  catchmentraster <- bb_get_catchmentsfromstreamnodesraster(bbopt$workingfolder)

  dd <- data.frame(matrix(NA,nrow=0,ncol=3))
  colnames(dd) <- c("cid","adjcid","mindhand1")
  k = 1 # tracking dd row entry

  sdf <- bbmodel$bbgeo$get_streamnodeList_as_dataframe()

  for (i in 1:nrow(catchments_streamnodes)) {

    cid <- catchments_streamnodes$pointid[i]
    downid <- catchments_streamnodes$downid[i]

    touchid <- sdf$nodeID[unlist(sf::st_touches(catchments_streamnodes$geometry[i], catchments_streamnodes$geometry))]
    # remove self and any immediate upstream/downstream streamnodes
    touchid <- touchid[which(touchid %notin% c(cid, downid, sdf$upnodeID1[i], sdf$upnodeID2[i]))]
    ind <- which(sdf$nodeID %in% touchid)

    if (length(ind)>0) {
      # find lowest HAND value at boundary

      for (j in 1:length(ind)) {

        adjid <- touchid[j]

        # find all HAND cells on the border
        ss <- st_intersection(catchments_streamnodes$geometry[i], catchments_streamnodes$geometry[ind[j]])
        ss <- ss %>% st_as_sf()
        ss <- st_buffer(ss, dist=bb_get_demres(bbopt)) # buffer line
        write_sf(ss,"temp_check2.shp")
        hh <- terra::extract(hand, ss)[,2]
        # filter hand values for those that fall in the catchment boundary of current catchment
        craster <- terra::extract(catchmentraster, ss)[,2]
        hh <- hh[which(craster==cid)]
        hh <- hh[!is.na(hh)] # filter NA values

        # temp plotting
        # plot(catchments_streamnodes$geometry)
        # plot(catchments_streamnodes$geometry[i], col='blue',add=TRUE)
        # plot(catchments_streamnodes$geometry[ind[j]],col='cyan',add=TRUE)
        # plot(ss, col='red',lwd=2, add=TRUE)

        if (length(hh)>0) {
          mindhand1 <- min(hh,na.rm=TRUE)
          dd[k, ] <- c(cid,adjid,mindhand1)
          k <- k+1
        }
      }
    }
  }

  ## restructure dd to mine up nodeId and downnodeID in same row

  dd$minhand2 <- NA

  for (i in 1:nrow(dd)) {

    print(i)

    ind <- which((dd$adjcid == dd$cid[i]) & (dd$cid == dd$adjcid[i]) )

    if (length(ind)==1) {
      dd$minhand2[i] <- dd[ind,]$mindhand1
      dd <- dd[-ind,]
      i <- i-1

    } else if (length(ind)>1) {
      stop("error with ind>1")
    }
  }

  ## checks for NA values
  if (any(is.na(dd$minhand1))) {
    warning("At least one NA value in minhand1")
  }

  if (any(is.na(dd$minhand2))) {
    warning("At least one NA value in minhand2")
  }

  # write.csv(dd,"madriver_streamnode_connections.bbg.csv",row.names=F,quote=F)
  # write.csv(dd,"madriver_streamnode_connections2.bbg.csv",row.names=F,quote=F)

  sbconndf <- dd

  return(sbconndf)
}
