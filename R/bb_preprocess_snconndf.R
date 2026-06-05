#' @title Computes streamnode connections at boundaries and minimum HAND values
#'
#' @description
#' Determines which streamnode boundaries overlap, and what the minimum HAND values are at each boundary.
#'
#' @param bbmodel blackbird model object
#' @return {snconndf which can be passed to \code{\link{bb_write_model_files_cpp}}}
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra extract
#' @improtFrom sf st_touches st_intersection st_as_sf st_buffer
#' @export bb_preprocess_snconndf
bb_preprocess_snconndf <- function(bbmodel=NULL) {

  if (is.null(bbmodel)) {
    stop("bbmodel is required")
  }

  bbopt <- bbmodel$bbopt

  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)
  hand <- bb_get_handraster(bbopt$workingfolder)
  dem <- bb_get_demraster(bbopt$workingfolder)
  handid <- bb_get_handpourpointIDraster(bbopt$workingfolder)
  catchmentraster <- bb_get_catchmentsfromstreamnodesraster(bbopt$workingfolder)
  rivershp <- bb_get_rivershp(bbopt$workingfolder)

  allcrds <- terra::crds(hand, df=TRUE, na.rm=FALSE)

  demres <- bb_get_demres(bbopt)

  mciddf <- structure(list(cellid = numeric(0), hh = numeric(0), ee = numeric(0),
    craster = integer(0), cellid2 = logical(0), hh2 = logical(0),
    ee2 = logical(0), craster2 = logical(0)), row.names = integer(0), class = "data.frame")

  dd <- data.frame(matrix(NA,nrow=0,ncol=5))
  colnames(dd) <- c("cid","adjcid","mindhand1","elev1","reachID")
  # k = 1 # tracking dd row entry

  sdf <- bbmodel$bbgeo$get_streamnodeList_as_dataframe()

  headwaternodes <- sdf[sdf$upnodeID1==-1,]$nodeID

  for (i in 1:nrow(catchments_streamnodes)) {

    # current streannode ID and downstream streamnode ID
    cid <- catchments_streamnodes$pointid[i]
    downid <- catchments_streamnodes$downid[i]

    if (sdf$upnodeID1[i] == -1) {
      # cid is a headwater basin, skip
      next
    }

    # check streamnode IDs of touching cells, filter out any immediate downstream or upstream nodes
    touchid <- sdf$nodeID[unlist(sf::st_touches(catchments_streamnodes$geometry[i], catchments_streamnodes$geometry))]
    # remove self and any immediate upstream/downstream streamnodes
    touchid <- touchid[which(touchid %notin% c(cid, downid, sdf$upnodeID1[i], sdf$upnodeID2[i]))]

    # xxx update to allow for spills US->DS nodes on same reach, but not DS->US

    # remove any for which currentid is upstream or downstream of a junction to
    blocked_reaches <- c(rivershp[which(rivershp$downID == catchments_streamnodes$reachID[i]),]$reachID, # which reaches drain to current reachID
                         rivershp[which(rivershp$reachID == catchments_streamnodes$reachID[i]),]$downID, # downstream reachID
                         catchments_streamnodes$reachID[i])                                              # current reachID (can't be on the same branch)
    # update touchid based on blocked reaches
    touchid <- touchid[which(sdf[sdf$nodeID %in% touchid, ]$reachID %notin% blocked_reaches)]

    # remove any headwater nodes it touches
    touchid <- touchid[which(touchid %notin% headwaternodes)]

    ind <- which(sdf$nodeID %in% touchid) # contains IDs for any streamnodes not in the immediate upstream/downstream of current

    if (length(ind)>0) {
      # find pairs of points at the boundary

      for (j in 1:length(ind)) {  # loop through each touching boundary to current streamnode

        adjid <- touchid[j]

        ## skip if already in mciddf
        if (nrow(mciddf)==0 | length(which(mciddf$craster2==cid & mciddf$craster==adjid))==0) {
          # find all HAND cells on the border

          ## draw line at the border of two catchments
          ss <- st_intersection(catchments_streamnodes$geometry[i], catchments_streamnodes$geometry[ind[j]])
          ss <- ss %>% st_as_sf()
          ss <- st_buffer(ss, dist=demres) # buffer line

          ## extract hand and DEM elevation values at the border
          hh <- terra::extract(hand, ss, cells=TRUE,na.rm=TRUE) # [,2]
          ee <- terra::extract(dem,ss, cells=TRUE,na.rm=TRUE) # [,2]
          craster <- terra::extract(catchmentraster, ss, cells=TRUE,na.rm=TRUE)
          df <- data.frame("cellid"=hh[,3], "hh"=hh[,2], "ee"=ee[,2], "craster"=craster[,2])
          # filter NA
          df <- df[which(!is.na(df$hh)),]
          df <- df[which(!is.na(df$ee)),]

          # take the df rows for the current id and find all the ones for adjid
          ciddf <- df[df$craster == cid,]
          adjiddf <- df[df$craster == adjid,]

          if (nrow(ciddf)>0) {  # iterate all cells at border for current cid

            # add cols
            ciddf$cellid2 <- NA
            ciddf$hh2 <- NA
            ciddf$ee2 <- NA
            ciddf$craster2 <- NA
            ciddf$reachID <- catchments_streamnodes$reachID[i]

            ## for each row, find a matching pair
            for (k in 1:nrow(ciddf)) {

              # computes distances
              ciddfk <- ciddf[k,]
              # adjiddfk <- adjiddf

              # filter by distance root 2*desmres from current cell centre
              dd <- rep(NA,nrow(adjiddf))
              for (ll in 1:nrow(adjiddf)) {
                dd[ll] <- dist_projected_vector(x1=as.numeric(allcrds[ciddfk$cellid,]),
                                           x2=as.numeric(allcrds[adjiddf$cellid[ll],]))
              }
              adjiddfk <- adjiddf[which(dd <= demres*sqrt(2)),]

              # filter NA
              adjiddfk <- adjiddfk[which(!is.na(adjiddfk$hh)),]
              adjiddfk <- adjiddfk[which(!is.na(adjiddfk$ee)),]

              if (nrow(adjiddfk)>0) {

                ## find the one with the smallest elevation
                adjiddfk <- adjiddfk[which(adjiddfk$ee == min(adjiddfk$ee)),]

                # find that with biggest difference in (e1+h1)-(e2+h2), store in ciddf
                ## xxx check if we want the min elevation or min elev+hand
                # adjiddfk <- adjiddfk[which(adjiddfk$ee+adjiddfk$hh == min(adjiddfk$ee+adjiddfk$hh)),]

                ciddf$cellid2[k] <- adjiddfk$cellid
                ciddf$hh2[k] <- adjiddfk$hh
                ciddf$ee2[k] <- adjiddfk$ee
                ciddf$craster2[k] <- adjiddfk$craster
              }
            }

            # take cell with the min elev + hand (xxx if that is what we are doing)
            # ciddf <- ciddf[which(ciddf$ee+ciddf$hh-ciddf$hh2-ciddf$ee2 == min(ciddf$ee+ciddf$hh-ciddf$hh2-ciddf$ee2,na.rm=TRUE)),]

            # merge ciddf to master
            if (nrow(mciddf)==0) {
              mciddf <- ciddf
            } else {
              mciddf <- rbind(mciddf,ciddf)
            }
        } # else skip

        }
      }
    }
  }

  ## filter for NA
  mciddf <- mciddf[!is.na(mciddf$cellid),]
  mciddf <- mciddf[!is.na(mciddf$craster),]
  mciddf <- mciddf[!is.na(mciddf$hh),]
  mciddf <- mciddf[!is.na(mciddf$ee),]
  mciddf <- mciddf[!is.na(mciddf$hh2),]
  mciddf <- mciddf[!is.na(mciddf$ee2),]

  ## sort for writing
  mciddf <- mciddf[order(mciddf$reachID, mciddf$craster, mciddf$craster2, mciddf$ee),]

  return(mciddf)
}
