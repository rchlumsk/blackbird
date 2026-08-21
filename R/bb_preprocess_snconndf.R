#' @title Computes streamnode connections at boundaries and minimum HAND values
#'
#' @description
#' Determines which streamnode boundaries overlap, and what the minimum HAND values are at each boundary.
#'
#' @param bbmodel blackbird model object
#' @param subsetnodeIDs list of integer nodeIDs to compute transfer for
#' @param handthresh numeric threshold for filtering values in the table (default -1)
#' @param remove_USDS_nodes boolean whether to remove any nodes that are upstream or downstream of each other (i.e. \code{transfer %in% c(1,-1)})
#' @param writefile boolean whether to write the file directly to
#' @return {snconndf which can be passed to \code{\link{bb_write_model_files_cpp}}}
#'
#' @details
#' If using \code{writefile}, it will immediately write out the snconndf to a bbg format file
#' without requiring the full model build to write with \code{\link{bb_write_model_files_cpp}}
#'
#' With \code{handthresh}, if the value is not -1, then table entries greater than the threshold will be removed.
#' Table entries will only be removed if both HAND values are greater than the threshold provided. This functionality
#' is present to remove rows that are highly unlikely to interact, i.e. values with HAND values greater than the
#' anticipated depths at the connection points between streamnodes.
#'
#' The \code{remove_USDS_nodes} boolean is set to remove any nodes that are upstream or downstream of each other, as these are
#' not allowed to transfer flow in the compiled Blackbird code unless they are configured as conditonal nodes. If the
#' modeller does not plan to configure them as conditional nodes, no flow will be transferred between these
#' nodes and this flag should be enabled to avoid extra computations that will be netted out anyways.
#'
#' This function requires that catchments for streamnodes be delineated, as well as all precursor steps,
#' such as DEM and HAND processing.
#'
#
#' @examples
#' # IOU examples
#'
#' @importFrom terra extract
#' @importFrom sf st_touches st_intersection st_as_sf st_buffer
#' @importFrom igraph graph_from_data_frame subcomponent
#' @export bb_preprocess_snconndf
bb_preprocess_snconndf <- function(bbmodel=NULL, subsetnodeIDs=NULL, handthresh=-1, remove_USDS_nodes=FALSE,
                                   csndf=NULL,
                                   writefile=FALSE) {

  if (is.null(bbmodel)) {
    stop("bbmodel is required")
  }

  bbopt <- bbmodel$bbopt

  if (!file.exists(bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder,returnobject = FALSE))) {
    stop("catchments_streamnodes must be processed first")
  }
  if (!file.exists(bb_get_handraster(bbopt$workingfolder,returnobject = FALSE))) {
    stop("hand raster must be processed first")
  }

  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder)
  hand <- bb_get_handraster(bbopt$workingfolder)
  dem <- bb_get_demraster(bbopt$workingfolder)
  handid <- bb_get_handpourpointIDraster(bbopt$workingfolder)
  catchmentraster <- bb_get_catchmentsfromstreamnodesraster(bbopt$workingfolder)
  rivershp <- bb_get_rivershp(bbopt$workingfolder)

  ## check conditional processing too
   runcond <- FALSE
  if (!is.null(csndf)) {
    runcond <- TRUE
  }

  if (runcond) {
    condhand <- terra::rast(csndf$handpath)
  }

  allcrds <- terra::crds(hand, df=TRUE, na.rm=FALSE)

  demres <- bb_get_demres(bbopt)

  mciddf <- structure(list(cellid = integer(0), hh = numeric(0), ee = numeric(0),
    craster = integer(0), cellid2 = logical(0), hh2 = logical(0),
    ee2 = logical(0), craster2 = logical(0), transfer=integer(0)), row.names = integer(0), class = "data.frame")

  dd <- data.frame(matrix(NA,nrow=0,ncol=5))
  colnames(dd) <- c("cid","adjcid","mindhand1","elev1","reachID")
  # k = 1 # tracking dd row entry

  # get sdf and calculate igraph network
  sdf <- bbmodel$bbgeo$get_streamnodeList_as_dataframe()
  sdf$nodeID <- as.integer(sdf$nodeID)
  sdf$downnodeID <- as.integer(sdf$downnodeID)
  g <- graph_from_data_frame(sdf[,c("nodeID","downnodeID")], directed = TRUE)
  # is_upstream(a = 200, b = 219, g)
  # is_upstream <- function(a, b) {
  #   b %in% subcomponent(g, a, mode = "out")
  # }

  # check that all subsetnodeIDs are valid
  if (!is.null(subsetnodeIDs)) {
    if (any(subsetnodeIDs %notin% sdf$nodeID)) {
      stop("invalid subsetnodeIDs found, check input against model geometry")
    }
  } else {
    subsetnodeIDs <- sdf$nodeID
  }

  headwaternodes <- sdf[sdf$upnodeID1==-1,]$nodeID

  for (i in 1:nrow(catchments_streamnodes)) {

    # current streannode ID and downstream streamnode ID
    cid <- catchments_streamnodes$pointid[i]
    downid <- catchments_streamnodes$downid[i]

    # skip if headwater basin
    if (sdf$upnodeID1[i] == -1) {
      # cid is a headwater basin, skip
      next
    }

    # skip if node is not in the subsetnodeID list
    if (cid %notin% subsetnodeIDs) {
      next
    }

    # check streamnode IDs of touching cells, filter out any immediate downstream or upstream nodes
    touchid <- sdf$nodeID[unlist(sf::st_is_within_distance(catchments_streamnodes$geometry[i], catchments_streamnodes$geometry,demres/2))]
    # remove self and any immediate upstream/downstream streamnodes
    touchid <- touchid[which(touchid %notin% c(cid, downid, sdf$upnodeID1[i], sdf$upnodeID2[i]))]

    # remove any nodes that are upstream of cid
    # isus <- unlist(lapply(touchid,FUN=function(x) {is_upstream(x,cid,g)}))
    # touchid <-


    # remove any for which currentid is upstream or downstream of a junction to
    # blocked_reaches <- c(rivershp[which(rivershp$downID == catchments_streamnodes$reachID[i]),]$reachID, # which reaches drain to current reachID
    #                      rivershp[which(rivershp$reachID == catchments_streamnodes$reachID[i]),]$downID, # downstream reachID
    #                      catchments_streamnodes$reachID[i])                                              # current reachID (can't be on the same branch)
    # # update touchid based on blocked reaches
    # touchid <- touchid[which(sdf[sdf$nodeID %in% touchid, ]$reachID %notin% blocked_reaches)]

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
          df <- data.frame("cellid"=hh[,3], "hh"=hh[,2], "ee"=ee[,2], "craster"=craster[,2], "hhc"=NA)
          if (runcond & (cid==csndf$mapsto | adjid==csndf$mapsto)) {
            hhc <- terra::extract(condhand, ss, cells=TRUE,na.rm=TRUE)
            df$hhc <- hhc[,2]
          }
          # filter NA
          df <- df[which(!is.na(df$hh)),]
          df <- df[which(!is.na(df$ee)),]
          # if (!is.null(df$hhc)) {
          #   df <- df[which(!is.na(df$hhc)),]
          # }

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
            ciddf$transfer <- 0

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
                ciddf$hhc[k] <- adjiddfk$hhc
              }
            }

            # take cell with the min elev + hand (xxx if that is what we are doing)
            # ciddf <- ciddf[which(ciddf$ee+ciddf$hh-ciddf$hh2-ciddf$ee2 == min(ciddf$ee+ciddf$hh-ciddf$hh2-ciddf$ee2,na.rm=TRUE)),]

            # update transfer status column
            for (k in common_elements(which(!is.na(ciddf$craster)), which(!is.na(ciddf$craster2)))) {
               if (is_upstream(ciddf[k,]$craster, ciddf[k,]$craster2, g)) {
                ciddf$transfer[k] <- 1 # means that craster is upstream of craster2, transfer only allowed craster -> craster2
               } else if (is_upstream(ciddf[k,]$craster2, ciddf[k,]$craster, g)) {
                ciddf$transfer[k] <- -1 # means that craster2 is upstream of craster, transfer only allowed craster2 -> craster
               }
            }

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
  # mciddf <- mciddf[!is.na(mciddf$hhc),]

  ## sort for writing
  snconndf <- mciddf[order(mciddf$reachID, mciddf$craster, mciddf$craster2, mciddf$ee),]

  ## filter for HAND values over threshold
  if (handthresh>0) {
    snconndf <- snconndf[-which(snconndf$hh>handthresh & snconndf$hh2>handthresh),]
  }

  ## filter for nodes that have an upstream/downstream relationship (i.e., transfer is 1 or -1)
  if (remove_USDS_nodes) {
    snconndf <- snconndf[-which(snconndf$transfer %in% c(1,-1)),]
  }

  if (runcond) {
    snconndf$condsnID <- NA
    snconndf$mapsto <- NA
    ind <- which((snconndf$craster==csndf$mapsto | snconndf$craster2==csndf$mapsto) & (snconndf$craster %in% csndf$fromids | snconndf$craster2 %in% csndf$fromids))
    snconndf[ind,]$condsnID <- rep(csndf$nodeID,length(ind))
    snconndf[ind,]$mapsto <- csndf$mapsto
  }

  ## temporarily update to basic format and just
  if (runcond) {
    # xxx later update script to just update the values
    snconndf[snconndf$craster2 == csndf$mapsto,]$craster2 <- csndf$nodeID
    snconndf[snconndf$craster2 == csndf$nodeID,]$hh2 <- snconndf[snconndf$craster2 == csndf$nodeID,]$hhc
  }

  if (writefile) {
    workingfolder <- bbmodel$bbopt$workingfolder
    modelname <- "snconnndf_file"
    ## write out full geometry info ----
    outputfile <- file.path(workingfolder,"model",sprintf("%s_streamnodeconnections.bbg",modelname))
    # xxx replace this path with one that comes from the bb_get_object function
    if (!dir.exists(file.path(workingfolder,"model"))) {
      dir.create(file.path(workingfolder,"model"))
    }
    bb_write_snconndf(snconndf,outputfile)
  }

  return(snconndf)
}
