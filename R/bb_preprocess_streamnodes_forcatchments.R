#' @title Preprocess points intended for catchment delineation as streamnodes
#'
#' @description
#' Performs numerous checks and processing steps on a set of points intended as catchment outlets
#' for streamnode configuration, then saves it.
#'
#' @param streamnodes As sf object of streamnodes or file path
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param overwrite whether to overwrite an existing file (default TRUE)
#' @param calc_downid boolean whether to calculate downid (assumes it is already provided if \code{TRUE}).
#'
#' @return \item{catchment_streamnodes}{returns catchments_streamnodes}
#'
#' @details
#' Assumes that the pointid increases further downstream, which is used to determine the downstream nodeID.
#'
#'
#' @examples
#'
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#'
#' nodes <- bb_sample_linepoints(lineshp=rivershp, pointdist=200)
#'
#' bb_preprocess_streamnodes_forcatchments(nodes)
#'
#' @importFrom sf st_write st_zm
#' @importFrom terra extract
#' @export bb_preprocess_streamnodes_forcatchments
bb_preprocess_streamnodes_forcatchments <- function(bbopt=NULL,
                                                    # snap_dist=NULL,
                                                    calc_downid=TRUE,
                                                    return_shp=FALSE, overwrite=TRUE
                                                    ) {


  ## xxx could remove need for this function in future iteration, move functionality to bb_preprocess_catchments_sf

  # streamnodes=NULL
  # return_shp=FALSE
  # overwrite=TRUE

  if (is.null(bbopt)) {stop("bbopt is required!")}

  workingfolder <- bbopt$workingfolder

  # if (is.null(streamnodes)) {stop("streamnodes is required")}
  if (is.null(workingfolder) & !return_shp) {stop("Either workingfolder must not be NULL, or return_shp must be TRUE")}

  # if ("sf" %notin% class(streamnodes)) {
  #   stop("streamnodes must be an sf object")
  # }

  ### xxx other checks and cleanup to add here

  ## snap streamnodes to high res flow accumulation, needed for proper elevation sampling
  # different from coarse point snapping done in catchment_sf algorithm
  streamnodes_file <- bb_get_streamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject=FALSE)

  if (!file.exists(streamnodes_file)) {
    stop("streamnodes file must exist")
  }

  snapped_streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder=workingfolder,returnobject = FALSE)
  # tf <- tempfile(fileext=".shp")
  # write streamnodes to file
  # write_sf(streamnodes, dsn=tf) # , delete_dsn = TRUE
  # rm(streamnodes)

  demres <- bb_get_demres(bbopt)

  # snapped_streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder=workingfolder,
  #                                                                       returnobject=FALSE)
  # demcond <- bb_get_demcondraster(workingfolder)
  # demres <- res(demcond)[1]
  # if (is.null(snap_dist)){
  #   snap_dist <- demres*4  # default to update xxx. can reduce once we have more accurate streams generated from flow accumulation
  # }

  if (!bbopt$use_euclidean) {
    flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  } else {
    flow_acc_file <- bb_get_euclideandistflowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  }

  ### iterative snapping of streamnodes

  if (bbopt$pourpoint_snap_dist > 0) {
    # only perform pourpoints function if snap dist > 0
    bb_wbt_pourpoints(streamnodes_file, flow_acc_file,
                      snapped_streamnodes_file, snap_dist = bbopt$pourpoint_snap_dist)
    snapped_streamnodes <- read_sf(snapped_streamnodes_file)


    ## check for snapped_streamnodes where the reachID is now different
    rivershp <- bb_get_rivershp(bbopt$workingfolder)
    ind <- check_reachIDs(snapped_streamnodes, rivershp)

    if (length(ind)>0) {

      # reduce snap distance iteratively
      snap_dist <- bbopt$pourpoint_snap_dist
      j <- 1

      while (j < 100) { # can put into bbopt the max number of snapping iterations if desired
        snap_dist <- snap_dist*0.8
        tf <- tempfile(fileext =".shp")
        # xxx could reduce this to just snapping the required points, if computationally expensive
        bb_wbt_pourpoints(streamnodes_file, flow_acc_file,
                          tf, snap_dist = snap_dist)
        snapped_streamnodes_temp <- read_sf(tf)
        snapped_streamnodes[ind,] <- snapped_streamnodes_temp[ind,]
        ind <- check_reachIDs(snapped_streamnodes, rivershp)
        if (length(ind)==0) {break}
        j <- j+1
      }
      if (length(ind)>0) {
        warning("iterative snapping failed, reachID for some points still varied")
        message(sprintf("List of point IDs in snapped_streamnodes that have a different reachID:\n%s",
                        paste0(snapped_streamnodes$pointid[ind], sep=", ", collapse=' ')))
      }
    }
  } else {
    # skip snapping, just read same streamnodes file
    snapped_streamnodes <- read_sf(streamnodes_file)
  }

  ## ----

  # if (calc_downid) {
  #
  #   snapped_streamnodes$downid <- NA
  #
  #   ### find point closest to outlet --
  #   target <- read_sf(outlet)
  #   # Find index of nearest point
  #   distances <- st_distance(target, snapped_streamnodes)
  #   nearest_idx <- which.min(distances)
  #   # Extract nearest point and its properties
  #   if (length(nearest_idx) != 1) {
  #     ## will need to modify for multiple outlets!!!!
  #     ## xxx
  #     stop("error locating point closest to outlet")
  #   } else {
  #     snapped_streamnodes[nearest_idx, ]$downid <- -1
  #   }
  #
  #   ## iterate through remainder of nodes
  #   last_idx <- nearest_idx
  #   allreaches <- unique(snapped_streamnodes$reachID)
  #   full_reaches <- c()
  #   while (any(is.na(snapped_streamnodes$downid))) {
  #
  #     # find point closest to last idx and on same branch
  #     temp <- snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes$reachID[last_idx] &
  #                           is.na(snapped_streamnodes$downid),]
  #
  #     if (nrow(temp)>0) {
  #       iddiff <- snapped_streamnodes$pointid[last_idx] - temp$pointid
  #       snapped_streamnodes[snapped_streamnodes$pointid == temp[which(iddiff==min(iddiff)),]$pointid,]$downid <-
  #         snapped_streamnodes$pointid[last_idx]
  #       last_idx <- which(snapped_streamnodes$pointid == temp[which(iddiff==min(iddiff)),]$pointid)
  #     } else {
  #
  #       # nothing else on this branch
  #
  #       # add last reach to full reaches
  #       full_reaches <- c(full_reaches,snapped_streamnodes$reachID[last_idx])
  #
  #       # find reach pointing to a full reach with missing na values
  #       which(snapped_streamnodes$rchdwnID == snapped_streamnodes$reachID[last_idx])
  #
  #     }
  #
  #
  #
  #
  #   }
  #
  #
  # }




  ## replace this whole algorithm with a flowacc independent script for network points

  # xxx issue here when euclidean is used

  # sample flowacc to determine downstream direction

  flowacc <- terra::rast(flow_acc_file)    # bb_get_flowaccraster(bbopt$workingfolder, returnobject = TRUE)
  snapped_streamnodes$flow_acc <- terra::extract(flowacc,snapped_streamnodes)[,2]
  rm(flowacc)
  if (any(is.na(snapped_streamnodes$flow_acc))) {
    stop("flowacc does not appear to cover the streamnodes (NA values found at streamnode points), requires full coverage")
  }

  ### determine downstream network
  if (calc_downid) {

    streamnodes_to_remove <- c() # pointID reference for streamnodes to remove

    snapped_streamnodes$downid <- NA

    # note, can fail if overlapping reaches from lineshp, need to have a unique reachID on each node without overlap

    for (i in 1:nrow(snapped_streamnodes)) {

      if (i < nrow(snapped_streamnodes)) {
        # check if next streamnodes is on same branch and flowacc is less
        if (snapped_streamnodes[(i+1),]$reachID == snapped_streamnodes[i,]$reachID &
            snapped_streamnodes[(i+1),]$pointid > snapped_streamnodes[i,]$pointid) {
          snapped_streamnodes[i,]$downid <- snapped_streamnodes[(i+1),]$pointid

          # check if the downstream sub id is in the reachID column, and the max elev is less than this one
        } else if (snapped_streamnodes[i,]$rchdwnID %in% snapped_streamnodes$reachID &
                   nrow(snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes[i,]$rchdwnID,])>0) {
          temp <- snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes[i,]$rchdwnID,]

          # find the upstream most one - min flow acc
          temp <- temp[temp$flow_acc == min(temp$flow_acc),]

          if (nrow(temp)==1) {
            snapped_streamnodes[i,]$downid <- temp$pointid
          } else if (nrow(temp)==2) {
            if (dist_projected(p1=temp[1,], p2=temp[2,]) < demres) {
              snapped_streamnodes[i,]$downid <- temp[1,]$pointid
              streamnodes_to_remove <- c(streamnodes_to_remove, temp[2,]$pointid)
            } else if (temp[1,]$flow_acc == temp[2,]$flow_acc) {
              # next two downstream points have the same elevation, but far enough apart to be from two grid cells
              snapped_streamnodes[i,]$downid <- temp[1,]$pointid
            }
          } else {

            # get the max distance between points
            max_dist <- 0
            for (i in 1:nrow(temp)) {
              for (j in 1:nrow(temp)) {
                dd <- dist_projected(p1=temp[i,], p2=temp[j,])
                if (dd > max_dist) {
                  max_dist <- dd
                }
              }
            }

            if (max_dist < demres) {
              snapped_streamnodes[i,]$downid <- temp[1,]$pointid
              streamnodes_to_remove <- temp[2:nrow(temp),]$pointid
            } else {
              stop("more than 2 streamnodes close together and more than demres, something wrong")
            }

          }
          # snapped_streamnodes[i,]$downid <- temp[temp$flowacc == max(temp$flowacc),]$pointid
        } else {
          snapped_streamnodes[i,]$downid <- -1
        }
      } else {
        if (snapped_streamnodes[i,]$rchdwnID %in% snapped_streamnodes$reachID &
                   nrow(snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes[i,]$rchdwnID,])>0) {
          # junction
          temp <- snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes[i,]$rchdwnID,]
          snapped_streamnodes[i,]$downid <-  temp[temp$pointid %notin% temp$downid,]$pointid
        } else {
          # assumed end of the line
          snapped_streamnodes[i,]$downid <- -1
        }
      }
    }

    # remove any streamnodes that need removing
    if (length(streamnodes_to_remove) > 0) {
      snapped_streamnodes <- snapped_streamnodes[-which(snapped_streamnodes$pointid %in% streamnodes_to_remove),]
      warning("streamnodes removed, may need to adjust network numbering")
    }

    if (any(is.na(snapped_streamnodes$downid))) {
      warning("bb_preprocess_streamnodes_forcatchments: failed to determine downstream ID for all streamnodes")
    }

    if (nrow(snapped_streamnodes[snapped_streamnodes$downid == -1,]) > 1) {
      warning("Multiple streamnodes with no outlet found, please check for accuracy. Also consider increasing the snap_dist")
    }

    ## ---- end of replacement

  }

  # check outputs
  if (any(is.na(snapped_streamnodes$downid ))) {
    warning("One or more downid in streamnodes is NA")
  }

  # write to workingfolder if provided
  if (!is.null(workingfolder)) {

    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }

    # snapped_streamnodes_file <- bb_get_streamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
    st_write(snapped_streamnodes, snapped_streamnodes_file, delete_dsn=overwrite)
  }

  if (return_shp) {
    return(snapped_streamnodes)
  } else {
    return(TRUE)
  }
}
