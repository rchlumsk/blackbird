#' @title Sample streamnode points along reaches
#'
#' @description
#' Samples regularly-spaced points along a set of reaches maintaining reach IDs in each
#'
#' @param lineshp rivershp line(s) as sf object on which to generate points
#' @param reachID_col column name in lineshp corresponding to reach IDs
#' @param pointdist distance between generated points in metres
#' @param firstpoint include the first point on the line in the sample (boolean)
#' @param lastpoint include the last point on the line in the sample (boolean)
#' @param overwrite whether to overwrite the writefile if it already exists
#' @param calc_downid whether to compute network connections
#' @param snap_points boolean whether to snap points to flow accumulation layer (default \code{TRUE})
#' @param return_shp boolean whether to return an sf object (default \code{FALSE})
#'
#' @return \item{TRUE}{returns \code{TRUE} if executed successfully}
#'
#' @details
#' This function is a wrapper for \code{bb_sample_linepoints_multiple}, but modifies the
#' lineshp to first use the specified \code{reachID_col}.
#'
#' @seealso \code{\link{{bb_sample_linepoints_multiple}} for sampling linepoints
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(system.file("extdata", "nithburg_river_EPSG3161.shp", package="blackbird"))
#'
#' nodes <- bb_sample_streamnodes_fromreaches(lineshp=rivershp, pointdist=200, reachID_col="SubId")
#'
#'
#' @importFrom sf as write_sf st_as_sf st_length st_line_sample st_geometry st_cast
#' @importFrom sp spsample
#' @importFrom terra rast extract
#' @export bb_sample_streamnodes_fromreaches
bb_sample_streamnodes_fromreaches <- function(lineshp=NULL, bbopt=NULL, reachID_col="reachID", downID_col="downID",
                                              pointdist=100, firstpoint=FALSE, lastpoint=FALSE,min_end_offset=NA,
                                              overwrite=TRUE, return_shp=FALSE) {

  ## modified this function to just sample points and put as streamnodes

  # writefile=NA, calc_downid=TRUE, snap_points=TRUE,

  # param sample_dem_path path for dem to sample elevation from; if NULL, elevation is not sampled
  # sample_dem_path=NULL,


  # lineshp=rivershp
  # reachID_col="reachID"
  # downID_col="downID"
  # pointdist=1000
  # firstpoint=TRUE
  # lastpoint=TRUE
  # min_end_offset=NA  # metres, map units
  # sample_dem_path=NULL
  # writefile=NA
  # overwrite=TRUE
  # calc_downid=TRUE
  # snap_points=TRUE

  if (is.null(bbopt)) {stop("bbopt is required")}

  if (is.null(lineshp)) {
    stop("lineshp is a required argument")
  }

  if ("sf" %notin% class(lineshp)) {
    stop("lineshp should be of class sf")
  }

  if (nrow(lineshp) < 1) {
    stop("no features detected in lineshp")
  }

  if (is.null(reachID_col)) {
    stop("reachID_col is required")
  }

  if (reachID_col %notin% colnames(lineshp)) {
    stop(sprintf("reachID_col %s not found in lineshp and is required",reachID_col))
  }

  if (is.na(min_end_offset)) {
    min_end_offset <- bb_get_demres(bbopt)
  }

  ## setup lineshp with reachID column
  lineshp$reachID <- lineshp[[reachID_col]]
  lineshp$rchdwnID <- lineshp[[downID_col]]

  ## call to bb_sample_linepoints_multiple
  linepoints <- bb_sample_linepoints_multiple(lineshp=lineshp, pointdist = pointdist,
                                firstpoint = firstpoint, lastpoint = lastpoint,
                                min_end_offset=min_end_offset,
                                overwrite = overwrite,
                                add_info_cols=c("reachID","rchdwnID"))

  # if (!bbopt$use_euclidean) {
  #   flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  # } else {
  #   flow_acc_file <- bb_get_euclideandistflowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  # }
  # flow_acc <- terra::rast(flow_acc_file)
  #
  # if (calc_downid) {
  #   # compute network links here
  #   linepoints$flow_acc <- terra::extract(flow_acc,linepoints)[,2]
  #   # rm(flow_acc)
  #   linepoints$downid <- NA
  #
  #   # compute downid for each pourpoint, even if out of snaps
  #   # generally assume that flow acc is higher in downstream
  #   for (i in 1:nrow(lineshp)) {
  #     temp <- linepoints[linepoints$reachID == lineshp$reachID[i],]
  #     reorder <- FALSE
  #     if (nrow(temp) >8) { # use average of 4 points
  #       if (mean(temp$flow_acc[1:4]) > mean(temp$flow_acc[(nrow(temp)-3):(nrow(temp))])) {
  #         # reorder in descending order
  #         reorder <- TRUE
  #       } # else continue
  #     } else { # use first and last points
  #       if (temp$flow_acc[1] > temp$flow[nrow(temp)]) {
  #         # reorder in descending order
  #         reorder <- TRUE
  #       } # else continue
  #     }
  #     # reorder points if needed
  #     if (reorder) {
  #       temp <- temp[order(-temp$pointid),]
  #       temp$pointid <- seq(min(temp$pointid),nrow(temp)+min(temp$pointid)-1)
  #     }
  #     # compute downid
  #     temp$downid[nrow(temp)] <- -1
  #     temp$downid[1:(nrow(temp)-1)] <- temp$pointid[2:nrow(temp)]
  #     # replace linepoints with temp
  #     linepoints <- rbind(
  #       temp,
  #       linepoints[linepoints$reachID != lineshp$reachID[i],]
  #     )
  #   }
  #
  #   # compute the junction downids
  #   temp <- linepoints[linepoints$downid == -1 & linepoints$rchdwnID!= -1,]
  #   for (i in 1:nrow(temp)) {
  #     temp2 <- linepoints[linepoints$reachID == temp[i,]$rchdwnID,]
  #     linepoints[linepoints$pointid == temp$pointid[i],]$downid <- temp2[temp2$pointid %notin% temp2$downid,]$pointid
  #   }
  # }
  #
  # ## sort by pointid
  # linepoints <- linepoints[order(linepoints$pointid),]

  ## write streamnodes
  streamnodes_file <- bb_get_streamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject=FALSE)
  # linepoints <- linepoints[,-(which(colnames(linepoints) %in% "flow_acc"))]
  if (file.exists(streamnodes_file)) {unlink(streamnodes_file)}
  write_sf(linepoints, streamnodes_file)
  # streamnodes <- linepoints
  # rm(linepoints)

  ## snap points, if requested, else write to file as is
  # snapped_streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
  # if (file.exists(snapped_streamnodes_file)) {unlink(snapped_streamnodes_file)}
  # if (snap_points) {
  #   bb_wbt_pourpoints(streamnodes_file, flow_acc_file,
  #                     snapped_streamnodes_file, snap_dist = bbopt$pourpoint_snap_dist)
  # } else {
  #   # write without snapping
  #   write_sf(linepoints, snapped_streamnodes_file)
  # }
  #
  # ## check snapped points against flow_acc, see if flow always increasing
  # snapped_streamnodes <- read_sf(snapped_streamnodes_file)
  # snapped_streamnodes$flow_acc <- terra::extract(flow_acc,snapped_streamnodes)[,2]
  # unlink(snapped_streamnodes_file)
  # write_sf(snapped_streamnodes,snapped_streamnodes_file) # write with flow_acc for snapped locations
  # # rm(flow_acc)
  #
  # snap_needed <- FALSE
  # for (i in 1:nrow(lineshp)) {
  #   temp <- snapped_streamnodes[snapped_streamnodes$reachID == snapped_streamnodes$reachID[i],]
  #   for (j in 1:nrow(temp)) {
  #     if (temp$downid[j] %in% temp$pointid) {
  #       if (temp[temp$pointid == temp$downid[j],]$flow_acc <= temp$flow_acc[j]) {
  #         warning("At least one streamnode is off the main flow accumulation path, consider snapping and/or with a higher snapping threshold (bbopt$pourpoint_snap_dist)")
  #         snap_needed <- TRUE
  #         break
  #       }
  #     }
  #   }
  #   if (snap_needed) {break}
  # }

  if (return_shp) {
    return(linepoints)
  } else {
    return(TRUE)
  }
}
