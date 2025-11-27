#' @title Prune the catchments_streamnodes based on provided IDs
#'
#' @description
#' Prunes specified catchments from catchments_streamnodes based on the provided IDs
#'
#' @param remove_IDs vector of catchment pointid to be removed (also from snapped_pourpoints, etc.)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return \item{catchment_streamnodes}{returns catchments_streamnodes}
#
#'
#' @examples
#'
#' # xxx TO DO
#' # IOU an example
#'
#' @importFrom sf st_write read_sf
#' @importFrom raster raster
#' @export bb_preprocess_catchments_sf_prune
bb_preprocess_catchments_sf_prune <- function(remove_IDs=NULL,
                                        workingfolder=NULL) {

  message(sprintf("Removing %i catchments and streamnodes based on incomplete intersection with rivershp", length(remove_IDs)))

  if (is.null(workingfolder)) {
    stop("You must not, provide a workingfolder that is naught")
  }

  if (any(remove_IDs %notin% catchments_streamnodes$pointid)) {
    warning("Not all remove_IDs found in catchments_streamnodes, please check. Will attempt to remove those found in catchments_streamnodes")
  }

  # get objects
  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder)
  snapped_streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder)
  streamnodes <- bb_get_streamnodesforcatchmentsshp(workingfolder)

  # remove operation and resave
  catchments_streamnodes <- catchments_streamnodes[catchments_streamnodes$pointid %notin% remove_IDs,]
  catchments_streamnodes[catchments_streamnodes$downid %in% remove_IDs,]$downid <- -1
  write_sf(catchments_streamnodes, dsn=bb_get_catchmentsfromstreamnodesshp(workingfolder,returnobject = FALSE), delete_dsn = TRUE)

  snapped_streamnodes <- snapped_streamnodes[snapped_streamnodes$pointid %notin% remove_IDs,]
  snapped_streamnodes[snapped_streamnodes$downid %in% remove_IDs,]$downid <- -1
  write_sf(snapped_streamnodes, dsn=bb_get_snappedstreamnodesforcatchmentsshp(workingfolder,returnobject = FALSE), delete_dsn = TRUE)

  streamnodes <- streamnodes[streamnodes$pointid %notin% remove_IDs,]
  streamnodes[streamnodes$downid %in% remove_IDs,]$downid <- -1
  write_sf(streamnodes, dsn=bb_get_streamnodesforcatchmentsshp(workingfolder,returnobject = FALSE), delete_dsn = TRUE)

  return(TRUE)
}
