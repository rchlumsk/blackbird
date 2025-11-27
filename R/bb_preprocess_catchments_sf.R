#' @title Create catchments for stream nodes as sf
#'
#' @description
#' Creates catchments as sf objects for each stream node provided.
#'
#' @param streamnodes stream nodes (point(s) sf) at which to generate catchments, ideally snapped to high flow accumulation
#' @param flow_dir_file flow direction raster (or file path)
#' @param flow_acc_file flow accumulation raster (or file path)
#' @param check_partial_river whether to check for catchments with partial rivershp coverage, remove those that aren't covered (default \code{TRUE})
#' @param check_edges_only in checking partial rivershp coverage, only check catchments at the edges (headwater and outlet catchments) to save computation
#' @param remove_short_edges additionally remove edge catchments with very short lengths, indicating incomplete coverage
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return \item{catchment_streamnodes}{returns catchments_streamnodes}
#
#' @details
#' \code{check_partial_river} uses the qgisprocess library to perform operations on each generated streamnode catchment, and check if the
#' rivershp intersects each one twice (i.e., covers it completely). Those with only partial coverage by a stream are pruned. Note that
#' this operation can be computationally expensive, and this step is skipped if set to \code{check_partial_river=FALSE}.
#'
#' @examples
#' library(raster)
#' library(sf)
#'
#' dem <- raster(system.file("extdata", "dem_Galt_10m.tif", package="blackbird"))
#' rivershp <- sf::read_sf(system.file("extdata", "river_main_clipped.shp", package="blackbird"))
#'
#' myworkingfolder <- tempdir()
#' myworkingfolder <- "temp_working"
#' hand_raster <- bb_preprocess_hand(dem, rivershp, workingfolder=myworkingfolder)
#'
#' # sample points along river with equal spacing for catchments
#' streamnodes <- bb_sample_linepoints(lineshp=st_zm(rivershp), pointdist=500)
#'
#' # create catchments at sampled points
#' catchments_streamnodes <- bb_preprocess_catchments_sf(streamnodes, workingfolder=myworkingfolder)
#'
#' plot(catchments_streamnodes$geometry)
#' plot(streamnodes$geometry, col='red', add=TRUE)
#'
#'
#' @importFrom sf st_write read_sf
#' @importFrom raster raster
#' @importFrom qgisprocess qgis_run_algorithm
#' @export bb_preprocess_catchments_sf
bb_preprocess_catchments_sf <- function(streamnodes=NULL,
                                        flow_dir_file=NULL, flow_acc_file=NULL,
                                        check_partial_river=TRUE,
                                        check_edges_only=TRUE,
                                        remove_partial_catchments=FALSE,
                                        remove_short_edges=FALSE,
                                        bbopt=NULL) {

  # streamnodes=NULL
  # streamnodes_idcol="pointid"
  # flow_dir_file=NULL
  # flow_acc_file=NULL
  # check_partial_river=TRUE
  # check_edges_only=TRUE
  # remove_partial_catchments = TRUE
  # remove_short_edges=TRUE

  if (is.null(bbopt)) {stop("bbopt is required")}
  workingfolder <- bbopt$workingfolder

  # find all items in workingfolder if provided
  # if (!is.null(workingfolder)) {
  #   if (is.null(flow_dir_file)) {
  #     flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject=FALSE)
  #   }
  #   if (is.null(flow_acc_file)) {
  #     flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  #   }
  #   if (is.null(streamnodes)) {
  #     streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
  #   }
  #
  # } else {
  #   workingfolder <- tempdir()
  #   writeextra <- FALSE
  # }

  if (!bbopt$use_euclidean) {
    flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject=FALSE)
    flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  } else {
    flow_dir_file <- bb_get_euclideandistflowdirraster(workingfolder = workingfolder, returnobject=FALSE)
    flow_acc_file <- bb_get_euclideandistflowaccraster(workingfolder = workingfolder, returnobject=FALSE)
  }
  streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = TRUE)

  # go through items and check if missing after working folder look
  if (is.null(streamnodes)) { stop("streamnodes must be provided")}
  if (is.null(flow_dir_file)) { stop("flow_dir_file must be provided")}
  if (is.null(flow_acc_file)) { stop("flow_acc_file must be provided")}

  ## check reachID col in streamnodes?
  if ("reachID" %notin% colnames(streamnodes)) {
    stop("reachID is required in streamnodes")
  }

  # check if ID column exists, otherwise create one
  if ("pointid" %notin% colnames(streamnodes)) {
    stop("Column pointid must be in streamnodes as an index")
    # streamnodes$pointid <- seq(1,nrow(streamnodes))
    # streamnodes_idcol <- "pointid"
    # write_sf(streamnodes, streamnodes_file, overwrite=TRUE)
  }
  # rm(streamnodes)

  ## temporarily add in the snap
  # flow_acc_file <- "bb_flowacc_dha nd_depth_5.00.tif"
  # flow_dir_file <- "bb_flowdir_dhand_depth_5.00.tif"
  # streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = TRUE)
  # streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
  # tf <- tempfile(fileext=".shp")
  #
  # bb_wbt_pourpoints(streamnodes_file, flow_acc_file,
  #                   tf, snap_dist = bbopt$pourpoint_snap_dist)
  # snapped_streamnodes <- read_sf(tf)
  # write_sf(snapped_streamnodes, streamnodes_file)
  # ---


  ## create catchments
  snapped_streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder=workingfolder,
                                                                     returnobject=FALSE)
  fn_catchment_ras <- bb_get_catchmentsfromstreamnodesraster(workingfolder=workingfolder,returnobject=FALSE)
  fn_catchment_vec <- bb_get_catchmentsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE)
  ## slower call for unnest basins, but better results?
  catchments_streamnodes <- bb_wbt_catchment(snapped_streamnodes_file, flow_dir_file,
                                             fn_catchment_ras, fn_catchment_vec, return_vector = TRUE)


  ## bring in vector version from QGIS interpretation of raster, native vector one doesn't seem to work as well

  # interpolate from contours using TIN interpolation
  tfpp <- tempfile(fileext=".shp")
  # write_sf(contcatch, tfpp, overwrite=TRUE)
  # tfpp <- paste0(tfpp,"::~::0::~::0::~::0") # qgis encoding to the path length data as interpolation data
  # tfidw <- tempfile(fileext=".tiff")
  # tfidw <- bb_get_reachlengthraster(workingfolder,returnobject=FALSE)
  # if (overwrite) {
    # unlink(tfidw)
  # }
  suppressMessages(qgis_run_algorithm(algorithm = "gdal:polygonize",
                                      INPUT=fn_catchment_ras,
                                      BAND=1,
                                      FIELD='VALUE',
                                      EIGHT_CONNECTEDNESS=FALSE,
                                      OUTPUT=tfpp))
  catchments_streamnodes <- read_sf(tfpp)
  write_sf(catchments_streamnodes, fn_catchment_vec, overwrite=TRUE)


  # add pointid from VALUE and sort by pointid
  ## catchments_streamnodes VALUE is the INDEX of the streamnodes pointid - updated with whitebox v2.4.0
  catchments_streamnodes$pointid <- streamnodes[catchments_streamnodes$VALUE,]$pointid
  # catchments_streamnodes$pointid <- catchments_streamnodes$VALUE  ## issue with pointIDs here, new version of whitebox?
  catchments_streamnodes <- catchments_streamnodes[order(catchments_streamnodes$pointid),]
  catchments_streamnodes <- catchments_streamnodes[,c("pointid","geometry")]

  # add reachID to each catchments_streamnodes, and other columns
  catchments_streamnodes <- left_join(catchments_streamnodes,
                                      data.frame("pointid"=streamnodes$pointid,
                                                 "reachID"=streamnodes$reachID,
                                                 "downid"=streamnodes$downid),
                                      by="pointid")

  # checks for duplicate catchment entries, merge those geometries
  dupIDs <- catchments_streamnodes[duplicated(catchments_streamnodes$pointid),]$pointid
  dupIDs <- unique(dupIDs)
  if (length(dupIDs) >0) {
    for (i in 1:length(dupIDs)) {
      temp <- catchments_streamnodes[catchments_streamnodes$pointid == dupIDs[i],]
      ind <- which(catchments_streamnodes$pointid == dupIDs[i])
      catchments_streamnodes[ind[1],]$geometry <-  st_union(temp$geometry)
      catchments_streamnodes <- catchments_streamnodes[-ind[2:length(ind)],]
    }
  }

  if (any(is.na(catchments_streamnodes$pointid))) {
    warning("One or more catchment pointids is NA, may encounter errors")
  }
  if (any(is.na(catchments_streamnodes$downid))) {
    warning("One or more catchment pointids is NA, may encounter errors")
  }

  # xxx add checks for any very small catchments

  # check that all catchments have a valid downid and reachid
  if (any(catchments_streamnodes$reachID %>% is.na())) {
    warning(sprintf("Bad catchments (%i) created, check results",
                    nrow(catchments_streamnodes[is.na(catchments_streamnodes$reachID),])))
  }

  # plot(catchments_streamnodes$geometry, col='blue')

  # cs2 <- catchments_streamnodes

  ## check geometry and simplify if needed
  # do this ahead of checks below
  if (any(!sf::st_is_valid(catchments_streamnodes))) {

    # ind <- which(!st_is_valid(catchments_streamnodes$geometry))
    # for (i in ind) {
    #   # catchments_streamnodes$geometry[i] <- st_make_valid(catchments_streamnodes$geometry[i])
    #   cs2$geometry[i] <- st_simplify(catchments_streamnodes$geometry[i])
    # }

    warning("Invalid geometries may exist in catchments_streamnodes")

    # catchments_streamnodes <- st_make_valid(catchments_streamnodes)  # sf::st_simplify(catchments_streamnodes) %>%
  }

  # plot(cs2$geometry, col='red')

  # plot(cs2$geometry[2045])

  # write_sf(catchments_streamnodes, "GrandRiverDemo/bb_catchments_fromstreamnodes.shp", overwrite=TRUE)

  ## remove any catchments and streamnodes with partial streams in them ----
  remove_IDs <- NULL
  # xxx update to only check edge catchments and save processing time?
  if (check_partial_river) {

    if (check_edges_only) {
      message(sprintf("Checking %i catchments for partial rivershp coverage, may take some time ...",
                      length(which(catchments_streamnodes$downid==-1 | (catchments_streamnodes$pointid %notin% catchments_streamnodes$downid))) ))
    } else {
      message(sprintf("Checking %i catchments for partial rivershp coverage, may take some time ...",nrow(catchments_streamnodes)))
    }

    rivershp <- bb_get_rivershp(workingfolder,returnobject = TRUE)
    tf1 <- tempfile(fileext=".shp") # polygon as lines
    tf2 <- tempfile(fileext=".shp") # points of intersection
    remove_IDs <- c()

    for (i in 1:nrow(catchments_streamnodes)) {

      if (!check_edges_only | (check_edges_only &
                               (catchments_streamnodes$downid[i]==-1 |
                                (catchments_streamnodes$pointid[i] %notin% catchments_streamnodes$downid)))) {
        suppressMessages(qgis_run_algorithm(algorithm = "native:polygonstolines",
                                            INPUT=catchments_streamnodes[i,],
                                            OUTPUT=tf1))

        suppressMessages(qgis_run_algorithm(algorithm = "native:lineintersections",
                                            INPUT=rivershp,
                                            INTERSECT=tf1,
                                            OUTPUT=tf2))
        if (nrow(read_sf(tf2)) == 1) {
          # check for two points of intersection in the
          remove_IDs <- c(remove_IDs, catchments_streamnodes$pointid[i])
        } else {
          check_length <- sf::st_intersection(catchments_streamnodes$geometry[i],rivershp) %>% st_length() %>% as.numeric() %>% sum()
          check_area <- sf::st_area(catchments_streamnodes$geometry[i]) %>% as.numeric()
          thresh_length <- sqrt(check_area/pi)
          if (check_length/thresh_length < 0.2) {
            if (remove_short_edges) {
              remove_IDs <- c(remove_IDs, catchments_streamnodes$pointid[i])
            } else {
              warning(sprintf("catchment with pointid %i not removed, but should be checked for possible short reach that does not cover catchment",
                              catchments_streamnodes$pointid[i]))
            }
          }
        }
      }
    }
    unlink(tf1)
    unlink(tf2)

    ## check which remove_IDs are middle links in the network (i.e. not a headwater or outlet catchment)
    # check looks for all where the relevant catchments have a downid within the network (!= -1) (not an outlet)
    # and where their IDs are another node's down ID (not a headwater)
    suppressWarnings(internal_remove_IDs <- remove_IDs[which(catchments_streamnodes[catchments_streamnodes$pointid %in% remove_IDs,]$downid != -1 &
                                   remove_IDs %in% catchments_streamnodes$downid)])

    if (length(internal_remove_IDs) > 0) {
      message(sprintf("Excluding some internal catchments from remove_IDs. Note that the following catchments have incomplete rivershp crossings (may be junctions):\n%s",
              paste0(internal_remove_IDs, collapse=", ")))
      remove_IDs <- remove_IDs[remove_IDs %notin% internal_remove_IDs]
    }

    if (length(remove_IDs) > 0 & remove_partial_catchments) {
      message(sprintf("Removing %i catchments and streamnodes based on incomplete intersection with rivershp", length(remove_IDs)))

      # points to remove, remove from catchments, streamnodes, and snapped_streamnodes
      catchments_streamnodes <- catchments_streamnodes[catchments_streamnodes$pointid %notin% remove_IDs,]
      if (any(catchments_streamnodes$downid %in% remove_IDs)) {
        catchments_streamnodes[catchments_streamnodes$downid %in% remove_IDs,]$downid <- -1
      }

      snapped_streamnodes <- sf::read_sf(snapped_streamnodes_file)
      snapped_streamnodes <- snapped_streamnodes[snapped_streamnodes$pointid %notin% remove_IDs,]
      if (any(snapped_streamnodes$downid %in% remove_IDs)) {
        snapped_streamnodes[snapped_streamnodes$downid %in% remove_IDs,]$downid <- -1
      }
      write_sf(snapped_streamnodes, dsn=snapped_streamnodes_file, delete_dsn = TRUE)

      streamnodes_file <- bb_get_streamnodesforcatchmentsshp(workingfolder=workingfolder, returnobject=FALSE)
      streamnodes <- read_sf(streamnodes_file)
      streamnodes <- streamnodes[streamnodes$pointid %notin% remove_IDs,]
      write_sf(streamnodes, dsn=streamnodes_file, delete_dsn = TRUE)
    }
  }


  # check again for validity just in case
  if (!all(st_is_valid(catchments_streamnodes))) {
    warning("Some invalid geometries still found in catchments_streamnodes")
  }

  # write catchments_streamnodes with updates back to file ----
  write_sf(catchments_streamnodes, fn_catchment_vec, overwrite=TRUE)

  # modify extents of catchments to match fine dem and other raster ----
  catchment_raster <- terra::rast(fn_catchment_ras)
  dem <- bb_get_demraster(workingfolder, returnobject = TRUE)
  if (!bb_check_extents(dem, catchment_raster)) {
    # can also use bb_match_raster_extents which uses crop and extend, but doesnt seem to always work
    # note that we cannot resample bilinear this one, as we need to preserve the IDs to use nearest neighbour (near) method
    # catchment_raster <- bb_match_raster_extents(catchment_raster,dem)
    catchment_raster <- bb_match_raster_extents_resample(catchment_raster,dem,method="near")
    writeRaster(catchment_raster, fn_catchment_ras, overwrite=TRUE)
  }

  # area check
  areas <- as.numeric(sf::st_area(catchments_streamnodes))
  if (any(areas <= 2*bb_get_demres(bbopt)^2)) {
    warning("Some catchment areas are less than 2 grid cells, delineation may need to be redone.")
    message(sprintf("catchment_streamnodes with <= 2 grid cells:\n%s",
                    paste0(catchments_streamnodes[which(areas <= 2*bb_get_demres(bbopt)^2),]$pointid, sep=', ', collapse =' ')))
  }

  if (is.null(remove_IDs)) {
    return(TRUE)
  } else {
    if (remove_partial_catchments) {
      message(sprintf("Removed catchments with the following IDs (pointid):\n%s",paste0(remove_IDs,collapse=", ")))
    } else {
      message(sprintf("Returning IDs of catchments suggested for removal (pointid column), but not removed from catchments_streamnodes:\n%s",paste0(remove_IDs,collapse=", ")))
    }
    return(remove_IDs)
  }
}
