#' @title Preprocess step to prepare the HAND raster
#'
#' @description
#' Creates the Height Above Nearest Drainage (HAND) raster.
#'
#' @param dem Digital Elevation Model (DEM) as a raster object
#' @param rivershp river shapefile as sf object
#' @param removesinks_method method to use in the sink removal algorithm for dem condioning (or 'skip' to use DEM as is)
#' @param overwrite if \code{TRUE}, will overwrite any written files
#' @param return_raster whether to return raster (default \code{TRUE})
#'
#' @return \item{hand}{returns the HAND in raster format}
#' \item{misc}{other spatial objects may be written to file, such as the
#' intermediate drainage basins, the zdrainage layer, sampled points along the rivershp, etc.}
#
#' @details
#' General procedure for this function is as follows:
#'     - TO DO
#'
#' Note that \code{bbopt$use_euclidean} is a boolean for whether to use euclidean distance
#' raster in determining nearby cells (used in flat areas)
#'
#' Note, other parameters to add (e.g. spacing in generating stream points, snapping distance for pourpoints, etc).
#'
#'
#' \code{removesinks_method} can be one of \code{breach_leastcost}, \code{breach}, \code{fill},
#' \code{fill_pd}, \code{fill_wl}, or \code{skip} (which skips the removal of sinks). Refer to \code{\link{bb_wbt_removesinks}} for more details.
#'
#' @examples
#' library(raster)
#' library(sf)
#'
#' dem <- raster(system.file("extdata", "dem_Galt_10m.tif", package="blackbird"))
#' rivershp <- sf::read_sf(system.file("extdata", "river_main_clipped.shp", package="blackbird"))
#'
#' # hand_raster <- bb_preprocess_hand(dem, rivershp)
#' hand_raster <- bb_preprocess_hand(dem, rivershp, workingfolder = "temp_working")
#'
#'
#' @importFrom sf st_zm st_write st_length st_sf st_sfc st_crs st_write
#' @importFrom exactextractr exact_extract
#' @importFrom raster rasterize overlay writeRaster
#' @importFrom dplyr distinct
#' @export bb_preprocess_hand
bb_preprocess_hand <- function(dem=NULL, flowdir=NULL, rivershp=NULL,
                               # workingfolder=NULL,
                               bbopt=NULL,
                               # sample_linepoints_dist=NULL,
                               removesinks_method="breach_leastcost",
                               # removesinks_dist=NULL,
                               # pourpoint_snap_dist=NULL,
                               overwrite=TRUE, return_raster=TRUE) {
  # dem=NULL
  # flowdir=NULL
  # rivershp=NULL
  # removesinks_method="breach_leastcost"
  # overwrite=TRUE
  # return_raster=TRUE

  if (is.null(bbopt) | "bb_options" %notin% class(bbopt)) {
    stop("bb_options class object bbopt is required")
  }

  if (bbopt$sample_linepoints_dist < 0) {
    stop("bbopt$sample_linepoints_dist must be > 0")
  }
  if (bbopt$removesinks_dist < 0) {
    stop("bbopt$removesinks_dist must be > 0")
  }
  if (bbopt$pourpoint_snap_dist < 0) {
    stop("bbopt$pourpoint_snap_dist must be > 0")
  }
  if (is.null(bbopt$workingfolder) | bbopt$workingfolder=="") {
    stop("bbopt$workingfolder must not be empty!")
  }

  workingfolder <- bbopt$workingfolder

  # dem=NULL
  # flowdir=NULL
  # rivershp=NULL
  # # sample_linepoints_dist=NULL
  # removesinks_method="breach_leastcost"  # "fill_wl"
  # removesinks_dist=20
  # pourpoint_snap_dist=20
  # overwrite=TRUE

  ## watershed delineation ----
  # dem_raster_file <- tempfile(pattern="dem_", tmpdir=workingfolder, fileext=".tif")
  dem_file <- bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE) # file.path(workingfolder, "bb_dem.tif")

  if (!file.exists(dem_file)) {
    stop("need to pre-process dem prior to hand calculations")
  }

  if (!bbopt$use_euclidean) {
    demcond_file <- bb_get_demcondraster(workingfolder = workingfolder, returnobject = FALSE)

    # remove sinks
    if (removesinks_method == "skip") {
      file.copy(dem_file, demcond_file)
    } else {
      bb_wbt_removesinks(dem_file, demcond_file,
                         method = removesinks_method, dist=bbopt$removesinks_dist)
    }
    message(sprintf("DEM conditioned with sink removal method %s",removesinks_method))

    flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)

  } else {

    # using euclidean distance rasters

    # process on the fly if not created in advance
    if (!file.exists(bb_get_euclideandistraster(bbopt$workingfolder,returnobject = FALSE))) {
      bb_preprocess_euclideandist(bbopt$workingfolder)  # use default fill method for euclidean dist
    }

    demcond_file <- bb_get_euclideandistcondraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_acc_file <- bb_get_euclideandistflowaccraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_dir_file <- bb_get_euclideandistflowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  }

  # get pour points and snap to high accumulation
  if (is.null(rivershp) ) {
    rivershp <- bb_get_rivershp(workingfolder)
  }
  demres <- bb_get_demres(bbopt)
  snapped_pourpoint_file <- bb_get_snappedpourpointshand(workingfolder = workingfolder, returnobject = FALSE)
  pourpoint_file <- bb_get_pourpointshand(workingfolder = workingfolder, returnobject = FALSE)
  pourpoints <- bb_sample_linepoints_multiple(lineshp = rivershp, pointdist = bbopt$sample_linepoints_dist,
                                              firstpoint = FALSE, lastpoint = FALSE,
                                              min_end_offset = demres,
                                              add_info_cols = c("reachID")) # keep reachID for interp in postproc
  pourpoints <- dplyr::distinct(pourpoints)

  # compute downID for each pourpoint, even if out of snaps
  # generally assume that flow acc is higher in downstream
  flowacc <- terra::rast(flow_acc_file) # bb_get_flowaccraster(bbopt$workingfolder, returnobject = TRUE)
  pourpoints$flowacc <- terra::extract(flowacc,pourpoints)[,2]
  rm(flowacc)
  pourpoints$downID <- NA
  for (i in 1:nrow(rivershp)) {
    temp <- pourpoints[pourpoints$reachID == rivershp$reachID[i],]
    reorder <- FALSE
    if (nrow(temp) >8) { # use average of 4 points
      if (mean(temp$flowacc[1:4],na.rm=TRUE) > mean(temp$flowacc[(nrow(temp)-3):(nrow(temp))],na.rm=TRUE)) {
        # reorder in descending order
        reorder <- TRUE
      } # else continue
    } else { # use first and last points
      if (temp$flowacc[1] > temp$flow[nrow(temp)]) {
        # reorder in descending order
        reorder <- TRUE
      } # else continue
    }
    # reorder points if needed
    if (reorder) {
      temp <- temp[order(-temp$pointid),]
      temp$pointid <- seq(min(temp$pointid),nrow(temp)+min(temp$pointid)-1)
    }
    # compute downID
    temp$downID[nrow(temp)] <- -1
    temp$downID[1:(nrow(temp)-1)] <- temp$pointid[2:nrow(temp)]
    # replace pourpoints with temp
    pourpoints <- rbind(
      temp,
      pourpoints[pourpoints$reachID != rivershp$reachID[i],]
    )
  }
  # rm(rivershp)
  # pourpoints$rchdwnID <- pourpoints$downID
  # pourpoints <- pourpoints[,-which(colnames(pourpoints)=="downID")]
  if (overwrite) {unlink(pourpoint_file)}
  sf::st_write(pourpoints, pourpoint_file)
  bb_wbt_pourpoints(pourpoint_file, flow_acc_file,
                    snapped_pourpoint_file, snap_dist = bbopt$pourpoint_snap_dist)

  ## reconcile snaps where points jump across junctions to another reachID
  # restore point coordinates to original when this happens
  # alternatively could just delete the points if they snap that far
  snapped_pourpoints <- read_sf(snapped_pourpoint_file)
  temp <- suppressWarnings(st_intersection(snapped_pourpoints,rivershp))
  temp <- temp[temp$reachID != temp$reachID.1,]
  if (nrow(temp)>0) {
    for (i in 1:nrow(temp)) {
      snapped_pourpoints[snapped_pourpoints$pointid == temp$pointid[i],]$geometry <-
        pourpoints[pourpoints$pointid == temp$pointid[i],]$geometry

    }
  }
  unlink(snapped_pourpoint_file)
  write_sf(snapped_pourpoints,snapped_pourpoint_file)
  rm(pourpoints)
  # rm(snapped_pourpoints)


  ### HAND calculations -----

  # proceed with delineation and hand raster stuff
  # get catchments (drainage basins for each stream point for HAND calculation)
  fn_catchment_ras <- bb_get_catchmentshandraster(workingfolder = workingfolder, returnobject = FALSE)
  fn_catchment_vec <- bb_get_catchmentshandshp(workingfolder = workingfolder, returnobject = FALSE)
  catchments <- bb_wbt_catchment(snapped_pourpoint_file, flow_dir_file,
                                 fn_catchment_ras, fn_catchment_vec, return_vector = TRUE)

  # rename VALUE as pointid to reduce ambiguity
  # cc <- colnames(catchments)
  # if ("VALUE" %in% cc) {colnames(catchments) <- gsub(pattern="VALUE", replacement = "pointid", x=cc)}

  ## catchments_streamnodes VALUE is the INDEX of the streamnodes pointid - updated with whitebox v2.4.0
  catchments$pointid <- snapped_pourpoints[catchments$VALUE,]$pointid

  # rasterize the snapped pour point IDs from HAND
  # note that dem is read in as raster here to support fasterize
  # dem <- raster::raster(bb_get_demraster(workingfolder, returnobject = FALSE))
  # pp_id_raster <- fasterize(sf=catchments, raster=dem, field='pointid',fun='first')
  ## terra alternative, about 5x slower it seems
  dem <- bb_get_demraster(workingfolder, returnobject = TRUE)
  pp_id_raster <- terra::rasterize(x=catchments, y=dem, field='pointid',fun='min')
  pp_id_raster_file <- bb_get_handpourpointIDraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(pp_id_raster, filename = bb_get_handpourpointIDraster(workingfolder,returnobject = FALSE), overwrite=overwrite)
  rm(pp_id_raster)

  ## get minimum DEM value in each catchment area ----
  ## catchments have IDs in the VALUE field
  # note: exactextractr and native terra::extract or raster::extract can't seem to handle 'multipart'' polygons with
  # small squares diagonally attached to other polygon bodies
  # seems to be better handled by QGIS algorithms
  tfpp <- tempfile(fileext=".shp")
  tfout <- tempfile(fileext=".shp")
  write_sf(catchments, tfpp, overwrite=TRUE)
  suppressMessages(qgis_run_algorithm(algorithm = "native:zonalstatisticsfb",
                                      INPUT=tfpp,
                                      INPUT_RASTER=bb_get_demraster(bbopt$workingfolder,returnobject = FALSE),
                                      RASTER_BAND=1,
                                      COLUMN_PREFIX='zdrain',
                                      STATISTICS=c(5),
                                      OUTPUT=tfout))
  catchment_temp <- read_sf(tfout)
  # catchments$zdrainage <- terra::extract(dem,catchments,fun="min")[,2] # exactextractr::exact_extract(x=dem, y=catchments, fun='min')
  catchments$zdrainage <- catchment_temp$zdrainmin
  rm(catchment_temp)
  sf::write_sf(catchments,fn_catchment_vec, overwrite=overwrite)

  # use rasterize to process zdrainage
  zdrainage_raster <- terra::rasterize(x=catchments, y=dem, field='zdrainage',fun='min')
  rm(catchments)
  zdrainage_raster_file <- bb_get_zdrainageraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(zdrainage_raster, filename = zdrainage_raster_file, overwrite=overwrite)

  # compute hand raster
  # hand_raster <- raster::overlay(dem,
  #                                zdrainage_raster,
  #                                fun=function(r1, r2){return(r1-r2)})
  hand_raster <- dem-zdrainage_raster

  hand_raster_file <- bb_get_handraster(workingfolder = workingfolder, returnobject = FALSE)
  writeRaster(hand_raster, filename = hand_raster_file, overwrite=overwrite)

  # add check that hand raster covers the rivershp domain
  rivershp <- bb_get_rivershp(workingfolder,returnobject = TRUE)
  temp <-
    bb_sample_linepoints_multiple(rivershp, pointdist=bbopt$sample_linepoints_dist, firstpoint=TRUE,lastpoint=TRUE) %>%
    terra::extract(hand_raster, y=.)
  if (any(is.na(temp))) {
    warning("The hand raster may not completely cover the rivershp extent, please check and re-run if needed to ensure coverage")
  }
  rm(rivershp)
  rm(temp)

  if (return_raster) {
    return(hand_raster)
  } else {
    return(TRUE)
  }
}
