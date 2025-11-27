#' @title Preprocess step to prepare the dynamic HAND rasters
#'
#' @description
#' Creates the Dynamic Height Above Nearest Drainage (DHAND) raster set.
#'
#' @param workingfolder folder to write secondary spatial outputs to
#' @param bbopt blackbird options class object (used to define the dhand depths)
#' @param removesinks_method method to use in the sink removal algorihtm for dem condioning
#' @param overwrite if \code{TRUE}, will overwrite any written files
#'
#' @return \item{\code{TRUE}}{when dhand rasters written to file}
#' \item{misc}{other spatial objects may be written to file, such as the
#' intermediate drainage basins, the zdrainage layer, sampled points along the rivershp, etc.}
#
#' @details
#' General procedure for this function is as follows:
#'     - TO DO
#'
#'
#' Note, other parameters to add (e.g. spacing in generating stream points, snapping distance for pourpoints, etc).
#'
#'
#' \code{removesinks_method} can be one of \code{breach_leastcost}, \code{breach}, \code{fill},
#' \code{fill_pd}, and \code{fill_wl}. Refer to \code{\link{bb_wbt_removesinks}} for more details.
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
#' @importFrom sf st_zm st_write st_length
#' @importFrom exactextractr exact_extract
#' @importFrom terra res lapp writeRaster rast
#' @importFrom dplyr distinct
#' @export bb_preprocess_dhand
bb_preprocess_dhand <- function(workingfolder=NULL,
                                bbopt=NULL,
                                removesinks_method="fill_wl",
                                # removesinks_dist=NULL,
                                overwrite=TRUE) {

  # removesinks_method="fill_wl"
  # # removesinks_dist=20
  # # pourpoint_snap_dist=20
  # overwrite=TRUE

  if (removesinks_method != "fill_wl") {
    warning("Highly recommend using fill_wl as the filling method, otherwise ensure this is at least a fill method and not a breach method")
  }

  if (is.null(bbopt) | "bb_options" %notin% class(bbopt)) {
    stop("bb_options class object bbopt is required")
  }

  if (bbopt$removesinks_dist < 0) {
    stop("bbopt$removesinks_dist must be > 0")
  }

  # find items if hand already processed
  hand_file <- bb_get_handraster(workingfolder = workingfolder, returnobject = FALSE)
  if (!file.exists(hand_file)) {
    stop("You must process the HAND files with bb_preprocess_hand before processing DHAND")
  }

  # file paths
  # demcond_file <- bb_get_demcondraster(workingfolder = workingfolder, returnobject = FALSE)
  # flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  # flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject = FALSE)
  snapped_pourpoint_file <- bb_get_snappedpourpointshand(workingfolder = workingfolder, returnobject = FALSE)
  snapped_pourpoints <- bb_get_snappedpourpointshand(workingfolder = workingfolder, returnobject = TRUE)

  # live rasters
  dem <- bb_get_demraster(workingfolder = workingfolder, returnobject = TRUE)
  hand_raster <- bb_get_handraster(workingfolder = workingfolder, returnobject = TRUE)
  hand_id_raster <- bb_get_handpourpointIDraster(workingfolder = workingfolder, returnobject = TRUE)
  zdrainage_raster <- bb_get_zdrainageraster(workingfolder = workingfolder, returnobject = TRUE)

  # if (is.null(sample_linepoints_dist) | is.null(removesinks_dist)) {
  #   demres <- raster::res(dem)
  #   if (demres[1] != demres[2]) {
  #     stop("DEM is not square resolution, should ideally be square (xres=yres)")
  #   }
  # }
  # if distances are NULL, estimate from DEM resolution
  # if (is.null(sample_linepoints_dist)) {
  #   # default, sample pour points equal to DEM resolution
  #   sample_linepoints_dist <- demres[1]
  #   # (demres[1]+demres[2])/2 # checks for dem to be square, so can simplify this
  # }
  # if (is.null(removesinks_dist)) {
  #   # default, looks for sinks up to 5x dem resolution
  #   demres <- res(dem)
  #   removesinks_dist <- demres[1]*5
  # }

  # directory for temporary and final files
  if (!dir.exists(file.path(workingfolder, "dhand"))){
    dir.create(file.path(workingfolder, "dhand"))
  }

  # make breached DEM
  dem_file <- bb_get_demraster(workingfolder,returnobject = FALSE)
  breachdem_file <- bb_get_dembreachraster(workingfolder,returnobject=FALSE)
  bb_wbt_removesinks(dem_file, breachdem_file,
                     method = "breach_leastcost", dist=bbopt$removesinks_dist)
  breachdem <- terra::rast(breachdem_file)



  ### Dynamic HAND (DHAND) calculations -----

  ## initialize from HAND results (dhand with depth=0)

  # dhand_raster_file <- sprintf("%s/dhand/bb_dhand_depth_%0.4fm.tif",workingfolder,0)
  dhand_raster_file <- bb_get_dhandraster(workingfolder,returnobject = FALSE,depth=0,filetype="depthraster")
  writeRaster(hand_raster, filename = dhand_raster_file, overwrite=overwrite)
  # dhand_id_raster_file <- sprintf("%s/dhand/bb_dhand_pourpoint_id_depth_%0.4fm.tif",workingfolder,0)
  dhand_id_raster_file <- bb_get_dhandraster(workingfolder,returnobject = FALSE,depth=0,filetype="idraster")
  writeRaster(hand_id_raster, filename = dhand_id_raster_file, overwrite=overwrite)

  message(sprintf("Depth of 0 written to file (from HAND results)"))

  # set last_dhand
  last_dhand <- hand_raster

  # sequence of depths for dynamic hand layers (zero performed already)
  # dhand_depths <- seq(0.5,5,by=1.0)
  dhand_depths <- bbopt$dhand_Hseq

  if (dhand_depths[1] == 0) {
    dhand_depths <- dhand_depths[2:length(dhand_depths)] # remove 0 depth since already handlded
  }

  for (i in 1:length(dhand_depths)) {

    message(sprintf("Processing depth of %.2f (depth %i of %i)",dhand_depths[i],i,length(dhand_depths)))

    ## process dem - shave hand area
    # set to dem    if last_dhand >= dhand_depths[i] (i.e. use dem if was not flooded at this depth from last hand)
    # REPLACED -  set to zdrain if last_dhand < dhand_depths[i] (i.e. breach to min drain elev)
    # NEW VERSION - set to breachdem if last_dhand < dhand_depths[i] (i.e., set to a breached version of DEM for more realistic watershed)
    newdem <- terra::lapp(c(last_dhand,dem,breachdem), fun=function(r1,r2,r3) {
      ifelse(!is.na(r1) & !is.na(r2) &!is.na(r3),
             ifelse(r1>=dhand_depths[i], r2, r3),
             r3) # set to breachdem if NA found
    })

    newdem_file <- file.path(workingfolder,"dhand/bb_newdem_handproc_temp.tiff")
    writeRaster(newdem, newdem_file, overwrite=TRUE)

    demcond_file <- file.path(workingfolder,"dhand/bb_demcond_handproc_temp.tiff")

    # fill pits
    bb_wbt_removesinks(newdem_file, demcond_file,
                       method = removesinks_method, dist=bbopt$removesinks_dist)

    # get flow accumulations
    flow_acc_file <- file.path(workingfolder,"dhand/bb_flowacc_handproc_temp.tiff")
    bb_wbt_flow_accumulation(demcond_file, flow_acc_file)

    # get flow directions
    flow_dir_file <- file.path(workingfolder,"dhand/bb_flowdir_handproc_temp.tiff")
    bb_wbt_flow_direction(demcond_file, flow_dir_file)

    # temporarily save the flow dir and flow acc files xxx ----
    file.copy(flow_dir_file,sprintf("bb_flowdir_dhand_depth_%.2f.tiff",dhand_depths[i]))
    file.copy(flow_acc_file,sprintf("bb_flowacc_dhand_depth_%.2f.tiff",dhand_depths[i]))
    ## ---

    # proceed with delineation and hand raster stuff
    # get catchments (drainage basins for each stream point for HAND calculation)
    fn_catchment_ras <- file.path(workingfolder,"dhand/bb_flowacc_handproc_temp.tiff")
    fn_catchment_vec <- file.path(workingfolder,"dhand/bb_flowacc_handproc_temp.shp")
    catchments <- bb_wbt_catchment(snapped_pourpoint_file, flow_dir_file,
                                   fn_catchment_ras, fn_catchment_vec, return_vector = TRUE)

    ## catchments_streamnodes VALUE is the INDEX of the streamnodes pointid - updated with whitebox v2.4.0
    catchments$pointid <- snapped_pourpoints[catchments$VALUE,]$pointid



    # temporarily save the catchment files xxx ----
    write_sf(catchments, sprintf("bb_dhand_catchments_depth_%.2f.shp",dhand_depths[i]))
    ## ---


    # rasterize the snapped pour point IDs from DHAND
    # pp_id_raster <- fasterize(sf=catchments, raster=dem, field='pointid',fun='first')
    pp_id_raster_temp <- terra::rasterize(x=catchments, y=dem, field='pointid',fun='min')

    # xxx possible switch to qgis rasterize function??

    # infill ID from HAND if DHAND ID is empty
    # done to compensate for lack of
    pp_id_raster <- terra::lapp(c(pp_id_raster_temp,hand_id_raster), fun=function(r1,r2) {
      ifelse(!is.na(r1), # if dhand id raster NA,
             r1,         # set to dhand id raster
             r2)         # else, set to hand id raster as an infill
    })

    pp_id_raster_file <- bb_get_dhandraster(workingfolder,returnobject = FALSE,depth=dhand_depths[i],filetype="idraster")
    writeRaster(pp_id_raster, filename = pp_id_raster_file, overwrite=overwrite)
    rm(pp_id_raster)


    ## get minimum DEM value in each catchment area ----
    # note: exactextractr and native terra::extract or raster::extract can't seem to handle 'multipart'' polygons with
    # small squares diagonally attached to other polygon bodies
    # seems to be better handled by QGIS algorithms
    tfpp <- tempfile(fileext=".shp")
    tfout <- tempfile(fileext=".shp")
    write_sf(catchments, tfpp, overwrite=TRUE)
    suppressMessages(qgis_run_algorithm(algorithm = "native:zonalstatisticsfb",
                                        INPUT=tfpp,
                                        INPUT_RASTER=demcond_file,
                                        RASTER_BAND=1,
                                        COLUMN_PREFIX='zdrain',
                                        STATISTICS=c(5),
                                        OUTPUT=tfout))
    catchment_temp <- read_sf(tfout)
    # catchments$zdrainage <- terra::extract(dem,catchments,fun="min")[,2] # exactextractr::exact_extract(x=dem, y=catchments, fun='min')
    catchments$zdrainage <- catchment_temp$zdrainmin
    rm(catchment_temp)
    # sf::write_sf(catchments,fn_catchment_vec, overwrite=overwrite)

    ## get minimum DEM value in each catchment area ----
    ## catchments have IDs in the VALUE field
    # demcond <- rast(demcond_file)
    # catchments$zdrainage <- exactextractr::exact_extract(x=demcond, y=catchments, fun='min')

    ## rasterize the zdrainage to raster ----
    zdrainage_raster <- terra::rasterize(catchments, y=dem, field=catchments$zdrainage)
    # zdrainage_raster_file <- bb_get_zdrainageraster(workingfolder = workingfolder, returnobject = FALSE)
    # writeRaster(zdrainage_raster, filename = zdrainage_raster_file, overwrite=overwrite)
    rm(catchments)

    ## calculate HAND as DEM - zdrainage ----
    # hand_demcond <- raster::overlay(demcond,
    #                              zdrainage_raster,
    #                              fun=function(r1, r2){
    #                                ifelse(!is.na(r1) & !is.na(r2),
    #                                       r1-r2,
    #                                       NA)})
    demcond <- rast(demcond_file)
    hand_demcond <- demcond-zdrainage_raster
    rm(demcond) # memory cleanup

    # hand_basedem <- raster::overlay(dem,
    #                              zdrainage_raster,
    #                              fun=function(r1, r2){
    #                                ifelse(!is.na(r1) & !is.na(r2),
    #                                       r1-r2,
    #                                       NA)})
    hand_basedem <- dem-zdrainage_raster

    # take old hand values where hand<dhand_depths[i] (since dem modified), take new hand values where dem not modified
    # dhand_raster <- overlay(stack(last_dhand,hand_basedem,hand_demcond),
    #                    fun=function(r1, r2, r3){
    #                      ifelse(!is.na(r1) & !is.na(r2) & !is.na(r3),
    #                           ifelse(r1<dhand_depths[i], r2, r3),NA)})
    # set vaue to hand_raster (not DHAND) where missing values found due to watershed delineation,
    #   which occurs near boundaries of DEM provided
    #   noting, that HAND will have more coverage as it breaches rather than fills
    dhand_raster <- lapp(c(last_dhand,hand_basedem,hand_demcond,hand_raster),
                         fun=function(r1,r2,r3,r4) {
                           ifelse(!is.na(r1) & !is.na(r2) & !is.na(r3),
                                  ifelse(r1<dhand_depths[i], r2, r3),
                                  r4)}) # NA or r2  # updated to set basedem (r2) if otherwise NA
                                # ifelse(r1<dhand_depths[i], ifelse(r2>0,r2,0), ifelse(r3>0,r3,0)),NA)}) # updated to set to min value of 0

    rm(hand_demcond) # memory cleanup
    rm(hand_basedem) # memory cleanup
    # dhand_raster_file <- sprintf("%s/dhand/bb_hand_depth_%0.4fm.tif",workingfolder,dhand_depths[i])
    dhand_raster_file <- bb_get_dhandraster(workingfolder,returnobject = FALSE,depth=dhand_depths[i],filetype="depthraster")
    writeRaster(dhand_raster, filename = dhand_raster_file, overwrite=overwrite)

    last_dhand <- dhand_raster
    rm(dhand_raster)
  }

  # memory cleanup
  rm(zdrainage_raster)
  rm(last_dhand)
  rm(dem)
  rm(newdem)

  # cleanup temp files
  unlink(newdem_file)
  unlink(demcond_file)
  unlink(flow_acc_file)
  unlink(flow_dir_file)
  unlink(demcond_file)
  unlink(list.files( sprintf("%s/dhand/",workingfolder), pattern="bb_flowacc_*", full.names=TRUE)) # all shapefile parts from catchment vector
  unlink(fn_catchment_ras)
  unlink(fn_catchment_vec)

  return(TRUE)
}
