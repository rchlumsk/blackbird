#' @title Generates rivershp from DEM
#'
#' @description
#' Generates a rivershp from DEM.
#'
#' @details
#' Outlet file or sf object is passed in order to identify the downstream limit of the watershed.
#' The DEM and all other required datasets are found from the working folder (such as a conditioned DEM).
#' The workingfolder is passed by bbopt.
#'
#' The outlet is only required if \code{clip_to_catchment==TRUE}.
#'
#' \code{simplify_reaches} is used to simplify reaches that are basically one reach but coded as two separate, often
#' leftover by the removal of smaller reaches that were previously formed at junction at these two longer reaches.
#' Simplifying the reaches helps to mitigate adding unnecessary streamnodes.
#'
#' @param bbopt blackbird options object
#' @param clip_to_catchment boolean whether to clip rivershp to a delineated upstream catchment from the outlet provided
#' @param outlet file path or sf object of a single point representing the downstream limit of interest
#' @param outlet_snap_dist snapping ditance for the outlet to the resulting rivershp (metres)
#' @param flowacc_threshold threshold in flow accumulation for streams to be generated
#' @param min_segment_length minimum length of line segment for river shp (in metres)
#' @param river_snap_dist snap distance to correct topology for the \code{\link{wbt_repair_stream_vector_topology}} algorithm
#' @param simplify_reaches whether to merge reaches that can be merged without changes to the network
#' @param return_shp whether to return the processed rivershp as sf object
#'
#' @return \item{rivershp}{returns generated rivershp}
#'
#' @examples
#' # IOU one example
#'
#'
#' @importFrom sf read_sf write_sf st_intersection
#' @importFrom whitebox wbt_repair_stream_vector_topology wbt_vector_stream_network_analysis
#' @export bb_preprocess_generate_rivershp
bb_preprocess_generate_rivershp <- function(bbopt=NULL,
                                   clip_to_catchment=TRUE,
                                   outlet=NULL, outlet_snap_dist=20,  flowacc_threshold=1e5,
                                   min_segment_length=100, river_snap_dist=3,
                                   simplify_reaches=TRUE,
                                   return_shp=FALSE) {

  # outlet_snap_dist=20
  # flowacc_threshold=1e5
  # min_segment_length=100
  # river_snap_dist=3
  # return_shp=FALSE

  # if (is.null(rivershp)) {stop("rivershp is required")}
  if (is.null(bbopt)) {stop("bbopt is required")}
  if (!dir.exists(bbopt$workingfolder)) {"Valid bbopt$workingfolder is required"}
  workingfolder <- bbopt$workingfolder

  if (clip_to_catchment){
    ## check outlet, convert to file path if sf provided
    if (!file.exists(outlet)) {
      stop("outlet file path does not exist")
    }
    if (!is.character(outlet)) {
      if ("sf" %notin% class(outlet)) {
        stop("outlet must be an sf object or a file path to one")
      } else {
        tf <- tempfile(fileext=".shp")
        write_sf(outlet, tf)
        outlet <- tf
      }
    } else {
      ## try catch for loading as sf
    }
  }

  if (river_snap_dist > 2*bb_get_demres(bbopt)) {
    warning(sprintf("river_snap_dist is high (%.1fm), recommend using no more than 2xdemres (%.1f) to avoid erroneous network results",
                    river_snap_dist, 2*bb_get_demres(bbopt)))
  }

  if (!file.exists(outlet)) {stop("outlet must be a valid file path")}

  ## file paths
  if (!bbopt$use_euclidean) {
    demcond_file <- bb_get_demcondraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_acc_file <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_dir_file <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  } else {
    demcond_file <- bb_get_euclideandistcondraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_acc_file <- bb_get_euclideandistflowaccraster(workingfolder = workingfolder, returnobject = FALSE)
    flow_dir_file <- bb_get_euclideandistflowdirraster(workingfolder = workingfolder, returnobject = FALSE)
  }

  ## new file paths
  tfchannels <- tempfile(fileext=".shp")
  tfrchannels <- tempfile(fileext=".tif")

  # analyze flow acc to check bounds
  flow_acc <- terra::rast(flow_acc_file)
  if (flowacc_threshold > max(flow_acc[!is.na(flow_acc)],na.rm=TRUE)) {
    stop("The flow accumulation threshold is larger than the max flow accumulation raster value, will not result in any channels.\nPlease reduce the flow threshold.")
  }

  ## generate channels

  bb_wbt_channels(flow_acc_file,flow_dir_file,tfrchannels,tfchannels,threshold=flowacc_threshold)
  rivershp <- read_sf(tfchannels)

  if (clip_to_catchment) {
    ## delineate basic watershed at outlet and remove other streams
    # snap outlet
    snapped_outlet_file <- tempfile(fileext=".shp")

    # snap outlet and check that it snapped
    bb_wbt_pourpoints(outlet, flow_acc_file,
                       snapped_outlet_file, snap_dist = outlet_snap_dist)

    snapped_outlet <- read_sf(snapped_outlet_file)

    if (!any(is.na(as.numeric(unlist(st_intersects(snapped_outlet,rivershp)))))) {
      message("Successfully snapped outlet to river")
    } else if (length(st_intersects(snapped_outlet,rivershp))>1) {
      stop("Multiple outlets found in provided file, please provide only one outlet")
    } else {
      stop("Failed to snap outlet to river shapefile, please ensure the outlet is within snap_dist of the channel.")
    }

    # generate catchment for entire watershed
    fn_catchment_ras <- tempfile(fileext=".tif")
    fn_catchment_vec <- tempfile(fileext=".shp")
    catchmentshp <- bb_wbt_catchment(snapped_outlet_file, flow_dir_file,
                                               fn_catchment_ras, fn_catchment_vec, return_vector = TRUE)

    # clip the streams vector
    if (!any(st_is_valid(catchmentshp))) {
      catchmentshp <- st_make_valid(catchmentshp)
    }
    rivershp <- suppressWarnings(sf::st_intersection(rivershp, catchmentshp))
    rivershp <- bb_drop_inconsistent_geomtypes(rivershp,"LINESTRING")
    # plot(catchmentshp$geometry)
    # plot(rivershp$geometry,col='blue',add=TRUE)
    if (file.exists(tfchannels)) {unlink(tfchannels)}
    write_sf(rivershp, dsn=tfchannels)
    rm(catchmentshp)
  }

  # repair stream topology (vector input)
  tf1 <- tempfile(fileext=".shp")

  # repair
  wbt_repair_stream_vector_topology(input=tfchannels, output=tf1, dist=river_snap_dist)

  # get vector analysis (vector input)
  tf2 <- tempfile(fileext=".shp")
  wbt_vector_stream_network_analysis(streams=tf1,output=tf2)
  # update links
  rivershp <- read_sf(tf2)
  rivershp[rivershp$DS_LINK_ID < 0,]$DS_LINK_ID <- -1  # set -99 to -1

  # remove short streams by threshold length that are not downstream of another line (query on vector shapefile)
  # note: wbt_remove_short_streams having issues
  if (!is.na(min_segment_length) & !is.null(min_segment_length) & min_segment_length!= 0) {
    rivershp$length <- st_length(rivershp)
    removeIDs <- rivershp[as.numeric(rivershp$length) < min_segment_length & rivershp$FID %notin% rivershp$DS_LINK_ID,]$FID
    message(sprintf("Removing %i streams (of %i) from rivershp based on length threshold",length(removeIDs),nrow(rivershp)))
    rivershp <- rivershp[rivershp$FID %notin% removeIDs,]
  }

  # xxx desired functionality - option to keep only the longest branch?

  # simplify reaches
  # look for reaches with a single downID that is not shared by any other reach downIDs
  if (simplify_reaches) {
    count_dsid <- rivershp %>%  dplyr::count(DS_LINK_ID)
    # if (count_dsid[count_dsid$DS_LINK_ID==-1,]$n > 1) {
    #   warning("More than one outlet found in river network, check for consistency")
    # }
    if (any(count_dsid$n > 2)) {
      stop("More than two matches to DS_ID, something has gone wrong in the generation of the rivershp")
    }
    count_dsid <- count_dsid[count_dsid$DS_LINK_ID != -1 & count_dsid$n==1,]

    if (nrow(count_dsid) > 1) {
      for (i in 1:nrow(count_dsid)) {
        jj <- which(rivershp$DS_LINK_ID == count_dsid$DS_LINK_ID[i])
        match_row <- which(rivershp$FID == rivershp$DS_LINK_ID[jj])
        rivershp$geometry[jj] <- summarise(rivershp[c(jj,match_row),])$geometry %>%  st_cast("MULTILINESTRING") %>% st_line_merge()
        rivershp$DS_LINK_ID[jj] <- rivershp$DS_LINK_ID[match_row]
        rivershp <- rivershp[-match_row,]
      }
    } else {
      message("No reaches to simplify")
    }
  }

  # write to river file
  rivershp$reachID <- rivershp$FID
  rivershp$downID <- rivershp$DS_LINK_ID
  # rivershp_file <- bb_get_genrivershp(workingfolder,returnobject=FALSE)
  rivershp_file <- bb_get_rivershp(workingfolder,returnobject=FALSE)
  rivershp <- rivershp[,c("reachID","downID","geometry")]
  if (file.exists(rivershp_file)) {
    unlink(rivershp_file)
  }
  write_sf(rivershp, dsn=rivershp_file)

  if (return_shp) {
    return(rivershp)
  } else {
    return(TRUE)
  }
}
