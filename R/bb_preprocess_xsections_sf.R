#' @title Create cross-sections for stream nodes as sf
#'
#' @description
#' Creates cross-sections as sf objects for each stream node provided.
#'
#' @param streamnodes stream nodes (point(s) sf) at which to generate catchments
#' @param streamnodes_idcol ID column for reference to use in catchments
#' @param snap_dist distance to use in snapping streamnodes to rivershp (metres)
#' @param buffer_radius distance to use in buffering point to determine coordinates for perpendicular calculation (metres)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return \item{catchment_streamnodes}{returns catchments_streamnodes}
#
#' @details
#'
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
#' @importFrom sf st_write
#' @importFrom raster raster
#' @export bb_preprocess_xsections_sf
bb_preprocess_xsections_sf <- function(streamnodes=NULL, streamnodes_idcol="pointid",
                                        rivershp=NULL,
                                        flow_dir=NULL, flow_acc=NULL,
                                        snap_dist=0.2,
                                        buffer_radius=0.5,
                                        xsection_halflength=50,
                                        workingfolder=NULL,
                                        write_xsections=TRUE,
                                        return_xsections=TRUE) {

  # streamnodes=NULL
  # streamnodes_idcol="pointid"
  # rivershp=NULL
  # # flow_dir=NULL
  # # flow_acc=NULL
  # snap_dist=10
  # xsection_halflength=50

  ## xxx update to just use the snapped streamnodes as they are, not snap again? Want to make sure things align with catchment locations

  writeextra <- TRUE # write extra files to folder if workingfolder provided directly

  # find all items in workingfolder if provided
  if (!is.null(workingfolder)) {
    # if (is.null(flow_dir)) {
    #   flow_dir <- bb_get_flowdirraster(workingfolder = workingfolder, returnobject=FALSE)
    # }
    # if (is.null(flow_acc)) {
    #   flow_acc <- bb_get_flowaccraster(workingfolder = workingfolder, returnobject=FALSE)
    # }
    if (is.null(streamnodes)) {
      streamnodes <- bb_get_streamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject=FALSE)
    }
    if (is.null(rivershp)) {
      rivershp <- bb_get_rivershp(workingfolder = workingfolder, returnobject=FALSE)
    }

  } else {
    workingfolder <- tempdir()
    writeextra <- FALSE
  }

  # go through items and check if missing after working folder look
  if (is.null(streamnodes)) { stop("streamnodes must be provided")}
  # if (is.null(flow_dir)) { stop("flow_dir must be provided")}
  # if (is.null(flow_acc)) { stop("flow_acc must be provided")}
  if (is.null(rivershp)) { stop("rivershp must be provided")}


  # go through and check class of each item
  # if (!is.character(flow_dir)) {
  #   if ("SpatRaster" %notin% class(flow_dir)) {
  #     stop("flow_dir must be a raster object or a file path to one")
  #   } else {
  #     tf <- tempfile(fileext=".tif")
  #     writeRaster(flow_dir, tf)
  #     flow_dir <- tf
  #   }
  # } else {
  #   if (!file.exists(flow_dir)) {
  #     stop("flow_dir file path does not exist")
  #   }
  #   ## try catch for loading a raster
  # }
  #
  # if (!is.character(flow_acc)) {
  #   if ("SpatRaster" %notin% class(flow_acc)) {
  #     stop("flow_acc must be a raster object or a file path to one")
  #   } else {
  #     tf <- tempfile(fileext=".tif")
  #     writeRaster(flow_acc, tf)
  #     flow_acc <- tf
  #   }
  # } else {
  #   if (!file.exists(flow_acc)) {
  #     stop("flow_acc file path does not exist")
  #   }
  #   ## try catch for loading a raster
  # }

  ## check stream nodes
  if (!is.character(streamnodes)) {
    if ("sf" %notin% class(streamnodes)) {
      stop("streamnodes must be an sf object or a file path to one")
    } else {
      tf <- tempfile(fileext=".shp")
      write_sf(streamnodes, tf)
      streamnodes <- tf
    }
  } else {
    if (!file.exists(streamnodes)) {
      stop("streamnodes file path does not exist")
    }
    ## try catch for loading as sf
  }

  ## check rivershp
  if (!is.character(rivershp)) {
    if ("sf" %notin% class(rivershp)) {
      stop("rivershp must be an sf object or a file path to one")
    } else {
      tf <- tempfile(fileext=".shp")
      write_sf(rivershp, tf)
      rivershp <- tf
    }
  } else {
    if (!file.exists(rivershp)) {
      stop("rivershp file path does not exist")
    }
    ## try catch for loading as sf
  }


  ## snap stream nodes to flow accumulation ----
  streamnodes_file <- streamnodes
  streamnodes <- read_sf(streamnodes_file)

  rivershp_file <- rivershp
  rivershp <- read_sf(rivershp_file)

  # check units
  if (st_crs(streamnodes)$units != "m") {
    stop("streamnodes must be in projected coordinate system with units of metres (m)")
  }

  ## check reachID col in streamnodes?
  if ("reachID" %notin% colnames(streamnodes)) {
    stop("reachID is required in streamnodes")
  }

  # check if ID column exists, otherwise create one
  if (is.null(streamnodes_idcol) | streamnodes_idcol %notin% colnames(read_sf(streamnodes_file))) {
    message("streamnodes_idcol not provided or not found in streamnodes; creating one called pointid")
    streamnodes$pointid <- seq(1,nrow(streamnodes))
    streamnodes_idcol <- "pointid"
    write_sf(streamnodes, streamnodes_file, overwrite=TRUE)
  }

  # write streamnodes to workingfolder if provided
  if (writeextra) {
    write_sf(streamnodes,
             # file.path(workingfolder,"bb_streamnodes_forcatchments.shp"))
            dsn = bb_get_streamnodesforcatchmentsshp(workingfolder=workingfolder, returnobject=FALSE))
  }

  # write snapped stream nodes to file if workingfolder provided

  # if (is.null(workingfolder)) {
  #   snapped_streamnodes_file <- tempfile("snapped_streamnodes_forcatchments_", fileext = ".shp")
  # } else {
  #   snapped_streamnodes_file <- file.path(workingfolder, "bb_snapped_streamnodes_forcatchments.shp")
  # }

  ## dont use the snapped streamnodes here, instead ensure that they intersect with rivershp?
  # snapped_streamnodes_file <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder=workingfolder,
  #                                                                    returnobject=FALSE)
  #
  # snapped_streamnodes <- bb_wbt_pourpoints(streamnodes, flow_acc, streamnodes_file,
  #                                          snapped_streamnodes_file, snap_dist = snap_dist)

  ## create sections
  sectionslist <- vector("list", nrow(streamnodes))

  for (i in 1:nrow(streamnodes)) {

    # snap to rivershp where IDs match up
    pnt <- st_snap(streamnodes$geometry[i], rivershp[rivershp$reachID == streamnodes$reachID[i],]$geometry, tolerance = snap_dist)

    # clip line within small radius of point, where rivershp IDs match up
    buff <- st_buffer(x=pnt,dist=buffer_radius)
    lineclip <- suppressWarnings(sf::st_intersection(rivershp[rivershp$reachID == streamnodes$reachID[i],], buff))

    if (nrow(lineclip) ==0) {
      stop(sprintf("Could not intersect snapped point %i with rivershp, consider a larger buffer radius or smaller snap distance.",i))
    }

    # simplify algorithm, just take the endpoints and perpendicular, even if there is a junction or something
    ## assume that the reachID consistency call above resolves junction issues
    spts <- st_coordinates(lineclip)
    n1 <- spts[1,]
    n2 <- spts[nrow(spts),]
    df <- calc_perp_points(n1,n2,xsection_halflength = xsection_halflength) # ,mp=st_coordinates(pnt)

    # process points to line, join to sections sf
    newline <- st_as_sf(df, coords=c("X","Y"), crs=crs(rivershp)) %>%
      st_combine() %>%
      st_cast("LINESTRING") %>%
      # st_multilinestring() %>%
      st_as_sf()

    # set crs
    st_crs(newline) <- crs(rivershp)
    sectionslist[[i]] <- newline
  }

  sections <- streamnodes

  for (i in 1:length(sectionslist)) {
    temp <- sectionslist[[i]]
    sf::st_set_geometry(temp, "geometry")
    sections$geometry[i] <- temp$x
  }

  if (write_xsections) {
    # write sections with updates to file
    outfile <- bb_get_xsectionsfromstreamnodesshp(
      workingfolder=workingfolder,
      returnobject=FALSE)

    write_sf(sections, outfile, overwrite=TRUE)
  }

  if (return_xsections) {
    return(sections)
  } else {
    return(TRUE)
  }
}
