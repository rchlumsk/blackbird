#' @title Create catchments as stream nodes
#'
#' @description
#' Converts catchments as sf objects to blackbird \code{catchment} class objects.
#'
#' @param snapped_streamnodes snapped stream nodes (point(s) sf) at which catchments were generated
#' @param catchment_streamnodes catchments sf object
#' @param catchments_raster catchments raster object
#' @param rivershp the river shapefile (or file path) for the main channel
#' @param dem Digital Elevation Model (DEM) as raster (or file path to one)
#' @param nraster a raster of Mannings n values (or file path to one)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return a list of \code{catchment} class objects in blackbird streamnode format
#
#' @details
#' to do xxx
#'
#'
#' @examples
#' library(raster)
#' library(sf)
#'
#' # xxx to do / redo this example with snapped_streamnodes and rivershp
#'
#' dem <- raster(system.file("extdata", "DEM_NB_10m_clipped.tif", package="blackbird"))
#'
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#' rivershp <- st_zm(rivershp)
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
#' nraster <- system.file("extdata", "nraster_0.06_NB.tif", package="blackbird")
#'
#' catchmentList <- bb_preprocess_catchments_streamnodes(streamnodes=streamnodes,
#'     catchments_streamnodes = catchments_streamnodes,
#'     nraster=nraster,
#'     workingfolder = myworkingfolder)
#'
#' @importFrom sf st_write
#' @importFrom raster raster
#' @export bb_preprocess_catchments_streamnodes
bb_preprocess_catchments_streamnodes <- function(
       snapped_streamnodes=NULL,
       catchments_streamnodes=NULL,
       catchments_raster=NULL,
       dem_raster=NULL,
       hand_raster=NULL,
       rivershp=NULL,
       dem=NULL,
       manningsn_raster=NULL,
       bbopt=NULL) {

  # snapped_streamnodes=NULL
  # catchments_streamnodes=NULL
  # catchments_raster=NULL
  # dem_raster=NULL
  # hand_raster=NULL
  # rivershp=NULL
  # dem=NULL
  # # demcond=NULL,
  # manningsn_raster=NULL

  if (is.null(bbopt)) {stop("bbopt is required")}
  if (is.null(bbopt$workingfolder) | bbopt$workingfolder == "") {
    stop("bbopt$workingfolder must be provided")
  }

  workingfolder <- bbopt$workingfolder

  writeextra <- TRUE

  # find all items in workingfolder if provided
  if (!is.null(workingfolder)) {

    if (is.null(snapped_streamnodes)) {
      # streamnodes <- bb_get_streamnodesforcatchmentsshp(workingfolder=workingfolder,returnobject=FALSE)
      snapped_streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
    }

    if (is.null(catchments_streamnodes)) {
      catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE)
    }

    if (is.null(catchments_raster)) {
      catchments_raster <- bb_get_catchmentsfromstreamnodesraster(workingfolder=workingfolder,returnobject=FALSE)
    }

    if (is.null(dem_raster)) {
      dem_raster <- bb_get_demraster(workingfolder=workingfolder,returnobject=FALSE)
    }

    if (is.null(hand_raster)) {
      hand_raster <- bb_get_handraster(workingfolder=workingfolder,returnobject=FALSE)
    }

    if (is.null(rivershp)) {
      rivershp <- bb_get_rivershp(workingfolder=workingfolder, returnobject=FALSE)
    }

    if (is.null(dem)) {
      dem <- bb_get_demraster(workingfolder=workingfolder, returnobject=FALSE)
    }

    if (is.null(manningsn_raster)) {
      manningsn_raster <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = FALSE)
    }

  } else {
    workingfolder <- tempdir()
    writeextra <- FALSE
  }

  # go through items and check
  if (is.null(catchments_streamnodes)) { stop("catchments_streamnodes must be provided")}
  if (is.null(catchments_raster)) { stop("catchments_raster must be provided")}
  if (is.null(dem_raster)) { stop("dem_raster must be provided")}
  if (is.null(hand_raster)) { stop("hand_raster must be provided")}
  if (is.null(snapped_streamnodes)) { stop("snapped_streamnodes must be provided")}
  if (is.null(rivershp)) { stop("rivershp must be provided")}
  if (is.null(manningsn_raster)) { stop("manningsn_raster must be provided")}

  # check that items are appropriate classes, convert to file path
  if (!is.character(catchments_streamnodes)) {
    if ("sf" %notin% class(catchments_streamnodes)) {
      stop("catchments_streamnodes must be an sf object or a file path to one")
    } else {
      tf <- tempfile(fileext=".shp")
      write_sf(catchments_streamnodes, tf)
      catchments_streamnodes <- tf
    }
  } else {
    if (!file.exists(catchments_streamnodes)) {
      stop("catchments_streamnodes file path does not exist")
    }
    ## try catch for loading as sf
  }

  if (!is.character(catchments_raster)) {
    if ("SpatRaster" %notin% class(catchments_raster)) {
      stop("catchments_raster must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(catchments_raster, tf)
      catchments_raster <- tf
    }
  } else {
    if (!file.exists(catchments_raster)) {
      stop("catchments_raster file path does not exist")
    }
    ## try catch for loading a raster
  }

  if (!is.character(dem_raster)) {
    if ("SpatRaster" %notin% class(dem_raster)) {
      stop("dem_raster must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(dem_raster, tf)
      dem_raster <- tf
    }
  } else {
    if (!file.exists(dem_raster)) {
      stop("dem_raster file path does not exist")
    }
    ## try catch for loading a raster
  }

  if (!is.character(hand_raster)) {
    if ("SpatRaster" %notin% class(hand_raster)) {
      stop("hand_raster must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(hand_raster, tf)
      hand_raster <- tf
    }
  } else {
    if (!file.exists(hand_raster)) {
      stop("hand_raster file path does not exist")
    }
    ## try catch for loading a raster
  }

  if (!is.character(snapped_streamnodes)) {
    if ("sf" %notin% class(snapped_streamnodes)) {
      stop("snapped_streamnodes must be an sf object or a file path to one")
    } else {
      tf <- tempfile(fileext=".shp")
      write_sf(snapped_streamnodes, tf)
      snapped_streamnodes <- tf
    }
  } else {
    if (!file.exists(snapped_streamnodes)) {
      stop("snapped_streamnodes file path does not exist")
    }
    ## try catch for loading as sf
  }

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

  if (!is.character(dem)) {
    if ("SpatRaster" %notin% class(dem)) {
      stop("dem must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(dem, tf)
      dem <- tf
    }
  } else {
    if (!file.exists(dem)) {
      stop("dem file path does not exist")
    }
    ## try catch for loading a raster
  }

  if (!is.character(manningsn_raster)) {
    if ("SpatRaster" %notin% class(manningsn_raster)) {
      stop("manningsn_raster must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(manningsn_raster, tf)
      manningsn_raster <- tf
    }
  } else {
    if (!file.exists(manningsn_raster)) {
      stop("manningsn_raster file path does not exist")
    }
    ## try catch for loading a raster
  }

  ## convert this to lapply with a defined processing function

  # load catchment and snapped_streamnodes in as sf
  catchments_streamnodes <- read_sf(catchments_streamnodes)
  snapped_streamnodes <- read_sf(snapped_streamnodes)
  rivershp <- read_sf(rivershp)

  ### separate spp exercise to get attributes from streamnodes here
  ## intersect the spp with catchments_streamnodes, write back in
  spp <- bb_get_snappedpourpointshand(bbopt$workingfolder,returnobject = TRUE)
  spp <- suppressWarnings(st_intersection(spp, catchments_streamnodes))
  spp$cpointid <- spp$pointid.1
  spp$cdownid <- spp$downid

  # update to get elevations
  dem <- bb_get_demraster(bbopt$workingfolder,returnobject = TRUE)
  spp$elev <- terra::extract(x=dem, y=spp)[,2]
  rm(dem)

  spp <- spp[,c("pointid","downID","reachID","cpointid","cdownid","elev","geometry")]
  write_sf(spp, bb_get_snappedpourpointshand(bbopt$workingfolder, returnobject = FALSE), overwrite=TRUE)

  ## create initial list of catchment objects
  mycatchmentList <- list(catchment())

  for (i in 1:nrow(catchments_streamnodes)) {

    if (i %% nrow(catchments_streamnodes)/10 == 0 | i == 1) {
      print(sprintf("Starting on row %i of %i",i,nrow(catchments_streamnodes)))
    }

    ## extract single catchment geometry
    onecatch <- catchments_streamnodes[i,]
    # onenode <- snapped_streamnodes[snapped_streamnodes$pointid == onecatch$pointid,]

    # get full river shp (even if junction)
    oneriver <- suppressWarnings(sf::st_intersection(rivershp, onecatch$geometry)) %>%
      st_as_sf()
    st_geometry(oneriver) <- "geometry"

    # handle geometry collections in oneriver
    if ("GEOMETRYCOLLECTION" %in% (lapply(oneriver$geometry,class) %>% unlist())) {
      for (j in 1:nrow(oneriver)) {
        if ("sfc_GEOMETRYCOLLECTION" %in% class(oneriver$geometry[j])) {
          newline <- st_collection_extract(oneriver$geometry[j],"LINESTRING") %>% st_as_sf() %>%
            group_by() %>%
            summarise()
          oneriver$geometry[j] <- newline$x
          rm(newline)
        }
      }
    }

    ## determine upnodeIDs and whether it is a junction
    isjunction <- FALSE
    upnodeID1 = -1
    upnodeID2 = -1
    upIDs <- catchments_streamnodes[which(catchments_streamnodes$downid == catchments_streamnodes$pointid[i]),]$pointid

    if (length(upIDs) == 0) {
      upnodeID1 = -1
      upnodeID2 = -1
    } else if (length(upIDs) == 1) {
      upnodeID1 = upIDs
      upnodeID2 = -1
    } else if (length(upIDs) == 2) {
      upnodeID1 = upIDs[1]
      upnodeID2 = upIDs[2]
      isjunction <- TRUE
    } else {
      stop(sprintf("incorrect number of upstream IDs found in streamnode nodeID= %i\n",catchments_streamnodes$pointid[i]))
    }

    # compute lengths
    us_reach_length1 <- -99
    us_reach_length2 <- -99
    if (!isjunction) {
      us_reach_length1 <- as.numeric(sum(st_length(oneriver)))
    } else {
      # junction - compute both lengths
      oneriver$length <- st_length(oneriver)
      us_reach_length1 <- as.numeric(sum(oneriver[oneriver$reachID == onecatch$reachID,]$length)) +
        as.numeric(sum(oneriver[oneriver$reachID == catchments_streamnodes[catchments_streamnodes$pointid==upIDs[1],]$reachID,]$length))/2
      us_reach_length2 <- as.numeric(sum(oneriver[oneriver$reachID == onecatch$reachID,]$length)) +
        as.numeric(sum(oneriver[oneriver$reachID == catchments_streamnodes[catchments_streamnodes$pointid==upIDs[2],]$reachID,]$length))/2
    }

    catchments_streamnodes[catchments_streamnodes$pointid==upIDs[1],]$reachID

    # if length is less than demres, set to desmres and issue warning
    demres <- bb_get_demres(bbopt)
    if (us_reach_length1 < demres & us_reach_length1 != -99 ) {
      warning(sprintf("us_reach_length1 at catchment with pointID %i was found to be %.2f, less than demres of %.2f, setting to demres",
                      catchments_streamnodes$pointid[i], us_reach_length1, demres))
      us_reach_length1 <- demres
    }
    if (us_reach_length2 < demres & us_reach_length2 != -99 ) {
      warning(sprintf("us_reach_length2 at catchment with pointID %i was found to be %.2f, less than demres of %.2f, setting to demres",
                      catchments_streamnodes$pointid[i], us_reach_length2, demres))
      us_reach_length2 <- demres
    }

    ## create catchment object
    mycatchment <- catchment(
      # dem = demclip,
      nodeID = as.integer(catchments_streamnodes$pointid[i]),
      nodetype="catchment",
      stationname <- sprintf("catch_%03d",catchments_streamnodes$pointid[i]),
      downnodeID = as.integer(catchments_streamnodes$downid[i]),
      upnodeID1 = as.integer(upnodeID1),
      upnodeID2 = as.integer(upnodeID2),
      # catchmentshp = onecatch,
      # catchmentnode = onenode,
      # reachshp = oneriver,
      reachID = as.integer(catchments_streamnodes$reachID[i]),
      isjunction=isjunction,
      us_reach_length1=us_reach_length1,
      us_reach_length2=us_reach_length2,
      catchmentraster=catchments_raster,  # move to geometry property
      demraster = dem_raster,# move to geometry property
      handraster = hand_raster,# move to geometry property
      nraster = manningsn_raster# move to geometry property
      # hand = handclip,
      # nraster = nrasterclip
    )

    mycatchmentList[[i]] <- mycatchment
  }

  message("catchmentList prepared successfully")
  message("Downstream reach lengths need to be updated in geometry object using geometry::compute_reach_lengths()")

  ## save as .rdata to working directory
  if (writeextra) {
    saveRDS(mycatchmentList,
         file = bb_get_catchmentlistrdata(workingfolder=workingfolder, returnobject=FALSE))
  }

  return(mycatchmentList)
}
