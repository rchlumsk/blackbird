#' @title Create cross-sections as stream nodes
#'
#' @description
#' Converts cross-sections as sf objects to blackbird \code{xsection} class objects.
#'
#' @details
#' Assumes that sections will use a vector of Manning's n values sampled from nraster, rather than specifying
#' bank stations and using a single LOB/ROB/Channel Manning's n value.
#'
#' @param streamnodes stream nodes (point(s) sf) at which xsections were generated
#' @param xsections_streamnodes xsection sf objects
#' @param rivershp the river shapefile (or file path) for the main channel
#' @param dem Digital Elevation Model (DEM) as raster (or file path to one)
#' @param nraster a raster of Mannings n values (or file path to one)
#' @param pointdist distance between sampled points in cross-section (metres, determined from dem resolution if NULL)
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#'
#' @return a list of \code{xsection} class objects in blackbird streamnode format
#
#' @details
#' to do xxx
#'
#'
#' @examples
#'
#' # to do
#'
#' @importFrom sf st_write
#' @importFrom raster raster
#' @export bb_preprocess_xsections_streamnodes
bb_preprocess_xsections_streamnodes <- function(
       streamnodes=NULL,
       xsections_streamnodes=NULL,
       rivershp=NULL,
       dem=NULL,
       # demcond=NULL,
       nraster=NULL,
       pointdist=NULL,
       # hand=NULL,
       workingfolder=NULL) {

  # XXX to do - consider splitting out processes for sampling elev/nraster and creating xsections object

  writeextra <- TRUE

  # find all items in workingfolder if provided
  if (!is.null(workingfolder)) {

    if (is.null(streamnodes)) {
      streamnodes <- bb_get_streamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = FALSE)
    }

    if (is.null(xsections_streamnodes)) {
      xsections_streamnodes <- bb_get_xsectionsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE)
    }

    if (is.null(rivershp)) {
      rivershp <- bb_get_rivershp(workingfolder=workingfolder, returnobject=FALSE)
    }

    if (is.null(dem)) {
      dem <- bb_get_demraster(workingfolder=workingfolder, returnobject=FALSE)
    }

    if (is.null(nraster)) {
      nraster <- bb_get_manningsnraster(workingfolder = workingfolder, returnobject = FALSE)
    }

  } else {
    workingfolder <- tempdir()
    writeextra <- FALSE
  }

  # go through items and check
  if (is.null(xsections_streamnodes)) { stop("xsections_streamnodes must be provided")}
  if (is.null(streamnodes)) { stop("streamnodes must be provided")}
  if (is.null(rivershp)) { stop("rivershp must be provided")}
  if (is.null(dem)) { stop("dem must be provided")}
  # if (is.null(demcond)) { stop("demcond must be provided")}
  if (is.null(nraster)) { stop("nraster must be provided")}
  # if (is.null(hand)) { stop("hand must be provided")}

  # check that items are appropriate classes, convert to file path
  if (!is.character(xsections_streamnodes)) {
    if ("sf" %notin% class(xsections_streamnodes)) {
      stop("xsections_streamnodes must be an sf object or a file path to one")
    } else {
      tf <- tempfile(fileext=".shp")
      write_sf(xsections_streamnodes, tf)
      xsections_streamnodes <- tf
    }
  } else {
    if (!file.exists(xsections_streamnodes)) {
      stop("xsections_streamnodes file path does not exist")
    }
    ## try catch for loading as sf
  }

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

  if (!is.character(nraster)) {
    if ("SpatRaster" %notin% class(nraster)) {
      stop("nraster must be a raster object or a file path to one")
    } else {
      tf <- tempfile(fileext=".tif")
      writeRaster(nraster, tf)
      nraster <- tf
    }
  } else {
    if (!file.exists(nraster)) {
      stop("nraster file path does not exist")
    }
    ## try catch for loading a raster
  }

  ## create initial list of catchment objects
  myxsectionList <- list(xsection())

  ## to do xxx - convert this to lapply with a defined processing function

  # load xsections and streamnodes in as sf
  xsections_streamnodes <- read_sf(xsections_streamnodes)
  streamnodes <- read_sf(streamnodes)
  rivershp <- read_sf(rivershp)
  dem <- raster::raster(dem)
  nraster <- raster::raster(nraster)

  ## check that streamnodes and xsection_streamnodes have the same number
  if (nrow(streamnodes) != nrow(xsections_streamnodes)) {
    stop("Expect same number of streamnodes and xsections")
  }

  # get dem resolution and assign to pointdist if NULL
  if (is.null(pointdist)) {
    pointdist <- max(res(dem))
  }

  # setup sdf like df for storing reach lengths and such
  ### to do xsections - may need to add some here for left and right lengths
  ## and also for catchments, this was just skipped in developing catchment lengths
  sdf <- data.frame(matrix(-99,nrow=nrow(xsections_streamnodes),ncol=8))
  colnames(sdf) <- c("stationname","station","ds_reach_length","us_reach_length","nodeID","downnodeID","upnodeID1","upnodeID2")

  for (i in 1:nrow(xsections_streamnodes)) {

    print(sprintf("Starting on row %i",i))

    ## extract single catchment geometry
    onexsec <- xsections_streamnodes[i,]
    onenode <- streamnodes[streamnodes$pointid == onexsec$pointid,]
    xsection_stationname <- sprintf("xsection_%03d",onexsec$pointid)

    # create point samples on cross-section
    ## include first and last point to ensure the entire xsection length is represented, even if duplicates points
    section_points <- bb_sample_linepoints_multiple(lineshp=onexsec, pointdist=pointdist, firstpoint=TRUE,lastpoint=TRUE)

    # sample dem values
    section_points$zz <- raster::extract(dem, section_points)

    # sample nraster values
    section_points$nn <- raster::extract(nraster, section_points)

    # calculate xx station
    section_points$xx[1] <- 0
    p1 <- section_points[c(1:(nrow(section_points)-1)),]
    p2 <- section_points[c(2:(nrow(section_points))),]
    section_points$xx[2:nrow(section_points)]  <- cumsum(dist_projected(p1,p2))

    sdf$nodeID[i] =  as.integer(onenode$pointid)
    sdf$downnodeID[i] = as.integer(onenode$downid)
    sdf$upnodeID1[i] = as.integer(-1)
    sdf$upnodeID2[i] = as.integer(-1)

    ## create xsection object
    myxsection <- xsection(
      nodeID = as.integer(onenode$pointid),
      nodetype="xsection",
      downnodeID = as.integer(onenode$downid),
      upnodeID1 = as.integer(-1),
      upnodeID2 = as.integer(-1),
      catchmentnode = onenode,
      reachID = as.integer(onexsec$reachID),
      xsectionshp = bb_get_xsectionsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE),
      xx=section_points$xx,
      zz=section_points$zz,
      Manning=section_points$nn,
      ds_length_LOB=-99,
      ds_length_ROB=-99,
      lbs_xx=-1,
      rbs_xx=-1
    )

    myxsection$stationname <- xsection_stationname
    myxsectionList[[i]] <- myxsection
  }

  # update all upnodeIDs
  for (i in 1:nrow(sdf)) {
      upIDs <- sdf[sdf$downnodeID == sdf$nodeID[i],]$nodeID

      if (length(upIDs) == 0) {
        sdf$upnodeID1[i] = -1
        sdf$upnodeID2[i] = -1
      } else if (length(upIDs) == 1) {
        sdf$upnodeID1[i] = upIDs
        sdf$upnodeID2[i] = -1
      } else if (length(upIDs) == 2) {
        sdf$upnodeID1[i] = upIDs[1]
        sdf$upnodeID2[i] = upIDs[2]
      } else {
        stop(sprintf("incorrect number of upstream IDs found in streamnode %i",i))
      }

      myxsectionList[[i]]$upnodeID1 <- as.integer(sdf$upnodeID1[i])
      myxsectionList[[i]]$upnodeID2 <- as.integer(sdf$upnodeID2[i])
  }


  # check that xsection shp is written
  if (!file.exists(bb_get_xsectionsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE))) {
    write_sf(xsections_streamnodes,
             bb_get_xsectionsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE),
             overwrite=TRUE)
  }

  print("note: reach lengths need to be updated in geometry object")

  ## save as .rdata to working directory
  if (writeextra) {
    saveRDS(myxsectionList,
         file = bb_get_xsectionlistrdata(workingfolder=workingfolder, returnobject=FALSE))
  }

  return(myxsectionList)
}
