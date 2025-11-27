#' @title Process reach length raster from streamnodes
#'
#' @description
#' Calculate the reach length raster based on streamnode locations
#'
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param dE elevation differential for generating contours (m)
#' @param raster_source raster to use in generating contours (default 'hand')
#' @param return_raster whether to return the processed dem as SpatRaster object
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return {\code{TRUE} if processed properly, or the raster if return_raster is TRUE}
#
#' @details
#' Uses elevation contour lines to determine the travel length within the catchment for a given raster cell. The
#' raster used to generate the contour lines can be either the hand raster (default) or the dem. DHAND raster use is
#' not supported here.
#'
#' Enclosed (circular) contours are detected and removed based on repeat point locations within a given contour line.
#'
#' Parameter \code{dE} is used to generate a sequence of elevation values from the range of raster values,
#' where the number of levels will be (max-min)/dE. Smaller \code{dE} will result in higher resolution representation of
#' travel paths, but longer processing time. If \code{NULL}, defaults to 1.0 (m).
#'
#' @examples
#' # xxx TO DO
#'
#'
#' @importFrom terra rast ext as.contour disagg
#' @importFrom sf st_as_sf st_cast st_intersection st_length
#' @importFrom qgisprocess qgis_run_algorithm
#' @export bb_preprocess_reach_lengths
bb_preprocess_reach_lengths <- function(workingfolder=NULL, dE=1.0, raster_source="hand", return_raster=FALSE, overwrite=TRUE) {

  # check on inputs
  if (!file.exists(bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE))) {
    stop("dem must be pre-processed first")
  }
  if (!file.exists(bb_get_rivershp(workingfolder = workingfolder, returnobject = FALSE))) {
    stop("rivershp must be pre-processed first")
  }
  if (!file.exists(bb_get_catchmentsfromstreamnodesshp(workingfolder = workingfolder, returnobject = FALSE))) {
    stop("catchment streamnodes must be generated first")
  }

  ## get rivershp
  rivershp <- bb_get_rivershp(workingfolder,returnobject = TRUE)
  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder=workingfolder,returnobject=TRUE)

  if (raster_source == "dem") {
    dem <- bb_get_demraster(workingfolder,returnobject = TRUE)
  } else if (raster_source == "hand") {
    dem <- bb_get_handraster(workingfolder,returnobject = TRUE)
  }

  demres <- res(dem)[1]
  demext <- terra::ext(dem)

  # get contour lines
  vals <- range(dem[!is.na(dem)])
  levels_contours <- seq(vals[1],vals[2],by=dE) # parameterize the contour resolution or compute from demres?
  contoursshp <- as.contour(x=dem, levels=levels_contours) %>%  terra::disagg() %>% sf::st_as_sf()
  rm(dem)

  # check for self-contained contours, remove
  check_keep <- function(x) {
    pts <- sf::st_cast(st_sfc(x),"POINT")
    if (any(duplicated(pts))) {
      return(FALSE)
    }
    return(TRUE)
  }
  contoursshp$keep <- lapply(contoursshp$geometry,check_keep) %>% unlist()
  contoursshp <- contoursshp[contoursshp$keep,]

  # intersect with catchments
  contcatch <- suppressWarnings(sf::st_intersection(contoursshp,catchments_streamnodes))

  # unpack and re-merge all multilinestrings to linestring within contcatch
  catchall <- contcatch[0,] # nice trick for initialization
  # slightly time consuming but works, replace with apply function at some point
  for (i in 1:nrow(contcatch)) {
    if (!(contcatch$geometry[i] %>% sf::st_is("LINESTRING"))) {
      temp <- contcatch[i,] %>% st_as_sfc() %>% st_cast("LINESTRING") %>% st_as_sf()
      contcatch[i,]$geometry <- temp[1,]$x
      catchall <- rbind(catchall,temp[2:nrow(temp),])
    }
  }

  catchall$geometry <- catchall$x
  catchall$reachID <- 1
  catchall <- sf::st_set_geometry(catchall,"geometry")

  contcatch <- rbind(contcatch[,c("reachID","geometry")],catchall[,c("reachID","geometry")])

  # compute lengths
  contcatch$length <- as.numeric(st_length(contcatch))

  # split river and compute length of river in each catchment
  splitriverff <- tempfile(fileext=".shp")
  suppressMessages(qgis_run_algorithm(algorithm = "native:intersection",
                                      INPUT=bb_get_rivershp(workingfolder,returnobject = FALSE),
                                      OVERLAY=bb_get_catchmentsfromstreamnodesshp(workingfolder=workingfolder,returnobject=FALSE),
                                      OUTPUT=splitriverff))
  splitriver <- read_sf(splitriverff)
  splitriver$length <- as.numeric(st_length(splitriver))

  # add in splitriver (rivershp lines) as a 'contour'
  contcatch <- rbind(contcatch[,c("length","geometry")], splitriver[c("length","geometry")])

  ## testing - write out and check
  # good point here for manual intervention and editing of contcatch if needed
  # write_sf(contcatch,"contcatch_temp.shp", overwrite=TRUE)
  # contcatch <- read_sf("contcatch_temp.shp")

  # interpolate from contours using TIN interpolation
  tfpp <- tempfile(fileext=".shp")
  write_sf(contcatch, tfpp, overwrite=TRUE)
  tfpp <- paste0(tfpp,"::~::0::~::0::~::0") # qgis encoding to the path length data as interpolation data
  tfidw <- bb_get_reachlengthraster(workingfolder,returnobject=FALSE)
  if (overwrite) {
    unlink(tfidw)
  }
  suppressMessages(qgis_run_algorithm(algorithm = "qgis:tininterpolation",
                                      INTERPOLATION_DATA=tfpp,
                                      EXTENT=demext,
                                      PIXEL_SIZE=demres,
                                      OUTPUT=tfidw))

  if (return_raster) {
    reach_length_raster <- bb_get_reachlengthraster(workingfolder,returnobject=TRUE)
    return(reach_length_raster)
  } else {
    return(TRUE)
  }
}
