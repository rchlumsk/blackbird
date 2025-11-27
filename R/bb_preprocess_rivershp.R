#' @title Saves rivershp to blackbird folder after checks
#'
#' @description
#' Performs numerous checks and processing steps on a channel shapefile, then saves it.
#'
#' @param rivershp river shapefile (line(s) sf) to save
#' @param workingfolder folder to read inputs from and write outputs to for blackbird operations
#' @param return_shp whether to return the processed rivershp as sf object
#' @param min_segment_length minimum length of line segment for hyd river shp (in metres)
#' @param sample_linepoints_dist distance to use in sampling points along streamline (if \code{NULL}, DEM resolution is used)
#' @param overwrite whether to overwrite an existing file (default TRUE)
#'
#' @return \item{rivershp}{returns processed rivershp}
#
#' @details
#'
#'
#' @examples
#' library(sf)
#'
#' rivershp <- sf::read_sf(system.file("extdata", "riverline_NB.shp", package="blackbird"))
#'
#' rivershp <- bb_preprocess_rivershp(rivershp)
#'
#'
#' @importFrom sf st_write st_zm st_transform st_sf st_sfc st_cast st_length
#' @importFrom terra res
#' @export bb_preprocess_rivershp
bb_preprocess_rivershp <- function(rivershp=NULL, workingfolder=NULL, return_shp=FALSE,
                                   min_segment_length=NULL, sample_linepoints_dist=NULL,
                                   skip_geom_checks=TRUE,
                                   overwrite=TRUE) {

  if (is.null(rivershp)) {stop("rivershp is required")}
  if (is.null(workingfolder) & !return_shp) {stop("Either workingfolder must not be NULL, or return_shp must be TRUE")}

  ## check if rivershp exists
  if (is.character(rivershp)) {

    if (!file.exists(rivershp)) {
      stop("rivershp file path does not exist")
    } else{
      rivershp <- read_sf(rivershp)
    }
  } else if ("sf" %notin% class(rivershp)) {
      stop("rivershp must be an sf object or a file path to one")
  }

  # check that dem processed first
  if (!file.exists(bb_get_demraster(workingfolder = workingfolder, returnobject = FALSE))) {
    stop("dem must be pre-processed first")
  }

  # use the dem raster for processing rivershp
  dem <- bb_get_demraster(workingfolder = workingfolder,returnobject = TRUE)

  if (is.null(min_segment_length)) {
    min_segment_length <-  max(res(dem))*1.01
    message(sprintf("min_segment_length set to %.2fm based on dem dimensions",
                    min_segment_length))
  }

  # get sample_linepoints_dist if NULL, estimate from DEM resolution
  if (is.null(sample_linepoints_dist)) {
    demres <- terra::res(dem)
    if (demres[1] != demres[2]) {
      stop("DEM is not square resolution, should ideally be square (xres=yres)")
    }
    sample_linepoints_dist <- demres[1]
    # (demres[1]+demres[2])/2 # checks for dem to be square, so can simplify this
  }

  # check CRS
  if (!compareCRS(rivershp, dem)) {
    rivershp <- st_transform(rivershp, st_crs(dem))
  }

  # process dimensions and make single segment
  ## note: can't do this with multiple network, will need to just apply this within each catchment later
  # rivershp <- bb_utility_merge_lineshp(rivershp)

  # Check that rivershp is oriented pointing downstream?
  # XXX TO DO
  # can do this by changing to points, sampling elevations from DEM at each point, and checking that slope is generally decreasing


  if (!skip_geom_checks) {
    # initialize object for storing new geometries
    rivershp_geom <- st_sf(st_sfc())
    st_crs(rivershp_geom) <- st_crs(rivershp)
    st_geometry(rivershp_geom) <- "geometry"

    # candidate for parallelization? Reasonably fast anyway
    message(sprintf("Beginning rivershp processing"))
    for (i in 1:nrow(rivershp)) { #  1:5) { #
      # for (i in 1:5) { #

      if (i %% 10 == 0) {
        message(sprintf("Processing rivershp %i of %i", i, nrow(rivershp)))
      }

      rivershp_segment <- rivershp[i,]

      total_length <- st_length(rivershp_segment)
      # rivershp_segment <- bb_utility_merge_lineshp(lineshp=rivershp_segment)
      rivershp_segment <- suppressWarnings(st_cast(rivershp_segment, "LINESTRING"))

      if (nrow(rivershp_segment) > 1) {
        segment_lengths <- st_length(rivershp_segment)
        if (units(segment_lengths)$numerator != "m") {stop("please use projected units with metre lengths for input files")}
        segment_lengths <- as.numeric(segment_lengths)
        rivershp_segment <- rivershp_segment[which(segment_lengths>=min_segment_length),]
        if (nrow(rivershp_segment) >1) {

          # try to recast into a single one
          # get pour points on rivershp
          pourpoints <- bb_sample_linepoints_multiple(lineshp = rivershp_segment, pointdist = sample_linepoints_dist,
                                                      firstpoint = TRUE, lastpoint = TRUE)

          rivershp_segment <- bb_utility_recast_rivershp(snapped_pourpoints_hand = dplyr::distinct(pourpoints))
          suppressWarnings(sf::st_crs(rivershp_segment) <- crs(rivershp)) # should make a separate function for recasting and sampling in one

          segment_lengths <- as.numeric(st_length(rivershp_segment))
          rivershp_segment <- rivershp_segment[which(segment_lengths>=min_segment_length),]

          if (nrow(rivershp_segment) >1) {
            warning(sprintf("failed to recast rivershp segment on row %i of rivershp",i))
          } else {
            check_length <- st_length(rivershp_segment)
            if (as.numeric(check_length) > as.numeric(total_length)*1.01) {

              # last attempt, try to reverse the order of segments and recast again
              rivershp_segment <- rivershp[i,]
              rivershp_segment <- suppressWarnings(st_cast(rivershp_segment, "LINESTRING"))

              rivershp_segment$lineid <- seq(nrow(rivershp_segment),1)
              rivershp_segment <- rivershp_segment[order(rivershp_segment$lineid),]

              pourpoints <- bb_sample_linepoints_multiple(lineshp = rivershp_segment, pointdist = sample_linepoints_dist,
                                                          firstpoint = TRUE, lastpoint = TRUE)

              rivershp_segment <- bb_utility_recast_rivershp(snapped_pourpoints_hand = dplyr::distinct(pourpoints))
              suppressWarnings(sf::st_crs(rivershp_segment) <- crs(rivershp)) # should make a separate function for recasting and sampling in one

              segment_lengths <- as.numeric(st_length(rivershp_segment))
              rivershp_segment <- rivershp_segment[which(segment_lengths>=min_segment_length),]
              check_length <- st_length(rivershp_segment)

              if (as.numeric(check_length) > as.numeric(total_length)*1.01) {
                warning(sprintf("need to manually inspect rivershp row %i, substantial increase in total length",i))
              }
            }
          }

        }
      }
      rivershp_geom[i,]$geometry <- rivershp_segment$geometry
    }

    rivershp_new <- st_drop_geometry(rivershp)
    rivershp_new$geometry <- rivershp_geom$geometry
    st_geometry(rivershp_new) <- "geometry"
  } else {
    rivershp_new <- rivershp
  }


  # write to workingfolder if provided
  if (!is.null(workingfolder)) {
    if (!dir.exists(workingfolder)) {
      dir.create(workingfolder)
    }
    rivershp_file <- bb_get_rivershp(workingfolder = workingfolder, returnobject = FALSE)

    # clear any old rivershp (delete_dsn doesn't work well with workingfolder in path)
    # trim the .shp to clear all components of shapefile
    ffr <- bb_get_rivershp(workingfolder = workingfolder, returnobject = FALSE, include_wf=FALSE)
    list.files(path=workingfolder, sprintf("%s*",bb_substrMRight(ffr,4)), full.names = TRUE) %>%
      unlink()

    st_write(rivershp_new, rivershp_file, overwrite=overwrite)
  }

  if (return_shp) {
    return(rivershp_new)
  } else {
    return(TRUE)
  }
}
