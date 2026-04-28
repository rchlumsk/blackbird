#' @title Processes rivershp to river reach list
#'
#' @description
#' Performs numerous checks and processing steps on a channel shapefile, then saves it.
#'
#' @param rivershp stream nodes (point(s) sf) at which to generate catchments
#' @param reachID_col column to use as reach IDs in rivershp
#' @param reachname_col column to use as reach names in rivershp (optional)
#'
#' @return \item{riverreachList}{returns object class list as riverreachList}
#
#' @details
#' If \code{reachname_col} is not provided, the reaches are named by "Reach <reachID_col>".
#'
#' @examples
#' library(sf)
#' rivershp <- sf::read_sf(rivershp_file <- sf::read_sf(system.file("extdata", "nithburg_river_sample_EPSG3161.shp", package="blackbird")))
#' rivershp <- bb_preprocess_riverreachlist(rivershp)
#'
#' @importFrom sf st_write st_zm
#' @export bb_preprocess_riverreachlist
bb_preprocess_riverreachlist <- function(bbopt=NULL, rivershp=NULL, reachID_col="reachID", reachname_col=NULL) {

  #  workingfolder=NULL, return_list=FALSE, overwrite=TRUE

  workingfolder <- bbopt$workingfolder

  if (!is.null(workingfolder) & is.null(rivershp)) {
    rivershp <- bb_get_rivershp(workingfolder = workingfolder, returnobject = TRUE)
  }

  if (is.null(rivershp)) {stop("rivershp is required")}
  if (is.null(reachID_col)) {stop("reachID_col is a required parameter.")}
  if (reachID_col %notin% colnames(rivershp)) {stop(sprintf("reachID_col %s not found in rivershp.",reachID_col))}

  if (!is.null(reachname_col))  {
    if (reachname_col %notin% colnames(rivershp)) {
      stop(sprintf("reachname_col %s not found in rivershp.",reachID_col))
    }
  }

  ## check length of each segment for short ones
  demres <- bb_get_demres(bbopt)

  rl <- as.numeric(st_length(rivershp))
  if (any(rl<3*demres)) {
    warning(sprintf("There were %i reach lengths found with a length less than %.2f, which is 3*DEM resolution. This may cause issues in the delineation.",
                    length(which(rl<3*demres)), demres*3))
  }

  if (!is.null(reachname_col)) {
    rivershp$reachname <- rivershp[[reachname_col]]
  } else {
    # default naming
    rivershp$reachname <- sprintf("Reach %s",rivershp[[reachID_col]])
  }

  ## check rivershp and import as sf
  if (is.character(rivershp)) {
    if (!file.exists(rivershp)) {
      stop("rivershp file path does not exist")
    } else{
      rivershp <- read_sf(rivershp)
    }
  } else if ("sf" %notin% class(rivershp)) {
      stop("rivershp must be an sf object or a file path to one")
  }


  ## create initial list of catchment objects
  myriverreachList <- list(riverreach())

  for (i in 1:nrow(rivershp)) {

    print(sprintf("Starting on row %i",i))

    ## extract single catchment geometry
    oneriver <- rivershp[i,]

    ## create catchment object
    myriverreach <- riverreach(
      reachID = as.integer(oneriver$reachID),
      reachname=rivershp$reachname,
      reachlength=bb_calc_linedist(lineshp=oneriver),
      reachshp=oneriver
    )

    myriverreachList[[i]] <- myriverreach
  }

  return(myriverreachList)
}
