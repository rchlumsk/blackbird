#' @title Computes streamnode connections at boundaries and minimum HAND values
#'
#' @description
#' Determines which streamnode boundaries overlap, and what the minimum HAND values are at each boundary.
#'
#' @param bbmodel blackbird model options object
#' @param condriver sf object of the conditional river path with appropriate column attributes
#' @param snID streamnode ID of the base streamnode (i.e., streamnode that the path exists in)
#' @param condsnID streamnode ID to use for the new conditional streamnode (should be not an existing integer in the existing)
#' @param fromIDs integer vector of nodeID from which flow is transferred to the condsnID
#' @param snconndf streamnode connection data frame, which will have all references to the snID updated to condsnID if provided
#' @param write_snconndf will additionally write the updated snconndf to a separate file if \code{TRUE}
#' @return {\code{TRUE} if the file is written successfully. If snconndf is provided, the updated snconndf will be returned as well}
#'
#' @details
#' This function requires a new rivershp to mark an alternate flow path within an existing streamnode in order to process the
#' alternate, conditional streamnode properties. The output of this function is a new .bbg file that can redirected to
#' from the main .bbg file of the model to mark a conditional flow route. The route is meant to be 'conditional'
#' on new flow being redirected to this conditional flow route based on water built up at the edges of a streamnode. If
#' passed to the main model, the conditional routing will be ignored unless \code{:EnableSpillFlows} is enabled in the .bbi file.#'
#'
#' \code{condsnID} is optional, a new ID will be found from the list of current ones if not provided.#'
#'
#' If using \code{writefile}, it will immediately write out the snconndf to a bbg format file
#' without requiring the full model build to write with \code{\link{bb_write_model_files_cpp}}
#'
#' This function requires that catchments for streamnodes be delineated, as well as all precursor steps,
#' such as DEM and HAND processing.
#'
#' This function essentially re-builds the model from HAND delineation to building geometry and calculating pre-processed
#' properties, and does all of this in a temporary folder, which is purged afterwards. The main output is a single new .bbg
#' file with all of the information required for the compiled version of Blackbird to use the new conditional channel route.
#' The snconndf table is processed in a separate function, and is ideally done prior to this function to ensure that the user has a clear
#' understanding of the conditional flow routes between streamnodes.
#
#' @examples
#' # IOU examples
#'
#' @importFrom sf write_sf
#' @export bb_preprocess_conditional_streamnode
bb_preprocess_conditional_streamnode <- function(bbmodel=NULL, condriver, snID=NULL, condsnID=NULL, fromIDs=NULL) { # snconndf=NULL, write_snconndf=FALSE

  if (is.null(bbmodel)) {
    stop("bbmodel is required")
  }

  snID <- as.integer(snID)
  condsnID <- as.integer(condsnID)
  fromIDs <- as.integer(fromIDs)

  # get sdf and find new ID
  sdf <- bbmodel$bbgeo$get_streamnodeList_as_dataframe()
  if (is.null(condsnID)) {
    # create a new ID not in the original model
    condsnID <- max(sdf$nodeID) +1
  } else {
    # check the condsnID is valid and not already existing
    if (!is.integer(condsnID) | condsnID < 0) {
      stop("condsnID must be a positive integer")
    }
    if (condsnID %in% sdf$nodeID) {
      stop("condsnID found in existing streamnode list, must provide a new ID for the conditional node")
    }
  }

  # check from IDs
  if (is.null(fromIDs)) {
    stop("Requires at least one valid fromID")
  } else {
    if (any(fromIDs %notin% sdf$nodeID)) {
      stop("One or more fromIDs not found in current model configuration, check inputs and revise.")
    }
  }

  bbopt <- bbmodel$bbopt
  catchmentraster <- bb_get_catchmentsfromstreamnodesraster(workingfolder=bbopt$workingfolder,returnobject = TRUE)
  demraster <- bb_get_demraster(workingfolder=bbopt$workingfolder, returnobject = TRUE)

  ## setup temporary working folder
  tempfolder <- tempdir()

  tempbbopt <- bb_options()
  tempbbopt$workingfolder <- tempfolder
  tempbbopt$sample_linepoints_dist <- bbopt$sample_linepoints_dist
  tempbbopt$pourpoint_snap_dist <- bbopt$pourpoint_snap_dist
  tempbbopt$removesinks_dist <- bbopt$removesinks_dist
  tempbbopt$use_preproc <- TRUE # now the default

  # save dem_file to tempdir
  bb_preprocess_dem(dem=demraster, workingfolder=tempfolder)
  bb_preprocess_rivershp(rivershp=condriver, workingfolder=tempfolder)

  ## run new HAND calcs
  rivershp <- bb_get_rivershp(workingfolder=tempfolder)
  bb_preprocess_hand(bbopt=tempbbopt)

  hand <- bb_get_handraster(workingfolder=tempfolder, returnobject = TRUE)
  rm(catchmentraster)
  # rm(hand)

  ## save hand raster to local folder
  outputfilehand <- bb_get_handconditionalraster(workingfolder=bbopt$workingfolder, returnobject=FALSE)
  writeRaster(hand,outputfilehand, overwrite=TRUE)

  # copy streamnodes over
  catchments_streamnodes <- bb_get_catchmentsfromstreamnodesshp(workingfolder=bbopt$workingfolder,returnobject=TRUE)
  catchments_streamnodes <- catchments_streamnodes[catchments_streamnodes$pointid == snID,]
  catchments_streamnodes$reachID <- condriver$reachID
  # catchments_streamnodes$pointid <- condsnID
  write_sf(catchments_streamnodes,
           bb_get_catchmentsfromstreamnodesshp(workingfolder = tempbbopt$workingfolder, returnobject = FALSE))
  rm(catchments_streamnodes)

  # catchment raster
  file.copy(bb_get_catchmentsfromstreamnodesraster(workingfolder=bbopt$workingfolder, returnobject = FALSE),
            bb_get_catchmentsfromstreamnodesraster(workingfolder=tempbbopt$workingfolder, returnobject = FALSE))

  # manningsn raster
  file.copy(bb_get_manningsnraster(workingfolder=bbopt$workingfolder, returnobject = FALSE),
            bb_get_manningsnraster(workingfolder=tempbbopt$workingfolder, returnobject = FALSE))

  # slope
  file.copy(bb_get_sloperaster(workingfolder=bbopt$workingfolder, returnobject = FALSE),
          bb_get_sloperaster(workingfolder=tempbbopt$workingfolder, returnobject = FALSE))

  # copy snapped streamnodes
  snapped_streamnodes <- bb_get_snappedstreamnodesforcatchmentsshp(workingfolder = workingfolder, returnobject = TRUE)
  snapped_streamnodes <- snapped_streamnodes[snapped_streamnodes$pointid == snID, ]
  write_sf(snapped_streamnodes, bb_get_snappedstreamnodesforcatchmentsshp(workingfolder=tempbbopt$workingfolder, returnobject = FALSE))
  rm(snapped_streamnodes)

  # reach lengths
  bb_preprocess_reach_lengths(workingfolder=tempbbopt$workingfolder)

  ## follow along with Rmd
  catchmentList <- bb_preprocess_catchments_streamnodes(bbopt=tempbbopt)

  rivershp <- bb_get_rivershp(returnobject = TRUE,workingfolder = tempbbopt$workingfolder)
  myriverreachList <- bb_preprocess_riverreachlist(bbopt=tempbbopt)

  # temporary gg just for the one conditional streamnode
  ggc <- blackbird::bb_geometry$new(geomname="tempgeom",
                             streamnodeList=catchmentList,
                             rivershp=bb_get_rivershp(tempbbopt$workingfolder),
                             riverreachList = myriverreachList)

  # update geometry reach lengths (needs to be updated for network)
  ggc$compute_reach_lengths()

  # update minimum elevations in streamnodes (cross-sections)
  ggc$compute_min_elevations_all(confine_to_bankstations=FALSE)

  # calculate bed slopes for each catchment (important in hand-manning method)
  ggc$compute_bed_slope_all()

  # calculate reach properties
  tempbbopt$use_dhand <- FALSE # use HAND for conditional
  tempbbopt$Hseq <- bbopt$Hseq
  ggc$compute_preprocessing_tables(bbopt=tempbbopt,runparallel = FALSE,skip_extent_checks=TRUE)

  # check
  # ggc$check_preprocessing_tables()

  ## write the conditional values
  outputfile <- file.path(bbopt$workingfolder,"model",sprintf("%s_conditionalsn_%i.bbg", modelname, condsnID))
  fc <- file(outputfile,open='w+')

  sdf <- ggc$get_streamnodeList_as_dataframe()

  # add maps to column
  sdf$nodeID <- condsnID
  sdf$downnodeID <- snID
  sdf[sdf$nodetype=="catchment",]$nodetype <- "REACH"

  ## basic streamnode properties ----
  writeLines("## Blackbird Geometry File (.bbg)",fc)
  writeLines("# Conditional Node File",fc)
  writeLines("#",fc)
  writeLines("# note: setting the downnodeID to same as the mapsto node, makes sense to add that flow to the same node to keep the same or larger depth extent.",fc)
  writeLines("#",fc)

  # streamnodes information
  writeLines("\n:Streamnodes", fc)
  writeLines(paste(c("  :Attributes",colnames(sdf)),collapse="  "),fc)
  for (i in 1:nrow(sdf)) {
    writeLines(paste(c("    ",sdf[i,]), collapse="  "),fc)
    # xxx add precision point control specific to different attributes
  }
  writeLines(":EndStreamnodes", fc)

  ## write preproc table
  i=1
  writeLines("\n:PreprocessedHydraulicTables", fc)
  writeLines(sprintf("  :PreprocHydTable %s # streamnode nodeID", ggc$streamnodeList[[i]]$nodeID), fc)
  ggc$streamnodeList[[i]]$nodeID <- as.integer(condsnID) # update in actual geometry object?
  ggc$streamnodeList[[i]]$depthdf$nodeID <- as.integer(condsnID)
  ggc$streamnodeList[[i]]$depthdf$downnodeID <- as.integer(snID)
  depthdf <- ggc$streamnodeList[[i]]$depthdf

  if (nrow(depthdf)==0) {
    next
  }

  # to do xxx - get the basic properties into the depthdf tables during preprocessing of tables
  depthdf$nodeID <- ggc$streamnodeList[[i]]$nodeID
  depthdf$reachID <- ggc$streamnodeList[[i]]$reachID
  depthdf$downnodeID <- ggc$streamnodeList[[i]]$downnodeID  # change to snID?
  depthdf$upnodeID1 <- ggc$streamnodeList[[i]]$upnodeID1
  depthdf$upnodeID2 <- ggc$streamnodeList[[i]]$upnodeID2

  writeLines(paste(c("    :Attributes",colnames(depthdf)),collapse="  "),fc)
  for (j in 1:nrow(depthdf)) {
    writeLines(paste(c("    ",depthdf[j,]), collapse="  "),fc)
  }
  writeLines("  :EndPreprocHydTable", fc)
  writeLines(":EndPreprocessedHydraulicTables", fc)

  # conditional streamnodes information
  writeLines(sprintf("\n:ConditionalStreamnode %i", condsnID), fc)
  writeLines(sprintf("  :MapsTo %i",snID),fc)
  writeLines(sprintf("  :FromIDs %s", paste(fromIDs, collapse=" ")),fc)

  ## write HAND path
  writeLines(sprintf("  :HANDPath %s",outputfilehand),fc)
  writeLines(":EndConditionalStreamnode", fc)

  ## close file
  close(fc)

  ## clean up the temporary files
  ff <- list.files(tempfolder, full.names = TRUE, recursive = TRUE)
  unlink(ff)

  ## return cond streamnode object
  csndf <- list(
    "nodeID"=condsnID,
    "mapsto"=snID,
    "fromids"=fromIDs,
    "handpath"=outputfilehand
  )

  return(csndf)
}
