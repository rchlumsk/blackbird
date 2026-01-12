#' @title Write Blackbird model input files
#'
#' @description
#' Writes model input files to Blackbird ascii forma, setup for cpp program
#'
#' @details
#' Model components that are not required for running the model and postprocessing are removed
#' (e.g. options related to preprocessing).
#'
#' @param workingfolder workingfolder
#' @param modelname prefix for model file names
#' @param bbmodel blackbird model object to write
#'
#' @return {returns \code{TRUE} if run successfully}
#
#' @examples
#' # to do
#'
#'
#' @importFrom raster raster writeRaster crs
#' @export bb_write_model_files_cpp
bb_write_model_files_cpp <- function(modelname="modelname",
                                 bbmodel=NULL) {

  if (is.null(bbmodel)) {stop("bbmodel is required")}

  geometry <- bbmodel$bbgeo
  bbopt <- bbmodel$bbopt
  flows <- bbmodel$bbfp
  bcc <- bbmodel$bbbc
  workingfolder <- bbmodel$bbopt$workingfolder

  if (is.null(geometry)) {stop("geometry is required")}
  if (is.null(bbopt)) {stop("bbopt is required")}
  if (is.null(flows)) {stop("flows is required")}
  if (is.null(bcc)) {stop("bcc is required")}

  ## write out full geometry info ----
  outputfile <- file.path(workingfolder,"model",sprintf("%s.bbg",modelname))
  # xxx replace this path with one that comes from the bb_get_object function
  if (!dir.exists(file.path(workingfolder,"model"))) {
    dir.create(file.path(workingfolder,"model"))
  }
  fc <- file(outputfile,open='w+')
  sdf <- geometry$get_streamnodeList_as_dataframe()

  # replace sdf$nodetype here for writing
  sdf[sdf$nodetype == "catchment",]$nodetype <- "REACH"
  sdf[sdf$nodetype == "xsection",]$nodetype <- "XSECTION"

  writeLines("## Blackbird Geometry File (.bbg)",fc)
  writeLines("# \n",fc)

  # streamnodes information
  writeLines("\n:Streamnodes", fc)
  writeLines(paste(c("  :Attributes",colnames(sdf)),collapse="  "),fc)
  for (i in 1:nrow(sdf)) {
    writeLines(paste(c("    ",sdf[i,]), collapse="  "),fc)
  }
  writeLines(":EndStreamnodes", fc)

  # xsection info
  if (suppressWarnings(any(lapply(geometry$streamnodeList,
           FUN=function(x)(if(x$nodetype=="xsection"){return(TRUE)} else {return(FALSE)}))))) {

    for (i in 1:geometry$get_streamnodelist_length()) { # TODO: change cross section writing

      writeLines(sprintf("\n:StreamnodeCrossSection %i", geometry$streamnodeList[[i]]$nodeID), fc)
      if (geometry$streamnodeList[[i]]$Manning_LOB != -1) {
        writeLines(sprintf("  :Manning_LOB %.4f", geometry$streamnodeList[[i]]$Manning_LOB), fc)
      }
      if (geometry$streamnodeList[[i]]$Manning_Main != -1) {
        writeLines(sprintf("  :Manning_Main %.4f", geometry$streamnodeList[[i]]$Manning_Main), fc)
      }
      if (geometry$streamnodeList[[i]]$Manning_ROB != -1) {
        writeLines(sprintf("  :Manning_ROB %.4f", geometry$streamnodeList[[i]]$Manning_ROB), fc)
      }
      if (geometry$streamnodeList[[i]]$xsectionshp != "") {
        writeLines(sprintf("  :xsectionshp %s", geometry$streamnodeList[[i]]$xsectionshp), fc)
      }

      if (length(geometry$streamnodeList[[i]]$xx)>1) {
        writeLines(paste(c("  :Attributes",c("xx","zz","Manning")),collapse="  "),fc)
        for (j in 1:length(geometry$streamnodeList[[i]]$xx)) {
          writeLines(sprintf("    %.4f %.4f %.4f",
                             geometry$streamnodeList[[i]]$xx[j],
                             geometry$streamnodeList[[i]]$zz[j],
                             geometry$streamnodeList[[i]]$Manning[j]),fc)
        }
      }
      writeLines(":EndStreamnodeCrossSection", fc)
    }
  }

  ## write out preprocessed tables ----
  geometry$write_preprocessed_depthdf(bbopt = bbopt, modelname = paste0(modelname, "_reaches"))

  # add in the redirect
  writeLines(sprintf("\n:RedirectToFile %s_reaches.bbg",modelname), fc)
  close(fc)

  ## write boundary conditions and flows ----
  outputfile <- file.path(workingfolder,"model",sprintf("%s.bbb",modelname))
  fc <- file(outputfile,open='w+')

  writeLines("## Blackbird Boundary Conditions File (.bbb)",fc)
  writeLines("# \n",fc)

  for(bc in bcc$bclist) {
    writeLines("\n:BoundaryCondition", fc)
    writeLines(sprintf("  :nodeID %s",bc$nodeID), fc)
    writeLines(sprintf("  :BCType %s",bc$bctype), fc)
    writeLines(sprintf("  :BCValue %s",bc$bcvalue), fc)
    writeLines(sprintf("  :InitialWSL %s",bc$init_WSL), fc)
    writeLines(":EndBoundaryCondition", fc)
  }

  writeLines("\n:SteadyFlows", fc)
  writeLines(paste(c("  :Attributes",colnames(flows$flowdf)),collapse="  "),fc)

  # subset headwater nodes
  ind <- which(sdf$upnodeID1 == -1)

  for (jj in ind) {
    writeLines(paste(c("    ",flows$flowdf[jj,]), collapse="  "),fc)
  }
  writeLines(":EndSteadyFlows", fc)

  ## write sources and sinks
  writeLines("\n:StreamnodeSourcesSinks", fc)
  writeLines(paste(c("  :Attributes","nodeID",rep(c("source","sink"), ncol(flows$flowdf)-1)  ),collapse="  "),fc)

  ## for all the source and sink terms that are not zero ....
  warning("script not configured to write sources and sinks, writing template that will need to be filled in manually")
  writeLines(paste(c("    ",sdf$nodeID[1],rep(c(0,0), ncol(flows$flowdf)-1)  ),collapse="  "),fc)

  writeLines(":EndStreamnodeSourcesSinks\n", fc)

  writeLines("# GlobalFlowMultiplier can be used to easily nudge flows in the model by the same multiplier", fc)
  writeLines(":GlobalFlowMultiplier   1.0", fc)

  close(fc)



  ## write main input options file ----

  outputfile <- file.path(workingfolder,"model",sprintf("%s.bbi",modelname))
  fc <- file(outputfile,open='w+')

  writeLines("## Blackbird Model Input File (.bbi)",fc)
  writeLines("# \n",fc)

  # writeLines(sprintf(":ModelName %s",modelname), fc)

  writeLines("### General Model Setup Options ----", fc)
  writeLines(sprintf(":ModelType %s",toupper(bbopt$modeltype)), fc)
  writeLines(sprintf(":RegimeType %s",toupper(bbopt$regimetype)), fc)
  # writeLines(sprintf(":gravity %s",bbopt$g), fc)
  # writeLines(paste(c(":Hseq", bbopt$Hseq), collapse=" "), fc)
  writeLines(sprintf(":Tolerance %s",bbopt$tolerance_cp), fc)
  writeLines(sprintf(":IterationLimit %s",bbopt$iteration_limit_cp), fc)
  writeLines(sprintf(":ToleranceNormalDepth %s",bbopt$tolerance_nd), fc)
  writeLines(sprintf(":IterationLimitNormalDepth %s",bbopt$iteration_limit_nd), fc)

  writeLines(sprintf(":WSLSplitNormalDepth %s",bbopt$next_WSL_split_nd), fc)
  writeLines(sprintf(":MaxRHRatio %s",bbopt$max_RHSQ_ratio), fc)
  writeLines(sprintf(":MinRHRatio %s",bbopt$min_RHSQ_ratio), fc)
  # writeLines(sprintf(":ExtrapolationMethod %s",bbopt$interp_extraplotion_method), fc)
  if (bbopt$interp_extraplotion_method == "stoponerror") {
    writeLines(":ExtrapolateDepthTable  FALSE", fc)
  } else {
    writeLines(":ExtrapolateDepthTable  TRUE", fc)
  }
  writeLines(sprintf(":NumExtrapolationPoints %s",bbopt$num_extrapolation_points), fc)
  # writeLines(sprintf(":DynamicHAND %s",bbopt$use_dhand), fc)
  writeLines(sprintf(":FrictionSlopeMethod %s",toupper(bbopt$friction_slope_method)), fc)
  writeLines(sprintf(":EnforceDeltaLeff %s",bbopt$enforce_delta_Leff), fc)
  writeLines(sprintf(":ReachLengthDelta %s",bbopt$delta_reachlength), fc)
  writeLines(sprintf(":ManningCompositeMethod %s",toupper(bbopt$Manning_composite_method)), fc)
  writeLines(sprintf(":SilentRun %s",bbopt$silent_cp), fc)  # update to merge silent_cp, silent_nd
  writeLines(sprintf(":FroudeThreshold %.4f",bbopt$Froude_threshold), fc)

  writeLines("\n### Calibration parameter ----", fc)
  writeLines(sprintf(":RoughnessMultiplier %s",bbopt$roughness_multiplier), fc)

  writeLines("\n### XSection Specific Options ----", fc)
  writeLines(sprintf(":XSectionDX %s",bbopt$dx), fc)
  writeLines(sprintf(":UseOverbankCalcs %s",bbopt$xs_use_obcalcs), fc)
  writeLines(sprintf(":XSectionConveyanceMethod %s",toupper(bbopt$xsection_conveyance_method)), fc)
  writeLines(sprintf(":ManningEnforceValues %s",bbopt$Manning_enforce_values), fc)

  writeLines("\n### Reach Specific Options ----", fc)
  writeLines(sprintf(":ReachConveyanceMethod %s",toupper(bbopt$catchment_conveyance_method)), fc)
  writeLines(sprintf(":ReachIntegrationMethod %s",toupper(bbopt$catchment_integration_method)), fc)

  writeLines("\n### PostProcessing Options ----", fc)
  writeLines(sprintf(":PostprocessingInterpolationMethod %s",gsub("-","_",toupper(bbopt$interpolation_postproc_method))), fc)
  writeLines(":GISPath GIS_files", fc)

  close(fc)

  # create a GIS files folder and copy relevant pieces over
  dir.create(file.path(workingfolder,"model","GIS_files"))

  # snapped_pourpoints_hand
  bb_get_snappedpourpointshand(bbopt$workingfolder) %>%
    write_sf(file.path(workingfolder,"model",bb_get_snappedpourpointshand(workingfolder="GIS_files", returnobject=FALSE)))
  # catchments_streamnodes shp
  bb_get_catchmentsfromstreamnodesshp(bbopt$workingfolder) %>%
    write_sf(file.path(workingfolder,"model",bb_get_catchmentsfromstreamnodesshp(workingfolder="GIS_files", returnobject=FALSE)))
  # catchments_streamnodes tif
  file.copy(bb_get_catchmentsfromstreamnodesraster(bbopt$workingfolder, returnobject = FALSE),
             file.path(workingfolder,"model",bb_get_catchmentsfromstreamnodesraster(workingfolder="GIS_files", returnobject=FALSE)))
  # bb_hand
  file.copy(bb_get_handraster(bbopt$workingfolder, returnobject = FALSE),
            file.path(workingfolder,"model",bb_get_handraster(workingfolder="GIS_files", returnobject=FALSE)))
  # bb_hand_pourpoint_id
  file.copy(bb_get_handpourpointIDraster(bbopt$workingfolder, returnobject = FALSE),
            file.path(workingfolder,"model",bb_get_handpourpointIDraster(workingfolder="GIS_files", returnobject=FALSE)))

  if (bbopt$use_dhand) {
    # bb_dhand layers
    # bb_dhand_id layers
    warning("DHAND used but layers not copied to GIS_files, please do this manually")
  }

  ## write geojson files
  bb_write_rivershp_geojson(bbopt)
  bb_write_snappedstreamnodes_geojson(bbopt)
  if (!is.null(bb_get_xsectionsfromstreamnodesshp(bbopt$workingfolder))) {bb_write_xsections_geojson(bbopt)}
  bb_write_catchmentstreamnodes_geojson(bbopt,include_results = FALSE)

  return(TRUE)
}
