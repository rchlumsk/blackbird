#' @title Read Blackbird model input files
#'
#' @description
#' Read model input files from Blackbird ascii format, and build model structure in R.
#'
#' @param workingfolder workingfolder
#' @param modelname prefix for model file names
#'
#' @return {returns \code{TRUE} if run successfully}
#
#' @examples
#' # to do
#'
#'
#' @export bb_read_model_files
bb_read_model_files <- function(workingfolder=NULL, modelname="modelname") {


  ## XXX TO DO - add support for reading in xsection blocks


  if (is.null(workingfolder)) {stop("Either workingfolder must not be NULL, or return_raster must be TRUE")}
  if (is.null(modelname)) {stop("modelname is required")}

  # look for all files
  if (!file.exists(file.path(workingfolder,"model",sprintf("%s.bbi",modelname)))) {stop("bbi file not found")}
  if (!file.exists(file.path(workingfolder,"model",sprintf("%s.bbg",modelname)))) {stop("bbg file not found")}
  if (!file.exists(file.path(workingfolder,"model",sprintf("%s.bbf",modelname)))) {stop("bbf file not found")}

  # build default/empty model components -----
  bbopt <- bb_options()
  bbgeo <- bb_geometry()
  bbfpList <- list(bb_flowprofile())

  # read bbi file ----
  tt <- readLines(file.path(workingfolder,"model",sprintf("%s.bbi",modelname)))

  # parse bbi line by line
  for (i in 1:length(tt)) {

    ss <- bb_tokenize(tt[i])

    if (is.null(ss) | length(ss) == 0) {
      # comment or blank line
    } else if (ss[1]==":ModelType") {
      bbopt$modeltype <- ss[2]
    } else if (ss[1]==":RegimeType") {
      bbopt$regimetype <- ss[2]
    } else if (ss[1]==":gravity") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$g <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":dx") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$dx <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":tolerance_cp") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$tolerance_cp <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":tolerance_nd") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$tolerance_nd <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":iteration_limit_cp") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$iteration_limit_cp <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":next_WSL_split_nd") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$next_WSL_split_nd <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":max_RHSQ_ratio") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$max_RHSQ_ratio <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":min_RHSQ_ratio") {
      if (!is.na(as.numeric(ss[2]))) {
        bbopt$min_RHSQ_ratio <- as.numeric(ss[2])
      } else {
        warning(sprintf("Error parsing numeric value %s on line %i of %s.bbi",ss[2],i,modelname))
      }
    } else if (ss[1]==":UsePreprocessedHydraulicTables") {
      bbopt$use_preproc <- TRUE
    } else if (ss[1]==":Hseq") {
      if (!is.na(as.numeric(ss[2]))) {
        vv <- as.numeric(ss[2])

        if (length(ss)<=2) {
          warning("Hseq requires more than one value.")
        } else {
          for (j in 3:length(ss)) {
            vv <- c(vv, as.numeric(ss[j]))
          }
          bbopt$Hseq <- vv
        }
      } else {
        warning(sprintf("Error parsing numeric values %s on line %i of %s.bbi, More than one value required",
                        ss[2],i,modelname))
      }
    } else {
      warning(sprintf("Unrecognized command %s on line %i of %s.bbi",ss[1],i,modelname))
    }
  }

  # read bbg file ----
  tt <- readLines(file.path(workingfolder,"model",sprintf("%s.bbg",modelname)))

  # parse bbg line by line
  i <- 0
  while (i <= length(tt)) {
    i <- i+1
    ss <- bb_tokenize(tt[i])

    if (is.null(ss) | length(ss) == 0) {
      # comment or blank line
    } else if (ss[1]==":Streamnodes") {

      sdf <- data.frame(matrix(NA,nrow=0,ncol=14))
      colnames(sdf) <- c("nodeID","nodetype","downnodeID","upnodeID1","upnodeID2","stationname",
                                  "station","reachID","ds_reach_length","us_reach_length1","us_reach_length2",
                                  "contraction_coeff","expansion_coeff","min_elev")
      jj <- 0 # tracking sdf index

      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

        if (is.null(ss) | length(ss) == 0) {
           # comment
        } else if (ss[1]==":EndStreamnodes") {
           break
        } else if (ss[1]==":Attributes") {
          if (ss[2] != "nodeID" & length(ss) != (ncol(sdf)+1) ) {
            stop(sprintf("Error reading :Streamnodes - expect %i attributes, beginining with nodeID",ncol(sdf)))
          }
        } else if (length(ss) == ncol(sdf)) {
             jj <- jj+1
             sdf[jj,] <- ss
         } else {
            warning(sprintf("Unrecognized command or incorrect syntax %s in :Streamnodes block on line %i of %s.bbg",ss[1],i,modelname))
         }
      }

      # update sdf columns to numeric
      sdf$nodeID <- as.integer(sdf$nodeID)
      sdf$downnodeID <- as.integer(sdf$downnodeID)
      sdf$upnodeID1 <- as.integer(sdf$upnodeID1)
      sdf$upnodeID2 <- as.integer(sdf$upnodeID2)
      sdf$station <- as.numeric(sdf$station)
      sdf$reachID <- as.integer(sdf$reachID)
      sdf$ds_reach_length <- as.numeric(sdf$ds_reach_length)
      sdf$us_reach_length1 <- as.numeric(sdf$us_reach_length1)
      sdf$us_reach_length2 <- as.numeric(sdf$us_reach_length2)
      sdf$contraction_coeff <- as.numeric(sdf$contraction_coeff)
      sdf$expansion_coeff <- as.numeric(sdf$expansion_coeff)
      sdf$min_elev <- as.numeric(sdf$min_elev)

      ## make streamnodes and add to geometry
      myStreamnodeList <- list(streamnode())

      for (j in 1:nrow(sdf)) {
        # add items

        if (sdf$nodetype[j] == "catchment") {
          myStreamnodeList[[j]] <- catchment(
            nodeID=sdf$nodeID[j],
            nodetype=sdf$nodetype[j],
            downnodeID=sdf$downnodeID[j],
            upnodeID1=sdf$upnodeID1[j],
            upnodeID2=sdf$upnodeID2[j],
            stationname=sdf$stationname[j],
            station=sdf$station[j],
            reachID=sdf$reachID[j],
            contraction_coeff=sdf$contraction_coeff[j],
            expansion_coeff=sdf$expansion_coeff[j],
            ds_reach_length=sdf$ds_reach_length[j],
            us_reach_length1=sdf$us_reach_length1[j],
            us_reach_length2=sdf$us_reach_length2[j],
            min_elev=sdf$min_elev[j]
          )
        } else if (sdf$nodetype[j] == "xsection") {
           myStreamnodeList[[j]] <- xsection(
            nodeID=sdf$nodeID[j],
            nodetype=sdf$nodetype[j],
            downnodeID=sdf$downnodeID[j],
            upnodeID1=sdf$upnodeID1[j],
            upnodeID2=sdf$upnodeID2[j],
            stationname=sdf$stationname[j],
            station=sdf$station[j],
            reachID=sdf$reachID[j],
            contraction_coeff=sdf$contraction_coeff[j],
            expansion_coeff=sdf$expansion_coeff[j],
            ds_reach_length=sdf$ds_reach_length[j],
            us_reach_length1=sdf$us_reach_length1[j],
            us_reach_length2=sdf$us_reach_length2[j],
            min_elev=sdf$min_elev[j]
           )
        } else {
          stop("TO DO - support other node types")
        }
      }
      bbgeo$streamnodeList <- myStreamnodeList
      rm(myStreamnodeList)

    } else if (ss[1]==":StreamnodeRasterPaths") {

      rpdf <- data.frame(matrix(NA,nrow=0,ncol=3))
      colnames(rpdf) <- c("nodeID","rastertype","path")
      jj <- 0 # tracking sdf index

      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

        if (is.null(ss) | length(ss) == 0) {
           # comment
        } else if (ss[1]==":EndStreamnodeRasterPaths") {
           break
        } else if (ss[1]==":Attributes") {
          if (ss[2] != "nodeID" & ss[3] != "rastertype" & length(ss) != 3 ) {
            stop("Error reading :Streamnodes - expect 3 attributes, beginining with nodeID and rastertype columns")
          }
        } else if (length(ss) == 2) {
          # case of just nodeID and type, empty path
             jj <- jj+1
             rpdf[jj,] <- c(ss,NA)
         } else if (length(ss) == 3) {
             jj <- jj+1
             rpdf[jj,] <- ss
         } else {
            warning(sprintf("Unrecognized command or incorrect syntax %s in :StreamnodeRasterPaths block on line %i of %s.bbg",ss[1],i,modelname))
         }
      }

      if (nrow(rpdf)>0) {
        # update nodeID to numeric
      rpdf$nodeID <- as.integer(rpdf$nodeID)

      ## store properties in geometry
      for (j in 1:nrow(rpdf)) {
        for (kk in 1:length(bbgeo$streamnodeList)) {
          if (rpdf$nodeID[j] == bbgeo$streamnodeList[[kk]]$nodeID) {
            # for some reason this doesn't work, so enumerate all four types :(
            # bbgeo$streamnodeList[[kk]][[rpdf$rastertype[j]]] <- rpdf$path[j]
            if (rpdf$rastertype[j]=="dem") {
              bbgeo$streamnodeList[[kk]]$dem <- rpdf$path[j]
            } else if (rpdf$rastertype[j]=="demcond") {
              bbgeo$streamnodeList[[kk]]$demcond <- rpdf$path[j]
            } else if (rpdf$rastertype[j]=="nraster") {
              bbgeo$streamnodeList[[kk]]$nraster <- rpdf$path[j]
            } else if (rpdf$rastertype[j]=="hand") {
              bbgeo$streamnodeList[[kk]]$hand <- rpdf$path[j]
            } else {
              stop(sprintf("Unrecoginzed rastertype %s in StreamnodeRasterPaths",rpdf$rastertype[j]))
            }
            break
          }
        }
      }
      } else {
        warning(":StreamnodeRasterPaths command included but no raster paths provided")
      }

    } else if (ss[1]==":RedirectToFile") {

      # xxx to do, and handle preprocessed tables
      stop("TO DO")
    } else if (ss[1]==":PreprocessedHydraulicTables") {

      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

        if (is.null(ss) | length(ss) == 0) {
          # comment
        } else if (ss[1]==":EndPreprocessedHydraulicTables") {
            break
        } else if (ss[1]==":PreprocHydTable") {
          if (!exists('sdf') | (as.integer(ss[2]) %notin%  sdf$nodeID)) {
            stop(sprintf("Error reading :PreprocHydTable - nodeID %i not found in streamnodeList; please ensure that the :Streamnodes command appears before the :PreprocessedHydraulicTables command",ss[2]))
          }

          pp_nodeID <- as.integer(ss[2])

          ppht <- bb_hydraulic_output_emptydf(nrow=1)
          jj <- 0 # tracking ppht index

          while (i <= length(tt)) {
            i <- i+1
            ss <- bb_tokenize(tt[i])

            if (ss[1]==":EndPreprocHydTable") {
               break
            } else if (is.null(ss) | length(ss) == 0) {
               # comment
            } else if (ss[1]==":Attributes") {
              if (ss[2] != "nodeID" & ss[3] != "reachID" & length(ss) != (ncol(ppht)+1) ) {
                stop(sprintf("Error reading :PreprocHydTable - expect %i attributes, beginining with nodeID and reachID columns",ncol(ppht)))
              }
            } else if (length(ss) == ncol(ppht)) {
                 jj <- jj+1
                 ppht[jj,] <- ss
             } else {
                warning(sprintf("Unrecognized command or incorrect syntax %s in :PreprocHydTable block on line %i of %s.bbg",ss[1],i,modelname))
             }
          }

          # make some columns numeric and store in geometry
          for (vv in c("nodeID", "reachID", "downnodeID", "upnodeID1", "upnodeID2")) {
            ppht[[vv]] <- as.integer(ppht[[vv]])
          }

          for (vv in c("station", "reach_length_DS", "reach_length_US1",
            "reach_length_US2", "Flow", "Flow_LOB", "Flow_Main", "Flow_ROB",
            "Min_Elev", "Depth", "WSL", "Velocity", "Velocity_LOB", "Velocity_Main",
            "Velocity_ROB", "K_Total", "K_LOB", "K_Main", "K_ROB", "alpha",
            "Area", "Area_LOB", "Area_Main", "Area_ROB", "HRadius", "HRadius_LOB",
            "HRadius_Main", "HRadius_ROB", "WetPerimeter", "WetPerimeter_LOB",
            "WetPerimeter_Main", "WetPerimeter_ROB", "Energy_total", "Velocity_head",
            "Froude", "Sf", "Sf_Avg", "Length_Effective", "Head_Loss", "Manning_Composite")) {
            ppht[[vv]] <- suppressWarnings(as.numeric(ppht[[vv]]))
          }

          if (any(ppht$nodeID[1] != ppht$nodeID)) {
            stop(sprintf("Error in reading preprocessed hydraulic table for nodeID %i: all nodeIDs must be the same within the table",ppht$nodeID[1]))
          }

          # store in geometry
          for (kk in 1:length(bbgeo$streamnodeList)) {
            if (ppht$nodeID[1] == bbgeo$streamnodeList[[kk]]$nodeID) {
              bbgeo$streamnodeList[[kk]]$depthdf <- ppht
              break
            } else if (ppht$nodeID[1] != bbgeo$streamnodeList[[kk]]$nodeID & kk == length(bbgeo$streamnodeList)) {
              stop(sprintf("Unrecognized nodeID %i in preprocessed hydraulic table; ensure that nodeIDs in :Streamnodes and :PreprocessedHydraulicTables are aligned."))
            }
          }

        } else {
            warning(sprintf("Unrecognized command or incorrect syntax %s in :PreprocessedHydraulicTables block on line %i of %s.bbg",ss[1],i,modelname))
         }
      }

    } else {
      warning(sprintf("Unrecognized command %s on line %i of %s.bbg",ss[1],i,modelname))
    }
  }

  # add riverreachList based on distinct reachIDs
  ## to do xxx
  # add other properties?? XXX TO DO


  # read bbf file ----
  tt <- readLines(file.path(workingfolder,"model",sprintf("%s.bbf",modelname)))

  # build default options for boundary conditions
  bcc <- bb_preprocess_default_boundarycondition(geometry = bbgeo)

  # parse bbf line by line
  i <- 0
  while (i <= length(tt)) {
    i <- i+1
    ss <- bb_tokenize(tt[i])

    if (is.null(ss) | length(ss) == 0) {
      # comment or blank line
    } else if (ss[1]==":BoundaryConditions") {

      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

         if (is.null(ss) | length(ss) == 0) {
           # comment
         } else if (ss[1]==":EndBoundaryConditions") {
           break
         } else if (ss[1]==":StationName") {
           if (!is.null(ss[2])) {
             bcc$stationname <- ss[2]
           }
         } else if (ss[1]==":Station") {
            if (!is.na(as.numeric(ss[2]))) {
              bcc$station <- as.numeric(ss[2])
            } else {
            warning(sprintf("Error parsing numeric value %s on line %i of %s.bbf",ss[2],i,modelname))
            }
         } else if (ss[1]==":Reach") {
           if (!is.null(ss[2])) {
             bcc$reach <- ss[2]
           }
         } else if (ss[1]==":Location") {
           if (!is.null(ss[2])) {
             bcc$location <- ss[2]
           }
         } else if (ss[1]==":BCType") {
           if (!is.null(ss[2])) {
             bcc$bctype <- ss[2]
           }
         } else if (ss[1]==":BCValue") {
            if (!is.na(as.numeric(ss[2]))) {
              bcc$bcvalue <- as.numeric(ss[2])
            } else {
            warning(sprintf("Error parsing numeric value %s on line %i of %s.bbf",ss[2],i,modelname))
            }
         } else if (ss[1]==":InitialWSL") {
            if (!is.na(as.numeric(ss[2]))) {
              bcc$init_WSL <- as.numeric(ss[2])
            } else {
            warning(sprintf("Error parsing numeric value %s on line %i of %s.bbf",ss[2],i,modelname))
            }
         } else {
            warning(sprintf("Unrecognized command %s in :BoundaryConditions block on line %i of %s.bbf",ss[1],i,modelname))
         }
      }

    } else if (ss[1]==":SteadyFlows") {

      # flows_df <- data.frame("nodeID"=NA,"Flows"=NA)
      # jj <- 0 # tracking flows_df index

      while (i <= length(tt)) {
        i <- i+1
        ss <- bb_tokenize(tt[i])

        if (is.null(ss) | length(ss) == 0) {
           # comment
        } else if (ss[1]==":EndSteadyFlows") {
           break
        } else if (ss[1]==":Attributes") {
          if (ss[2] != "nodeID") {
            stop("Error reading :SteadyFlows - expect nodeID and Flows columns")
          }

          # initialize flow profiles
          flows_df <- data.frame(matrix(NA,nrow=0,ncol=length(ss)-1))
          colnames(flows_df) <- c("nodeID",ss[3:length(ss)])
          jj <- 0 # tracking flows_df index

        } else {

           if (!any(is.na(as.numeric(ss))) & exists('flows_df')) {
             jj <- jj+1
             flows_df[jj,] <- as.numeric(ss)
           } else if (!any(is.na(as.numeric(ss))) & !exists('flows_df')) {
             stop(":Attributes must appear before numeric data")
           } else {
             stop("Error parsing numeric values in :SteadyFlows block")
           }
        }
      }
    } else {
      warning(sprintf("Unrecognized command %s on line %i of %s.bbf",ss[1],i,modelname))
    }
  }

  # convert flows_df to flow profiles
  for (jj in 1:(ncol(flows_df)-1) ) {
    bbfpList[[jj]] <- bb_flowprofile(
      profilename=colnames(flows_df[2:(jj+1)]),
      profiletype="steady",
      nodeID=as.integer(flows_df$nodeID),
      flowvalue=flows_df[,(jj+1)] )
  }

  # assemble model class ----
  bbmodel <- bb_model(
      bbopt=bbopt,
      bbgeo=bbgeo,
      bbbc=bcc,
      bbfpList=bbfpList
  )

  return(bbmodel)
}
