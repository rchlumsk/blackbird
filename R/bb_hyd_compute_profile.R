#' @title Hydraulic Profile Calculation
#'
#' Calculates the water profile(s) using the standard step method.
#'
#' **Currently assumes subcritical regime throughout the profile, will be updated in future versions.**
#'
#' @details
#' Writes to the workingfolder results folder.
#'
#' Can set \code{update_preproc_tables=FALSE} if preproc tables are manually updated first.
#' This can reduce some overhead if needed (process bbgeom$update_preproc_tables_bymethod prior to
#' looped model runs or with many profiles).
#'
#' @param bbmodel Blackbird model object
#' @param write_results boolean whether to write results to file
#' @param update_preproc_tables boolean whether to update the preproc tables based on current bbopt methods
#'
#' @return \item{hydraulic_output}{Table of computed hydraulic parameters by streamnode and flow profile}
#'
#' @examples
#' # IOU an example
#'
#' @export bb_hyd_compute_profile
bb_hyd_compute_profile <- function(bbmodel, write_results=FALSE, update_preproc_tables=TRUE) {

  # unpack bbmodel for convenience
  geometry=bbmodel$bbgeo
  flowprofile=bbmodel$bbfp
  boundary_conditions=bbmodel$bbbc
  bbopt=bbmodel$bbopt
  method=bbopt$regimetype
  workingfolder = bbopt$workingfolder

  Froude_threshold <- bbopt$Froude_threshold # default 0.94
  # threshold below which flow is subcritical, and above it may be supercritical

  # check modeltype
  if (bbopt$modeltype %notin% bb_get_modeltypes()) {
    stop(sprintf("bbopt$modeltype %s not recognized.",bbopt$modeltype))
  }

  # check method
  if (method != "subcritical") {
    stop("Only subcritical mode with hardcoded options currently available.")
  }

  # check geometry
  if ("bb_geometry" %notin% class(geometry)) {
    stop("geometry must be of class bb_geometry")
  }
  if (length(geometry$streamnodeList) <= 1) {
    stop("Calculation requires at least 2 stream nodes in the geometry object.")
  }

  ## Check boundary condition
  if (class(boundary_conditions)[1] != "bb_boundaryconditionlist") {
    stop("boundary_conditions must be of type bb_boundaryconditionlist")
  }

  for (i in 1:length(boundary_conditions$bclist)) {
    if (boundary_conditions$bclist[[i]]$bctype %notin% bb_get_boundary_types()) {
      stop("Boundary condition must be valid (check function bb_get_boundary_types).")
    }
    if (is.na(boundary_conditions$bclist[[i]]$bcvalue) | boundary_conditions$bclist[[i]]$bcvalue <= 0) {
      stop("Boundary condition value must be of value greater than 0.")
    }
  }

  # check flow profile
  if ("bb_flowprofile" %notin% class(flowprofile)) {
    stop("A valid flowprofile object must be included in bbmodel")
  }

  # check bbopt
  if ("bb_options" %notin% class(bbopt)) {
    stop("bbopt object must be of class bb_options.")
  }
  if (bbopt$dx <= 0 | class(bbopt$dx) != "numeric") {
    stop("dx in bbopt must be a numeric value greater than 0.")
  }
  find_next_node2 <- function(mmm) {
    if (nrow(mmm[mmm$is_computed == 0,]) == 0) {
      warning("no uncomputed nodes left!")
      returnID <- -1
    }

    # find an uncomputed node with other nodes on same reach
    # temp <- mmm[mmm$is_computed == 0,]
    computed_nodes <- mmm[mmm$is_computed == 1,]
    uncomputed_nodes <- mmm[mmm$is_computed == 0,]
    temp <- uncomputed_nodes[which(uncomputed_nodes$downnodeID %in% computed_nodes$nodeID),]$nodeID

    if (length(temp>=1)) {
      returnID <- which(mmm$nodeID == temp[1])
    } else {
      stop("out of nodes? or error in network")
    }
    return(returnID)
  }

  # xxx to do - add validation for reach name here too
  streamnodedf <- geometry$get_streamnodeList_as_dataframe()

  ## check flow profile with built in tools
  suppressMessages(flowprofile$check_flowprofile())

  mm <- bb_hydraulic_output_emptydf(nrow(streamnodedf))

  bcdf <- boundary_conditions$get_bclist_dataframe()

  if (any(bcdf$nodeID %notin% streamnodedf$nodeID)) {
    # check that method is subcrit, and see which streamnode the boundary conditions apply at
    for (i in 1:length(boundary_conditions$bclist)) {
      if (boundary_conditions$bclist[[i]]$nodeID %notin% streamnodedf$nodeID) {
        stop(sprintf("boundarycondtions nodeID %i not found in geometry nodeID",
                     boundary_conditions$bclist[[i]]$nodeID))
      }
    }
  }

  mm$nodeID <- streamnodedf$nodeID
  mm$reachID <- streamnodedf$reachID
  mm$downnodeID <- streamnodedf$downnodeID
  mm$upnodeID1 <- streamnodedf$upnodeID1
  mm$upnodeID2 <- streamnodedf$upnodeID2
  mm$stationname <- streamnodedf$stationname
  mm$station <- streamnodedf$station
  mm$Min_Elev <- streamnodedf$min_elev
  mm$reach_length_DS <- streamnodedf$ds_reach_length
  mm$reach_length_US1 <- streamnodedf$us_reach_length1
  mm$reach_length_US2 <- streamnodedf$us_reach_length2
  mm$bed_slope <- streamnodedf$bed_slope

  ## setup flowprofile and nodeID
  bbfp_nodeID <- flowprofile$flowdf$nodeID
  bbfp_flowdf <- data.frame(flowprofile$flowdf[,2:ncol(flowprofile$flowdf)])
  colnames(bbfp_flowdf) <- colnames(flowprofile$flowdf)[2:ncol(flowprofile$flowdf)]

  ## check that mm$nodeID and bbfp_nodeID align
  if (any(as.character(bbfp_nodeID) != as.character(mm$nodeID))) {
    stop("nodeID in flowprofile and geometry not aligned")
  }

  # update preproc tables by the method
  if (bbopt$use_preproc) {
    geometry$update_preproc_tables_bymethod(bbopt = bbopt)
  }

  # iterate through columns of flow profiles
  for (jj in 1:ncol(bbfp_flowdf)) {

    mm$Flow <- bbfp_flowdf[,jj]

    if (!(bbopt$silent_cp)) {
      print(sprintf("Beginning calculations for flow profile %i of %i",jj,ncol(bbfp_flowdf)))
    }

    ## solve boundary condition nodes first ---

    for (ii in 1:nrow(bcdf)) {

      i <- which(streamnodedf$nodeID == bcdf$nodeID[ii])

      # i = start_computation_index

      if (!(bbopt$silent_cp)) {
        print(sprintf("Computing profile for streamnode %i",i))
      }

      mm[i,]$Min_Elev <- geometry$streamnodeList[[i]]$min_elev

      if (bbopt$modeltype == "hand-manning") {

        mm[i,]$WSL <- geometry$streamnodeList[[i]]$compute_normal_depth(
          Flow=mm[i,]$Flow,
          Slope = mm[i,]$bed_slope,
          init_WSL = -99, # ignore boundary conditions in hand-manning approach
          bbopt = bbopt
        )$WSL

      } else {

        # estimate first streamnode from supplied boundary conditions
        if (bcdf$bctype[1] == "normal_depth") {

          # warning if slope value is not reasonable
          if (bcdf[1,]$bcvalue <=0 | bcdf[1,]$bcvalue > 1) {
            warning("bcvalue may not be a reasonable slope, please check!")
          }

          mm[i,]$WSL <- geometry$streamnodeList[[i]]$compute_normal_depth(
            Flow=mm[i,]$Flow,
            Slope = bcdf[1,]$bcvalue,
            init_WSL = bcdf[1,]$init_WSL,
            bbopt = bbopt
          )$WSL

          mm[i,]$Sf <- bcdf[1,]$bcvalue # override Sf from preproc

        } else if (bcdf[1,]$bctype == "set_wsl") {
          mm[i,]$WSL <- bcdf[1,]$bcvalue

          if (mm[i,]$WSL < mm[i,]$Min_Elev) {
            stop("set_wsl used as boundary condition but value provided is less than minimum elevation, revise boundary condition!")
          }

        } else if (bcdf[1,]$bctype == "set_depth") {

          if (bcdf[1,]$bcvalue >= 0) {
            mm[i,]$WSL <- bcdf[1,]$bcvalue+mm[i,]$Min_Elev
          } else {
            stop("set_depth used as boundary condition, value must be >=0")
          }

        } else {
          stop("Error in boundary condition input type")
        }
      }
      mm[i,]$Depth <- mm[i,]$WSL - mm[i,]$Min_Elev

      ## get additional mm properties
      # note: if normal depth used as BC, some inefficiency as normal_depth returns only WSL but
      # other hydraulic properties are calculated as well, essentially running the initial profile twice.
      mm[i,] <- geometry$streamnodeList[[i]]$compute_profile(Flow = mm[i,]$Flow,
                                                             WSL = mm[i,]$WSL,
                                                             mm=mm[i,],
                                                             bbopt=bbopt)
    }

    ## determine computation order
    is_junction <- rep(0, nrow(mm))
    for (i in 1:nrow(mm)) {
      if (mm$upnodeID1[i] != -1 & mm$upnodeID2[i] != -1 ) {
        is_junction[i] <- 1
      }
    }

    # assign reach downstream points
    reach_base <- rep(0, nrow(mm))
    reaches <- unique(mm$reachID)
    for (rr in reaches) {
      temp <- mm[mm$reachID == rr,]
      downID <- temp[temp$downnodeID %notin% temp$nodeID,]$nodeID
      reach_base[which(mm$nodeID==downID)] <- 1
    }

    ## track which elements have been computed
    is_computed <- rep(0, nrow(mm))
    is_computed[which(streamnodedf$nodeID %in% bcdf$nodeID)] <- 1

    ## computational order
    computation_order <-  rep(NA, nrow(mm))
    computation_order[1:nrow(bcdf)] <- which(streamnodedf$nodeID %in% bcdf$nodeID)

    mmm <- cbind(mm, is_junction, reach_base, is_computed)

    # determine computation order
    for (kk in (nrow(bcdf)+1):nrow(mm)) {
      # print(sprintf("kk is %i",kk))
      i <- find_next_node2(mmm)
      # print(i)
      computation_order[kk] <- i
      mmm[i,]$is_computed <- 1
    }

    if (any(is.na(computation_order))) {
      stop("failed to parse the computation order of nodes")
    }

    ## run iterative standard step method
    for (i in computation_order[(nrow(bcdf)+1):length(computation_order)]) {

      if (!(bbopt$silent_cp)) {
        print(sprintf("Computing profile for streamnode %i (%s)",i, streamnodedf$stationname[i]))
      }

      if (bbopt$modeltype == "hand-manning") {

        mm[i,]$Min_Elev <- geometry$streamnodeList[[i]]$min_elev

        mm[i,]$WSL <- geometry$streamnodeList[[i]]$compute_normal_depth(
          Flow=mm[i,]$Flow,
          Slope = mm[i,]$bed_slope, # assume local bed slope equal to Sf
          init_WSL = -99, # ignore boundary conditions in hand-manning approach
          bbopt = bbopt
        )$WSL

        mm[i,] <- geometry$streamnodeList[[i]]$compute_profile(Flow = mm[i,]$Flow,
                                                               WSL = mm[i,]$WSL,
                                                               mm=mm[i,],
                                                               bbopt=bbopt)
      } else {
        # find downstream (solved) node from current
        previousmm <- mm[mm$nodeID == mm[i,]$downnodeID,]

        # initial setting of items
        mm[i,]$Min_Elev <- geometry$streamnodeList[[i]]$min_elev  #  min(geometry$streamnodeList[[i]]$zz)
        mm[i,]$WSL <- previousmm$Depth + mm[i,]$Min_Elev # best guess at upstream WSL, same depth applied to bottom bed elevation
        mm[i,]$Depth <- previousmm$Depth  # or  mm[i,]$WSL - mm[i,]$Min_Elev

        err_lag2 <- NA
        err_lag1 <- NA
        prevWSL_lag1 <- NA
        prevWSL_lag2 <- NA
        min_err_WSL <- NA
        min_err <- 1e6
        actual_err <- NA
        min_Fr <- NA

        found_supercritical <- FALSE # reset before every j iteration, use in iteration as needed

        for (j in 1:bbopt$iteration_limit_cp) {
          # print(sprintf("j is %i",j))
          prevWSL_lag2 <- prevWSL_lag1
          prevWSL_lag1 <- mm[i,]$WSL

          mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
            Flow=mm[i,]$Flow, WSL=mm[i,]$WSL, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)

          max_depth_change <- 0.5*mm[i,]$Depth

          # print(sprintf("nodeID is %i, i is %i, j is %i, mm$Head_Loss is %.4f, K_ROB is %.3f, Depth is %.3f, velocity head is %.4f, head loss is %.2f, Sf_Avg is %.4f",
          #               mm[i,]$nodeID,i,j,mm[i,]$Head_Loss,
          #               mm[i,]$K_ROB, mm[i,]$Depth, mm[i,]$Velocity_head, mm[i,]$Head_Loss, mm[i,]$Sf_Avg))

          comp_WSL <- previousmm$WSL + previousmm$Velocity_head + mm[i,]$Head_Loss - mm[i,]$Velocity_head

          # print(sprintf("comp_WSL is %.2f, min_elev is %.2f",comp_WSL,mm[i,]$Min_Elev))

          # checks/modifications against Min_Elev
          if (comp_WSL <= mm[i,]$Min_Elev) {
            if (mm[i,]$Flow == 0) {
              comp_WSL <- mm[i,]$Min_Elev
            } else {
              # previously used Min_Elev + 0.01
              # created instability with extreme head loss over longer distances
              # better to iterate with min_elev + best guess at depth from prevmm, and slowly drive depth down
              ## through the averaging with mm[i,]$WSL and Min_Elev+previousmm$Depth in next section then to set too low
              comp_WSL <- max(mm[i,]$Min_Elev+0.05+0.05*(1-j/bbopt$iteration_limit_cp), comp_WSL) # max(mm[i,]$Min_Elev+0.5, comp_WSL)
            }
          }

          # print(sprintf("comp_WSL is %.2f, min_elev is %.2f, depth would be %.2f",comp_WSL,mm[i,]$Min_Elev, comp_WSL-mm[i,]$Min_Elev))

          # print(sprintf("streamnode %i, iteration %i, previous WSL is %.3f, next WSL is %.3f",
          #               i, j, mm[i,]$WSL, comp_WSL))

          # check for divergence in comp_WSL (potential dx resolution issue)
          if (is.na(comp_WSL) | is.nan(comp_WSL)) {
            stop("Algorithm diverged, consider reducing dx parameter in bb_options argument.")
          }

          err_lag2 <- err_lag1
          err_lag1 <- comp_WSL - prevWSL_lag1 # mm[i,]$WSL
          err_diff <- err_lag2 - err_lag1
          assum_diff <- prevWSL_lag2 - prevWSL_lag1

          if (abs(err_lag1) < min_err) {
            min_err <- abs(err_lag1)
            actual_err <- err_lag1
            min_err_WSL <- prevWSL_lag1
            min_Fr <- mm[i,]$Froude
          }

          mm[i,]$WS_err <- err_lag1
          mm[i,]$K_err <- (mm[i,]$Flow - mm[i,]$K_Total*sqrt(mm[i,]$Sf))
          mm[i,]$cp_iterations <- j

          ## debugging statement
          # print(sprintf("out of tolerance %s, mm[i,]$Depth= %.3f, mm[i,]$WSL= %.4f , comp_WSL= %.4f, err_lag1= %.5f, min_err= %.5f, Fr= %.3f",
          #                 j,mm[i,]$Depth, mm[i,]$WSL,comp_WSL,err_lag1,min_err, mm[i,]$Froude))

          if (abs(err_lag1) > bbopt$tolerance_cp) {

            if (j >= bbopt$iteration_limit_cp) {
              message(sprintf("Iteration limit %s reached on streamnode %s",bbopt$iteration_limit_cp,mm[i,]$nodeID))

              # setting to min error result
              mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
              Flow=mm[i,]$Flow, WSL=min_err_WSL, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)

                mm[i,]$WS_err <- actual_err
                mm[i,]$K_err <- (mm[i,]$Flow - mm[i,]$K_Total*sqrt(mm[i,]$Sf))

              if (min_err < 0.03 & mm[i,]$Froude <= Froude_threshold) { # xxx set threshold for min err as parameter in bbopt
                # keeping the min err result
                message(sprintf("setting to min error result on streamnode %s",mm[i,]$nodeID))
              } else {
                # set to critical depth

                # get critical depth
                rr <- optim(par=c(mm[i,]$WSL+1),
                      fn=get_total_energy,
                      mm=mm, geometry=geometry,i=i,previousmm=previousmm,bbopt=bbopt,
                      method="Brent",
                      lower=mm[i,]$Min_Elev,
                      upper=mm[i,]$Min_Elev+max(bbopt$Hseq))

                if (rr$convergence==0) {
                  mm[i,]$Depth_Critical <- rr$value-mm[i,]$Min_Elev

                  mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
                      Flow=mm[i,]$Flow, WSL=mm[i,]$Min_Elev + mm[i,]$Depth_Critical, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)

                  mm[i,]$WS_err <- NA
                  mm[i,]$K_err <- (mm[i,]$Flow - mm[i,]$K_Total*sqrt(mm[i,]$Sf))

                  if (!(bbopt$silent_cp)) {
                    message(sprintf("setting to critical depth result on streamnode %s",mm[i,]$nodeID))
                  }
                  break

                } else {
                  # have to keep min err since failed to compute critical depth
                  if (!(bbopt$silent_cp)) {
                    message(sprintf("Failed to get critical depth at streamnode %i, keeping lowest err result",mm[i,]$nodeID))
                  }
                  break
                }
                 # warning(sprintf("Iteration limit %s reached, setting to critical depth result on streamnode %s",bbopt$iteration_limit_cp,mm[i,]$nodeID))
              }

              break
            }

            if (j == 1) {
              if (!bbopt$silent_cp) {print("setting based on second trial rules")}
              # setting based on second trial rules
              proposed_WSL <- mm[i,]$WSL + 0.7*(err_lag1)
              if (abs(proposed_WSL - mm[i,]$WSL) > max_depth_change) {
                mm[i,]$WSL <- mm[i,]$WSL + (proposed_WSL-mm[i,]$WSL)*(max_depth_change/abs(proposed_WSL-mm[i,]$WSL))
              } else {
                mm[i,]$WSL <- proposed_WSL
              }
            } else {

              # modified this to use the averaged for small diff method with larger j iterations too
              # may be a better approach
              if (abs(err_diff) < 0.03 | j > (bbopt$iteration_limit_cp/2) | j>20) {
                # for small error differences, secant method can fail
                # take average of prevWSL_lag1 and prevWSL_lag2
                proposed_WSL <- mean(c(comp_WSL,prevWSL_lag1))

                # change by half of max_depth_change in direction of comp_WSL if massive swing
                if (abs(comp_WSL-prevWSL_lag1)>max_depth_change) {
                  proposed_WSL <- prevWSL_lag1 +0.5*max_depth_change*((comp_WSL-prevWSL_lag1)/abs(comp_WSL-prevWSL_lag1))
                }

                mm[i,]$WSL <- proposed_WSL
              } else {
                # secant method
                proposed_WSL <- prevWSL_lag2 - err_lag2*assum_diff/err_diff
                if (abs(proposed_WSL - mm[i,]$WSL) > max_depth_change) {
                  mm[i,]$WSL <- mm[i,]$WSL + (proposed_WSL-mm[i,]$WSL)*(max_depth_change/abs(proposed_WSL-mm[i,]$WSL))
                } else {
                  mm[i,]$WSL <- proposed_WSL
                }
              }
            }

          } else {

            if (mm[i,]$Froude <= Froude_threshold) {
              if (!(bbopt$silent_cp)) {
                print(sprintf("Iterated on WSL at streamnode %i within tolerance on iteration %s",i,j))
              }
              break
            } else {

              if (!(bbopt$silent_cp)) {message("need to check crit depth")}
              # need to compute critical depth at streamnode

              rr <- optim(par=c(mm[i,]$WSL+1),
                      fn=get_total_energy,
                      mm=mm, geometry=geometry,i=i,previousmm=previousmm,bbopt=bbopt,
                      method="Brent",
                      lower=mm[i,]$Min_Elev,
                      upper=mm[i,]$Min_Elev+max(bbopt$Hseq))

              if (rr$convergence==0) {
                mm[i,]$Depth_Critical <- rr$value-mm[i,]$Min_Elev

                if (mm[i,]$Depth < mm[i,]$Depth_Critical) {

                  if (found_supercritical) {

                    # already found supercritical. Set depth to critical depth and move on with warning
                    mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
                      Flow=mm[i,]$Flow, WSL=mm[i,]$Min_Elev + mm[i,]$Depth_Critical, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)

                    if (!(bbopt$silent_cp)) {message("setting to supercritical")}
                    break

                  } else {

                    if (!(bbopt$silent_cp)) { message("first time with supercritical, resetting")}

                    # first time getting supercritical depth. Set depth to well above supercritical and continue
                    mm[i,] <- geometry$streamnodeList[[i]]$compute_profile_next(
                      Flow=mm[i,]$Flow, WSL=mm[i,]$Min_Elev + mm[i,]$Depth_Critical + 1, mm=mm[i,], prevmm = previousmm, bbopt = bbopt)

                    # reset algorithm
                    j=1
                    min_err <- 1e6
                    # dont break
                  }
                  found_supercritical <- TRUE

                } else {
                  # not supercritical even though Froude is high
                  if (mm[i,]$Froude > 1) {
                    warning(sprintf("Depth found to not be supercritical even though Froude is >1 at streamnode %i",mm[i,]$nodeID))
                  }
                  break
                }

              } else {
                warning(sprintf("Failed to converge in critical depth calculation for streamnode %i. Leaving depth as is, which may be supercritical",mm[i,]$nodeID))
                break
              }
            }
          }
        }
      }

      if (mm[i,]$Velocity==0 & mm[i,]$Depth>0) {
        warning(sprintf("Possible error and/or erroneous hydraulic properties at streamnode %i, zero velocity and depth>0 detected",
                mm[i,]$nodeID))
      }

      # update static properties
      mm[i,]$station <- streamnodedf$station[i]
      mm[i,]$reach_length_DS <- streamnodedf$ds_reach_length[i]
      mm[i,]$reach_length_US <- streamnodedf$us_reach_length[i]
    }

    # check that results are within the Hseq bounds
    if (bbopt$use_preproc) {
      max_depth <- max(bbopt$Hseq)

      if (max(mm$Depth) > max_depth) {
        warning(sprintf("Results for flow of %.2f estimated a depth beyond those provided for pre-processing;\nResults should be re-run with a broader set of depths.",mm[i,]$Flow))
      }
    }

    mm$flowprofile <- colnames(bbfp_flowdf)[jj]
    message(sprintf("Successfully completed hydraulic calculations for flow profile %i of %i.",
                    jj, ncol(bbfp_flowdf)))

    # bind results to one large set
    if (jj == 1) {
      hydraulic_output <- mm
    } else {
      hydraulic_output <- rbind(hydraulic_output,mm)
    }
  }

  # basic checks for instabilities
  if (any(hydraulic_output$alpha > 5)) {
    warning("alpha values greater than 5 found, indicating possible instability in results. Please review.")
  }
  if (any(hydraulic_output$Velocity > 50)) {
    warning("Velocities greater than 50 m/s found, indicating possible instability in results. Please review.")
  }
  if (any(hydraulic_output$Depth > 10)) {
    warning("Depths greater than 10 m found, indicating possible instability in results. Please review.")
  }
  if (any(hydraulic_output[is.finite(hydraulic_output$Sf_Avg),]$Sf_Avg > 1)) {
    warning("Average friction slope greater than 1 found, indicating possible instability in results. Please review.")
  }
  if (any(hydraulic_output$cp_iterations[!is.na(hydraulic_output$cp_iterations)] > bbopt$iteration_limit_cp)) {
    warning("Iteration limit hit at one or more streamnodes, consider increasing bbopt$iteration_limit_cp")
  }

  message("Successfully completed all hydraulic calculations :-)")

  ## write hydraulic results to table in working folder
  if (write_results) {
    workingfolder <- bbopt$workingfolder

    if (!is.null(workingfolder)) {
      if (!dir.exists(workingfolder)) {
        dir.create(workingfolder)
      }
      if (!dir.exists(file.path(workingfolder,"results"))) {
        dir.create(file.path(workingfolder,"results"))
      }

      write.csv(x=hydraulic_output,
                file = bb_get_results_hydraulic_output(workingfolder,returnobject = FALSE),
                row.names=FALSE, quote=FALSE)
      message(sprintf("Wrote hydraulic results to %s",bb_get_results_hydraulic_output(workingfolder,returnobject = FALSE)))
    } else {
      warning("bbopt$workingfolder was NULL, results not written to file.")
    }
  }

  return("hydraulic_output"=hydraulic_output)
}

