#' Author: Michael Flanagin
#' Date started: 3 July 2018 13:13
#' Date last modified: 24 July 2018 11:00
#' objective: Analysis of existing output
#' 

# --------------------------------------------------------------------------- # 
#' This script evaluates metrics on output from "simulator" objects defined by 'simulation_name'.
#' 
#' It will [1] compute metrics on simulation output values
#'         [2] test allocations for complete separation (BINARY OUTCOMES ONLY)
#'         [3] write to 'metricfile_name' and (if appropriate) metricfile_name_valid_subsetted' 
#' [input values: "est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations"] 
#' [output metrics: "power.pvalue", "power.rerand","power.ci", "coverage", "bias", "separation_status"]
# --------------------------------------------------------------------------- # 
#' [ 9 August 2018 (Thursday) ] 
#' [11 : 45] Added settings to customize:
#' >> script performance (write_progressfile, write_outputfiles, etc)
#' >> Memory persistence and deallocation ('keep_output_in_environment', 'remove_output_after_sourcing')
#' [ 12:15 ]  TODO(michael): update flags for identifying complete separation!
#' [ 13:32 ] output references are broken in cases where output objects deleted manually. 
#' [ 14:28 ] FIXED! Note: used get_simulation_with_all_files() to load in simulation.
#' [ 14:33 ] Running batch-2 processing, took on avg 6sec to run code (vs 120sec!) 
#' [ 14:34 ] Running batch-1 processing
# --------------------------------------------------------------------------- # 
#' [ 12 August 2018 (Sunday) ]
#' [ 16:30 ] added toggle `load_simulation_from_all_files` to load in new simulation output (rerand adjusted ests)

library("simulator");

#' [ 'results_directory' contains folder 'files' with 'sim-{simulation_name}.Rdata' ] 
batch_no <- 2;  # NOTE: if 'batch_no' is 1 or 2, will do data subsetting on separation status.
simulation_name <- paste0("alloc-simulation-batch-", batch_no, "-of-4");
results_directory <- paste0("/Users/Moschops/Documents/MSThesis/datasets/batch-", batch_no, "/");
nsim <- 5010;  #' number of simulations (for pre-allocation)

#' [ Load in simulation from simulation .Rdata object, or from all files in folder? ]
load_simulation_from_all_files <- TRUE

###############################################################################
#' [0AA] Define settings to identify separation status 
###############################################################################
use_glm_convergence_criterion <- TRUE;  # test glm() for convergence to detect separation?

#' [ anyzero() tests each Y:Z outcome:treatment allocation pair for separation.    ]
#' [ GLM will not converge if any cell of the 2x2 (Z x Y) contingency table is 0.  ]
anyzero <- function( .draw, show.table  = TRUE ){
  dtable <- table( .draw$Z, .draw$Y )
  any( dtable == 0 )
}

#' [ custom tryCatch() function ]
# custom tryCatch to return result and warnings -- http://stackoverflow.com/a/24569739/2271856
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}


###############################################################################
#' [0A] Define settings
###############################################################################
timestamp_output <- TRUE;  # Add 'Y-m-d_H-M' to output folder?
round_results <- TRUE;  # Round results when writing table to 'digits_to_round_to'?
digits_to_round_to <- 3;

write_progressfile <- FALSE;  # Write progress file to .csv?
write_parameterfile <- FALSE;  # Write parameter file to .csv?
write_outputfiles <- FALSE;  # Write raw output files to .csv after processing from .Rdata?

keep_output_in_environment <- TRUE;  # make list of lists containing output?
remove_output_after_sourcing <- FALSE;  # de-allocate variables after sourcing?

###############################################################################
#' [0B] Specify file / directory I/O 
###############################################################################
if( timestamp_output ){
  simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H-%M")
  output_directory <- paste0( results_directory, "../../results/output_", simulation_timestamp, "/" )
  if(!dir.exists( output_directory )){
    dir.create( output_directory );
  }
}else{
  output_directory <- paste0( results_directory, "../../results/" )
  if(!dir.exists( output_directory )){
    dir.create( output_directory );
  }
}

#' [ 'metricfile_name' contains evaluations of processed simulation output ]
metricfile_name <- paste0( output_directory, "metrics-", simulation_name, ".csv" ); 
#' [ 'metricfile_name_subset_valid' are metrics subsetted on 'valid' draws (i.e. non-separation) ] 
metricfile_name_subset_valid <- paste0( output_directory, "metrics-subset-", simulation_name, ".csv" ); 
#' [ 'progressfile_name' contains the progress of simulations (ie. nsims by analysis method) ]
progressfile_name <- paste0( output_directory, "progress-", simulation_name, ".csv");
#' [ 'parameterfile_name' contains parameters by 'id_sim' (string of params) and 'modelno' ]
parameterfile_name <- paste0( output_directory, "parameters-", simulation_name, ".csv");
#' [ 'outputfile_name' contains output in .csv format (compared to .Rdata) ]
outputfile_name <- paste0( output_directory, "output-", simulation_name, ".csv");

###############################################################################
#' [1] Load in existing simulation
###############################################################################
if( !load_simulation_from_all_files ){
  cat(paste0("Simulation file 'sim-", simulation_name, ".Rdata' exists... loading from .Rdata file... \n"))
  simulation <- load_simulation(name = simulation_name, dir = results_directory)
}else{
  simulation <- get_simulation_with_all_files(dir = results_directory)
}

###############################################################################
#' [2] Specify method outputs for which analysis/progress tracking is desired
###############################################################################
alloc_method <- c("CR", "SBR", "CAA-MI-2-PBA-0.70", "CAA-MI-2-PBA-1.00");
analysis_method <- c("REG", "RERAND");
adjustment <- c("ADJ", "UN")

#' List of valid output method names
pastey <- function( ... ){ paste( ..., sep = "_")}
method_names_short <- do.call(pastey, expand.grid( alloc_method[1:3], analysis_method[1], adjustment ))
# c( "CR_REG_ADJ", "SBR_REG_ADJ", "CAA-MI-2-PBA-0.70_REG_ADJ" )
# c( "CR_REG_UN", "SBR_REG_UN", "CAA-MI-2-PBA-0.70_REG_UN" )
method_names_rerand <- do.call(pastey, expand.grid( alloc_method[3], analysis_method[2], adjustment ))
# c( "CAA-MI-2-PBA-0.70_RERAND_ADJ", "CAA-MI-2-PBA-0.70_RERAND_UN" )
method_names_deterministic <- do.call(pastey, expand.grid( alloc_method[4], analysis_method, adjustment ))

#' We will attempt to process these method outputs:
methods_to_process <- c(
  method_names_short
  , method_names_rerand
  #  , method_names_deterministic 
)

###############################################################################
#' [2] Writing table of parameters by 'modelno' and 'id_sim'
###############################################################################

cat("Creating parameter table for simulation models and writing to ", parameterfile_name, "... \n"); ptm <- proc.time();
#' This is a data frame of parameters by 'modelno'
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- simulation@dir; })
sim_models <- load( simulation@model_refs )
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- ""; })
params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ unlist( .model@params[c(1:6, 11:16)] ) })))
# 'id_sim' is a concatenated string of all model parameters, hyphen '-' separated
params_by_model$id_sim <- apply( params_by_model, 1, function( .string ) paste0( .string, collapse = "-" ) )
# 'model_no' is a simpler (integer) lookup of models for this simulation.
params_by_model$model_no <- dimnames( params_by_model )[[1]]
if( write_parameterfile ){
  write.csv( params_by_model, file = parameterfile_name, row.names = FALSE )
  cat(paste0("Success! parameter table written to ", parameterfile_name, ". \n"))
}
cat("Elapsed time: \n"); print( proc.time() - ptm );

# Make outputfile_names 
outputfile_names <- paste0( simulation@name, "-model-", params_by_model$model_no, "-output.csv" )

###############################################################################
#' [2] Simulation progress table by 'id_sim'
###############################################################################
if( write_progressfile ){
  #' We will track progress on these methods:
  methods_all <- c( method_names_short, method_names_rerand, method_names_deterministic )

  cat("Creating progress table for simulation results... \n"); ptm <- proc.time();
  methods_all_parsed <- cbind( methods_all, do.call("rbind", strsplit( methods_all, split = "_")))
  colnames( methods_all_parsed ) <- c( "method_name", "alloc_method", "analysis_method", "adjustment")
  
  progress_table <- data.frame( do.call("rbind", rep(list( methods_all_parsed ), times = length(model( simulation )))) )
  dimnames( progress_table )[[ 2 ]] <- c( "method_name", "alloc_method", "analysis_method", "adjustment")
  progress_table$modelno <- do.call("c", sapply(1:length(model( simulation )), function(.x) rep( .x, length( methods_all )), simplify = FALSE))
  progress_table$id_sim <- do.call("c", sapply( params_by_model$id_sim, function(.x) rep( .x, length( methods_all )), simplify = FALSE))
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
}


###############################################################################
#' [2] Process results
###############################################################################

num_simulation_models <- length(model( simulation, reference = TRUE ))  # number of models with data to be analyzed
if( keep_output_in_environment ){
  dfs_by_model <- vector( mode = "list", length = num_simulation_models );
  metrics_by_model <- vector( mode = "list", length = num_simulation_models );
  metrics_by_model_subsetted <- vector( mode = "list", length = num_simulation_models );
}
for( sim_j in 1:num_simulation_models ){
  cat(paste0("[ Model ", sim_j, " of ", num_simulation_models," ][-|       ] Loading output from simulation [ ", 
             simulation@name, " ]...\n")); ptm.all <- proc.time();
  #' Try loading simulation output for model 'sim_j' with output names in 'methods_to_process'
  tryCatch({
    output_j <- output( simulation, subset = sim_j, methods = methods_to_process )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n"); next})
  cat(paste0("Success! \nElapsed time (loading output): \n")); print( proc.time() - ptm.all );
  
  #' Parse 'method_name' strings into {"method_name", "alloc_method", "analysis_method", "adjustment", "nsim"}
  output_method_names <- sapply( output_j, function( .object ){ .object@method_name }) 
  methods_included_parsed <- cbind( output_method_names, 
                                    t(sapply( strsplit( output_method_names, split = "_"), function(.listobj){unlist( .listobj )})),
                                    nsim = 0, time_elapsed = NA )
  colnames( methods_included_parsed ) <- c("method_name", "alloc_method", "analysis_method", "adjustment", "nsim", "time_elapsed")
  
  #' [ NEW 9-Aug-18: Flag conditions by 'separationIndicator' ]
  if( batch_no %in% 1:2 ){
    output_allocs_only <- output( simulation, subset = sim_j, methods = alloc_method )
    output_alloc_names <- sapply( output_allocs_only, function( .Output.obj ){ .Output.obj@method_name })
    draws_look <- draws( simulation, subset = sim_j )
    
    #' [ NEW 12-Aug-18: Flag conditions by 'glm_converged' ]
    if( use_glm_convergence_criterion ){
      #' [ test all draws for glm convergence ]
      adjusted_alloc_names <- methods_included_parsed[ methods_included_parsed[,"adjustment"] == "ADJ", "alloc_method"]
      glm_convergence_status_adj <- matrix( nrow = nsim, ncol = length( adjusted_alloc_names ), 
                                            dimnames = list( 1:nsim, adjusted_alloc_names ))
      unadjusted_alloc_names <- methods_included_parsed[ methods_included_parsed[,"adjustment"] == "UN", "alloc_method"]
      glm_convergence_status_un <- matrix( nrow = nsim, ncol = length( unadjusted_alloc_names ), 
                                              dimnames = list( 1:nsim, unadjusted_alloc_names ))
      
      cat(paste0("[ Model ", sim_j, " ][-|       ] Checking convergence status... [ ", simulation@name, " ]...\n")); ptm.all <- proc.time();
      #' [ NOTE: should iterate over rows of `methods_included_parsed`. ]
      for( sim_i in seq_len( nsim ) ){
        for( alloc_j in seq_along( output_alloc_names ) ){
          df_ij <- data.frame(output_allocs_only[[ alloc_j ]]@out[[ sim_i ]][1:2])
          df_ij$X <- draws_look@draws[[ sim_i ]]$X
          myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
          glm_convergence_status_adj[ sim_i, alloc_j ] <- glm_test$converged
          if( !glm_test$converged ){
            cat(paste0("Simulation [ ", sim_i, " ] Alloc no. [", alloc_j ," ] DID NOT CONVERGE (ADJUSTED).\n"))
          }
          #' [ see if unadjusted estimates are computed. if so, then check convergence status. ]
          if( output_allocs_only[[ alloc_j ]]@method_name == "CR" ){
            myTryCatch( glm_test <- glm( Y ~ Z, family = quasibinomial(link="logit"), data = df_ij ) )
            glm_convergence_status_un[ sim_i, "CR" ] <- glm_test$converged
            if( !glm_test$converged ){
              cat(paste0("Simulation [ ", sim_i, " ] Alloc no. [", alloc_j ," ] DID NOT CONVERGE (UNADJUSTED).\n"))
            }
          }else if( output_allocs_only[[ alloc_j ]]@method_name == "SBR" ){
            myTryCatch( glm_test <- glm( Y ~ Z, family = quasibinomial(link="logit"), data = df_ij ) )
            glm_convergence_status_un[ sim_i, "SBR" ] <- glm_test$converged
            if( !glm_test$converged ){
              cat(paste0("Simulation [ ", sim_i, " ] Alloc no. [", alloc_j ," ] DID NOT CONVERGE (UNADJUSTED).\n"))
            }
          }
        }
      }
      cat(paste0("Success! \nElapsed time (loading output): \n")); print( proc.time() - ptm.all );
    }

    #' matrix 'separation_status' tracks indicator of if separation = TRUE. 
    separation_status <- matrix( nrow = nsim, ncol = length( output_allocs_only ), dimnames = list( 1:nsim, output_alloc_names ))
    for( i in seq_along( output_alloc_names ) ){
      separation_status[, i] <- unlist(lapply( output_allocs_only[[ i ]]@out, anyzero ));
    }
  }
  
  #' Parse model parameters into unique identifier 'id_sim'
  index_exclude <- which( sapply(model( simulation )[[ sim_j ]]@params, function(.x){length(unlist(.x)) > 1} )) # exclude non-scalars
  id_sim <- paste0(model( simulation )[[ sim_j ]]@params[ -index_exclude ], collapse = "-")
  cat(paste0("Unique ID for simulation: ", id_sim, "\n\n")); 
  

  
  cat(paste0("[ Model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
  cat(paste0("[ Model ", sim_j, " ][----|     ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"));
  
  #' Define lists that will store output data
  num_outputs <- length( output_method_names );
  dfs_out_j <- vector( mode = "list", length = num_outputs );
  metrics_by_out_j <- vector( mode = "list", length = num_outputs );
  dfs_out_j_subsetted <- vector( mode = "list", length = num_outputs );
  metrics_by_out_j_subsetted_validse <- vector( mode = "list", length = num_outputs );
  
  true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
  for( i in 1:length( output_j )){
    dfs_out_j[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
    dimnames(dfs_out_j[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
    #' Compute statistics on estimates
    dfs_out_j[[i]]$method_name <- output_j[[i]]@method_name
    dfs_out_j[[i]]$power.pvalue <- with( dfs_out_j[[i]], p < 0.05 )
    dfs_out_j[[i]]$power.rerand <- with( dfs_out_j[[i]], est < cilower | est > ciupper ) 
    dfs_out_j[[i]]$power.ci <- with( dfs_out_j[[i]], 0 < cilower | 0 > ciupper )
    dfs_out_j[[i]]$coverage <- with( dfs_out_j[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs_out_j[[i]]$bias <- with( dfs_out_j[[i]], est - true_trt_effect )
    #' [ NEW 9-Aug-18: add in separation status indicator, computed earlier in 'separation_status' matrix ]
    alloc_method_output_j <- methods_included_parsed[i ,"alloc_method"]
    if( batch_no %in% 1:2 ){
      dfs_out_j[[i]]$separation_status <- separation_status[, alloc_method_output_j]
      if( use_glm_convergence_criterion ){
        if( methods_included_parsed[ i, "adjustment" ] == "ADJ" ){
          dfs_out_j[[i]]$glm_converged <- glm_convergence_status_adj[, alloc_method_output_j]
        }else{
          dfs_out_j[[i]]$glm_converged <- glm_convergence_status_un[, alloc_method_output_j]
        }
      }
    }else{
      dfs_out_j[[i]]$separation_status <- FALSE;
      if( use_glm_convergence_criterion ){
        dfs_out_j[[i]]$glm_converged <- FALSE;
      }
    }
    #' Get simulation metrics: 
    #' > 'time_elapsed' := overall time computing each method, 
    methods_included_parsed[ i, "time_elapsed" ] <- round(sum(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[[10]][3] )}, numeric(1) )),2)
    #' > 'nsim' := all simulations  
    methods_included_parsed[ i, "nsim"] <- dim( dfs_out_j[[i]] )[1]
    #' Compute means of relevant statistics
    indices_colMeans <- which( dimnames(dfs_out_j[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", 
                                                                    "power.rerand","power.ci", "coverage", "bias", "separation_status"))
    metrics_by_out_j[[i]] <- c(colMeans(dfs_out_j[[i]][, indices_colMeans]),
                               median_bias = median( dfs_out_j[[i]]$bias ),
                               nsim = dim( dfs_out_j[[i]] )[1], 
                               methods_included_parsed[ i, "time_elapsed" ],
                               modelno = sim_j, 
                               method_name = output_j[[i]]@method_name);
    
    #' If outcome is binary, then subset on indicator of non-separation (avoid convergence issues with GLM)
    if( batch_no %in% 1:2 ){
      dfs_out_j_subsetted[[i]] <- dfs_out_j[[ i ]][ dfs_out_j[[ i ]]$glm_converged, indices_colMeans ]
      metrics_by_out_j_subsetted_validse[[i]] <- c(colMeans(dfs_out_j_subsetted[[i]]), 
                                                   median_bias = median( dfs_out_j_subsetted[[i]]$bias ),
                                                   nsim = dim( dfs_out_j_subsetted[[i]] )[1], 
                                                   methods_included_parsed[ i, "time_elapsed" ],
                                                   modelno = sim_j, 
                                                   method_name = output_j[[i]]@method_name);
    }
  }
  cat("Success! \nElapsed time: \n\n"); print( proc.time() - ptm );
  
  #' Write output files to .csv?
  dfs_out_all <- do.call( rbind, dfs_out_j )
  if( write_outputfiles ){
    outputfile_name <- outputfile_names[ sim_j ]
    if(!file.exists( outputfile_name )){
      cat(paste0("\nNOTE: Output file: ", outputfile_name, " does not exist. \nCreating output file and saving...\n\n"))
      write.csv( dfs_out_all, file = outputfile_name, row.names = FALSE )
    }else{
      cat(paste0("Appending output from model ", sim_j, " to file ", outputfile_name, "...\n"))
      write.table( dfs_out_all, file = outputfile_name, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
  }

  cat(paste0("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n\n"))
  metrics_all_output <- do.call( rbind, metrics_by_out_j )
  if( round_results ){ #' note: disabling scientific notation
    variables_to_round <- c("power.pvalue", "power.rerand","power.ci", "coverage", "bias", "median_bias", "separation_status")
    metrics_all_output[, variables_to_round ] <- 
      format(round(as.numeric(metrics_all_output[, variables_to_round ]), digits_to_round_to ), scientific = FALSE)
  }
  metrics_all_with_id <- cbind.data.frame( alloc_method = methods_included_parsed[, "alloc_method"], metrics_all_output,
                                          model( simulation )[[ sim_j ]]@params[-index_exclude]) 
  cat("Success! \n\n")
  
  cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
  if(!file.exists( metricfile_name )){
    cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n\n"))
    write.csv( metrics_all_with_id, file = metricfile_name, row.names = FALSE )
  }else{
    cat(paste0("Appending metrics to file ", metricfile_name, "...\n"))
    write.table( metrics_all_with_id, file = metricfile_name, sep = ",", append = TRUE, quote = FALSE,
                 col.names = FALSE, row.names = FALSE)
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  #' If outcome is binary, then write subsetted metrics to file!
  if( batch_no %in% 1:2 ){
    cat(paste0("[ model ", sim_j, " ][------|  ] Computing metrics, EXCLUDING cases with complete separation...\n\n"))
    metrics_all_output_validse <- do.call( rbind, metrics_by_out_j_subsetted_validse )
    if( round_results ){ #' note: disabling scientific notation
      metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")] <- 
        format(round(as.numeric(metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")]), digits_to_round_to ), scientific = FALSE)
    }
    metrics_all_with_id_validse <- cbind.data.frame( alloc_method = methods_included_parsed[, "alloc_method"], metrics_all_output_validse,
                                                     model( simulation )[[ sim_j ]]@params[-index_exclude]) 
    cat("Success! \n\n")
    
    cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write (SUBSETTED) metrics to: ", metricfile_name_subset_valid, "...\n")); ptm <- proc.time();
    if(!file.exists( metricfile_name_subset_valid )){
      cat(paste0("\nNOTE: file: ", metricfile_name_subset_valid, " does not exist. \nCreating file and saving...\n\n"))
      write.csv( metrics_all_with_id_validse, file = metricfile_name_subset_valid, row.names = FALSE )
    }else{
      cat(paste0("Appending metrics to file ", metricfile_name_subset_valid, "...\n"))
      write.table( metrics_all_with_id_validse, file = metricfile_name_subset_valid, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  }

  #' Write progress table to .csv 
  if( write_progressfile ){
    cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write progress table to: ", progressfile_name, "...\n")); ptm <- proc.time();
    #' Make table of all methods we want simulation output for.
    methods_all_parsed <- cbind( methods_all, do.call("rbind", strsplit( methods_all, split = "_")), nsim = 0, time_elapsed = NA)
    colnames( methods_all_parsed ) <- c( "method_name", "alloc_method", "analysis_method", "adjustment", "nsim", "time_elapsed")
    
    #' Get indices of methods with no output.
    method_indices_not_incld <- which( !(methods_all_parsed[, "method_name"] %in% methods_included_parsed[, "method_name"]) )
    progress_by_method_by_model <- cbind(rbind( methods_included_parsed, methods_all_parsed[ method_indices_not_incld, ]),
                                         modelno = sim_j, id_sim = id_sim )
    if(!file.exists( progressfile_name )){
      cat(paste0("\nNOTE: file: ", progressfile_name, " does not exist. \nCreating file and saving...\n\n"))
      write.csv( progress_by_method_by_model, file = progressfile_name, row.names = FALSE )
    }else{
      cat(paste0("Appending progress to file ", progressfile_name, "...\n"))
      write.table( progress_by_method_by_model, file = progressfile_name, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
    #' Compare output methods to 'methods_all_parsed'
    cat(paste0("Simulation [ ", sim_j, " ] has these outputs:\n"))
    print( methods_included_parsed );
    cat(paste0("\n Simulation [ ", sim_j, " ] is missing these outputs:\n"))
    print( methods_all_parsed[ method_indices_not_incld, ] );
  }
  
  #' Save output for each model (so it doesn't disappear in the loop!)
  if( keep_output_in_environment ){
    dfs_by_model[[ sim_j ]] <- dfs_out_all;
    metrics_by_model[[ sim_j ]] <- metrics_all_with_id;
    #' only save subsetted metrics if they exist!
    if( batch_no %in% 1:2 ){
      metrics_by_model_subsetted[[ sim_j ]] <- metrics_all_with_id_validse;
    }
  }
  
  #' Deallocate memory in models after each loop.
  #' ref: https://www.r-bloggers.com/speed-trick-assigning-large-object-null-is-much-faster-than-using-rm/
  if( remove_output_after_sourcing ){
    dfs_out_j <- NULL;
    dfs_out_all <- NULL;
    metrics_by_out_j <- NULL;
    metrics_by_out_j_subsetted_validse <- NULL; 
    metrics_all_output <- NULL;
    metrics_all_output_validse <- NULL;
    metrics_all_with_id <- NULL;
    metrics_all_with_id_validse <- NULL;
  }
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all ); cat("\n\n\n\n");
}

if( keep_output_in_environment ){
  cat("The following variables are stored in the environment:\n")
  cat("`dfs_by_model`              : list of data.frames of all results.\n")
  cat("`metrics_by_model`          : list of data.frames of all metrics.\n")
  cat("`metrics_by_model_subsetted`: list of data.frames of all metrics for subsetted simulations.\n")
}
