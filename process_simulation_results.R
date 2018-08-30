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
#' >> script performance (write_progressfile, .write_outputfiles, etc)
#' >> Memory persistence and deallocation ('.keep_output_in_environment', '.remove_output_after_sourcing')
#' [ 12:15 ]  TODO(michael): update flags for identifying complete separation!
#' [ 13:32 ] output references are broken in cases where output objects deleted manually. 
#' [ 14:28 ] FIXED! Note: used get_simulation_with_all_files() to load in simulation.
#' [ 14:33 ] Running batch-2 processing, took on avg 6sec to run code (vs 120sec!) 
#' [ 14:34 ] Running batch-1 processing
# --------------------------------------------------------------------------- # 
#' [ 12 August 2018 (Sunday) ]
#' [ 16:30 ] added toggle `load_simulation_from_all_files` to load in new simulation output (rerand adjusted ests)
# --------------------------------------------------------------------------- # 
#' [ 14 August 2018 (Tuesday) ]
#' [ 09:25] Final modifications to `glm_converged` toggle. Focused on using column names to access.

library("simulator");

#' [ 'results_directory' contains folder 'files' with 'sim-{simulation_name}.Rdata' ] 
batch_no <- 3;  # NOTE: if 'batch_no' is 1 or 2, will do data subsetting on separation status.
simulation_name <- paste0("alloc-simulation-batch-", batch_no, "-of-4");
results_directory <- paste0("/Users/Moschops/Documents/MSThesis/datasets/batch-", batch_no, "/");
nsim <- 5010;  #' number of simulations (for pre-allocation)

#' [ Load in simulation from simulation .Rdata object, or from all files in folder? ]
load_simulation_from_all_files <- TRUE

###############################################################################
#' [0AA] Define settings to identify separation status 
###############################################################################
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

#'[ test_glm_convergence_both_adj_unadj() returns convergence result to glm(), adjusting for X if `adjustX` is TRUE. ]
test_glm_convergence <- function( drawobj, outobj, adjustX = TRUE){
  df_ij <- data.frame( outobj[1:2] )
  if( adjustX ){
    df_ij$X <- drawobj$X 
    myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
  }else{
    myTryCatch( glm_test <- glm( Y ~ Z, family = quasibinomial(link="logit"), data = df_ij ) )
  }
  return( glm_test$converged )
}

#'[ test_glm_convergence_both_adj_unadj() returns convergence result to glm() for both unadjusted and adjusted models. ]
test_glm_convergence_both_adj_unadj <- function( drawobj, outobj ){
  df_ij <- data.frame( outobj[1:2] )
  df_ij$X <- drawobj$X 
  myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
  myTryCatch( glm_test_un <- glm( Y ~ Z, family = quasibinomial(link="logit"), data = df_ij ) )
  return(c(adj = glm_test$converged,
           un = glm_test_un$converged))
}

###############################################################################
#' [0A] Define settings
###############################################################################
.timestamp_output <- TRUE;  # Add 'Y-m-d_H-M' to output folder?
.round_results <- FALSE;  # Round results when writing table to '.digits_to_round_to'?
.digits_to_round_to <- 3;

.write_parameterfile <- TRUE;  # Write parameter file to .csv?
.write_outputfiles <- FALSE;  # Write raw output files to .csv after processing from .Rdata?

.keep_output_in_environment <- TRUE;  # make list of lists containing output?
.remove_output_after_sourcing <- TRUE;  # de-allocate variables after sourcing?

###############################################################################
#' [0B] Specify file / directory I/O 
###############################################################################
if( .timestamp_output ){
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
if( .write_parameterfile ){
  write.csv( params_by_model, file = parameterfile_name, row.names = FALSE )
  cat(paste0("Success! parameter table written to ", parameterfile_name, ". \n"))
}
cat("Elapsed time: \n"); print( proc.time() - ptm );

# Make outputfile_names 
outputfile_names <- paste0( simulation@name, "-model-", params_by_model$model_no, "-output.csv" )

###############################################################################
#' [2] Process results
###############################################################################
#' [ `alloc_methods` are the allocation methods for evaluation ]
alloc_methods <- c("CR", "SBR", "CAA-MI-2-PBA-0.70", "CAA-MI-2-PBA-1.00"); 
num_simulation_models <- length(model( simulation, reference = TRUE ))  # number of models with data to be analyzed
if( .keep_output_in_environment ){
  dfs_by_model <- vector( mode = "list", length = num_simulation_models );
  metrics_by_model <- vector( mode = "list", length = num_simulation_models );
  metrics_by_model_subsetted <- vector( mode = "list", length = num_simulation_models );
}

for( model_i in 1:num_simulation_models ){
  cat(paste0("[ Model ", model_i, " of ", num_simulation_models," ][-|       ] Loading output from simulation [ ", 
             simulation@name, " ]...\n")); ptm.all <- proc.time();
  #' Try loading simulation output for model 'model_i' with output names in 'methods_to_process'
  out_model_i <- output( simulation, subset = model_i, methods = methods_to_process )
  cat(paste0("Success! \nElapsed time (loading output): \n")); print( proc.time() - ptm.all );
  
  #' Parse 'method_name' strings into {"method_name", "alloc_method", "analysis_method", "adjustment", "nsim"}
  output_method_names <- sapply( out_model_i, function( .object ){ .object@method_name }) 
  methods_included_parsed <- cbind( output_method_names, 
                                    t(sapply( strsplit( output_method_names, split = "_"), function(.listobj){unlist( .listobj )})),
                                    nsim = 0, time_elapsed = NA )
  colnames( methods_included_parsed ) <- c("method_name", "alloc_method", "analysis_method", "adjustment", "nsim", "time_elapsed")
  
  #' [ NEW 9-Aug-18: Flag conditions by 'separationIndicator' ]
  if( batch_no %in% 1:2 ){
    #' [ NEW 12-Aug-18: Flag conditions by 'glm_converged' ]
    cat(paste0("[ Model ", model_i, " ][---|      ] Testing output for glm() convergence...\n")); ptm <- proc.time();
    glm_convergence_by_output_j <- matrix( nrow = nsim, ncol = nrow( methods_included_parsed ))
    colnames( glm_convergence_by_output_j ) <- methods_included_parsed[, "method_name"]
    #' [ Get (X) draws ]
    draws_model_i <- draws( simulation, subset = model_i )@draws
    #' [ Get unique alloc_method names ]
    alloc_methods_unique <- unique( methods_included_parsed[, "alloc_method"] )
    for( method_k in seq_along( alloc_methods_unique )){
      alloc_name_k <- alloc_methods_unique[ method_k ]
      cat(paste0("Testing method: ", alloc_name_k, " for glm() convergence...\n" ))
      # indices for method_k in output table.
      method_k_indices <- which( alloc_name_k == methods_included_parsed[ , "alloc_method" ] )
      #' [ Get (Z,Y) output for corresponding alloc_method ]
      ZY_model_i <- output( simulation, subset = model_i, methods = alloc_name_k )@out
      if( alloc_name_k == "CAA-MI-2-PBA-0.70" ){
        #' TODO(michael): test to make sure there's no difference between adjusted and unadjusted estimates!
        glm_convergence_by_output_j[ , method_k_indices ] <- mapply( test_glm_convergence, draws_model_i, ZY_model_i, MoreArgs = list( adjustX = TRUE ))
      }else{
        glm_convergence_by_output_j[, method_k_indices ] <- t( mapply( test_glm_convergence_both_adj_unadj, draws_model_i, ZY_model_i ) )
      }
    }
    #' matrix 'separation_status' tracks indicator of if separation = TRUE. 
    separation_status <- matrix( nrow = nsim, ncol = length( alloc_methods_unique ), dimnames = list( 1:nsim, alloc_methods_unique ))
    allocs_outcomes_model_i <- output( simulation, subset = model_i, methods = alloc_methods_unique )
    for( out_j in seq_along( alloc_methods_unique ) ){
      separation_status[, out_j] <- unlist(lapply( allocs_outcomes_model_i[[ out_j ]]@out, anyzero ));
    }
    cat(paste0("Success! \nElapsed time (testing glm() convergence): \n")); print( proc.time() - ptm.all );
  }
  
  #' Parse model parameters into unique identifier 'id_sim'
  index_exclude <- which( sapply(model( simulation )[[ model_i ]]@params, function(.x){length(unlist(.x)) > 1} )) # exclude non-scalars
  id_sim <- paste0(model( simulation )[[ model_i ]]@params[ -index_exclude ], collapse = "-")
  cat(paste0("Unique ID for simulation: ", id_sim, "\n\n")); 
  
  #' Define lists that will store output data
  num_outputs <- length( output_method_names );
  dfs_out_j <- vector( mode = "list", length = num_outputs );
  metrics_by_out_j <- vector( mode = "list", length = num_outputs );
  dfs_out_j_subsetted <- vector( mode = "list", length = num_outputs );
  metrics_by_out_j_subsetted_validse <- vector( mode = "list", length = num_outputs );
  
  true_trt_effect <- model( simulation )[[ model_i ]]@params$bZ
  
  cat(paste0("[ Model ", model_i, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
  cat(paste0("[ Model ", model_i, " ][----|     ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"));
  
  for( out_j in 1:length( out_model_i )){
    dfs_out_j[[ out_j ]]  <- data.frame(t(vapply( out_model_i[[ out_j ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
    dimnames(dfs_out_j[[ out_j ]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
    #' Compute statistics on estimates TODO(michael): is using with() here referring to the wrong dfs_out_j[[ model_i ]]??
    dfs_out_j[[ out_j ]]$method_name <- out_model_i[[ out_j ]]@method_name
    dfs_out_j[[ out_j ]]$power.pvalue <- with( dfs_out_j[[ out_j ]], p < 0.05 )
    dfs_out_j[[ out_j ]]$power.rerand <- with( dfs_out_j[[ out_j ]], est < cilower | est > ciupper ) 
    dfs_out_j[[ out_j ]]$power.ci <- with( dfs_out_j[[ out_j ]], 0 < cilower | 0 > ciupper )
    dfs_out_j[[ out_j ]]$coverage <- with( dfs_out_j[[ out_j ]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs_out_j[[ out_j ]]$bias <- with( dfs_out_j[[ out_j ]], est - true_trt_effect )
    #' [ NEW 9-Aug-18: add in separation status indicator, computed earlier in 'separation_status' matrix ]
    if( batch_no %in% 1:2 ){
      alloc_method_out_model_i <- methods_included_parsed[ out_j, "alloc_method" ]
      dfs_out_j[[ out_j ]]$separation_status <- separation_status[, alloc_method_out_model_i ]
      dfs_out_j[[ out_j ]]$glm_converged <- glm_convergence_by_output_j[, out_j ]
    }
    #' Get simulation metrics: 
    #' > 'time_elapsed' := overall time computing each method, 
    methods_included_parsed[ out_j, "time_elapsed" ] <- round(sum(vapply( out_model_i[[ out_j ]]@out, function( .list ){ unlist( .list[[10]][3] )}, numeric(1) )),2)
    #' > 'nsim' := all simulations  
    methods_included_parsed[ out_j, "nsim"] <- dim( dfs_out_j[[ out_j ]] )[1]
    #' Compute means of relevant statistics
    indices_colMeans <- which( dimnames(dfs_out_j[[ out_j ]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", 
                                                                          "power.rerand","power.ci", "coverage", "bias"))
    metrics_by_out_j[[ out_j ]] <- c(colMeans(dfs_out_j[[ out_j ]][, indices_colMeans]),
                                     median_bias = median( dfs_out_j[[ out_j ]]$bias ),
                                     nsim = dim( dfs_out_j[[ out_j ]] )[1], 
                                     methods_included_parsed[ out_j, "time_elapsed" ],
                                     modelno = model_i, 
                                     method_name = out_model_i[[ out_j ]]@method_name);
    
    #' If outcome is binary, then subset on indicator of non-separation (avoid convergence issues with GLM)
    if( batch_no %in% 1:2 ){
      dfs_out_j_subsetted[[ out_j ]] <- dfs_out_j[[ out_j ]][ dfs_out_j[[ out_j ]]$glm_converged & !dfs_out_j[[ out_j ]]$separation_status, indices_colMeans ]
      metrics_by_out_j_subsetted_validse[[ out_j ]] <- c(colMeans(dfs_out_j_subsetted[[ out_j ]]), 
                                                   median_bias = median( dfs_out_j_subsetted[[ out_j ]]$bias ),
                                                   nsim = dim( dfs_out_j_subsetted[[ out_j ]] )[1], 
                                                   methods_included_parsed[ out_j, "time_elapsed" ],
                                                   modelno = model_i, 
                                                   method_name = out_model_i[[ out_j ]]@method_name);
    } # end if outcome = BINARY
  }
  cat("Success! \nElapsed time: \n\n"); print( proc.time() - ptm );
  
  dfs_out_all <- do.call( rbind, dfs_out_j )
  metrics_all_output <- do.call( rbind, metrics_by_out_j )
  metrics_all_with_id <- cbind.data.frame( alloc_method = methods_included_parsed[, "alloc_method"], metrics_all_output,
                                           model( simulation )[[ model_i ]]@params[-index_exclude]) 
  #' Write output files to .csv?
  if( .write_outputfiles ){
    outputfile_name <- outputfile_names[ model_i ]
    if(!file.exists( outputfile_name )){
      cat(paste0("\nNOTE: Output file: ", outputfile_name, " does not exist. \nCreating output file and saving...\n\n"))
      write.csv( dfs_out_all, file = outputfile_name, row.names = FALSE )
    }else{
      cat(paste0("Appending output from model ", model_i, " to file ", outputfile_name, "...\n"))
      write.table( dfs_out_all, file = outputfile_name, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
  }

  if( .round_results ){ #' note: disabling scientific notation
    variables_to_round <- c("power.pvalue", "power.rerand","power.ci", "coverage", "bias", "median_bias" )
    metrics_all_output[, variables_to_round ] <- 
      format(round(as.numeric(metrics_all_output[, variables_to_round ]), .digits_to_round_to ), scientific = FALSE)
  }
  
  cat(paste0("[ model ", model_i, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
  if(!file.exists( metricfile_name )){
    cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n\n"))
    write.csv( metrics_all_with_id, file = metricfile_name, row.names = FALSE )
  }else{
    cat(paste0("Appending metrics to file ", metricfile_name, "...\n"))
    write.table( metrics_all_with_id, file = metricfile_name, sep = ",", append = TRUE, quote = FALSE,
                 col.names = FALSE, row.names = FALSE)
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );

  if( batch_no %in% 1:2 ){
    cat(paste0("[ model ", model_i, " ][------|  ] Computing metrics, EXCLUDING cases with complete separation...\n\n"))
    metrics_all_output_validse <- do.call( rbind, metrics_by_out_j_subsetted_validse )
    if( .round_results ){ #' note: disabling scientific notation
      metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")] <- 
        format(round(as.numeric(metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")]), .digits_to_round_to ), scientific = FALSE)
    }
    metrics_all_with_id_validse <- cbind.data.frame( alloc_method = methods_included_parsed[, "alloc_method"], metrics_all_output_validse,
                                                     model( simulation )[[ model_i ]]@params[-index_exclude]) 
    cat("Success! \n\n")
    
    cat(paste0("[ model ", model_i, " ][--------|] Attempting to write (SUBSETTED) metrics to: ", metricfile_name_subset_valid, "...\n")); ptm <- proc.time();
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
  
  #' Save output for each model (so it doesn't disappear in the loop!)
  if( .keep_output_in_environment ){
    dfs_by_model[[ model_i ]] <- dfs_out_all;
    metrics_by_model[[ model_i ]] <- metrics_all_with_id;
    #' only save subsetted metrics if they exist!
    if( batch_no %in% 1:2 ){
      metrics_by_model_subsetted[[ model_i ]] <- metrics_all_with_id_validse;
    }
  }
  
  #' Deallocate memory in models after each loop.
  #' ref: https://www.r-bloggers.com/speed-trick-assigning-large-object-null-is-much-faster-than-using-rm/
  if( .remove_output_after_sourcing ){
    dfs_out_j <- NULL;
    dfs_out_all <- NULL;
    metrics_by_out_j <- NULL;
    metrics_by_out_j_subsetted_validse <- NULL; 
    metrics_all_output <- NULL;
    metrics_all_output_validse <- NULL;
    metrics_all_with_id <- NULL;
    metrics_all_with_id_validse <- NULL;
  }
  cat(paste0("Simulation model [ ", model_i, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all ); cat("\n\n\n\n");
}

if( .keep_output_in_environment ){
  cat("The following variables are stored in the environment:\n")
  cat("`dfs_by_model`              : list of data.frames of all results.\n")
  cat("`metrics_by_model`          : list of data.frames of all metrics.\n")
  cat("`metrics_by_model_subsetted`: list of data.frames of all metrics for subsetted simulations.\n")
}

plot_results <- FALSE
if( plot_results ){
  hibiscus <- as.data.table( dfs_by_model[[ 39 ]] )
  hibiscus <- hibiscus[ method_name == "CAA-MI-2-PBA-0.70_RERAND_ADJ" ]
  setkey( hibiscus, cilower, est )
  require("plotrix")
  with( hibiscus[order(est)], plotrix::plotCI( 1:5010, y = est, ui = ciupper, li = cilower, col = ifelse( coverage, "red", "blue") ))
  
}


eval_metrics_by_hand <- FALSE
if( eval_metrics_by_hand ){
#' [ NEW 17 Aug 2018: evaluate metrics on each data frame in `dfs_by_model` ]
methname <- "CR_REG_ADJ"
mets <- vector( mode = "list", 10 )
for( mod_i in seq_along( dfs_by_model[1:10] ) ){
  foob <- dfs_by_model[[ mod_i ]][ dfs_by_model[[mod_i]]$method_name == methname, ]
  indices_colMeans <- which( dimnames(foob)[[2]] %in% c("adjusted","rerandomized", "power.pvalue", 
                                                                        "power.rerand","power.ci", "coverage", "bias"))
  print(paste0("Model ", mod_i))
  print( colMeans(foob[, indices_colMeans]) )
  mets[[ mod_i ]] <- c(colMeans(foob[, indices_colMeans]),
                       median_bias = median( foob$bias ),
                       nsim = dim( foob )[1], 
                       methods_included_parsed[ 6, "time_elapsed" ],
                       modelno = mod_i, 
                       method_name = unique( foob$method_name ));
  rm("foob")
}
foob <- dfs_by_model[[1]]
}

#' If outcome is binary, then subset on indicator of non-separation (avoid convergence issues with GLM)
process_subsetting_after_running <- FALSE

if( process_subsetting_after_running ){
  cat(paste0("[ model ", model_i, " ][------|  ] Computing metrics, EXCLUDING cases with complete separation...\n\n"))
  
  dfs_out_j_subsetted <- vector( mode = "list", num_simulation_models )
  metrics_by_out_j_subsetted_validse <- vector( mode = "list", num_simulation_models )
  
  for( model_q in 1:num_simulation_models ){
    dfs_out_j <- split( dfs_by_model[[ model_q ]], dfs_by_model[[ model_q ]]$method_name )
    for( out_j in seq_along( dfs_out_j ) ){
      dfs_out_j_subsetted[[ out_j ]] <- dfs_out_j[[ out_j ]][ dfs_out_j[[ out_j ]]$glm_converged & !dfs_out_j[[ out_j ]]$separation_status, indices_colMeans ]
      metrics_by_out_j_subsetted_validse[[ out_j ]] <- c(colMeans(dfs_out_j_subsetted[[ out_j ]]), 
                                                   median_bias = median( dfs_out_j_subsetted[[ out_j ]]$bias ),
                                                   nsim = dim( dfs_out_j_subsetted[[ out_j ]] )[1], 
                                                   methods_included_parsed[ out_j, "time_elapsed" ],
                                                   modelno = model_q, 
                                                   method_name = names( dfs_out_j )[i]);
    } # end for( methods i )
    metrics_all_output_validse <- do.call( rbind, metrics_by_out_j_subsetted_validse )
    if( .round_results ){ #' note: disabling scientific notation
      metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")] <- 
        format(round(as.numeric(metrics_all_output_validse[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")]), .digits_to_round_to ), scientific = FALSE)
    }
    metrics_all_with_id_validse <- cbind.data.frame( alloc_method = methods_included_parsed[, "alloc_method"], metrics_all_output_validse,
                                                     model( simulation )[[ model_q ]]@params[-index_exclude]) 
    cat(paste0("[ model ", model_q, " ][--------|] Attempting to write (SUBSETTED) metrics to: ", metricfile_name_subset_valid, "...\n")); ptm <- proc.time();
    if(!file.exists( metricfile_name_subset_valid )){
      cat(paste0("\nNOTE: file: ", metricfile_name_subset_valid, " does not exist. \nCreating file and saving...\n\n"))
      write.csv( metrics_all_with_id_validse, file = metricfile_name_subset_valid, row.names = FALSE )
    }else{
      cat(paste0("Appending metrics to file ", metricfile_name_subset_valid, "...\n"))
      write.table( metrics_all_with_id_validse, file = metricfile_name_subset_valid, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  } # end for( model_q )
  cat("Success! \n\n")
}


