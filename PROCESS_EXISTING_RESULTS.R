#' Author: Michael Flanagin
#' Date started: 3 July 2018 13:13
#' Date last modified: 6 July 2018 15:03
#' objective: Analysis of existing output

library("simulator");

###############################################################################
#' [0] Define settings
###############################################################################
#' [ 'results_directory' contains folder 'files' with 'sim-{simulation_name}.Rdata' ] 
simulation_name <- "alloc-simulation-batch-1-of-4"
results_directory <- "/Users/Moschops/Documents/MSThesis/datasets/batch-1/results/"
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H-%M")  

#' [ 'metricfile_name' contains evaluations of processed simulation output ] 
metricfile_name <- paste0( results_directory, "../metrics-", simulation_name, ".csv" ); 
#' [ 'progressfile_name' contains the progress of simulations (ie. nsims by analysis method) ]
progressfile_name <- paste0( results_directory, "../progress-", simulation_name, ".csv");
#' [ 'parameterfile_name' contains parameters by 'id_sim' (string of params) and 'modelno' ]
parameterfile_name <- paste0( results_directory, "../parameters-", simulation_name, ".csv");

round_results <- TRUE;
large_se_breakpoint <- 1000;  # break point for calling SE 'large' (aka complete/quasi-separation)
digits_to_round_to <- 6;

###############################################################################
#' [1] Load in existing simulation
###############################################################################
if(!exists( "simulation" )){
  if(file.exists(paste0(results_directory, "files/sim-", simulation_name, ".Rdata"))){
    cat("Simulation file exists... loading from .Rdata file... \n")
    simulation <- load_simulation(name = simulation_name, dir = results_directory)
  }else{
    simulation <- get_simulation_with_all_files(dir = results_directory)
  }
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
method_names_rerand <- do.call(pastey, expand.grid( alloc_method[3], analysis_method[2], adjustment ))
method_names_deterministic <- do.call(pastey, expand.grid( alloc_method[4], analysis_method, adjustment ))

#' We will attempt to process these method outputs:
methods_to_process <- c(
  method_names_short
  , method_names_rerand
  #  , method_names_deterministic 
)

#' We will track progress on these methods:
methods_all <- c( method_names_short, method_names_rerand, method_names_deterministic )


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
write.csv( params_by_model, file = parameterfile_name, row.names = FALSE )
cat(paste0("Success! parameter table written to ", parameterfile_name, ". \nElapsed time: \n")); print( proc.time() - ptm );


###############################################################################
#' [2] Simulation progress table by 'id_sim'
###############################################################################
cat("Creating progress table for simulation results... \n"); ptm <- proc.time();
methods_all_parsed <- cbind( methods_all, do.call("rbind", strsplit( methods_all, split = "_")))
colnames( methods_all_parsed ) <- c( "method_name", "alloc_method", "analysis_method", "adjustment")

progress_table <- data.frame( do.call("rbind", rep(list( methods_all_parsed ), times = length(model( simulation )))) )
dimnames( progress_table )[[ 2 ]] <- c( "method_name", "alloc_method", "analysis_method", "adjustment")
progress_table$modelno <- do.call("c", sapply(1:length(model( simulation )), function(.x) rep( .x, length( methods_all )), simplify = FALSE))
progress_table$id_sim <- do.call("c", sapply( params_by_model$id_sim, function(.x) rep( .x, length( methods_all )), simplify = FALSE))
cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );

###############################################################################
#' [2] Process results
###############################################################################
for( sim_j in 1:length(model( simulation )) ){
  cat(paste0("[ Model ", sim_j, " ][-|       ] Loading output from simulation [ ", simulation@name, " ]...\n")); ptm.all <- proc.time();
  tryCatch({
    output_j <- output( simulation, methods = methods_to_process )[[ sim_j ]] #' Model sim_j, 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n"); next})
  cat(paste0("Success! \nElapsed time (loading output): \n")); print( proc.time() - ptm.all );
  
  index_exclude <- which( sapply(model( simulation )[[ sim_j ]]@params, function(.x){length(unlist(.x)) > 1} )) # exclude non-scalars
  id_sim <- paste0(model( simulation )[[ sim_j ]]@params[ -index_exclude ], collapse = "-")
  cat(paste0("Unique ID for simulation: ", id_sim, "\n\n")); 
  
  #' Get method names with output (compare to 'methods_all_parsed')
  output_method_names <- sapply( output_j, function( .object ){ .object@method_name }) 
  #' Parse 'method_name' strings into {"method_name", "alloc_method", "analysis_method", "adjustment", "nsim"}
  methods_included_parsed <- cbind( output_method_names, 
                                    t(sapply( strsplit( output_method_names, split = "_"), function(.listobj){unlist( .listobj )})),
                                    nsim = 0, time_elapsed = NA )
  colnames( methods_included_parsed ) <- c("method_name", "alloc_method", "analysis_method", "adjustment", "nsim", "time_elapsed")
  
  #' Make table of all methods we want simulation output for.
  methods_all_parsed <- cbind( methods_all, do.call("rbind", strsplit( methods_all, split = "_")), nsim = 0, time_elapsed = NA)
  colnames( methods_all_parsed ) <- c( "method_name", "alloc_method", "analysis_method", "adjustment", "nsim", "time_elapsed")

  cat(paste0("[ Model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
  cat(paste0("[ Model ", sim_j, " ][----|     ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"));
  dfs_out_j <- list();
  metrics_by_out_j <- list();
  true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
  for( i in 1:length( output_j )){
    dfs_out_j[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
    dimnames(dfs_out_j[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
    dfs_out_j[[i]]$power.pvalue <- with( dfs_out_j[[i]], p < 0.05 )
    dfs_out_j[[i]]$power.rerand <- with( dfs_out_j[[i]], est < cilower | est > ciupper ) 
    dfs_out_j[[i]]$power.ci <- with( dfs_out_j[[i]], 0 < cilower | 0 > ciupper )
    dfs_out_j[[i]]$coverage <- with( dfs_out_j[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs_out_j[[i]]$bias <- with( dfs_out_j[[i]], est - true_trt_effect )
    dfs_out_j[[i]]$segt1k <- with( dfs_out_j[[i]], se > large_se_breakpoint )  # NEW: how many estimates have SE greater than 1000?
    #' TODO(michael): find draws corresponding to large SEs (suspect quasi- or complete separation e.g. one group has 0/all events)
    methods_included_parsed[ i, "time_elapsed" ] <- round(sum(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[[10]][3] )}, numeric(1) )),2)
    methods_included_parsed[ i, "nsim"] <- dim( dfs_out_j[[i]] )[1]
    indices_colMeans <- which( dimnames(dfs_out_j[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias", "segt1k"))
    metrics_by_out_j[[i]] <- c(colMeans(dfs_out_j[[i]][, indices_colMeans]), nsim = dim( dfs_out_j[[i]] )[1], 
                               methods_included_parsed[ i, "time_elapsed" ],
                               modelno = sim_j, method_name = output_j[[i]]@method_name);
  }
  cat("Success! \nElapsed time: \n\n"); print( proc.time() - ptm );
  
  #' Compare output methods to 'methods_all_parsed'
  cat(paste0("Simulation [ ", sim_j, " ] has these outputs:\n"))
  print( methods_included_parsed );
  cat(paste0("\n Simulation [ ", sim_j, " ] is missing these outputs:\n"))
  print( methods_all_parsed[ method_indices_not_incld, ] );
  
  cat(paste0("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n\n"))
  metrics_all_output <- do.call(rbind, metrics_by_out_j)
  if( round_results ){ #' note: disabling scientific notation
    metrics_all_output[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias", "segt1k")] <- 
      format(round(as.numeric(metrics_all_output[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias", "segt1k")]), digits_to_round_to ), scientific = FALSE)
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
  
  cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write progress table to: ", metricfile_name, "...\n")); ptm <- proc.time();
  #' Get indices of methods with no output.
  method_indices_not_incld <- which( !(methods_all_parsed[, "method_name"] %in% methods_included_parsed[, "method_name"]) )
  progress_by_method_by_model <- cbind(rbind( methods_included_parsed, methods_all_parsed[ method_indices_not_incld, ]),
                                       modelno = sim_j, id_sim = id_sim )
  if(!file.exists( progressfile_name )){
    cat(paste0("\nNOTE: file: ", progressfile_name, " does not exist. \nCreating file and saving...\n\n"))
    write.csv( progress_by_method_by_model, file = metricfile_name, row.names = FALSE )
  }else{
    cat(paste0("Appending progress to file ", progressfile_name, "...\n"))
    write.table( progress_by_method_by_model, file = progressfile_name, sep = ",", append = TRUE, quote = FALSE,
                 col.names = FALSE, row.names = FALSE)
  }
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all );
  cat("\n\n\n\n")
}
