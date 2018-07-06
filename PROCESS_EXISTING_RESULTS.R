#' 3 July 2018 13:13
#' Batch 1: Binary outcome, binary predictor
#' Analysis of existing output

library("simulator");

###############################################################################
#' [0] Define settings
###############################################################################
simulation_name <- "alloc-simulation-batch-1-of-4"
#' [ 'results_directory' contains folder 'files' with .Rdata model, draw, output, evals ] 
results_directory <- "/Users/Moschops/Documents/MSThesis/datasets/batch-1/results"
#' [ 'metricfile_name' contains model, draw, output, evaluation info ] 
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H-%M")  
metricfile_name <- paste0( results_directory, "metrics-", simulation_name, ".csv" ); 
completionfile_name <- paste0( results_directory, "progress-", simulation_name, ".csv");
round_results <- TRUE;
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
alloc_method <- c("CR", "SBR", "CAA-MI-2-PBA-0.70", "CAA-MI-2-PBA-1.00");
analysis_method <- c("REG", "RERAND");
adjustment <- c("ADJ", "UN")

#' List of valid output method names
pastey <- function( ... ){ paste( ..., sep = "_")}
method_names_short <- do.call(pastey, expand.grid( alloc_method[1:3], analysis_method[1], adjustment ))
method_names_rerand <- do.call(pastey, expand.grid( alloc_method[3], analysis_method[2], adjustment ))
method_names_deterministic <- do.call(pastey, expand.grid( alloc_method[4], analysis_method, adjustment ))

#' We will attempt to process these method outputs:
methods_to_use <- c(
  method_names_short
  , method_names_rerand
  #  , method_names_deterministic 
)

#' We will track progress on these methods:
methods_all <- c( method_names_short, method_names_rerand, method_names_deterministic )

#' This is a data frame of parameters by 'modelno'
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- simulation@dir; })
sim_models <- load( simulation@model_refs )
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- ""; })
params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ unlist( .model@params[c(1:6, 11:16)] ) })))
params_by_model$modelno <- dimnames( params_by_model )[[1]]
###############################################################################

do.call("rbind", sapply(rep( methods_all, length(model( simulation ))), function( .x ) strsplit( .x, split = "_")))
foo <- expand.grid( methods_all, 1:length(model(simulation)))

methods_all_parsed <- do.call("rbind", strsplit( methods_all, split = "_"))
progress_table <- data.frame( do.call("rbind", rep(list( methods_all_parsed ), times = length(model( simulation )))) )
dimnames( progress_table )[[ 2 ]] <- c("alloc_method", "analysis_method", "adjustment")
progress_table$modelno <- do.call("c", sapply(1:length(model( simulation )), function(.x) rep( .x, length( methods_all )), simplify = FALSE))

###############################################################################
#' [2] Process results
###############################################################################
for( sim_j in 1:length(model( simulation )) ){
  cat(paste0("[ Model ", sim_j, " ][-|       ] Loading output from simulation [ ", simulation@name, " ]...\n")); ptm.all <- proc.time();
  tryCatch({
    output_j <- output( simulation, methods = methods_to_use )[[ sim_j ]] #' Model sim_j, 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n"); next})
  output_method_names <- sapply( output_j, function( .object ){ .object@method_name }) #' get method names (compare to 'methods_to_use')
  methods_included_parsed <- t(sapply( strsplit( output_method_names, split = "_"), function(.listobj){unlist( .listobj )}))
  dimnames( methods_included_parsed ) <- list( output_method_names, c("alloc_method", "analysis_method", "adjustment"))
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm.all );
  cat(paste0("Simulation [ ", sim_j, " ] has these outputs:\n"))
  print( methods_included_parsed )
  
  cat(paste0("[ model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
  dfs <- list();
  for( i in 1:length( output_j )){
    dfs[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
    dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  cat(paste0("[ model ", sim_j, " ][----|    ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n")); ptm <- proc.time();
  metrics_by_dfs <- list();
  true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
  for( i in 1:length( dfs )){
    dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
    dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper ) 
    dfs[[i]]$power.ci <- with( dfs[[i]], 0 < cilower | 0 > ciupper )
    dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
    inds <- which( dimnames(dfs[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias"))
    metrics_by_dfs[[i]] <- c(colMeans(dfs[[i]][, inds]), numsim = dim( dfs[[i]] )[1], modelno = sim_j );
  }
  cat(paste0("Success! \nElapsed time: \n\n")); print( proc.time() - ptm );
  
  index_exclude <- which( sapply(model( simulation )[[ sim_j ]]@params, function(.x){length(unlist(.x)) > 1} )) # exclude non-scalars
  id_sim <- paste0(model( simulation )[[ sim_j ]]@params[ -index_exclude ], collapse = "-")
  cat(paste0("Unique ID for simulation output file: ", id_sim, ".csv\n\n")); 
  
  cat(paste0("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n\n"))
  metrics_df <- do.call(rbind, metrics_by_dfs)
  metrics_df_with_id <- cbind.data.frame( alloc_method = methods_included_parsed[,"alloc_method"], metrics_df,
                                          model( simulation )[[ sim_j ]]@params[-index_exclude]) 
  cat("Success! \n\n")
  
  if( round_results ){
    metrics_df_with_id[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")] <- 
      round( metrics_df_with_id[, c("power.pvalue", "power.rerand","power.ci", "coverage", "bias")], digits_to_round_to )
  }
  cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
  if(!file.exists( metricfile_name )){
    cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n\n"))
    write.csv( metrics_df_with_id, file = metricfile_name, row.names = FALSE )
    cat(paste0("\nNOTE: file: ", completionfile_name, " does not exist. \nCreating file and saving...\n\n"))
    write.csv( metrics_df_with_id, file = completionfile_name, row.names = FALSE )
  }else{
    cat(paste0("Appending metrics to file ", metricfile_name, "...\n"))
    write.table( metrics_df_with_id, file = metricfile_name, sep = ",", append = TRUE, quote = FALSE,
                 col.names = FALSE, row.names = FALSE)
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all );
  cat("\n\n\n\n")
}
