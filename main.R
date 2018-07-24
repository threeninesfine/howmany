## @knitr main
## author             = Michael Flanagin
## date started       = 2018 May 16
## date last modified = 2018 May 28
## advisor            = Amalia Magaret
## objective          = Main simulator file.

library("simulator") # this file was created under simulator version 0.2.0
# --------------------------------------------------------------------------- #
#' ------------------------ [ 0. Conditions ] ------------------------------- #
# --------------------------------------------------------------------------- #
#' [ step 0: set file management ]
#' Batch 1 will have: outcome = "binary" & predictor = "binary"
simulation_name <- "alloc-simulation-batch-1-of-4"
#' Batch 2 will have: outcome = "binary" & predictor = "continuous"
# simulation_name <- "alloc-simulation-batch-2-of-4"
#' Batch 3 will have: outcome = "continuous" & predictor = "binary"
# simulation_name <- "alloc-simulation-batch-3-of-4"
#' Batch 4 will have: outcome = "continuous" & predictor = "continuous"
# simulation_name <- "alloc-simulation-batch-4-of-4"

sim_trial_size = c( 96 )
sim_outcome_type = c( "binary" )
sim_outcome_marginal_prevalence = list( 0.10, 0.50 ) #' doesn't matter when Y continuous
sim_prognostic_factor_type = c( "binary" ) # c("continuous", "binary"),
sim_prognostic_factor_prevalence = list( 0.25, 0.50 )
sim_prognostic_factor_number = c( 2 )
sim_prognostic_factor_effect_size = list( 1.1, 3 )
sim_treatment_assignment_effect_size = list(1, 1.1, 3)
sim_entry_time_effect_size = c( 1 )
sim_allocation_ratio = c( 0.50 )
sim_alpha = c( 0.05 )

# --------------------------------------------------------------------------- #
#' -------------------------- [ I. Settings ] ------------------------------- #
# --------------------------------------------------------------------------- #

#' [ 'results_directory' contains folder 'files' with .Rdata model, draw, output, evals ] 
results_directory <- "./results/"
#' [ 'metricfile_name' contains model, draw, output, evaluation info ] 
metricfile_name <- paste0( results_directory, "metrics-simulation.csv" ); 
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H-%M") #' [ timestamp ]
num_cores_parallel <- max(1, parallel::detectCores() - 1); 
num_simulations <- 5000 #' [ simulations per design matrix (all cores!)]
num_simulations_per_core <- ceiling( num_simulations / num_cores_parallel ); 
num_reallocations <- 500; #' TODO: revert this! #' [ rerandomized allocations per simulated trial ]

#' [ Determine which phase of the simulation to be run ]
generate_model <- TRUE #' [ Phase 1: define all models ]
draw_from_model <- TRUE #' [ Phase 1.5: simulate draws from models (created or loaded from saved state) ]
allocate_groups <- TRUE #' [ Phase 2: simulate outcomes and allocate tx groups ]
estimate_effects <- TRUE #' [ Phase 3: estimate tx effects (adjusted, and potentially unadjusted) ]
estimate_unadjusted_effects <- TRUE #' [ (Phases 3,4) estimate treatment effects (unadjusted)]
estimate_rerandomized_errors <- TRUE #' [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ]
run_deterministic_CAA <- FALSE #' [ Phase 4.5: allocate tx groups by deterministic CAA (and do the other steps too) ]
evaluate_metrics <- TRUE #' [ Phase 5: evaluate metrics on output (TODO: don't run on 'alloc methods')]

source("model_functions.R") #' [ step 1: define your Model, its parameters, and simulation function (produces 'draws') ]
source("method_functions.R") #' [ step 2: define your Methods, data analysis approaches (produces 'out') ]
source("eval_functions.R")#' [ step 3: define your Metrics, evaluation measures (produces 'eval'?) ]

# --------------------------------------------------------------------------- #
#' -------------------------- [ II. Methods ] ------------------------------- #
# --------------------------------------------------------------------------- #

#' [ Step 1: define treatment allocation Methods ]
CR <- make_complete_randomization_with_outcomes()
SBR <- make_stratified_block_randomization_with_outcomes()
CAA_probabilistic <- make_covariate_adaptive_allocation_with_outcomes(allocation_biasing_probability = 0.7)

#' [ Step 2b: define MethodExtensions to estimate tx effects ]
adjusted_ests <- estimate_regression_parameters( adjusted = TRUE, return_extended_method = FALSE )
unadjusted_ests <-  estimate_regression_parameters( adjusted = FALSE, return_extended_method = FALSE )
#' [ Step 3b: define MethodExtensions to compute rerandomized std. error ests ]
adjusted_ests_rerandomized <-   rerandomized_error_estimates( adjusted = TRUE, 
                                                              num_rerandomizations = num_reallocations, 
                                                              return_extended_method = FALSE )
unadjusted_ests_rerandomized <-   rerandomized_error_estimates( adjusted = FALSE, 
                                                                num_rerandomizations = num_reallocations, 
                                                                return_extended_method = FALSE )

# --------------------------------------------------------------------------- #
#' ------------ [ III. Simulation design & evaluation ] --------------------- #
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
#' ---------- [ Phase 1: define all models, simulate draws ] ---------------- #
# --------------------------------------------------------------------------- #
if( generate_model ){ 
  simulation <- new_simulation(name = simulation_name,
                               label = paste0("Simulation run at ", simulation_timestamp),
                               dir = results_directory) %>%
    generate_model(make_trial_allocation_model,
                   trial_size = sim_trial_size,
                   outcome_type = sim_outcome_type,
                   outcome_marginal_prevalence = sim_outcome_marginal_prevalence,
                   prognostic_factor_type = sim_prognostic_factor_type,
                   prognostic_factor_prevalence = sim_prognostic_factor_prevalence,
                   prognostic_factor_number = sim_prognostic_factor_number,
                   prognostic_factor_effect_size = sim_prognostic_factor_effect_size,
                   treatment_assignment_effect_size = sim_treatment_assignment_effect_size,
                   entry_time_effect_size = sim_entry_time_effect_size,
                   allocation_ratio = sim_allocation_ratio,
                   alpha = sim_alpha,
                   vary_along = c(# "trial_size",
                                  "outcome_marginal_prevalence",
                                  "prognostic_factor_prevalence",
                                  # "prognostic_factor_number",
                                  "prognostic_factor_effect_size",
                                  "treatment_assignment_effect_size"
                                  # "entry_time_effect_size"
                                  # "prognostic_factor_type",
                                  # "allocation_ratio",
                                  # "allocation_max_imbalance"
                                  #  "outcome_type"
                   )) 
}

if( draw_from_model ){
  cat(paste0("[ 1 ] Drawing from simulation model...\n")); ptm <- proc.time(); 
  capture.output({
    simulation <- simulate_from_model(object = simulation,
                                      nsim = num_simulations_per_core,
                                      index = 1:num_cores_parallel,
                                      parallel = list(socket_names = num_cores_parallel))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (draw from model): \n")); print( proc.time() - ptm );
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 2: simulate outcomes and allocate tx groups ] ----------- #
# --------------------------------------------------------------------------- #
if( allocate_groups ){ 
  cat(paste0("[ 2 ] Allocating groups, by complete randomization (CR)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = CR, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (allocate groups by CR): \n")); print( proc.time() - ptm );
  cat(paste0("[ 2 ] Allocating groups, by stratified block randomization (SBR)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = SBR, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (allocate groups by SBR): \n")); print( proc.time() - ptm );
  cat(paste0("[ 2 ] Allocating groups, by covariate adaptive allocation (CAA)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = CAA_probabilistic, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (allocate groups by CAA): \n")); print( proc.time() - ptm );
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 3: estimate tx effects (adjusted, unadjusted) ] --------- #
# --------------------------------------------------------------------------- #
if( estimate_effects ){
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (complete randomization)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = CR + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, complete randomization): \n")); print( proc.time() - ptm );
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (stratified block randomization)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = SBR + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, stratified block randomization): \n")); print( proc.time() - ptm );
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (covariate adaptive allocation)...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = CAA_probabilistic + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, covariate adaptive allocation): \n")); print( proc.time() - ptm );
  
  if( estimate_unadjusted_effects ){
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (complete randomization)...\n")); ptm <- proc.time();
    capture.output({
      simulation <- run_method(object = simulation, methods = CR + unadjusted_ests, parallel = list( socket_names = num_cores_parallel ))
    }, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, complete randomization): \n")); print( proc.time() - ptm );
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (stratified block randomization)...\n")); ptm <- proc.time();
    capture.output({
      simulation <- run_method(object = simulation, methods = SBR + unadjusted_ests, parallel = list( socket_names = num_cores_parallel ))
    }, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, stratified block randomization): \n")); print( proc.time() - ptm );
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (covariate adaptive allocation)...\n")); ptm <- proc.time();
    capture.output({
      simulation <- run_method(object = simulation, methods = CAA_probabilistic + unadjusted_ests, parallel = list( socket_names = num_cores_parallel ))
    }, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, covariate adaptive allocation): \n")); print( proc.time() - ptm );
  }
}

# --------------------------------------------------------------------------- #
#' ---- [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ] ---- #
# --------------------------------------------------------------------------- #
if( estimate_rerandomized_errors ){
  cat(paste0("[ 4a ] Estimating adjusted treatment effect errors (for CAA only) by rerandomization...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation,
                             methods = CAA_probabilistic + adjusted_ests_rerandomized,
                             parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (estimate adjusted effects for CAA, rerandomized): \n")); print( proc.time() - ptm );
  if( estimate_unadjusted_effects ){
    cat(paste0("[ 4b ] Estimating adjusted treatment effect errors (for CAA only) by rerandomization...\n")); ptm <- proc.time();
    capture.output({
      simulation <- run_method(object = simulation,
                               methods = CAA_probabilistic + unadjusted_ests_rerandomized,
                               parallel = list( socket_names = num_cores_parallel ))
    }, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (estimate UNadjusted effects for CAA, rerandomized): \n")); print( proc.time() - ptm );
  }
}

# --------------------------------------------------------------------------- #
#' --- [ Phase 4.5: evaluate deterministic covariate adaptive allocation ] -- #
# --------------------------------------------------------------------------- #
if( run_deterministic_CAA ){
  CAA_deterministic <- make_covariate_adaptive_allocation_with_outcomes(allocation_biasing_probability = 1)
  cat(paste0("[ 2 ] Allocating groups, by covariate adaptive allocation (CAA), DETERMINISTICALLY...\n")); ptm <- proc.time();
  capture.output({
    simulation <- run_method(object = simulation, methods = CAA_deterministic, parallel = list( socket_names = num_cores_parallel )) %>% 
      run_method(methods = list( CAA_deterministic + adjusted_ests,
                                   CAA_deterministic + unadjusted_ests,
                                   CAA_deterministic + adjusted_ests_rerandomized,
                                   CAA_deterministic + unadjusted_ests_rerandomized ), 
                 parallel = list( socket_names = num_cores_parallel ))
  }, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (allocate groups by CAA, DETERMINISTICALLY...): \n")); print( proc.time() - ptm );
}

# --------------------------------------------------------------------------- #
#' ----------- [ Phase 5: Evaluate metrics & stash results ] ---------------- #
# --------------------------------------------------------------------------- #
if( evaluate_metrics ){
  ###############################################################################
  #' [0] Define settings
  ###############################################################################
  timestamp_output <- FALSE
  #' [ 'results_directory' contains folder 'files' with 'sim-{simulation_name}.Rdata' ] 
  simulation_name <- simulation@name;
  results_directory <- "/Users/Moschops/Documents/MSThesis/datasets/results/"
  if( timestamp_output ){
    simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H-%M")
    output_directory <- paste0( results_directory, "../../results/output_", simulation_timestamp, "/" )
    if(!dir.exists( output_directory )){
      dir.create( output_directory );
    }
  }else{
    output_directory <- paste0( results_directory, "../../results-NEW/" )
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
  
  
  round_results <- TRUE;
  large_se_breakpoint <- 1000;  # break point for calling SE 'large' (aka complete/quasi-separation)
  large_est_breakpoint <- 40; # break point for calling an estimate 'large' (aka complete/quasi-separation)
  digits_to_round_to <- 3;
  
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
    dfs_out_j_subsetted <- list();
    metrics_by_out_j_subsetted_validse <- list();
    true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
    for( i in 1:length( output_j )){
      dfs_out_j[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
      dimnames(dfs_out_j[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
      dfs_out_j[[i]]$power.pvalue <- with( dfs_out_j[[i]], p < 0.05 )
      dfs_out_j[[i]]$power.rerand <- with( dfs_out_j[[i]], est < cilower | est > ciupper ) 
      dfs_out_j[[i]]$power.ci <- with( dfs_out_j[[i]], 0 < cilower | 0 > ciupper )
      dfs_out_j[[i]]$coverage <- with( dfs_out_j[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
      dfs_out_j[[i]]$bias <- with( dfs_out_j[[i]], est - true_trt_effect )
      dfs_out_j[[i]]$segt1k <- with( dfs_out_j[[i]], se > large_se_breakpoint | abs( est ) > large_est_breakpoint )
      #' 'time_elapsed' := overall time computing each method, 'nsim' := all simulations  
      methods_included_parsed[ i, "time_elapsed" ] <- round(sum(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[[10]][3] )}, numeric(1) )),2)
      methods_included_parsed[ i, "nsim"] <- dim( dfs_out_j[[i]] )[1]
      indices_colMeans <- which( dimnames(dfs_out_j[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias", "segt1k"))
      metrics_by_out_j[[i]] <- c(colMeans(dfs_out_j[[i]][, indices_colMeans]), nsim = dim( dfs_out_j[[i]] )[1], 
                                 methods_included_parsed[ i, "time_elapsed" ],
                                 modelno = sim_j, method_name = output_j[[i]]@method_name);
      dfs_out_j_subsetted[[i]] <- subset( dfs_out_j[[ i ]], subset = segt1k == FALSE, 
                                          select = c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias"))
      metrics_by_out_j_subsetted_validse[[i]] <- c(colMeans(dfs_out_j_subsetted[[i]]), nsim = dim( dfs_out_j_subsetted[[i]] )[1], 
                                                   methods_included_parsed[ i, "time_elapsed" ],
                                                   modelno = sim_j, method_name = output_j[[i]]@method_name);
    }
    cat("Success! \nElapsed time: \n\n"); print( proc.time() - ptm );
    
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
    
    
    cat(paste0("[ model ", sim_j, " ][------|  ] Computing metrics, EXCLUDING cases with complete separation...\n\n"))
    metrics_all_output_validse <- do.call(rbind, metrics_by_out_j_subsetted_validse)
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
    
    cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write progress table to: ", metricfile_name, "...\n")); ptm <- proc.time();
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
    cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all );
    cat("\n\n\n\n")
  }
}


