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

sim_trial_size = list( 32, 96 )
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
run_deterministic_CAA <- TRUE #' [ Phase 4.5: allocate tx groups by deterministic CAA (and do the other steps too) ]
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
                   vary_along = c("trial_size",
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
  for( sim_j in 1:length(model( simulation )) ){
    cat(paste0("[ Model ", sim_j, " ][-|       ] Loading output from simulation [ ", simulation@name, " ]...\n")); ptm.all <- proc.time();
    output_j <- output( simulation )[[ sim_j ]] #' Model sim_j, 
    output_method_names <- sapply( output_j, function( .object ){ .object@method_name })
    methods_to_exclude <- c("CR", "SBR", "CAA", "CAA-MI-2-PBA-0.70", "CAA-MI-2-PBA-1.00");  #' exclude output from list that only contains allocation methods
    index_output_methods_to_include <- which(!( output_method_names %in% methods_to_exclude ))
    methods_included_parsed <- t(sapply( strsplit( output_method_names[ index_output_methods_to_include ], split = "_"), function(.listobj){unlist( .listobj )}))
    dimnames( methods_included_parsed ) <- list( index_output_methods_to_include, c("alloc_method", "analysis_method", "adjustment"))
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm.all );
    cat(paste0("Simulation [ ", sim_j, " ] has these outputs:\n"))
    print( methods_included_parsed )
    
    cat(paste0("[ model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
    dfs <- list();
    for( i in 1:length( index_output_methods_to_include )){
      out.index <- index_output_methods_to_include[ i ]
      dfs[[ i ]]  <- data.frame(t(vapply( output_j[[ out.index ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
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
      metrics_by_dfs[[i]] <- colMeans(dfs[[i]][, inds])
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
    
    index_exclude <- which( sapply(model( simulation )[[ sim_j ]]@params, function(.x){length(unlist(.x)) > 1} )) # exclude non-scalars
    id_sim <- paste0(model( simulation )[[ sim_j ]]@params[ -index_exclude ], collapse = "-")
    cat(paste0("Unique ID for simulation output file: ", id_sim, ".csv\n")); 
    
    cat(paste0("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n"))
    metrics_df <- do.call(rbind, metrics_by_dfs)
    metrics_df_with_id <- cbind.data.frame( alloc_method = methods_included_parsed[,"alloc_method"], metrics_df,
                                            model( simulation )[[ sim_j ]]@params[-index_exclude]) 
    cat("Success! \n\n")
    
    cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
    if(!file.exists( metricfile_name )){
      cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n"))
      write.csv( metrics_df_with_id, file = metricfile_name, row.names = FALSE )
    }else{
      cat(paste0("Appending metrics to file ", metricfile_name, "...\n"))
      write.table( metrics_df_with_id, file = metricfile_name, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
    cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time (secs):\n")); print( proc.time() - ptm.all );
    cat("\n\n\n\n")
  }
}


