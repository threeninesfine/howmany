## @knitr main
## author             = Michael Flanagin
## date started       = 2018 May 16
## date last modified = 2018 May 28
## advisor            = Amalia Magaret
## objective          = Main simulator file.

library("simulator") # this file was created under simulator version 0.2.0

# --------------------------------------------------------------------------- #
#' -------------------------- [ I. Settings ] ------------------------------- #
# --------------------------------------------------------------------------- #
#' [ step 0: set file management ]
simulation_name <- "alloc-model-AWS"
results_directory <- "./results/"
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H:%M") #' [ timestamp ]
num_cores_parallel <- max(1, parallel::detectCores() - 1); #' [ for parallel computing ]
num_simulations <- 10; #' [ simulaions per design matrix (PER CORE!)]
num_simulations_per_core <- ceiling( num_simulations / num_cores_parallel ); 
num_reallocations <- 20; #' [ rerandomized allocations per simulated trial ]

#' [ Determine which phase of the simulation to be run ]
generate_model <- TRUE #' [ Phase 1: define all models, simulate draws ]
allocate_groups <- TRUE #' [ Phase 2: simulate outcomes and allocate tx groups ]
estimate_effects <- FALSE #' [ Phase 3: estimate tx effects (adjusted, unadjusted) ]
estimate_rerandomized_errors <- TRUE #' [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ]
unadjusted_analyses <- FALSE #' [ (Phases 3,4) for now, ignore unadjusted analyses (TODO: complete!) ]
analyze_results <- FALSE #' [ Phase 5: analyze results (tables and figures) ]


source("model_functions.R") #' [ step 1: define your Model, its parameters, and simulation function (produces 'draws') ]
source("method_functions.R") #' [ step 2: define your Methods, data analysis approaches (produces 'out') ]
source("eval_functions.R")#' [ step 3: define your Metrics, evaluation measures (produces 'eval'?) ]

# --------------------------------------------------------------------------- #
#' -------------------------- [ II. Methods ] ------------------------------- #
# --------------------------------------------------------------------------- #

#' [ Step 1: define treatment allocation Methods ]
SR <- make_complete_randomization_with_outcomes()
SBR <- make_stratified_block_randomization_with_outcomes()
CAA <- make_covariate_adaptive_allocation_with_outcomes()

allocation_methods_list <- list( SR, SBR, CAA ); # pass these to ExtendedMethods (for tracking index:method)

#' [ Step 2b: define ExtendedMethods to estimate tx effects ]
get_adjusted_tx_effect <- lapply( allocation_methods_list, function( .alloc_method ){ 
  estimate_regression_parameters( base_method = .alloc_method, adjusted = TRUE, return_extended_method = TRUE )})
get_unadjusted_tx_effect <- lapply( allocation_methods_list, function( .alloc_method ){ 
  estimate_regression_parameters( base_method = .alloc_method, adjusted = FALSE, return_extended_method = TRUE )})

#' [ Step 3b: define ExtendedMethods to compute rerandomized std. error ests ]
get_rerandomized_errors_adjusted_tx_effect <- lapply( allocation_methods_list, function( .alloc_method ){ 
  rerandomized_error_estimates( base_method = .alloc_method, adjusted = TRUE, num_rerandomizations = num_reallocations, return_extended_method = TRUE )})
get_rerandomized_errors_unadjusted_tx_effect <- lapply( allocation_methods_list, function( .alloc_method ){ 
  rerandomized_error_estimates( base_method = .alloc_method, adjusted = FALSE, num_rerandomizations = num_reallocations, return_extended_method = TRUE )})

#' [ Step 2a (DEPRECATED): define MethodExtensions to estimate tx effects ]
est_tx_effect_adjusted <- estimate_regression_parameters( adjusted = TRUE, return_extended_method = FALSE ) #' [ Method (a.1) ]
est_tx_effect_unadjusted <- estimate_regression_parameters( adjusted = FALSE, return_extended_method = FALSE ) #' [ Method (a.2) ]

#' [ Step 3a (DEPRECATED): define MethodExtensions to compute rerandomized std. error ests ]
rerand_err_ests_adjusted <- rerandomization_error_estimates_method_extension( adjusted = TRUE, num_rerandomizations = num_reallocations );
rerand_err_ests_unadjusted <- rerandomization_error_estimates_method_extension( adjusted = FALSE, num_rerandomizations = num_reallocations );

# --------------------------------------------------------------------------- #
#' ------------ [ III. Simulation design & evaluation ] --------------------- #
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
#' ---------- [ Phase 1: define all models, simulate draws ] ---------------- #
# --------------------------------------------------------------------------- #
if( generate_model ){ 
  simulation <- new_simulation(name = simulation_name,
                               label = "Randomization-allocation-methods-model",
                               dir = results_directory) %>%
    generate_model(make_trial_allocation_model,
                   trial_size = list(32, 64, 96),
                   outcome_type = c("binary"), # , "continuous"),
                   outcome_marginal_prevalence = c( 0.50 ),
                   prognostic_factor_type = c( "binary" ), # c("continuous", "binary"),
                   prognostic_factor_prevalence = c( 0.50 ), # c( 0.25, 0.50 ),
                   prognostic_factor_number = c( 2 ), # c( 1, 2, 3 ),
                   prognostic_factor_effect_size = list( 1, 1.1, 3 ),
                   treatment_assignment_effect_size = list(1, 1.1, 3),
                   entry_time_effect_size = list( 1, 3 ),
                   allocation_ratio = c( 0.50 ),
                   num_rerandomizations = c( 500 ),
                   #                 allocation_biasing_probability = c( 1.0 ),
                   alpha = c( 0.05 ),
                   vary_along = c("trial_size",
                                  #                   "prognostic_factor_prevalence",
                                  #                   "prognostic_factor_number",
                                  "prognostic_factor_effect_size",
                                  "treatment_assignment_effect_size",
                                  "entry_time_effect_size"
                                  #                  "allocation_biasing_probability"
                                  #                   "outcome_marginal_prevalence",
                                  #                   "prognostic_factor_type",
                                  #                   "allocation_ratio",
                                  #                   "allocation_max_imbalance"
                                  #                   "outcome_type",
                                  #                   "effect_sizes",
                   )) %>%
    simulate_from_model(nsim = num_simulations_per_core,
                        index = 1:num_cores_parallel,
                        parallel = list(socket_names = num_cores_parallel))
}else{ #' [ if generate_model = FALSE, load in existing model ]
  simulation <- load_simulation(name = simulation_name, dir = results_directory)
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 2: simulate outcomes and allocate tx groups ] ----------- #
# --------------------------------------------------------------------------- #
if( allocate_groups ){ 
  simulation <- run_method(object = simulation,
                           methods = list( SR, SBR ),
                           parallel = list( socket_names = num_cores_parallel ))
  #' [ run CAA separately (may take a while) ]
  simulation <- run_method(object = simulation,
                           methods = list( CAA ),
                           parallel = list( socket_names = num_cores_parallel ))
  
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 3: estimate tx effects (adjusted, unadjusted) ] --------- #
# --------------------------------------------------------------------------- #
if( estimate_effects ){
  simulation <- run_method(object = simulation,
                           methods = get_adjusted_tx_effect[1:2],
                           parallel = list( socket_names = num_cores_parallel ))
  #' [ run CAA separately (may take a while) ]
  simulation <- run_method(object = simulation,
                           methods = get_adjusted_tx_effect[3],
                           parallel = list( socket_names = num_cores_parallel ))
  if( unadjusted_analyses ){
    simulation <- run_method(object = simulation,
                             methods = get_unadjusted_tx_effect[1:2],
                             parallel = list( socket_names = num_cores_parallel ))
    #' [ run CAA separately (may take a while) ]
    simulation <- run_method(object = simulation,
                             methods = get_unadjusted_tx_effect[3],
                             parallel = list( socket_names = num_cores_parallel ))
  }
}

# --------------------------------------------------------------------------- #
#' ---- [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ] ---- #
# --------------------------------------------------------------------------- #
if( estimate_rerandomized_errors ){
  simulation <- run_method(object = simulation,
                           methods = get_adjusted_tx_effect[1:2],
                           parallel = list( socket_names = num_cores_parallel ))
  #' [ run CAA separately (may take a while) ]
  simulation <- run_method(object = simulation,
                           methods = get_adjusted_tx_effect[3],
                           parallel = list( socket_names = num_cores_parallel ))
  if( unadjusted_analyses ){
    simulation <- run_method(object = simulation,
                             methods = get_unadjusted_tx_effect[1:2],
                             parallel = list( socket_names = num_cores_parallel ))
    #' [ run CAA separately (may take a while) ]
    simulation <- run_method(object = simulation,
                             methods = get_unadjusted_tx_effect[3],
                             parallel = list( socket_names = num_cores_parallel ))
  }
}
  

# --------------------------------------------------------------------------- #
#' --------- [ Phase 5: analyze results (tables and figures) ] -------------- #
# --------------------------------------------------------------------------- #
if( analyze_results ){
  #' -------------------------------------------------------------------------- #
  #' ---------------- [ plotting metrics by vars ] ---------------------------- #
  #' -------------------------------------------------------------------------- #
  simulation %>%
    subset_simulation() %>%
    plot_eval_by(sim = simulation, 
                 metric_name = "coverage", 
                 varying = "trial_size",
                 main = "Coverage probability by trial size")
  
  #' -------------------------------------------------------------------------- #
  #' ------------------ [ adding more replicates ] ---------------------------- #
  #' -------------------------------------------------------------------------- #
  
  sim2 <- simulation %>% subset_simulation(methods = "") %>%
    rename("Allocation-model-rerandomized") %>%
    relabel("Effect of rerandomization on inference") %>%
    run_method(methods = estimate_treatment_effects_model_based_uncertainty + rerandomize)
  
  #' -------------------------------------------------------------------------- #
  #' ------------------- [ viewing data and results ] ------------------------- #
  #' -------------------------------------------------------------------------- #
  #' [ accessing 'draws' ]
  d <- draws( simulation )
  #' [ then we could get the simulated draws as follows:
  d@draws$r1.1 
  
  #' [ accessing 'out' ]
  o <- out( simulation )
  #' [ then we could get the computed output as follows: ]
  
  .draw_object <- draws( simulation )[[1]]
  .out_object <- output( simulation )[[1]]
  
  .model <- model( simulation )[[1]]
  .draw <- draws( simulation )[[ 1 ]]@draws[[1]]
  .out <- output( simulation )[[1]]@out[[1]]
  base_method <- SR
}




