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
sim_trial_size = list( 32, 64, 96 )
sim_outcome_type = c( "continuous" )
sim_outcome_marginal_prevalence = c( 0.50 ) #' doesn't matter when Y continuous
sim_prognostic_factor_type = c( "binary" ) # c("continuous", "binary"),
sim_prognostic_factor_prevalence = list( 0.25, 0.50 )
sim_prognostic_factor_number = c( 2 )
sim_prognostic_factor_effect_size = list( 1, 3 )
sim_treatment_assignment_effect_size = list(1, 1.1, 3)
sim_entry_time_effect_size = list( 1, 3 )
sim_allocation_ratio = c( 0.50 )
sim_num_rerandomizations = c( 500 )
sim_alpha = c( 0.05 )

# --------------------------------------------------------------------------- #
#' -------------------------- [ I. Settings ] ------------------------------- #
# --------------------------------------------------------------------------- #
#' [ step 0: set file management ]
simulation_name <- "alloc-model-AWS"
results_directory <- "./results/"
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H:%M") #' [ timestamp ]
num_cores_parallel <- max(1, parallel::detectCores() - 1); #' [ for parallel computing ]
num_simulations <- 10000; #' [ simulations per design matrix (all cores!)]
num_simulations_per_core <- ceiling( num_simulations / num_cores_parallel ); 
num_reallocations <- 500; #' [ rerandomized allocations per simulated trial ]

#' [ Determine which phase of the simulation to be run ]
eval_on_subset <- TRUE  #' [ Phase 0: define all models, simulate draws ]
generate_model <- TRUE #' [ Phase 1: define all models, simulate draws ]
allocate_groups <- TRUE #' [ Phase 2: simulate outcomes and allocate tx groups ]
estimate_effects <- FALSE #' [ Phase 3: estimate tx effects (adjusted, unadjusted) ]
estimate_rerandomized_errors <- FALSE #' [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ]
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
  rerandomized_error_estimates( base_method = .alloc_method, adjusted = TRUE, 
                                num_rerandomizations = num_reallocations, return_extended_method = TRUE )})
get_rerandomized_errors_unadjusted_tx_effect <- lapply( allocation_methods_list, function( .alloc_method ){ 
  rerandomized_error_estimates( base_method = .alloc_method, adjusted = FALSE, 
                                num_rerandomizations = num_reallocations, return_extended_method = TRUE )})

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
                   num_rerandomizations = sim_num_rerandomizations,
                   alpha = sim_alpha,
                   vary_along = c("trial_size",
                                  # "outcome_marginal_prevalence",
                                  "prognostic_factor_prevalence",
                                  # "prognostic_factor_number",
                                  "prognostic_factor_effect_size",
                                  "treatment_assignment_effect_size",
                                  "entry_time_effect_size"
                                  # "prognostic_factor_type",
                                  # "allocation_ratio",
                                  # "allocation_max_imbalance"
                                  #  "outcome_type"
                   )) %>%
    simulate_from_model(nsim = num_simulations_per_core,
                        index = 1:num_cores_parallel,
                        parallel = list(socket_names = num_cores_parallel))
}else{ #' [ if generate_model = FALSE, load in existing model ]
  simulation <- load_simulation(name = simulation_name, dir = results_directory)
}

if( eval_on_subset ){
  simsub <- subset_simulation(simulation,
                              prognostic_factor_prevalence == 0.5 & 
                                prognostic_factor_effect_size == 3 & 
                                treatment_assignment_effect_size == 3 &
                                entry_time_effect_size == 1 )
  #' [ Allocation methods ]
  simsub <- run_method(object = simsub,
                       methods = list( SR, SBR ),
                       parallel = list( socket_names = num_cores_parallel ))
  
  simsub <- run_method(object = simsub,
                       methods = list( CAA ),
                       parallel = list( socket_names = num_cores_parallel ))
  
  #' [ Estimate treatment effect ]
  simsub <- run_method(object = simsub,
                           methods = get_adjusted_tx_effect[1:2],
                           parallel = list( socket_names = num_cores_parallel ))
  simsub <- run_method(object = simsub,
                           methods = get_adjusted_tx_effect[3],
                           parallel = list( socket_names = num_cores_parallel ))
  
  simulation <- run_method(object = simulation,
                           methods = get_rerandomized_errors_adjusted_tx_effect[1:2],
                           parallel = list( socket_names = num_cores_parallel ))
  #' [ run CAA separately (may take a while) ]
  simulation <- run_method(object = simulation,
                           methods = get_rerandomized_errors_adjusted_tx_effect[3],
                           parallel = list( socket_names = num_cores_parallel ))
}else{
  
  
  # --------------------------------------------------------------------------- #
  #' -------- [ Phase 2: simulate outcomes and allocate tx groups ] ----------- #
  # --------------------------------------------------------------------------- #
  if( allocate_groups ){ 
    simsub <- run_method(object = simsub,
                         methods = list( SR, SBR ),
                         parallel = list( socket_names = num_cores_parallel ))
    #' [ run CAA separately (may take a while) ]
    simsub <- run_method(object = simsub,
                         methods = list( CAA ),
                         parallel = list( socket_names = num_cores_parallel ))
    if( estimate_rerandomized_errors ){
      simsub <- run_method(object = simsub,
                           methods = get_adjusted_tx_effect[1:2],
                           parallel = list( socket_names = num_cores_parallel ))
      #' [ run CAA separately (may take a while) ]
      simsub <- run_method(object = simsub,
                           methods = get_adjusted_tx_effect[3],
                           parallel = list( socket_names = num_cores_parallel ))
    }
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
                             methods = get_rerandomized_errors_adjusted_tx_effect[1:2],
                             parallel = list( socket_names = num_cores_parallel ))
    #' [ run CAA separately (may take a while) ]
    simulation <- run_method(object = simulation,
                             methods = get_rerandomized_errors_adjusted_tx_effect[3],
                             parallel = list( socket_names = num_cores_parallel ))
    if( unadjusted_analyses ){
      simulation <- run_method(object = simulation,
                               methods = get_rerandomized_errors_unadjusted_tx_effect[1:2],
                               parallel = list( socket_names = num_cores_parallel ))
      #' [ run CAA separately (may take a while) ]
      simulation <- run_method(object = simulation,
                               methods = get_rerandomized_errors_unadjusted_tx_effect[3],
                               parallel = list( socket_names = num_cores_parallel ))
    }
  }
  
  # --------------------------------------------------------------------------- #
  #' --------- [ Phase 5: analyze results (tables and figures) ] -------------- #
  # --------------------------------------------------------------------------- #
  if( analyze_results ){
    #' [ todo: mock tables and figures here! ]
  }
  
  
}



