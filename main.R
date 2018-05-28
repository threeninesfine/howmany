## main.R
## author             = Michael Flanagin
## date started       = 2018 May 16
## date last modified = 2018 May 21
## advisor            = Amalia Magaret
## objective          = Main simulator file.


# --------------------------------------------------------------------------- #
#' -------------------------- [ I. Settings ] ------------------------------- #
# --------------------------------------------------------------------------- #
library("simulator") # this file was created under simulator version 0.2.0

#' [ step 0: set output directory for simulation results ]
results_directory <- "./results/"
simulation_timestamp <- strftime(Sys.time(), format = "%Y-%m-%d_%H:%M") #' [ timestamp ]
num_cores_parallel <- max(1, parallel::detectCores() - 1); #' [ for parallel computing ]
num_simulations <- 10; #' [ simulaions per design matrix (PER CORE!)]
num_simulations_per_core <- ceiling( num_simulations / num_cores_parallel ); 
num_reallocations <- 20; #' [ rerandomized allocations per simulated trial ]

#' [ Setting parameters ]
run_scratch_code <- FALSE #' [ evaluate scratch code (for debugging)? ]
estimate_unadjusted_effects <- FALSE #' [ TODO: unadjusted estimates may be costly time-wise. Not run for now? ]
run_without_magrittr_piping <- FALSE #' [ run each simulation step separately? (different syntax) ]
source("model_functions.R") #' [ step 1: define your Model, its parameters, and simulation function (produces 'draws') ]
source("method_functions.R") #' [ step 2: define your Methods, data analysis approaches (produces 'out') ]
source("eval_functions.R")#' [ step 3: define your Metrics, evaluation measures (produces 'eval'?) ]

# --------------------------------------------------------------------------- #
#' -------------------------- [ II. Methods ] ------------------------------- #
# --------------------------------------------------------------------------- #

#' [ Step 1: define Methods ]
SR <- make_complete_randomization_with_outcomes()
SBR <- make_stratified_block_randomization_with_outcomes()
CAA <- make_covariate_adaptive_allocation_with_outcomes()

#' [ Step 2a: define MethodExtensions to (a) estimate tx effects ]
est_adjusted_tx_effect <- estimate_regression_parameters_method_extension( adjusted = TRUE ) #' [ Method (a.1) ]
est_unadjusted_tx_effect <- estimate_regression_parameters_method_extension( adjusted = FALSE ) #' [ Method (a.2) ]

#' [ Step 3b: define MethodExtensions to (a) estimate tx effects and (b) rerandomized std. error ests ]
rerand_err_ests_adj <- rerandomization_error_estimates_method_extension( adjusted = TRUE, num_rerandomizations = num_reallocations );
rerand_err_ests_unadj <- rerandomization_error_estimates_method_extension( adjusted = FALSE, num_rerandomizations = num_reallocations );

# --------------------------------------------------------------------------- #
#' ------------ [ III. Simulation design & evaluation ] --------------------- #
# --------------------------------------------------------------------------- #

simulation <- new_simulation(name = "alloc-model-28-May",
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
                 )) simulation %>%
  simulate_from_model(nsim = num_simulations_per_core,
                      index = 1:num_cores_parallel,
                      parallel = list(socket_names = num_cores_parallel)) %>%
  run_method(methods = CAA,
             parallel = list( socket_names = num_cores_parallel )) %>%
  run_method(methods = CAA + est_adjusted_tx_effect,
             parallel = list( socket_names = num_cores_parallel )) %>% 
  run_method(methods = CAA + rerand_err_ests_adj,
             parallel = list( socket_names = num_cores_parallel ))
  
#' [ 25 May notes: ]
#' [ > using method_extension vs. extended_method results in re-computing results? ]
run_method(methods = SBR,
           parallel = list( socket_names = num_cores_parallel )) %>%
  run_method(methods = list(CR + est_adjusted_tx_effect,
                            SBR + est_adjusted_tx_effect),
             parallel = list( socket_names = num_cores_parallel ))

  
  run_method(methods = list( SR, SBR, CAA),
             parallel = list( socket_names = num_cores_parallel )) %>%
  run_method(methods = list( SR + est_adjusted_tx_effect,
                             SBR + est_adjusted_tx_effect,
                             CAA + est_adjusted_tx_effect ),
             parallel = list( socket_names = num_cores_parallel )) %>% 
  run_method(methods = list( SR + rerand_err_ests_adj,
                             SBR + rerand_err_ests_adj,
                             CAA + rerand_err_ests_adj ),
             parallel = list( socket_names = num_cores_parallel ))


if( run_without_magrittr_piping ){
  #' [ Step 2b: run Methods (a.1) and (a.2) on simulation ]
  simulation <- run_method(object = simulation,
                           methods = list( SR + est_adjusted_tx_effect,
                                           SBR + est_adjusted_tx_effect,
                                           CAA + est_adjusted_tx_effect ),
                           parallel = list( socket_names = num_cores_parallel ))
  
  simulation <- run_method(object = simulation,
                           methods = list( SR + rerand_err_ests_adj,
                                           SBR + rerand_err_ests_adj,
                                           CAA + rerand_err_ests_adj ),
                           parallel = list( socket_names = num_cores_parallel ))
  
  if( estimate_unadjusted_effects ){
    simulation <- run_method(object = simulation,
                             methods = list(SR + est_unadjusted_tx_effect,
                                            SBR + est_unadjusted_tx_effect,
                                            CAA + est_unadjusted_tx_effect ),
                             parallel = list( socket_names = num_cores_parallel ))
    
    simulation <- run_method(object = simulation,
                             methods = list( SR + rerand_err_ests_unadj,
                                             SBR + rerand_err_ests_unadj,
                                             CAA + rerand_err_ests_unadj ),
                             parallel = list( socket_names = num_cores_parallel ))
  }
}



# --------------------------------------------------------------------------- #
#' ------------------- [ below is scratch code] ----------------------------- #
#' ------------------- [ below is scratch code] ----------------------------- #
#' ------------------- [ below is scratch code] ----------------------------- #
#' ------------------- [ below is scratch code] ----------------------------- #
# --------------------------------------------------------------------------- #



if( run_scratch_code ){
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
  
  
  
  .draw_object <- draws( simz )[[1]]
  .out_object <- output( simz )[[1]]
  
  .model <- model( simz )[[1]]
  .draw <- draws( simz )[[ 1 ]]@draws[[1]]
  .out <- output( simz )[[1]]@out[[1]]
  base_method <- SR
}




