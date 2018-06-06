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
sim_prognostic_factor_number = list( 1, 2 )
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
generate_model <- TRUE #' [ Phase 1: define all models, simulate draws ]
allocate_groups <- TRUE #' [ Phase 2: simulate outcomes and allocate tx groups ]
estimate_effects <- TRUE #' [ Phase 3: estimate tx effects (adjusted, unadjusted) ]
estimate_rerandomized_errors <- FALSE #' [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ]
evaluate_metrics <- TRUE #' [ Phase 5: evaluate metrics on output (TODO: don't run on 'alloc methods')]
unadjusted_analyses <- FALSE #' [ (Phases 3,4) for now, ignore unadjusted analyses (TODO: complete!) ]
analyze_results <- FALSE #' [ Phase 6: analyze results (tables and figures) ]
followup_analysis <- FALSE #' [ Phase 7: add more detail to simulation ]

source("model_functions.R") #' [ step 1: define your Model, its parameters, and simulation function (produces 'draws') ]
source("method_functions.R") #' [ step 2: define your Methods, data analysis approaches (produces 'out') ]
source("eval_functions.R")#' [ step 3: define your Metrics, evaluation measures (produces 'eval'?) ]

# --------------------------------------------------------------------------- #
#' -------------------------- [ II. Methods ] ------------------------------- #
# --------------------------------------------------------------------------- #

#' [ Step 1: define treatment allocation Methods ]
SR <- make_complete_randomization_with_outcomes()
SBR <- make_stratified_block_randomization_with_outcomes()
CAA_deterministic <- make_covariate_adaptive_allocation_with_outcomes(allocation_biasing_probability = 1)
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
                                  "prognostic_factor_number",
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

# --------------------------------------------------------------------------- #
#' -------- [ Phase 2: simulate outcomes and allocate tx groups ] ----------- #
# --------------------------------------------------------------------------- #
if( allocate_groups ){ 
  simulation <- run_method(object = simulation,
                           methods = list( SR, SBR, CAA_deterministic, CAA_probabilistic),
                           parallel = list( socket_names = num_cores_parallel ))
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 3: estimate tx effects (adjusted, unadjusted) ] --------- #
# --------------------------------------------------------------------------- #
if( estimate_effects ){
  simulation <- run_method(object = simulation,
                           methods = list( SR + adjusted_ests,
                                           SBR + adjusted_ests,
                                           CAA_deterministic + adjusted_ests,
                                           CAA_probabilistic + adjusted_ests),
                           parallel = list( socket_names = num_cores_parallel ))
  if( unadjusted_analyses ){
    simulation <- run_method(object = simulation,
                             methods = list( SR + unadjusted_ests,
                                             SBR + unadjusted_ests,
                                             CAA_deterministic + unadjusted_ests,
                                             CAA_probabilistic + unadjusted_ests),
                             parallel = list( socket_names = num_cores_parallel ))
  }
}

# --------------------------------------------------------------------------- #
#' ---- [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ] ---- #
# --------------------------------------------------------------------------- #
if( estimate_rerandomized_errors ){
  simulation <- run_method(object = simulation,
                           methods = list( CAA_deterministic + adjusted_ests_rerandomized,
                                           CAA_probabilistic + adjusted_ests_rerandomized ),
                           parallel = list( socket_names = num_cores_parallel ))
  if( unadjusted_analyses ){
    simulation <- run_method(object = simulation,
                             methods = list( CAA_deterministic + unadjusted_ests_rerandomized,
                                             CAA_probabilistic + unadjusted_ests_rerandomized ),
                             parallel = list( socket_names = num_cores_parallel ))
  }
}
 

# --------------------------------------------------------------------------- #
#' -------------- [ Phase 5: evaluate metrics on output ] ------------------- #
# --------------------------------------------------------------------------- #
if( evaluate_metrics ){
  simulation <- evaluate(object = simulation,
                         metrics = list( coverage,
                                         power_p_value,
                                         power_ci,
                                         bias,
                                         mse ))
}

# --------------------------------------------------------------------------- #
#' --------- [ Phase 6: analyze results (tables and figures) ] -------------- #
# --------------------------------------------------------------------------- #
if( analyze_results ){
  #' [ Subset evaluations by removing evals on 'allocation methods' - they are mostly NA ]
  method_names <- unique( sapply( simulation@evals_refs[[1]], function( .object ){ .object@method_name }) )
  useful_methods <- method_names[which(nchar( method_names ) > 48)]
  baz <- subset_evals(evals( simulation ), method_names = useful_methods )
  
  tabulate_eval(object = baz,
                method_names = useful_methods[c(1,4,2,5,3,6)],
                metric_name = "coverage",
                se_format = "None",
                output_type = "latex",
                format_args = list(nsmall=3, digits=3))
  tabulate_eval(object = baz,
                method_names = useful_methods[c(1,4,2,5,3,6)],
                metric_name = "power_p_value",
                se_format = "None",
                output_type = "latex",
                format_args = list(nsmall=3, digits=3))
  tabulate_eval(object = baz,
                method_names = useful_methods[c(1,4,2,5,3,6)],
                metric_name = "power_ci",
                se_format = "None",
                output_type = "latex",
                format_args = list(nsmall=3, digits=3))
  tabulate_eval(object = baz,
                method_names = useful_methods[c(1,4,2,5,3,6)],
                metric_name = "bias",
                se_format = "None",
                output_type = "latex",
                format_args = list(nsmall=3, digits=3))
}


# --------------------------------------------------------------------------- #
#' -------- [ Phase 7: Repeat (specific) simulation conditions ] ------------ #
# --------------------------------------------------------------------------- #

if( followup_analysis ){
  #' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
  muh_models <- load( simulation@model_refs )
  params_by_model <- as.data.frame(t(sapply( muh_models, function( .model ){ .model@params[c(1:6, 11:16)]  })))
  model_indexes_of_interest <- with( params_by_model, which( treatment_assignment_effect_size == 3 & 
                                                               prognostic_factor_effect_size == 3 & 
                                                               entry_time_effect_size == 1 &
                                                               prognostic_factor_number == 2 & 
                                                               prognostic_factor_prevalence == 0.5 &
                                                               trial_size == 96 &
                                                               outcome_marginal_prevalence == 0.5 ))
  
  sim2 <- subset_simulation( simulation, subset = model_indexes_of_interest )
  sim2 <- simulate_from_model( sim2,
                               nsim = num_simulations_per_core,
                               index = 1:num_cores_parallel,
                               parallel = list(socket_names = num_cores_parallel))
  
  
  #' need to generate base_method() output first
  sim2 <- run_method(object = sim2,
                     methods = list( SR, SBR, CAA_deterministic, CAA_probabilistic),
                     parallel = list( socket_names = num_cores_parallel ))
  
  if( estimate_effects ){
    sim2 <- run_method(object = sim2,
                             methods = list( SR + adjusted_ests,
                                             SBR + adjusted_ests,
                                             CAA_deterministic + adjusted_ests,
                                             CAA_probabilistic + adjusted_ests),
                             parallel = list( socket_names = num_cores_parallel ))
    if( unadjusted_analyses ){
      sim2 <- run_method(object = sim2,
                               methods = list( SR + unadjusted_ests,
                                               SBR + unadjusted_ests,
                                               CAA_deterministic + unadjusted_ests,
                                               CAA_probabilistic + unadjusted_ests),
                               parallel = list( socket_names = num_cores_parallel ))
    }
  }
  
  # --------------------------------------------------------------------------- #
  #' ---- [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ] ---- #
  # --------------------------------------------------------------------------- #
  if( estimate_rerandomized_errors ){
    sim2 <- run_method(object = sim2,
                             methods = list( CAA_deterministic + adjusted_ests_rerandomized,
                                             CAA_probabilistic + adjusted_ests_rerandomized ),
                             parallel = list( socket_names = num_cores_parallel ))
    if( unadjusted_analyses ){
      sim2 <- run_method(object = sim2,
                               methods = list( CAA_deterministic + unadjusted_ests_rerandomized,
                                               CAA_probabilistic + unadjusted_ests_rerandomized ),
                               parallel = list( socket_names = num_cores_parallel ))
    }
  }
  
  sim2 <- evaluate(object = sim2,
                   metrics = list( coverage,
                                   power_p_value,
                                   power_ci,
                                   bias,
                                   mse))  
  method_names <- unique( sapply( sim2@evals_refs[[1]], function( .object ){ .object@method_name }) )
  useful_methods <- method_names[which(nchar( method_names ) > 48)]
}


