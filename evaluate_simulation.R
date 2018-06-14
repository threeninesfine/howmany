## @knitr evaluate_simulation
## author             = Michael Flanagin
## date started       = 6 Jun 2018
## date last modified = 6 Jun 2018
## advisor            = Amalia Magaret
## objective          = Evaluate generated simulation

run_chapter1 <- FALSE
run_chapter2 <- FALSE
run_chapter3 <- FALSE 

library("simulator")
#' -------------------------------------------------------------------------- #
#'         [ Chapter 1: How to view simulation objects from stored Rdata ]
#' -------------------------------------------------------------------------- #
if( run_chapter1 ){
  #' [ 1.1) Pick a simulation to evaluate ]
  dir_of_dirs <- "~/Downloads/MSThesis/expanded_datasets/"
  #' dirs <- sapply( dir(), function( .dir ){ paste0(dir_of_dirs, .dir, "/results/", collapse="/")})
  resultsdir_path <- "~/Downloads/MSThesis/expanded_datasets/sim-study/results/" 
  
  #' [ 1.2) Describe the datasets, pipe to .txt file (output is verbose) ]
  sink( file = "sim-study-description.txt", append = TRUE )
  describe( resultsdir_path )
  sink()
  
  #' [ 1.3) Skip all the steps and make simulation with all files.]
  sim <- get_simulation_with_all_files(dir = resultsdir_path)
  
  #' [ 1.4) Evaluate contents (get output, model name, etc )] 
  sim_contents <- get_contents( dir = resultsdir_path )
  
  #' [ 1.4.1) Find indices such that it has draws, evals, output etc]
  sim_draw_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$draws) )
  sims_with_draws <- which( sim_draw_length > 0 )
  
  sim_out_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$out) )
  sims_with_output <- which( sim_out_length > 0 )
  
  sim_evals_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$evals) )
  sims_with_evals <- which( sim_evals_length > 0 )
  
  #' [ 1.4.2) Get model names with draws, evals, and output]
  indices_with_data <- intersect( sims_with_evals, intersect( sims_with_output, sims_with_draws ))
  
  #' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
  sim_models <- load( sim@model_refs )
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
  
  params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ .model@params[c(1:6, 11:16)]  })))
  model_indexes_of_interest <- with( params_by_model, which( treatment_assignment_effect_size == 3 & 
                                                               prognostic_factor_effect_size == 3 & 
                                                               entry_time_effect_size == 1 &
                                                               prognostic_factor_number == 2 & 
                                                               prognostic_factor_prevalence == 0.5 &
                                                               trial_size == 96 &
                                                               outcome_marginal_prevalence == 0.5 ))
  
  sim2 <- subset_simulation( sim, subset = model_indexes_of_interest )
  
}


#' -------------------------------------------------------------------------- #
#'         [ Chapter 2: How to load simulation results (output and evals)]
#' -------------------------------------------------------------------------- #
if( run_chapter2 ){
  if( modify_reference_dirs ){
    sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
    sapply( 1:length( sim@draws_refs ), function( .index ){ sim@draws_refs[[ .index ]][[1]]@dir <<- sim@dir; }) #' assuming only one draw
    sapply( 1:length( sim@output_refs ), function( .index.i ){ num_output_refs <- length( sim@output_refs[[ .index.i ]] );
      sapply( 1:num_output_refs, function( .index.j ){ sim@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})}) 
    sapply( 1:length( sim@evals_refs ), function( .index.i ){ num_evals_refs <- length( sim@evals_refs[[ .index.i ]] );
      sapply( 1:num_evals_refs, function( .index.j ){ sim@evals_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})})
  }
  
  muh_output <- output( sim2 )
  
  method_names <- sapply( sim2@evals_refs[[1]], function( .object ){ .object@method_name }) 
  useful_methods <- c("adjusted-regression-ests", "adjusted-regression-ests-RERANDOMIZED-error-ests",
                      "unadjusted-regression-ests", "unadjusted-regression-ests-RERANDOMIZED-error-ests")
  
  muh_evals <- evals( sim2 ) %>% 
    subset_evals( method_names = useful_methods,
                  metric_names = c("coverage", "power_p_value", "power_ci", "power_rerand" ) )
  
  #' load in the models, draws, and output.
  muh_models <- load( sim2@model_refs )
  # muh_draws <- load( sim@draws_refs )
  muh_evals <- load_evals_from_ref( sim@evals_refs )
  df_evals <- as.data.frame( muh_evals )
  
  muh_metrics <- apply( df_evals[,c("coverage", "power_p_value", "power_ci", "bias", "mse")], 2, mean )
  
  muh_evals_new <- evaluate(object = muh_output, metrics = list(coverage, power_p_value, power_ci ))
  
  #' ------------------------------------------------------------------------ #
  #' -------------------------- junk code below ----------------------------- #
  
  if( modify_reference_dirs ){
    sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
    sapply( 1:length( sim@draws_refs ), function( .index ){ sim@draws_refs[[ .index ]][[1]]@dir <<- ""; }) #' assuming only one draw
    sapply( 1:length( sim@output_refs ), function( .index.i ){ num_output_refs <- length( sim@output_refs[[ .index.i ]] );
    sapply( 1:num_output_refs, function( .index.j ){ sim@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- "";})}) 
  }
}
