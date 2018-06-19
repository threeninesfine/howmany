## @knitr evaluate_simulation
## author             = Michael Flanagin
## date started       = 6 Jun 2018
## date last modified = 6 Jun 2018
## advisor            = Amalia Magaret
## objective          = Evaluate generated simulation



library("simulator")


load_sim_from_file <- FALSE
save_output_as_csv <- FALSE
run_chapter3 <- FALSE 
#' -------------------------------------------------------------------------- #
#'         [ Chapter 1: How to view simulation objects from stored Rdata ]
#' -------------------------------------------------------------------------- #
if( load_sim_from_file ){
  resultsdir_path <- "~/Downloads/MSThesis/expanded_datasets/sim-study/results/" 
  sim <- get_simulation_with_all_files(dir = resultsdir_path )
  sim_contents <- get_contents( dir = resultsdir_path ) #' [ Evaluate contents (get model, draw, output, evaluation info) ] 

  #' [ Find models that have draws, output, and/or evaluations ]
  sim_draw_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$draws) )
  sim_out_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$out) )
  sim_evals_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$evals) )
  
  #' [ Get model names with draws, evals, and output]
  sims_with_output <- which( sim_out_length > 0 )
  sims_with_draws <- which( sim_draw_length > 0 )
  sims_with_evals <- which( sim_evals_length > 0 )
  indices_with_data <- intersect( sims_with_evals, intersect( sims_with_output, sims_with_draws ))
  
  #' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
  sim_models <- load( sim@model_refs )
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
  params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ .model@params[c(1:6, 11:16)]  })))
  
  #' [ Subset models to scenarios of interest ]
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
if( save_output_as_csv ){
  #' [ 'modify_reference_dirs' changes 'simulator' object directory attributes (for loading objects to memory from references) ]
  if( modify_reference_dirs ){
    sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
    sapply( 1:length( sim@draws_refs ), function( .index ){ sim@draws_refs[[ .index ]][[1]]@dir <<- sim@dir; }) #' assuming only one draw
    sapply( 1:length( sim@output_refs ), function( .index.i ){ num_output_refs <- length( sim@output_refs[[ .index.i ]] );
      sapply( 1:num_output_refs, function( .index.j ){ sim@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})}) 
    sapply( 1:length( sim@evals_refs ), function( .index.i ){ num_evals_refs <- length( sim@evals_refs[[ .index.i ]] );
      sapply( 1:num_evals_refs, function( .index.j ){ sim@evals_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})})
  }
  
  #' [ Find the indexes corresponding to regression estimates, extract and save them. ]
  muh_output <- output( sim2 )
  output_method_names <- sapply( muh_output, function( .object ){ .object@method_name })
  methods_of_interest <- c("adjusted-regression-ests", "adjusted-regression-ests-RERANDOMIZED-error-ests",
                      "unadjusted-regression-ests", "unadjusted-regression-ests-RERANDOMIZED-error-ests")
  output_indices_methods_of_interest <- which( output_method_names %in% useful_methods )
  
  #' [ Change proc_time values to elapsed_time (to allow making data.frames)]
  sapply( output_indices_methods_of_interest, function( out_i ){ 
    sapply( 1:length( muh_output[[ out_i ]]@out ), function( draw_j ){
      muh_output[[ out_i ]]@out[[draw_j]]$time <<- muh_output[[ out_i ]]@out[[draw_j]]$time[3]; 
    })
  })
  
  #' [ Save outputs to data frame ]
  dfs <- list();
  for( i in 1:length( output_indices_methods_of_interest )){
    out.index <- output_indices_methods_of_interest[ i ]
    dfs[[ i ]]  <- data.frame(t(sapply( muh_output[[ out.index ]]@out, function( .list ){ unlist( .list )})))
    if(dim( dfs[[i]] )[2] == 9){
      dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "time.elapsed")
    }else{
      dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations", "time.elapsed")
    } 
  }
  
  true_trt_effect <- 2;
  
  metrics_by_dfs <- list();
  for( i in 1:length( dfs )){
    dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
    dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper )
    dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
    metrics_by_dfs[[i]] <- apply( dfs[[i]][, c("power.pvalue", "power.rerand", "coverage", "bias")], 2, mean)
  }

  # store all information on metrics in metrics_all
  metrics_all <- data.frame(do.call( rbind, metrics_by_dfs ))
  
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
