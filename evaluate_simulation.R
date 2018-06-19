## @knitr evaluate_simulation
## author             = Michael Flanagin
## date started       = 6 Jun 2018
## date last modified = 6 Jun 2018
## advisor            = Amalia Magaret
## objective          = Evaluate generated simulation



library("simulator")


load_sim_from_file = FALSE
run_scratch_code = FALSE
write_evals = TRUE
write_output = TRUE
resultsdir_path <- "~/Downloads/MSThesis/expanded_datasets/sim-study/results/" 
output_dir = "./results/"
metrics_file_name = "metrics-simulation.csv"

#' -------------------------------------------------------------------------- #
#'         [ Chapter 1: How to view simulation objects from stored Rdata ]
#' -------------------------------------------------------------------------- #
if( load_sim_from_file ){
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
  # model_indexes_of_interest <- with( params_by_model, which( treatment_assignment_effect_size == 3 & 
  #                                                              prognostic_factor_effect_size == 3 & 
  #                                                              entry_time_effect_size == 1 &
  #                                                              prognostic_factor_number == 2 & 
  #                                                              prognostic_factor_prevalence == 0.5 &
  #                                                              trial_size == 96 &
  #                                                              outcome_marginal_prevalence == 0.5 ))
  # 
  # sim2 <- subset_simulation( sim, subset = model_indexes_of_interest )
}


for( sim_j in 1:4 ){
  cat(paste0("[ model ", sim_j, " ][-|       ] Loading output from simulation model [ ", sim_j, " ]...\n")); ptm.all <- proc.time();
  muh_output <- output( sim )[[ sim_j ]]
  output_method_names <- sapply( muh_output, function( .object ){ .object@method_name })
  methods_of_interest <- c("adjusted-regression-ests", "adjusted-regression-ests-RERANDOMIZED-error-ests",
                           "unadjusted-regression-ests", "unadjusted-regression-ests-RERANDOMIZED-error-ests")
  output_indices_methods_of_interest <- which( output_method_names %in% methods_of_interest )
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm.all );
  cat(paste0("Simulation [ ", sim_j, " ] has these indexes of interest:\n"))
  cat(paste0("Output index [", output_indices_methods_of_interest, "]: ", output_method_names[ output_indices_methods_of_interest ], "\n") );
  
  cat("[ model ", sim_j, " ][--|      ] Changing proc_time values to elapsed_time (to allow making data.frames)...\n"); ptm <- proc.time();
  sapply( output_indices_methods_of_interest, function( out_i ){ 
    sapply( 1:length( muh_output[[ out_i ]]@out ), function( draw_j ){
      muh_output[[ out_i ]]@out[[draw_j]]$time <<- muh_output[[ out_i ]]@out[[draw_j]]$time[3]; 
    })
  })
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  cat("[ model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n"); ptm <- proc.time();
  dfs <- list();
  for( i in 1:length( output_indices_methods_of_interest )){
    out.index <- output_indices_methods_of_interest[ i ]
    dfs[[ i ]]  <- data.frame(t(sapply( muh_output[[ out.index ]]@out, function( .list ){ unlist( .list )})))
    if(dim( dfs[[i]] )[2] == 9){
      dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "time.elapsed")
      dfs[[i]]$num_rerandomizations = 0;
    }else{
      dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations", "time.elapsed")
    } 
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  cat("[ model ", sim_j, " ][----|    ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"); ptm <- proc.time();
  metrics_by_dfs <- list();
  true_trt_effect <- model( sim2 )@params$bZ
  for( i in 1:length( dfs )){
    dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
    dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper )
    dfs[[i]]$power.ci <- with( dfs[[i]], 0 < cilower | 0 > ciupper )
    dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
    metrics_by_dfs[[i]] <- apply( dfs[[i]][, c("adjusted","rerandomized","power.pvalue", "power.rerand","power.ci", "coverage", "bias")], 2, mean)
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
      
  cat("[ model ", sim_j, " ][-----|   ] Combining Output data.frames for saving...\n"); ptm <- proc.time();
  output_df <- dfs[[1]];
  for( i in 2:length( dfs )){
    output_df <- merge(dfs_all, dfs[[i]], all = TRUE);
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  index_exclude <- which( sapply(model( sim )[[ sim_j ]]@params, function(.x) length(.x)) > 1 ) # exclude non-scalars
  id_sim <- paste0(model( sim )[[ sim_j ]]@params[ -index_exclude ], collapse = "-")
  cat(paste0("Unique ID for simulation output file: ", id_sim, ".csv\n")); 
  
  cat("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n")
  metrics_df <- do.call(rbind, metrics_by_dfs)
  metrics_df_with_id <- cbind.data.frame( model( sim2 )@params[-index_exclude], metrics_df )
  cat("Success! \n")
  
  outfile_name <- paste0(output_dir,"output_", id_sim, ".csv");
  cat(paste0("[ model ", sim_j, " ][-------| ] Attempting to write output file to: ", outfile_name, "...\n")); ptm <- proc.time();
  if( write_output ){
    write.csv( output_df, file = outfile_name, row.names = FALSE )
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  }
  
  metricfile_name <- paste0(output_dir,"metrics_", id_sim, ".csv");
  cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
  if( write_evals ){
    if(!file.exists( metricfile_name )){
      cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n"))
      write.csv( metrics_df_with_id, file = metricfile_name, row.names = FALSE )
    }else{
      cat("ERROR: File exists! Not writing metrics...\n")
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  }
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time:")); print( proc.time() - ptm.all );
}



#' -------------------------------------------------------------------------- #
#'         [ Chapter 2: How to load simulation results (output and evals)]
#' -------------------------------------------------------------------------- #
if( run_scratch_code ){
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
  

  
  #' [ Compute metrics on data frame output]
  metrics_by_dfs <- list();
  true_trt_effect <- model( sim2 )@params$bZ
  for( i in 1:length( dfs )){
    dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
    dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper )
    dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
    dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
    metrics_by_dfs[[i]] <- apply( dfs[[i]][, c("adjusted","rerandomized","power.pvalue", "power.rerand", "coverage", "bias")], 2, mean)
  }
  
  #' [ Output: Merge all dfs into single data frame (to save)]
  output_df <- dfs[[1]];
  for( i in 2:length( dfs )){
    output_df <- merge(dfs_all, dfs[[i]], all = TRUE);
  }
  
  #' [ Make unique identifier for simulation results ]
  index_exclude <- which( sapply(model(sim2)@params, function(.x) length(.x)) > 1 ) # exclude non-scalars
  id_sim <- paste0(model(sim2)@params[ -index_exclude ], collapse = "-")
  
  #' [ Evaluations: Combine metrics into one table, append simulation conditions]
  metrics_df <- do.call(rbind, metrics_by_dfs)
  metrics_df_with_id <- cbind.data.frame( model( sim2 )@params[-index_exclude], metrics_df )
  
  if( write_output ){
    write.csv( output_df, file = paste0(output_dir,"output_", id_sim, ".csv"), row.names = FALSE )
  }
  if( write_evals ){
    write.csv( metrics_df_with_id, file = paste0(output_dir, metrics_file_name), row.names = FALSE, append = TRUE)
  }
  
  if( modify_reference_dirs ){
    sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
    sapply( 1:length( sim@draws_refs ), function( .index ){ sim@draws_refs[[ .index ]][[1]]@dir <<- ""; }) #' assuming only one draw
    sapply( 1:length( sim@output_refs ), function( .index.i ){ num_output_refs <- length( sim@output_refs[[ .index.i ]] );
    sapply( 1:num_output_refs, function( .index.j ){ sim@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- "";})}) 
  }
  #' ------------------------------------------------------------------------ #
  #' -------------------------- junk code below ----------------------------- #
  
  #' muh_evals <- evals( sim2 ) %>% 
  #'   subset_evals( method_names = useful_methods,
  #'                 metric_names = c("coverage", "power_p_value", "power_ci", "power_rerand" ) )
  #' 
  #' #' load in the models, draws, and output.
  #' muh_models <- load( sim2@model_refs )
  #' # muh_draws <- load( sim@draws_refs )
  #' muh_evals <- load_evals_from_ref( sim@evals_refs )
  #' df_evals <- as.data.frame( muh_evals )
  #' 
  #' muh_metrics <- apply( df_evals[,c("coverage", "power_p_value", "power_ci", "bias", "mse")], 2, mean )
  #' 
  #' muh_evals_new <- evaluate(object = muh_output, metrics = list(coverage, power_p_value, power_ci ))
}



