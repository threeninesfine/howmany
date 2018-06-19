## @knitr evaluate_simulation
## author             = Michael Flanagin
## date started       = 6 Jun 2018
## date last modified = 19 Jun 2018
## advisor            = Amalia Magaret
## objective          = Evaluate generated simulation



library("simulator")


load_sim_from_file = FALSE
write_evals = TRUE
write_output = TRUE

# dir( resfiles );
# c("contY-binX-big", "howmany-binY-binX", "sim-alloc-contY-binX", "sim-study")
resfiles <- "~/Downloads/MSThesis/expanded_datasets/"
resultsdir_path <- "~/Downloads/MSThesis/expanded_datasets/sim-alloc-contY-binX/results/"
output_dir = "./results/"
metricfile_name <- paste0(output_dir,"metrics-simulation.csv");
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


for( sim_j in indices_with_data ){
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
    output_df <- merge(output_df, dfs[[i]], all = TRUE);
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
  

  cat(paste0("[ model ", sim_j, " ][--------|] Attempting to write metrics to: ", metricfile_name, "...\n")); ptm <- proc.time();
  if( write_evals ){
    if(!file.exists( metricfile_name )){
      cat(paste0("\nNOTE: file: ", metricfile_name, " does not exist. \nCreating file and saving...\n"))
      write.csv( metrics_df_with_id, file = metricfile_name, row.names = FALSE )
    }else{
      cat(paste0("Appending metrics to file ", metricfile_name, "...\n"))
      write.table( metrics_df_with_id, file = "", sep = ",", append = TRUE, quote = FALSE,
                      col.names = FALSE, row.names = FALSE)
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  }
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time:")); print( proc.time() - ptm.all );
}


