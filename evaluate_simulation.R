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

datasets <- c("contY-binX-big", "howmany-binY-binX", "sim-alloc-contY-binX", "sim-study")
base_dir <- "~/Downloads/MSThesis/expanded_datasets/"
resultsdir_path <- paste0( base_dir, datasets[ 2 ], "/results/")
output_dir = "./results/"
metricfile_name <- paste0(output_dir,"metrics-simulation.csv");
#' -------------------------------------------------------------------------- #
#'         [ Chapter 1: How to view simulation objects from stored Rdata ]
#' -------------------------------------------------------------------------- #
if( load_sim_from_file ){
  sim <- get_simulation_with_all_files(dir = output_dir )
  sim_contents <- get_contents( dir = output_dir ) #' [ Evaluate contents (get model, draw, output, evaluation info) ] 
  
  #' [ Find models that have draws, output, and/or evaluations ]
  sim_draw_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$draws) )
  sim_out_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$out) )
  sim_evals_length <- sapply( sim_contents$objects, function( .obj ) length(.obj$evals) )
  
  #' [ Get model names with draws, evals, and output]
  sims_with_output <- which( sim_out_length > 0 )
  sims_with_draws <- which( sim_draw_length > 0 )
  sims_with_evals <- which( sim_evals_length > 0 )
  sim_indices_with_data <- intersect( sims_with_evals, intersect( sims_with_output, sims_with_draws ))
  
  #' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
  sim_models <- load( sim@model_refs )
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
  params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ unlist( .model@params[c(1:6, 11:16)] ) })))
  
  #' [ Subset models to scenarios of interest ]
  model_indexes_of_interest <- with( params_by_model, which( treatment_assignment_effect_size %in% c(1,3) &
                                                               prognostic_factor_effect_size %in% c(1,3) &
                                                               entry_time_effect_size == c(1,3) &
                                                               prognostic_factor_number == 2 &
                                                               prognostic_factor_prevalence == 0.5 &
                                                               trial_size %in% c(32,64,96) &
                                                               outcome_marginal_prevalence == 0.5 ))
  
  sim2 <- subset_simulation( sim, subset = model_indexes_of_interest )
}

#' [ 'metricfile_name' contains model, draw, output, evaluation info ] 
metricfile_name <- "./results-TEST/metrics-simulation.csv"

for( sim_j in 1:length(model( simulation )) ){
  cat(paste0("[ Output ", sim_j, " ][-|       ] Loading output from simulation [ ", simulation@name, " ]...\n")); ptm.all <- proc.time();
  output_j <- output( simulation )[[ sim_j ]] #' Model sim_j, 
  output_method_names <- sapply( output_j, function( .object ){ .object@method_name })
  methods_to_exclude <- c("CR", "SBR", "CAA");  #' exclude output from list that only contains allocation methods
  index_output_methods_to_include <- which(!( output_method_names %in% methods_to_exclude ))
  methods_included_parsed <- t(sapply( strsplit( output_method_names[ index_output_methods_to_include ], split = "_"), function(.listobj){unlist( .listobj )}))
  dimnames( methods_included_parsed ) <- list( index_output_methods_to_include, c("alloc_method", "analysis_method", "adjustment"))
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm.all );
  cat(paste0("Simulation [ ", sim_j, " ] has these outputs:\n"))
  print( methods_included_parsed )
  
  cat("[ model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n"); ptm <- proc.time();
  dfs <- list();
  for( i in 1:length( index_output_methods_to_include )){
    out.index <- index_output_methods_to_include[ i ]
    dfs[[ i ]]  <- data.frame(t(vapply( output_j[[ out.index ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
    dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
  }
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  cat("[ model ", sim_j, " ][----|    ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"); ptm <- proc.time();
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
  
  cat("[ model ", sim_j, " ][------|  ] Combining metrics with simulation conditions...\n")
  metrics_df <- do.call(rbind, metrics_by_dfs)
  metrics_df_with_id <- cbind.data.frame( alloc_method = methods_included_parsed[,"alloc_method"], metrics_df,
                                          model( simulation )[[ sim_j ]]@params[-index_exclude]) 
  cat("Success! \n")
  
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
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time:")); print( proc.time() - ptm.all );
}


