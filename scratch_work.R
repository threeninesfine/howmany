# ---
#   title: "Scratch"
#   author: Michael Flanagin
#   date: June 27, 2018
#   output: .R script
# ---

# --------------------------------------------------------------------------- #
#' Problem 1: Dichotomizing continuous valued vector OR matrix and returning matrix
dichotomize <- function( x, x_cutpoint = 0 ) return( x >= x_cutpoint );

bar <- as.matrix( runif(32) )
baz <- as.matrix( cbind( runif(32), rnorm(32)) )

#' a) apply() over rows and cols
system.time({bar[] <- apply( bar, 1:2, FUN = function(.x) as.numeric(.x >= 0) ); dim( bar )})
#' b) current implementation [ not working ]
system.time({bar <- as.numeric(bar >= 0); dim( bar ) })
#' c) [ best case ] vapply() with function, specify output type as numeric(1)
system.time( baz[] <- vapply( baz, dichotomize, numeric(1)) )
system.time( bar[] <- vapply( bar, dichotomize, numeric(1)) )

# --------------------------------------------------------------------------- #
#' Problem 2: Using [[ ]] vs. $ to internally reference lists

bar <- list(foo = "binary", baz = 2)
system.time(print( bar$foo ))
system.time(print( bar[["foo"]] ))

# --------------------------------------------------------------------------- #
#' Problem 3: extracting simulation Output >> sim_X_outputs_YZ.csv
dir( "~/Documents/MSThesis/code/expanded_datasets/howmany-binY-binX/results/files/")
#' [ sim_contents contains model, draw, output, evaluation info ] 
sim_contents <- get_contents( dir = paste0(getwd(), "/results-TEST") ) 
#' [ sim_descrip contains counts of model, draw, output, evaluations by simulation ] 
sim_descrip <- t(sapply( sim_contents$objects, function( .modelObj ){
  ndraws <- sum(vapply(.modelObj$draws, length, numeric(1)));
  nevals <- sum(vapply(.modelObj$evals, length, numeric(1)));
  nout <- sum(vapply(.modelObj$out, length, numeric(1)));
  return(c(ndraws = ndraws, nevals = nevals,nout = nout))}))
row.names( sim_descrip ) <- NULL;
indices_with_output <- which( sim_descrip[, "nout"] > 0 )

describe(paste0(getwd(), "/results-TEST"))

sim <- get_simulation_with_all_files(dir = output_dir )

#' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
sim_models <- load( sim@model_refs )
sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- ""; })
params_by_model <- as.data.frame(t(sapply( sim_models, function( .model ){ unlist( .model@params[c(1:6, 11:16)] ) })))

#' [ Subset models to scenarios of interest ]
if( FALSE ){
  model_indexes_of_interest <- with( params_by_model, which( treatment_assignment_effect_size %in% c(1,3) &
                                                               prognostic_factor_effect_size %in% c(1,3) &
                                                               entry_time_effect_size == c(1,3) &
                                                               prognostic_factor_number == 2 &
                                                               prognostic_factor_prevalence == 0.5 &
                                                               trial_size %in% c(32,64,96) &
                                                               outcome_marginal_prevalence == 0.5 ))
  
  sim2 <- subset_simulation( sim, subset = model_indexes_of_interest )
}



for( sim_j in indices_with_output ){
  cat(paste0("[ model ", sim_j, " ][-|       ] Loading output from simulation model [ ", sim_j, " ]...\n")); ptm.all <- proc.time();
  output_j <- output( sim )[[ sim_j ]]
  output_method_names <- sapply( output_j, function( .object ){ .object@method_name })
  #' example data
  # methods_of_interest <- c("CR_REG_UN", "SBR_REG_ADJ", "CAA_RERAND_ADJ")
  foo.parsed <- t(sapply( strsplit( output_method_names, split = "_"), function(.listobj){unlist( .listobj )}))
  rand.method <- foo.parsed[,1]
  analysis.method <- foo.parsed[,2]
  adjusted <- foo.parsed[,3]
  
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm.all );
  cat(paste0("Simulation [ ", sim_j, " ] has these indexes of interest:\n"))
  
  cat("[ model ", sim_j, " ][--|      ] Changing proc_time values to elapsed_time (to allow making data.frames)...\n"); ptm <- proc.time();
  #' NOTE: can just save first e.g. 9 indices rather than modifying output object. TODO: implement this!
  sapply( output_indices_methods_of_interest, function( out_i ){ 
    sapply( 1:length( output_j[[ out_i ]]@out ), function( draw_j ){
      #' Modifying output to scalars
      output_j[[ out_i ]]@out[[draw_j]]$time <<- output_j[[ out_i ]]@out[[draw_j]]$time[3]; 
      output_j[[ out_i ]]@out[[draw_j]]$hash <<- 1; 
    })
  })
  cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  
  cat("[ model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n"); ptm <- proc.time();
  dfs <- list();
  for( i in 1:length( output_indices_methods_of_interest )){
    out.index <- output_indices_methods_of_interest[ i ]
    dfs[[ i ]]  <- data.frame(t(sapply( output_j[[ out.index ]]@out, function( .list ){ unlist( .list )})))
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
  metrics_df_with_id <- cbind.data.frame( model( sim2 )@params[-index_exclude], metrics_df ) # TODO: add rand.method (e.g. "CR", "SBR", "CAA")
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
      write.table( metrics_df_with_id, file = metricfile_name, sep = ",", append = TRUE, quote = FALSE,
                   col.names = FALSE, row.names = FALSE)
    }
    cat(paste0("Success! \nElapsed time: \n")); print( proc.time() - ptm );
  }
  cat(paste0("Simulation model [ ", sim_j, " ] processing complete. \nTotal time:")); print( proc.time() - ptm.all );
}




