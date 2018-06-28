# ---
#   title: "Scratch"
#   author: Michael Flanagin
#   date: June 27, 2018
#   output: .R script
# ---

# --------------------------------------------------------------------------- #
#' Problem 1: Dichotomizing continuous valued vector OR matrix and returning matrix
#' ref: https://stackoverflow.com/questions/8579257/r-applying-function-over-matrix-and-keeping-matrix-dimensions
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
#' [ 'sim_contents' contains model, draw, output, evaluation info ] 
sim_contents <- get_contents( dir = paste0(getwd(), "/results-TEST") )


#' [ sim_descrip contains counts of model, draw, output, evaluations by simulation ] 
sim_descrip <- t(sapply( sim_contents$objects, function( .modelObj ){
  ndraws <- sum(vapply(.modelObj$draws, length, numeric(1)));
  nevals <- sum(vapply(.modelObj$evals, length, numeric(1)));
  nout <- sum(vapply(.modelObj$out, length, numeric(1)));
  return(c(ndraws = ndraws, nevals = nevals,nout = nout))}))
row.names( sim_descrip ) <- NULL;
sim_indices_with_output <- which( sim_descrip[, "nout"] > 0 )

describe(paste0(getwd(), "/results-TEST"))

sim <- get_simulation_with_all_files(dir = output_dir )

#' [ Get data frame of model parameters -- to easily locate model index corresp. to a particular configuration ]
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- simulation@dir; })
simulation_models <- load( simulation@model_refs )
sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- ""; })
params_by_model <- as.data.frame(t(sapply( simulation_models, function( .model ){ unlist( .model@params[c(1:6, 11:16)] ) })))


# --------------------------------------------------------------------------- #
# NOTE: include this in main.R after testing
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


# --------------------------------------------------------------------------- #
#' Problem 4: Find index to output with regression estimates, using method names
output_method_names <- sapply( muh_output, function( .object ){ .object@method_name })
head_method_names <- sapply( strsplit( output_method_names, split = "-"), function(.listobj){unlist( .listobj )[1]})
methods_of_interest <- which( head_method_names %in% c("adjusted", "unadjusted") )

# --------------------------------------------------------------------------- #
#' Problem 5: comparing performance of transform() vs. new assignment
dftest <- data.frame(foo = runif(50000), bar = rnorm(50000))
system.time(dftest$baz <- with( dftest, foo * bar - bar ))
system.time(dftest <- transform( dftest, bat = foo * bar - bar ))
dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )


# --------------------------------------------------------------------------- #
#' Problem 5: Checking new naming workflow using a subset of simulations
#' NOTE: assumed that 'simulation' object is already defined, as well as many variables from main.R 
draw_from_model = TRUE
allocate_groups = TRUE
estimate_effects = TRUE
unadjusted_analyses = FALSE
estimate_rerandomized_errors = TRUE
if( draw_from_model ){
  simulation <- subset_simulation( simulation, subset = 1:2 )
  cat(paste0("[ 1 ] Drawing from simulation model...\n")); ptm <- proc.time();
  simulation <- simulate_from_model(object = simulation,
                                nsim = num_simulations_per_core,
                                index = 1:num_cores_parallel,
                                parallel = list(socket_names = num_cores_parallel))
  cat(paste0("Success! \nElapsed time (draw from model): \n")); print( proc.time() - ptm );
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 2: simulate outcomes and allocate tx groups ] ----------- #
# --------------------------------------------------------------------------- #
if( allocate_groups ){ 
  cat(paste0("[ 2 ] Allocating groups, by complete randomization (CR)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = CR, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (allocate groups by CR): \n")); print( proc.time() - ptm );
  cat(paste0("[ 2 ] Allocating groups, by stratified block randomization (SBR)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = SBR, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (allocate groups by SBR): \n")); print( proc.time() - ptm );
  cat(paste0("[ 2 ] Allocating groups, by covariate adaptive allocation (CAA)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = CAA_probabilistic, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (allocate groups by CAA): \n")); print( proc.time() - ptm );
  
}

# --------------------------------------------------------------------------- #
#' -------- [ Phase 3: estimate tx effects (adjusted, unadjusted) ] --------- #
# --------------------------------------------------------------------------- #
if( estimate_effects ){
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (complete randomization)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = CR + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, complete randomization): \n")); print( proc.time() - ptm );
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (stratified block randomization)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = SBR + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, stratified block randomization): \n")); print( proc.time() - ptm );
  cat(paste0("[ 3a ] Estimating adjusted treatment effects (covariate adaptive allocation)...\n")); ptm <- proc.time();
  simulation <- run_method(object = simulation, methods = CAA_probabilistic + adjusted_ests, parallel = list( socket_names = num_cores_parallel ))
  cat(paste0("Success! \nElapsed time (estimate adjusted effects, covariate adaptive allocation): \n")); print( proc.time() - ptm );
  
  if( unadjusted_analyses ){
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (complete randomization)...\n")); ptm <- proc.time();
    capture.output({ simulation <- run_method(object = simulation, methods = CR + unadjusted_ests, parallel = list( socket_names = num_cores_parallel )) })
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, complete randomization): \n")); print( proc.time() - ptm );
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (stratified block randomization)...\n")); ptm <- proc.time();
    simulation <- run_method(object = simulation, methods = SBR + unadjusted_ests, parallel = list( socket_names = num_cores_parallel ))
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, stratified block randomization): \n")); print( proc.time() - ptm );
    cat(paste0("[ 3b ] Estimating unadjusted treatment effects (covariate adaptive allocation)...\n")); ptm <- proc.time();
    simulation <- run_method(object = simulation, methods = CAA_probabilistic + unadjusted_ests, parallel = list( socket_names = num_cores_parallel ))
    cat(paste0("Success! \nElapsed time (estimate unadjusted effects, covariate adaptive allocation): \n")); print( proc.time() - ptm );
  }
}

# --------------------------------------------------------------------------- #
#' ---- [ Phase 4: estimate rerandomized errors (adjusted, unadjusted) ] ---- #
# --------------------------------------------------------------------------- #
if( estimate_rerandomized_errors ){
  cat(paste0("[ 4a ] Estimating adjusted treatment effect errors (for CAA only) by rerandomization...\n")); ptm <- proc.time();
  capture.output({ simulation <- run_method(object = simulation,
                           methods = CAA_probabilistic + adjusted_ests_rerandomized,
                           parallel = list( socket_names = num_cores_parallel )) }, file = "/dev/null")
  cat(paste0("Success! \nElapsed time (estimate adjusted effects for CAA, rerandomized): \n")); print( proc.time() - ptm );
  if( unadjusted_analyses ){
    cat(paste0("[ 4b ] Estimating adjusted treatment effect errors (for CAA only) by rerandomization...\n")); ptm <- proc.time();
    capture.output({ simulation <- run_method(object = simulation,
                                          methods = CAA_probabilistic + unadjusted_ests_rerandomized,
                                          parallel = list( socket_names = num_cores_parallel ))}, file = "/dev/null")
    cat(paste0("Success! \nElapsed time (estimate UNadjusted effects for CAA, rerandomized): \n")); print( proc.time() - ptm );
  }
}

bar <- data.frame( X = runif(32), Z = rnorm(n=32, sd=4) )  # example data
bar$Y <- with( bar, X + Z + rnorm(n=32, sd=1) )  # Y = X + Z + Gaussian noise
# fit_model() returns lm() object for downstream use
fit_model <- function( predictor_variable ){  
  lm( Y ~ predictor_variable, data = bar )
}

fit_model( bar$Z )
