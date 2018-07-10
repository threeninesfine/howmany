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
#' Problem 6: Checking new naming workflow using a subset of simulations
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



# --------------------------------------------------------------------------- #
#' Problem 7: Suppressing output in R
# ref: https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r
capture.output({ print("foo") }, file = "/dev/null")


# --------------------------------------------------------------------------- #
#' Problem 8: two loops or one, which is faster?

#' Case A: two loops
cat(paste0("\n[ Model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
dfs <- list();
for( i in 1:length( output_j )){
  dfs[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
  dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
}
cat(paste0("Success! \nElapsed time: \n")); 

cat(paste0("[ Model ", sim_j, " ][----|    ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n")); 
metrics_by_dfs <- list();
true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
for( i in 1:length( dfs )){
  dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
  dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper ) 
  dfs[[i]]$power.ci <- with( dfs[[i]], 0 < cilower | 0 > ciupper )
  dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
  dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
  indices_colMeans <- which( dimnames(dfs[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias"))
  metrics_by_dfs[[i]] <- c(colMeans(dfs[[i]][, indices_colMeans]), nsim = dim( dfs[[i]] )[1], modelno = sim_j );
}
cat(paste0("Success! \nElapsed time (TWO loops): \n\n")); print( proc.time() - ptm );

#' Case B: one loop
cat(paste0("[ Model ", sim_j, " ][---|      ] Converting Output objects to data frame...\n")); ptm <- proc.time();
cat(paste0("[ Model ", sim_j, " ][----|     ] Computing metrics {power (p-value), power (rerandomized CI), power (wald CI), coverage, bias} for each Output object...\n"));
dfs <- list();
metrics_by_dfs <- list();
true_trt_effect <- model( simulation )[[ sim_j ]]@params$bZ
for( i in 1:length( output_j )){
  dfs[[ i ]]  <- data.frame(t(vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[1:9] )}, numeric(9))))
  dimnames(dfs[[i]])[[2]] <- c("est", "se", "t", "p", "adjusted", "rerandomized", "cilower", "ciupper", "num_rerandomizations")
  dfs[[i]]$power.pvalue <- with( dfs[[i]], p < 0.05 )
  dfs[[i]]$power.rerand <- with( dfs[[i]], est < cilower | est > ciupper ) 
  dfs[[i]]$power.ci <- with( dfs[[i]], 0 < cilower | 0 > ciupper )
  dfs[[i]]$coverage <- with( dfs[[i]], cilower < true_trt_effect & true_trt_effect < ciupper )
  dfs[[i]]$bias <- with( dfs[[i]], est - true_trt_effect )
  indices_colMeans <- which( dimnames(dfs[[i]])[[2]] %in% c("adjusted","rerandomized", "power.pvalue", "power.rerand","power.ci", "coverage", "bias"))
  metrics_by_dfs[[i]] <- c(colMeans(dfs[[i]][, indices_colMeans]), nsim = dim( dfs[[i]] )[1], modelno = sim_j );
}
cat("Success! \nElapsed time: \n\n"); print( proc.time() - ptm );

# Success! 
#   Elapsed time (TWO loops): 
#   
#   user  system elapsed 
# 0.520   0.082   0.657 
# 
# Success! 
#   Elapsed time (ONE loop): 
#   
#   user  system elapsed 
# 0.433   0.049   0.480 

# --------------------------------------------------------------------------- #
# --------------------- [ 7 July 2018 (Saturday) ] -------------------------- #
# --------------------------------------------------------------------------- #
#' Problem 9: standard errors too large, complete/quasi-separation?
outie <- output_j[[7]]

#' Scroll through output, looking for big numbers.
for( i in 1:length( output_j )){
  stderrs <- vapply( output_j[[ i ]]@out, function( .list ){ unlist( .list[[2]] )}, numeric(1) )
  indices_bigse <- which( stderrs > 1000 )
  hist( stderrs )
}


head( indices_bigse )

# --------------------------------------------------------------------------- #
# ----------------------- [ 9 July 2018 (Monday) ] -------------------------- #
# --------------------------------------------------------------------------- #
#' Problem 10: Analyze existing data (from manage_datasets.R c. 19 June 2018)

#' [ 0) Create paths to folders of simulation results ]
dir_of_dirs <- "~/Downloads/MSThesis/expanded_datasets/"
dirs <- sapply( dir(), function( .dir ){ paste0(dir_of_dirs, .dir, "/results/", collapse="/")})

#' [ 1) Describe the datasets, pipe to .txt file (output is verbose) ]
sink( file = "contY-binX-big-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/contY-binX-big/results/" )
sink()

sink( file = "howmany-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/howmany/results" ) #' 48mb 288 models no draws
sink()

sink( file = "sim-alloc-contY-binX-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/sim-alloc-contY-binX/results/" )
sink()

sink( file = "sim-study-description.txt", append = TRUE )
describe( "~/Downloads/MSThesis/expanded_datasets/sim-study/results/" )
sink()

#' -------------------------------------------------------------------------- #
#' [ 3) Evaluate content of 'contY-binX-big': 169 models, 35 models with draws ]
#' -------------------------------------------------------------------------- #
evaluate_this_simulation <- FALSE
if( evaluate_this_simulation ){
  resultsdir_path <-  "~/Downloads/MSThesis/expanded_datasets/contY-binX-big/results/"
  
  #' [4) Skip all the steps and make simulation with all files.]
  sim <- get_simulation_with_all_files(dir = resultsdir_path)
  
  #' [1) Evaluate contents (get output, model name, etc )] 
  contents_sim <- get_contents( dir = resultsdir_path )
  
  #' [2) Find indices such that it has draws, evals, output etc]
  si_draw_length <- sapply( contents_sim$objects, function( .obj ) length(.obj$draws) )
  si_has_draws <- which( si_draw_length > 0 )
  
  si_out_length <- sapply( contents_sim$objects, function( .obj ) length(.obj$out) )
  si_has_out <- which( si_out_length > 0 )
  
  si_evals_length <- sapply( contents_sim$objects, function( .obj ) length(.obj$evals) )
  si_has_evals <- which( si_evals_length > 0 )
  
  #' [3) Get model names with draws, evals, and output]
  indices_with_data <- intersect( si_has_out, intersect( si_has_evals, si_has_draws ))
  
  simsub <- subset_simulation(sim, subset = indices_with_data )
  simsub <- rename( simsub, name = "contY-binX-big-subset" ) 
  simsub <- relabel( simsub, label = "Continuous Y - binary X with 35 models" ) 
  save_simulation(simsub)
  
  #' add directory to all model refs, draws refs, output refs, and evals refs.
  sapply( 1:length( simsub@model_refs ), function( .index ){ simsub@model_refs[[ .index ]]@dir <<- simsub@dir; })
  sapply( 1:length( simsub@draws_refs ), function( .index ){ simsub@draws_refs[[ .index ]][[1]]@dir <<- simsub@dir; }) #' assuming only one draw
  sapply( 1:length( simsub@output_refs ), function( .index.i ){ 
    num_output_refs <- length( simsub@output_refs[[ .index.i ]] );
    sapply( 1:num_output_refs, function( .index.j ){ 
      simsub@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- simsub@dir;})})
  
  sapply( 1:length( simsub@evals_refs ), function( .index.i ){ 
    num_evals_refs <- length( simsub@evals_refs[[ .index.i ]] );
    sapply( 1:num_evals_refs, function( .index.j ){ 
      simsub@evals_refs[[ .index.i ]][[ .index.j ]]@dir <<- simsub@dir;})})
  
  #' load in the models, draws, and output.
  muh_models <- load( simsub@model_refs )
  muh_evals <- load_evals_from_ref( simsub@evals_refs )
  df_evals <- as.data.frame( muh_evals )
  
  muh_metrics <- apply( df_evals[,c("coverage", "power_p_value", "power_ci", "bias", "mse")], 2, mean )
  #' muh_draws <- load( simsub@draws_refs )
  
  #' Look at what output references are available
  methods_by_model <- sapply( simsub@output_refs, function( .outref.lvl1 ){
    table( sapply( .outref.lvl1, function( .outref.lvl2 ){ .outref.lvl2@method_name }) )})
  
  #' find output for example scenario to load, but first lets look at parameters 
  #' Look at parameter settings
  params_by_model <- as.data.frame(t(sapply( muh_models, function( .model ){ .model@params[c(1:6, 11:16)]  })))
  
  #' [ Choosing model 34 to modify output ]
  #' params_by_model[34, ]
  #'    trial_size outcome_type outcome_marginal_prevalence prognostic_factor_number prognostic_factor_type prognostic_factor_prevalence num_rerandomizations alpha
  # 34         96   continuous                         0.5                        2                 binary                          0.5                  500  0.05
  # prognostic_factor_effect_size treatment_assignment_effect_size entry_time_effect_size allocation_ratio
  # 34                             3                                3                      1              0.5
  
  muh_output <- load( simsub@output_refs[[34]][1:15] )
}else{
  sim <- load_simulation( name = "contY-binX-big-subset",
                          dir = "~/Downloads/MSThesis/expanded_datasets/contY-binX-big/results/" )
  
  #' fix model references
  sapply( 1:length( sim@model_refs ), function( .index ){ sim@model_refs[[ .index ]]@dir <<- sim@dir; })
  sapply( 1:length( sim@draws_refs ), function( .index ){ sim@draws_refs[[ .index ]][[1]]@dir <<- sim@dir; }) #' assuming only one draw
  sapply( 1:length( sim@output_refs ), function( .index.i ){ 
    num_output_refs <- length( sim@output_refs[[ .index.i ]] );
    sapply( 1:num_output_refs, function( .index.j ){ 
      sim@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})})
  
  sapply( 1:length( sim@evals_refs ), function( .index.i ){ 
    num_evals_refs <- length( sim@evals_refs[[ .index.i ]] );
    sapply( 1:num_evals_refs, function( .index.j ){ 
      sim@evals_refs[[ .index.i ]][[ .index.j ]]@dir <<- sim@dir;})})
  
  #' load adjusted estimates in.
  adj_ests <- load( sim@output_refs[[34]][1:15] )
  allocs <- load( sim@output_refs[[34]][16:30] )
}


#' Test method functions
SR <- make_complete_randomization_with_outcomes()
SBR <- make_stratified_block_randomization_with_outcomes()
CAA <- make_covariate_adaptive_allocation_with_outcomes()
adjusted_ests <- estimate_regression_parameters(return_extended_method = FALSE, adjusted = TRUE)

fix_directories <- function( simulation ){
  sapply( 1:length( simulation@model_refs ), function( .index ){ simulation@model_refs[[ .index ]]@dir <<- ""; })
  sapply( 1:length( simulation@draws_refs ), function( .index ){ simulation@draws_refs[[ .index ]][[1]]@dir <<- simulation@dir; }) #' assuming only one draw
  sapply( 1:length( simulation@output_refs ), function( .index.i ){ 
    num_output_refs <- length( simulation@output_refs[[ .index.i ]] );
    sapply( 1:num_output_refs, function( .index.j ){ 
      simulation@output_refs[[ .index.i ]][[ .index.j ]]@dir <<- simulation@dir;})})
  
  sapply( 1:length( simulation@evals_refs ), function( .index.i ){ 
    num_evals_refs <- length( simulation@evals_refs[[ .index.i ]] );
    sapply( 1:num_evals_refs, function( .index.j ){ 
      simulation@evals_refs[[ .index.i ]][[ .index.j ]]@dir <<- simulation@dir;})})
}

#' Read in output by variable
times <- sapply( 1:9, function( method_index ){ output( sim2 )[[ method_index ]]@out[[1]]$time })


# --------------------------------------------------------------------------- #
# --------------------- [ 10 July 2018 (Tuesday) ] -------------------------- #
# --------------------------------------------------------------------------- #
#' Problem 11: Tables and figures
#' Amalia Meier Magaret <amag@uw.edu>	Sat, Jul 7, 2018 at 2:08 PM
#' OK, just to reiterate.  (I had to go watch the final moments of a world cup game, back now.)
#' The table columns should be randomization method and analysis method, including whether adjusted for covariates.
#' The table rows can be outcome and covariate prevalences and effect sizes (betas).  
#' And number of participants can be row too.  I think that’s everything.  
#' One table for coverage, one for power, one for bias.
#' So exciting your progress!  Talk to you soon.

mettbl <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-all-9Jul18.csv" )
metrics_bYbX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-1-of-4.csv" )
metrics_bYcX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-2-of-4.csv" )
metrics_cYbX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-3-of-4.csv" )

# --------------------------------------------------------------------------- #
# ------------------ [ Continuous Y, continuous X ] ------------------------- #
# --------------------------------------------------------------------------- #
metrics_cYcX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-4-of-4.csv" )
# --------------------------------------------------------------------------- #
# --------------------------- [ COVERAGE ] ---------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl4.1.cov.adj <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl4.2.cov.adj <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl4.3.cov.adj <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl4.4.cov.adj <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl4.1.cov.un <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl4.2.cov.un <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl4.3.cov.un <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl4.4.cov.un <- subset( metrics_cYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4a: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl4.coverage.adj <- cbind( CR = tbl4.1.cov.adj[,"coverage"], 
                            SBR = tbl4.2.cov.adj[,"coverage"], 
                            CAA_model = tbl4.3.cov.adj[,"coverage"], 
                            CAA_rerand = tbl4.4.cov.adj[,"coverage"], tbl4.3.cov.adj[,-1] )

#' Table 4b: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl4.coverage.un <- cbind( CR = tbl4.1.cov.un[,"coverage"], 
                            SBR = tbl4.2.cov.un[,"coverage"], 
                            CAA_model = tbl4.3.cov.un[,"coverage"], 
#                            CAA_rerand = tbl4.4.cov.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                           tbl4.3.cov.un[,-1] )

# --------------------------------------------------------------------------- #
# ---------------------------- [ BIAS ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl4.1.bias.adj <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl4.2.bias.adj <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl4.3.bias.adj <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl4.4.bias.adj <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl4.1.bias.un <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl4.2.bias.un <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl4.3.bias.un <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl4.4.bias.un <- subset( metrics_cYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4c: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl4.bias.adj <- cbind( CR = tbl4.1.bias.adj[,"bias"], 
                            SBR = tbl4.2.bias.adj[,"bias"], 
                            CAA_model = tbl4.3.bias.adj[,"bias"], 
                            CAA_rerand = tbl4.4.bias.adj[,"bias"], tbl4.3.bias.adj[,-1] )

#' Table 4d: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl4.bias.un <- cbind( CR = tbl4.1.bias.un[,"bias"], 
                           SBR = tbl4.2.bias.un[,"bias"], 
                           CAA_model = tbl4.3.bias.un[,"bias"], 
                           #                            CAA_rerand = tbl4.4.bias.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                           tbl4.3.bias.un[,-1] )

# --------------------------------------------------------------------------- #
# --------------------------- [ POWER ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl4.1.power.adj <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                           subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl4.2.power.adj <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                           subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl4.3.power.adj <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl4.4.power.adj <- subset( metrics_cYcX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl4.1.power.un <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl4.2.power.un <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl4.3.power.un <- subset( metrics_cYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl4.4.power.un <- subset( metrics_cYcX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4c: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl4.power.adj <- cbind( CR = tbl4.1.power.adj[,"power.pvalue"], 
                        SBR = tbl4.2.power.adj[,"power.pvalue"], 
                        CAA_model = tbl4.3.power.adj[,"power.pvalue"], 
                        CAA_rerand = tbl4.4.power.adj[,"power.rerand"], tbl4.3.power.adj[,-1] )

#' Table 4d: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl4.power.un <- cbind( CR = tbl4.1.power.un[,"power.pvalue"], 
                       SBR = tbl4.2.power.un[,"power.pvalue"], 
                       CAA_model = tbl4.3.power.un[,"power.pvalue"], 
                            CAA_rerand = tbl4.4.power.un[,"power.rerand"], #' TODO(michael): run unadjusted, rerandomized simulations!
                       tbl4.3.power.un[,-1] )



