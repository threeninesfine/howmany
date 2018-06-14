## @knitr manage_datasetss
## author             = Michael Flanagin
## date started       = 1 Jun 2018
## date last modified = 3 Jun 2018
## advisor            = Amalia Magaret
## objective          = Analyze results of analysis

library("simulator")

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
