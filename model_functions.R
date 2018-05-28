## @knitr models

#' [ make_trial_allocation_model() ]
make_trial_allocation_model <- function(trial_size,
                                        outcome_type,
                                        outcome_marginal_prevalence,
                                        prognostic_factor_type,
                                        prognostic_factor_prevalence,
                                        prognostic_factor_number,
                                        prognostic_factor_effect_size,
                                        treatment_assignment_effect_size,
                                        entry_time_effect_size,
                                        allocation_ratio,
                                        num_rerandomizations,
                                        alpha )
{
  #' [ use: > creates allocation model to evaluate treatment allocation methods. ]
  #' [      > stores allocation model parameters "alloc_" ]
  #' [      > stores uncertainty estimation parameters "num_rerandomizations" and "alpha" ]

  #' [effect sizes] Convert effect size into log(OR) or mean difference.
  if( outcome_type == "binary" ){
    bX <- log(as.numeric( prognostic_factor_effect_size ))
    bT <- log(as.numeric( entry_time_effect_size ))
    bZ <- log(as.numeric( treatment_assignment_effect_size ))
  }else{
    bX <- prognostic_factor_effect_size - 1
    bT <- entry_time_effect_size - 1
    bZ <- treatment_assignment_effect_size - 1
  }
  bX <- rep( bX, times = prognostic_factor_number );
  
  #' [prognostic factors] Set risk factor prevalences to list of vectors of their PMFs.
  pmfX <- c( 1 - prognostic_factor_prevalence,
             prognostic_factor_prevalence )
  probsX <- lapply( pmfX, function( .probX ){
    rep( .probX, prognostic_factor_number )})
  
  new_model(name = sprintf(paste0("trial_size-%s-outcome-%s-predictor-%s-",
                                  "num_predictors-%.0f-outcome-prev-%.2f-predictor_prev-%.2f-",
                                  "bX-%.1f-bT-%.1f-bZ-%.1f"),
                           trial_size, outcome_type, prognostic_factor_type, prognostic_factor_number,
                           outcome_marginal_prevalence, prognostic_factor_prevalence, bX[1], bT, bZ),
            label = sprintf(paste0("Allocation model - trial_size-%s-outcome-%s-predictor-%s-",
                                   "num_predictors-%.0f-outcome-prev-%.2f-predictor_prev-%.2f-",
                                   "bX-%.2f-bT-%.2f-bZ-%.2f"),
                            trial_size, outcome_type, prognostic_factor_type, prognostic_factor_number,
                            outcome_marginal_prevalence, prognostic_factor_prevalence, bX[1], bT, bZ),
            params = list(trial_size = trial_size,
                          outcome_type = outcome_type,
                          outcome_marginal_prevalence = outcome_marginal_prevalence,
                          prognostic_factor_number = prognostic_factor_number,
                          prognostic_factor_type = prognostic_factor_type,
                          prognostic_factor_prevalence = prognostic_factor_prevalence,
                          probsX = probsX,
                          bX = bX,
                          bT = bT,
                          bZ = bZ,
                          num_rerandomizations = num_rerandomizations,
                          alpha = alpha ),
            simulate = function(trial_size, prognostic_factor_type, prognostic_factor_number, probsX, nsim ){
              #' [Prognostic factors (X)]
              if( prognostic_factor_type == "binary" ){
                .X <- sapply( probsX, FUN = function( .probs ){ .levels = length( .probs );
                sample( 0:(.levels - 1), size = trial_size * nsim, replace = TRUE, prob = .probs )})
              }else{
                .X <- matrix(rnorm( trial_size * prognostic_factor_number * nsim ),
                             nrow = trial_size * nsim, ncol = prognostic_factor_number);
              }
              
              #' [Entry time (T)] Uniform entry time between -1/2 and 1/2, sorted for each trial.
              .Tobs <- matrix(replicate(n = nsim, sort(runif(n = trial_size, min=-1/2, max=1/2))))
              
              .sim.df <- data.frame(row.names = 1:(nsim * trial_size));
              .sim.df$sim <- sort(rep(1:nsim, trial_size));
              .sim.df$X <- .X;
              .sim.df$Tobs <- .Tobs;
              
              return(split( .sim.df, f = .sim.df$sim ))})
}