## @knitr methods
## author             = Michael Flanagin
## date started       = 2018 May 16
## date last modified = 2018 May 28
## advisor            = Amalia Magaret
## objective          = Methods to allocate treatment, simulate outcomes
##                        ExtendedMethod to estimate regression model parameters
##                        ExtendedMethod to create rerandomized uncertainty estimates

# --------------------------------------------------------------------------- #
#' --------- [ allocation methods that also simulate outcomes ] ------------- #
# --------------------------------------------------------------------------- #

#' [ helper function 1: simulate_outcomes(). (v1.2) from Model@draw (X,Tobs) return (Y,Z) ]
simulate_outcomes <- function( model, draw, Z ){
  logit <- function( .p ){ log( .p / ( 1 - .p ) ) }
  expit <- function( .logodds ){ exp( .logodds ) / ( 1 + exp( .logodds )) }
  #' [intercept] Compute intercept such that marginal outcome prevalence is equal to 'outcome_marginal_prevalence'
  model_intercept <- 0;
  if( model$outcome_type == "binary" ){
    if( model$prognostic_factor_type == "binary" ){
      #' [assumption] X binary implies X ~ mult(n, p, 'prognostic_factor_number')
      expected_value_X <- sapply( model$probsX, function(.probs){sum(.probs * 0:(length(.probs)-1))})
      model_intercept <- logit( model$outcome_marginal_prevalence ) - sum( model$bX * expected_value_X, model$bZ * 1/2 )
    }else{
      #' [assumption] X continuous implies X ~ N(0,1)
      model_intercept <- logit( outcome_marginal_prevalence ) - 1/2 * model$bZ
    }
  }
  linear_predictor <- model_intercept + draw$X %*% model$bX + Z * model$bZ + draw$Tobs * model$bT
  #' [Outcome (Y)] simulate by sampling from a multinomial or adding Gaussian noise.
  Y <- rep( NA, times = model$trial_size );
  if( model$outcome_type == "binary" ){ # sampling from Multinomial(n, p. numX)
    Y <- sapply(expit( linear_predictor ), function( .prob ){rbinom(n = length( .prob ), size = 1, prob = .prob )})
  }else{ # add Gaussian noise
    Y <- linear_predictor + matrix(rnorm(n = model$trial_size, mean = 0, sd = 1), nrow = model$trial_size)
  }
  return( Y = Y )
} # returns outcomes (Y)

#' [function 1: make_complete_randomization_with_outcomes()]
make_complete_randomization_with_outcomes <- function( allocation_ratio = NULL, simulate_outcome = TRUE ){
  allocation_ratio <- ifelse(is.null( allocation_ratio ), 0.5, allocation_ratio ) #' [set allocation ratio to 0.5 if NULL]
  new_method(name = "CR",
             label = sprintf("Allocation - complete randomization - allocation ratio %.2f", allocation_ratio),
             settings = list( allocation_ratio = allocation_ratio ),
             method = function( model, draw, allocation_ratio, simulate_outcome = TRUE ){
               #'[ Redefine method parameters: allocation_ratio (just in case base_method is called from an extension) ]
               # allocation_ratio <- ifelse(is.null( allocation_ratio ), 0.5, allocation_ratio) 
               allocation_ratio <- model$allocation_ratio;
               Z_complete_rand <- rbinom( n = model$trial_size, prob = allocation_ratio, size = 1 )
               if( simulate_outcome ){
                 Y <- simulate_outcomes( model, draw, Z = Z_complete_rand )
                 return(list( Y = Y, Z = Z_complete_rand ))
               }else{
                 return(list( Z = Z_complete_rand ))
               }
             })
} # returns allocation sequence (Z) outcome (Y)

#' [function 2: make_stratified_block_randomization_with_outcomes( )]
make_stratified_block_randomization_with_outcomes <- function( block_size = NULL,
                                                               simulate_outcome = TRUE ){
  #' [ Define method parameters: block_size, X_cutpoint ]
  #' block_size <- ifelse( is.null( block_size ), 8, block_size ) #' [ todo: extend this to specify custom block sizes]
  new_method(name ="SBR",
             label = "Allocation - stratified block randomization - block size TBD",    
             # name = sprintf("allocation_SBR-block-size_%.0f", block_size),
             # label = sprintf("Allocation - stratified block randomization - block size %.0f", block_size),
             # settings = list( block_size = block_size, X_cutpoint = X_cutpoint ),
             method = function( model, draw, block_size = NULL, simulate_outcome = TRUE ){
               dichotomize <- function( x, cutpoint = 0 ){ x >= cutpoint }
               #' [ Redefine method parameters: block_size, X_cutpoint (just in case base_method is called from an extension)]
               block_size <- model$trial_size / ( 2 ^ model$prognostic_factor_number );
               .X <- matrix( draw$X[, 1:model$prognostic_factor_number], ncol = model$prognostic_factor_number );
               #' [ Dichotomize prognostic variables by X_cutpoint ]
               if( model$prognostic_factor_type == "continuous" ){
                 .X[] <- vapply( .X, dichotomize, numeric(1) )
               }
               
               strata_labels <- apply( .X, 1, FUN=function(.row){ paste0(.row, collapse="") } );
               Z_SBR <- rep( NA, times = model$trial_size )
               names( Z_SBR ) <- strata_labels; # get strata labels for each observation
               strata_size <- table( strata_labels );  # get table of observed counts for each strata.
               num_randomized_blocks <- ceiling( strata_size / block_size );   # for each strata, get minimum number of blocks to allocate all subjects.
               for( strata.i in 1:length( strata_size ) ){
                 allocation_vec <- do.call("c", replicate(n = num_randomized_blocks[ strata.i ],
                                                          expr = sample( x=rep(0:1, ceiling( block_size / 2 )),
                                                                         size = block_size, replace=FALSE), simplify=FALSE));
                 # then associate each strata member with an assignment.
                 Z_SBR[ names(Z_SBR) == names( strata_size )[ strata.i] ] <- allocation_vec[ 1:strata_size[ strata.i] ];
               }
               if( simulate_outcome ){
                 Y <- simulate_outcomes( model, draw, Z = Z_SBR )
                 return(list( Y = Y, Z = Z_SBR ))
               }else{
                 return(list( Z = Z_SBR ))
               }
             })
} # returns allocation sequence (Z), outcome (Y)

#' [function 3: make_covariate_adaptive_allocation_with_outcomes( )]
make_covariate_adaptive_allocation_with_outcomes <- function( allocation_max_imbalance = NULL,
                                                              allocation_biasing_probability = NULL,
                                                              simulate_outcome = TRUE ){
  #' [ Define method parameters: max_imbalance_vector, allocation_biasing_probability, X_cutpoint ]
  allocation_max_imbalance <- ifelse(is.null( allocation_max_imbalance ), 2, allocation_max_imbalance )
  allocation_biasing_probability <- ifelse(is.null( allocation_biasing_probability ), 1.0, allocation_biasing_probability )
  
  new_method(name = sprintf("CAA-MI-%.0f-PBA-%.2f", allocation_max_imbalance, allocation_biasing_probability),
    # name = sprintf("CAA-max-imbalance_%.0f-pbiasedalloc_%.2f", allocation_max_imbalance, allocation_biasing_probability ),
             label = sprintf("Allocation - covariate adaptive allocation - max imbalance %.0f - allocation biasing probability %.2f", 
                             allocation_max_imbalance, allocation_biasing_probability ),
             settings = list( allocation_max_imbalance = allocation_max_imbalance,
                              allocation_biasing_probability = allocation_biasing_probability ),
             method = function( model, draw, allocation_max_imbalance = NULL, allocation_biasing_probability = NULL, simulate_outcome = TRUE ){
               #' [ Format method parameters: max_imbalance_vector, allocation_biasing_probability, X_cutpoint ]
               if(is.null( allocation_max_imbalance )){ #' [ case 1: default imbalance is 2 for all vars X,Z ]
                 max_imbalance_vector <- rep( 2, model$prognostic_factor_number + 1 )
               }else if(length( allocation_max_imbalance ) < model$prognostic_factor_number){ #' [case 2: if scalar, make vector]
                 max_imbalance_vector <- rep( allocation_max_imbalance, model$prognostic_factor_number + 1 )
               }else{ #' [case 3: OK!]
                 max_imbalance_vector <- allocation_max_imbalance
               }
               allocation_biasing_probability <- ifelse(is.null( allocation_biasing_probability ), 1.0, allocation_biasing_probability)
               
               #' [ Dichotomize prognostic variables by X_cutpoint ]
               dichotomize <- function( x, cutpoint = 0 ){ x >= cutpoint }
               .X <- matrix( draw$X[, 1:model$prognostic_factor_number] );
               if( model$prognostic_factor_type == "continuous" ){
                 .X[] <- vapply( .X, dichotomize, numeric(1) )
               }
               
               
               strata_labels <- apply( .X, 1, FUN=function(.row){ paste0(.row, collapse="") } ); # get strata membership for each observation
               Z_CAR <- rep( NA, times = model$trial_size );
               names( Z_CAR ) <- strata_labels; # make allocation vector 'Z', name vector by stratum membership (e.g. "101")
               names_observed_strata <- lapply( 1:model$prognostic_factor_number, function(.col){sort(unique( draw$X[, .col] ))} );
               levels_by_strata <- sapply( names_observed_strata, function(.listobj){length( .listobj )} );
               # '.alloc' tracks the history of covariate + treatment assignments i.e. the (X,Z) pairs 
               #   for sequential assessment of covariate imbalance across treatment groups.
               .alloc <- array(0, dim=c( 2, levels_by_strata ), dimnames=c(list(c("ctrl", "tx")), names_observed_strata))
               nforced <- 0;
               for( subject_i in 1:model$trial_size ){ 
                 covar_indices_subj_i <- sapply( 1:model$prognostic_factor_number, function(.j){
                   which( names_observed_strata[[ .j ]] == draw$X[subject_i, .j] )} );
                 for( covar_j in 1:model$prognostic_factor_number ){ #' [ considering each covariate 'covar_j' in decreasing order of importance, ]
                   tx_ctrl_counts_by_subj_i_covar_j <- apply(.alloc, c(1, (covar_j+1)), sum)[ , covar_indices_subj_i[covar_j]]; #' consider tx/control assignments for subject 'subject_i's observed covar_j
                   observed_tx_imbalance_subj_i_covar_j <- diff( tx_ctrl_counts_by_subj_i_covar_j ); #' [ evaluate potential imbalance := #"tx" - #"ctrl" ]
                   #' [ compare imbalance to a priori limits (specified by 'max_imbalance_vector') ]
                   if( abs( observed_tx_imbalance_subj_i_covar_j ) >= max_imbalance_vector[ covar_j ] ){ #' [ if potential allocation would meet/exceed imbalance, ]
                     nforced <- nforced + 1; #' track forced allocation
                     group_minimizing_imbal <- ifelse(observed_tx_imbalance_subj_i_covar_j > 0, 0, 1); #' positive imbalance implies alloc to "ctrl" minimizes imbalance.
                     assign_to_min_group <- rbinom(n = 1, size = 1, prob = allocation_biasing_probability); #' [assign to group that minimizes imbalance with probability prob_biased.]
                     Z_CAR[ subject_i ] <- ifelse( assign_to_min_group,
                                                   group_minimizing_imbal,
                                                   1 - group_minimizing_imbal );
                     index_new_allocation <- rbind(c(Z_CAR[ subject_i ] + 1, covar_indices_subj_i )); #' note: must use rbind to make a matrix to index an array!
                     .alloc[ index_new_allocation ] <- .alloc[ index_new_allocation ] + 1; #' update allocation matrix with new assignment.
                     break;
                   } # end if 'imbalance is greater than max allowed imbalance'
                 } # end for 'covar_j'
                 #' [if we reach the last (least important) X covar and haven't assigned Z yet,]
                 if( covar_j == model$prognostic_factor_number && is.na( Z_CAR[ subject_i ] ) ){
                   Z_CAR[ subject_i ] <- rbinom( n = 1, size = 1, prob = model$allocation_ratio ); #' [ randomize to treatment and control with probability 'allocation_ratio' ]
                   index_new_allocation <- rbind(c( Z_CAR[ subject_i ] + 1, covar_indices_subj_i )); #' update allocation matrix with new assignment.
                   .alloc[ index_new_allocation ] <- .alloc[ index_new_allocation ] + 1;
                 } # end if 'no imbalance passes max imbalance' 
               } # end for 'subject_i'
               if( simulate_outcome ){
                 Y <- simulate_outcomes( model, draw, Z = Z_CAR )
                 return(list( Y = Y, Z = Z_CAR, nforced = nforced ))
               }else{
                 return(list( Z = Z_CAR, nforced = nforced ))
               }
             })
} # returns allocation sequence (Z), outcome (Y), number forced allocations (nforced)

# --------------------------------------------------------------------------- #
#' ------------------ [ Estimate regression parameters ] -------------------- #
# --------------------------------------------------------------------------- #

#' [ helper function 2: get_regression_estimates() returns (est, se, t, p) ]
compute_regression_estimates <- function( model, draw, out, adjusted ){
  #' [ from Model@draw (X,T) and Method@out (Y,Z) estimate (est, se, t, p) ]
  
  #' [1] make fitted model, adjusted or unadjusted for prognostic factors X
  fitted_model <- out$Y ~ out$Z
  if( adjusted ){
    fitted_model <- update( fitted_model, ~. + draw$X )
  }
  #' [2] fit (generalized) linear model
  if( model$outcome_type == "binary" ){
    fitted_model_ests <- summary(glm(formula = fitted_model,
                                     family = quasibinomial(link="logit")))$coef[2,];
  }else{
    fitted_model_ests <- summary(lm(formula = fitted_model))$coef[2,];
  }
  names( fitted_model_ests ) <- c("est", "se", "t", "p");
  return( fitted_model_ests ) 
}# returns (est, se, t, p)

#' [function 4: estimate_regression_parameters() returns (est, se, t, p, adjusted, rerandomized, cilower, ciupper)]
estimate_regression_parameters <- function( base_method = NULL, adjusted = NULL, return_extended_method = TRUE ){
  
  #' [ use: pass 'allocation_method' to base_method when calling fn() ]
  #' [ note: requires running base_method on simulation in order to work! ]
  #' [       this approach will save both base_method and method_extension output)]
  #' [ NOTE-2: don't bother with base_method, use base_method + extended_method ]
  
  #' [make_ci() makes a Wald-type 1-alpha% confidence interval, accepts both scalar/vector input. ]
  make_ci <- function( est, se, alpha ){
    .make_ci <- function( est, se, alpha ){ return( est + c(-1, 1) * qnorm(1 - alpha/2) * se )}
    .make_cis <- Vectorize( .make_ci, vectorize.args=c("est", "se"));
    nterms <- length( est )
    if( nterms > 1 ){
      out <- t(.make_cis( est = est, se = se, alpha = alpha ));
      colnames( out ) <- c("cilower", "ciupper");
    }else{
      out <- .make_ci( est = est, se = se, alpha = alpha );
      names( out ) <- c("cilower", "ciupper");
    }
    return( out )
  } # end make_ci()
  
  adjusted <- ifelse(is.null( adjusted ), TRUE, adjusted ); #' [default is adjusted = TRUE]
  
  if( return_extended_method & !is.null( base_method )){ #' [ if passed base_method, return ExtendedMethod object ]
    return(new_extended_method(name = paste0("REG", ifelse( adjusted, "_ADJ", "_UN" )),
                               label = paste0("estimate regression model treatment effect, ",
                                              ifelse( adjusted, "adjusted ", "unadjusted "),
                                              "for prognostic factors"),
                               base_method = base_method,
                               extended_method = function( model, draw, out, base_method ){
                                 fitted_model <- compute_regression_estimates( model = model, draw = draw, out = out, adjusted = adjusted )
                                 wald_type_confints <- make_ci( est = fitted_model["est"], se = fitted_model["se"], alpha = model$alpha );
                                 return(list( est = fitted_model["est"],
                                              se = fitted_model["se"],
                                              t = fitted_model["t"],
                                              p = fitted_model["p"],
                                              adjusted = adjusted,
                                              rerandomized = FALSE,
                                              cilower = wald_type_confints["cilower"],
                                              ciupper = wald_type_confints["ciupper"],
                                              num_rerandomizations = 0))}))
  }else{
    return(new_method_extension(name = paste0("REG", ifelse( adjusted, "_ADJ", "_UN" )),
                                label = paste0("estimate regression model treatment effect, ",
                                               ifelse( adjusted, "adjusted ", "unadjusted "),
                                               "for prognostic factors"),
                                method_extension = function( model, draw, out, base_method ){
                                  fitted_model <- compute_regression_estimates( model = model, draw = draw, out = out, adjusted = adjusted )
                                  wald_type_confints <- make_ci( est = fitted_model["est"], se = fitted_model["se"], alpha = model$alpha );
                                  return(list( est = fitted_model["est"],
                                               se = fitted_model["se"],
                                               t = fitted_model["t"],
                                               p = fitted_model["p"],
                                               adjusted = adjusted,
                                               rerandomized = FALSE,
                                               cilower = wald_type_confints["cilower"],
                                               ciupper = wald_type_confints["ciupper"],
                                               num_rerandomizations = 0))}))
  }
}

#' [function 5: rerandomization_error_estimates_method_extension() 
rerandomized_error_estimates <- function( base_method = NULL, adjusted = NULL, num_rerandomizations = NULL, return_extended_method = TRUE ){
  #' [ use: add directly to an allocation method returning allocs (Z) and outcomes (Y) ]
  #' [ example: run_method('allocation_method' + 'rerand_err_ests') ]
  #' [ returns: (est, se_rerand, t, p_rerand, adjusted, rerandomized = TRUE, cilower_rerand, ciupper_rerand)]

  #' [make_ci() makes a Wald-type 1-alpha% confidence interval, accepts both scalar/vector input. ]
  make_ci <- function( est, se, alpha ){
    .make_ci <- function( est, se, alpha ){ return( est + c(-1, 1) * qnorm(1 - alpha/2) * se )}
    .make_cis <- Vectorize( .make_ci, vectorize.args=c("est", "se"));
    nterms <- length( est )
    if( nterms > 1 ){
      out <- t(.make_cis( est = est, se = se, alpha = alpha ));
      colnames( out ) <- c("cilower", "ciupper");
    }else{
      out <- .make_ci( est = est, se = se, alpha = alpha );
      names( out ) <- c("cilower", "ciupper");
    }
    return( out )
  } # end make_ci()
  
  #' [ set defaults if not specified in the function call or simulation model. ]
  if(is.null( num_rerandomizations )) num_rerandomizations = 500; 
  if(is.null( adjusted )) adjusted = TRUE; 
  
  if( return_extended_method & !is.null( base_method )){
    return(new_extended_method(name = paste0("RERAND", ifelse( adjusted, "_ADJ", "_UN" )),
                         label = paste0("estimate treatment effect, ",
                                        ifelse( adjusted, "adjusted ", "unadjusted "),
                                        "for prognostic factors, rerandomized"),
                         base_method = base_method,
                         extended_method = function( model, draw, out, base_method ){
                           #' [ step 1: estimate regression parameters from observed data (est, se, t, p)]
                           model_ests_observed <- compute_regression_estimates( model = model, draw = draw, out = out, adjusted = adjusted )
                           
                           #' [ step 2: estimate regression parameters from #('num_rerandomizations') re-randomized allocations  ]
                           model_ests_rerand <- 
                             t(sapply( 1:num_rerandomizations, function( rerand_i ){
                             #' [ copy output and replace observed allocs (Z) with rerandomized allocs ]
                             out_rerand_j <- out; 
                             out_rerand_j$Z <- base_method@method( model = model, draw = draw, simulate_outcome = FALSE )$Z
                             return( compute_regression_estimates( model = model, draw = draw, out = out_rerand_j, adjusted = adjusted ))}))
                           
                           #' [ step 3: compute significance measures from rerandomized output ]
                           se_rerand <- sd( model_ests_rerand[,"est"], na.rm = TRUE)
                           #' [ TODO: (p-value Q): how many re-randomized allocations have a more extreme outcome than what was observed? ]
                           p_rerand <- mean(abs( model_ests_rerand[,"t"] ) >= abs( model_ests_observed["t"] )) #' [ by (a) |t_rerand| >= |t_obs| vs. (b) p_rerand <  p_obs ]
                           
                           ci_rerand <- quantile( model_ests_rerand[,"est"], probs = c( model$alpha/2, 1 - model$alpha/2 ))
                           return(list( est = model_ests_observed["est"],
                                        se = se_rerand,
                                        t = model_ests_observed["t"],
                                        p = p_rerand,
                                        adjusted = adjusted,
                                        rerandomized = TRUE,
                                        cilower = ci_rerand[1],
                                        ciupper = ci_rerand[2],
                                        num_rerandomizations = num_rerandomizations ))}))
  }else{
    return(new_method_extension(name = paste0("RERAND", ifelse( adjusted, "_ADJ", "_UN" )),
                         label = paste0("estimate regression model treatment effect, ",
                                        ifelse( adjusted, "adjusted ", "unadjusted "),
                                        "for prognostic factors, RERANDOMIZED error estimates"),
                         method_extension = function( model, draw, out, base_method ){
                           memoization.hash.ests.by.alloc <- new.env( hash = TRUE ); #' hash table for storing computations
                           #' [ step 1: estimate regression parameters from observed data (est, se, t, p)]
                           model_ests_observed <- compute_regression_estimates( model = model, draw = draw, out = out, adjusted = adjusted )
                           model_ests_rerand <- matrix(NA, nrow = num_rerandomizations, ncol = 4, dimnames = list(NULL, c("est", "se", "t", "p")))
                           model_ests_rerand[ 1, ] <- model_ests_observed; #' always include observed data in rerandomized set (guarantees p > 0)
                           key_obs <- paste( out$Z, collapse = ":" );
                           assign( key_obs, model_ests_observed, envir = memoization.hash.ests.by.alloc ); # assign observed ests first key:value pair
                           
                           #' [ step 2: estimate regression parameters from #('num_rerandomizations') re-randomized allocations  ]
                           out_rerand_j <- out; #' copy output object (for calling base_method with rerandomized allocs)
                           for( rerand_j in 2:num_rerandomizations ){
                             out_rerand_j$Z <- base_method@method( model = model, draw = draw, simulate_outcome = FALSE )$Z
                             key_j <- paste( out_rerand_j$Z, collapse = ":" );
                             if(!exists( key_j, envir = memoization.hash.ests.by.alloc )){
                               model_ests_rerand[ rerand_j, ] <- compute_regression_estimates( model = model, draw = draw, out = out_rerand_j, adjusted = adjusted );
                               assign( key_j, model_ests_rerand[ rerand_j, ], envir = memoization.hash.ests.by.alloc );
                             }else{
                               model_ests_rerand[ rerand_j, ] <- get( key_j, envir = memoization.hash.ests.by.alloc );
                             }
                           }
                           
                           #' [ step 3: compute significance measures from rerandomized output ]
                           se_rerand <- sd( model_ests_rerand[,"est"], na.rm = TRUE)
                           #' [ TODO: (p-value Q): how many re-randomized allocations have a more extreme outcome than what was observed? ]
                           p_rerand <- mean(abs( model_ests_rerand[,"t"] ) >= abs( model_ests_observed["t"] )) #' [ by (a) |t_rerand| >= |t_obs| vs. (b) p_rerand <  p_obs ]
                           ci_rerand <- quantile( model_ests_rerand[,"est"], probs = c( model$alpha/2, 1 - model$alpha/2 ))
                           return(list( est = model_ests_observed["est"],
                                        se = se_rerand,
                                        t = model_ests_observed["t"],
                                        p = p_rerand,
                                        adjusted = adjusted,
                                        rerandomized = TRUE,
                                        cilower = ci_rerand[1],
                                        ciupper = ci_rerand[2],
                                        num_rerandomizations = num_rerandomizations ))}))
  }
}