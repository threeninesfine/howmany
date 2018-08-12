# ------------------------- [ Batch 2 of 4 ] -------------------------------- #
# ------------------ [ binary Y, continuous X ] ----------------------------- #
# --------------------------------------------------------------------------- #
metrics_bYcX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-2-of-4.csv" )
# --------------------------------------------------------------------------- #
# --------------------------- [ COVERAGE ] ---------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl2.1.cov.adj <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl2.2.cov.adj <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl2.3.cov.adj <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl2.4.cov.adj <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl2.1.cov.un <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                         subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl2.2.cov.un <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                         subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl2.3.cov.un <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl2.4.cov.un <- subset( metrics_bYcX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 2a: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl2.coverage.adj <- cbind( CR = tbl2.1.cov.adj[,"coverage"], 
                            SBR = tbl2.2.cov.adj[,"coverage"], 
                            CAA_model = tbl2.3.cov.adj[,"coverage"], 
                            CAA_rerand = tbl2.4.cov.adj[,"coverage"], tbl2.3.cov.adj[,-1] )

#' Table 2b: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl2.coverage.un <- cbind( CR = tbl2.1.cov.un[,"coverage"], 
                           SBR = tbl2.2.cov.un[,"coverage"], 
                           CAA_model = tbl2.3.cov.un[,"coverage"], 
                           #                            CAA_rerand = tbl2.4.cov.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                           tbl2.3.cov.un[,-1] )

# --------------------------------------------------------------------------- #
# ---------------------------- [ BIAS ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl2.1.bias.adj <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl2.2.bias.adj <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl2.3.bias.adj <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl2.4.bias.adj <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl2.1.bias.un <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl2.2.bias.un <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl2.3.bias.un <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl2.4.bias.un <- subset( metrics_bYcX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 2c: Bias (adjusted tx effect). setting = (continuous Y, continuous X)
tbl2.bias.adj <- cbind( CR = tbl2.1.bias.adj[,"bias"], 
                        SBR = tbl2.2.bias.adj[,"bias"], 
                        CAA_model = tbl2.3.bias.adj[,"bias"], 
                        CAA_rerand = tbl2.4.bias.adj[,"bias"], tbl2.3.bias.adj[,-1] )

#' Table 2d: Bias (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl2.bias.un <- cbind( CR = tbl2.1.bias.un[,"bias"], 
                       SBR = tbl2.2.bias.un[,"bias"], 
                       CAA_model = tbl2.3.bias.un[,"bias"], 
                       #                            CAA_rerand = tbl2.4.bias.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                       tbl2.3.bias.un[,-1] )

# --------------------------------------------------------------------------- #
# --------------------------- [ POWER ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl2.1.power.adj <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                            subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl2.2.power.adj <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                            subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl2.3.power.adj <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl2.4.power.adj <- subset( metrics_bYcX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl2.1.power.un <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl2.2.power.un <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl2.3.power.un <- subset( metrics_bYcX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl2.4.power.un <- subset( metrics_bYcX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 2c: Power (adjusted tx effect). setting = (binary Y, continuous X)
tbl2.power.adj <- cbind( CR = tbl2.1.power.adj[,"power.pvalue"], 
                         SBR = tbl2.2.power.adj[,"power.pvalue"], 
                         CAA_model = tbl2.3.power.adj[,"power.pvalue"], 
                         CAA_rerand = tbl2.4.power.adj[,"power.rerand"], tbl2.3.power.adj[,-1] )

#' Table 2d: Power (unadjusted tx effect). setting = (binary Y, continuous X)
tbl2.power.un <- cbind( CR = tbl2.1.power.un[,"power.pvalue"], 
                        SBR = tbl2.2.power.un[,"power.pvalue"], 
                        CAA_model = tbl2.3.power.un[,"power.pvalue"], 
                        CAA_rerand = tbl2.4.power.un[,"power.rerand"], #' TODO(michael): run unadjusted, rerandomized simulations!
                        tbl2.3.power.un[,-1] )



