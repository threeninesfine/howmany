# ------------------------- [ Batch 3 of 4 ] -------------------------------- #
# ------------------ [ Continuous Y, binary X ] ----------------------------- #
# --------------------------------------------------------------------------- #
metrics_cYbX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-3-of-4.csv" )
# --------------------------------------------------------------------------- #
# --------------------------- [ COVERAGE ] ---------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl3.1.cov.adj <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl3.2.cov.adj <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl3.3.cov.adj <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl3.4.cov.adj <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl3.1.cov.un <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl3.2.cov.un <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl3.3.cov.un <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl3.4.cov.un <- subset( metrics_cYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4a: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl3.coverage.adj <- cbind( CR = tbl3.1.cov.adj[,"coverage"], 
                            SBR = tbl3.2.cov.adj[,"coverage"], 
                            CAA_model = tbl3.3.cov.adj[,"coverage"], 
                            CAA_rerand = tbl3.4.cov.adj[,"coverage"], tbl3.3.cov.adj[,-1] )

#' Table 4b: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl3.coverage.un <- cbind( CR = tbl3.1.cov.un[,"coverage"], 
                           SBR = tbl3.2.cov.un[,"coverage"], 
                           CAA_model = tbl3.3.cov.un[,"coverage"], 
                           #                            CAA_rerand = tbl3.4.cov.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                           tbl3.3.cov.un[,-1] )

# --------------------------------------------------------------------------- #
# ---------------------------- [ BIAS ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl3.1.bias.adj <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl3.2.bias.adj <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl3.3.bias.adj <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl3.4.bias.adj <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl3.1.bias.un <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl3.2.bias.un <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl3.3.bias.un <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl3.4.bias.un <- subset( metrics_cYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4c: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl3.bias.adj <- cbind( CR = tbl3.1.bias.adj[,"bias"], 
                        SBR = tbl3.2.bias.adj[,"bias"], 
                        CAA_model = tbl3.3.bias.adj[,"bias"], 
                        CAA_rerand = tbl3.4.bias.adj[,"bias"], tbl3.3.bias.adj[,-1] )

#' Table 4d: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl3.bias.un <- cbind( CR = tbl3.1.bias.un[,"bias"], 
                       SBR = tbl3.2.bias.un[,"bias"], 
                       CAA_model = tbl3.3.bias.un[,"bias"], 
                       #                            CAA_rerand = tbl3.4.bias.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                       tbl3.3.bias.un[,-1] )

# --------------------------------------------------------------------------- #
# --------------------------- [ POWER ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl3.1.power.adj <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl3.2.power.adj <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl3.3.power.adj <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl3.4.power.adj <- subset( metrics_cYbX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl3.1.power.un <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl3.2.power.un <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl3.3.power.un <- subset( metrics_cYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl3.4.power.un <- subset( metrics_cYbX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, entry_time_effect_size, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 4c: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl3.power.adj <- cbind( CR = tbl3.1.power.adj[,"power.pvalue"], 
                         SBR = tbl3.2.power.adj[,"power.pvalue"], 
                         CAA_model = tbl3.3.power.adj[,"power.pvalue"], 
                         CAA_rerand = tbl3.4.power.adj[,"power.rerand"], tbl3.3.power.adj[,-1] )

#' Table 4d: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl3.power.un <- cbind( CR = tbl3.1.power.un[,"power.pvalue"], 
                        SBR = tbl3.2.power.un[,"power.pvalue"], 
                        CAA_model = tbl3.3.power.un[,"power.pvalue"], 
                        CAA_rerand = tbl3.4.power.un[,"power.rerand"], #' TODO(michael): run unadjusted, rerandomized simulations!
                        tbl3.3.power.un[,-1] )



