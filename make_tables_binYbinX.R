# ------------------------- [ Batch 1 of 4 ] -------------------------------- #
# ---------------------- [ binary Y, binary X ] ----------------------------- #
# --------------------------------------------------------------------------- #
metrics_bYbX <- read.csv( "/Users/Moschops/Documents/MSThesis/results/metrics-alloc-simulation-batch-1-of-4.csv" )
# --------------------------------------------------------------------------- #
# --------------------------- [ COVERAGE ] ---------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl1.1.cov.adj <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence),
                          subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl1.2.cov.adj <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl1.3.cov.adj <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl1.4.cov.adj <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl1.1.cov.un <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl1.2.cov.un <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl1.3.cov.un <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl1.4.cov.un <- subset( metrics_bYbX, select = c( coverage, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                         subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 1a: Coverage (adjusted tx effect). setting = (continuous Y, continuous X)
tbl1.coverage.adj <- cbind( CR = tbl1.1.cov.adj[,"coverage"], 
                            SBR = tbl1.2.cov.adj[,"coverage"], 
                            CAA_model = tbl1.3.cov.adj[,"coverage"], 
                            CAA_rerand = tbl1.4.cov.adj[,"coverage"], tbl1.3.cov.adj[,-1] )

ordd <- with( tbl1.coverage.adj, order( outcome_marginal_prevalence, 
                                        prognostic_factor_prevalence, 
                                        treatment_assignment_effect_size, 
                                        prognostic_factor_effect_size))
tbl1.coverage.adj.ord <- tbl1.coverage.adj[ ordd, ]
tbl1.coverage.adj.ord
look <- tbl1.coverage.adj.ord$trial_size == 32
tbl1.coverage.adj.ord[ look, ]
#' NOTES:
#' 1) inflated coverage for outcome_marginal_prevalence == 0.10
#' > likely due to overinflated SEs
#' 2) for outcome_marginal_prevalence == 0.50,
#' > coverage of CAA_rerand 100% (implies less power)

#' Table 1b: Coverage (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl1.coverage.un <- cbind( CR = tbl1.1.cov.un[,"coverage"], 
                           SBR = tbl1.2.cov.un[,"coverage"], 
                           CAA_model = tbl1.3.cov.un[,"coverage"], 
                           #                            CAA_rerand = tbl1.4.cov.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                           tbl1.3.cov.un[,-1] )

ordd <- with( tbl1.coverage.un, order( outcome_marginal_prevalence, 
                                        prognostic_factor_prevalence, 
                                        treatment_assignment_effect_size, 
                                        prognostic_factor_effect_size))
tbl1.coverage.un.ord <- tbl1.coverage.un[ ordd, ]
tbl1.coverage.un.ord
look <- tbl1.coverage.un.ord$trial_size == 32
tbl1.coverage.un.ord[ look, ]


# --------------------------------------------------------------------------- #
# ---------------------------- [ BIAS ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl1.1.bias.adj <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl1.2.bias.adj <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl1.3.bias.adj <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl1.4.bias.adj <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl1.1.bias.un <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl1.2.bias.un <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl1.3.bias.un <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl1.4.bias.un <- subset( metrics_bYbX, select = c( bias, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                          subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 1c: Bias (adjusted tx effect). setting = (continuous Y, continuous X)
tbl1.bias.adj <- cbind( CR = tbl1.1.bias.adj[,"bias"], 
                        SBR = tbl1.2.bias.adj[,"bias"], 
                        CAA_model = tbl1.3.bias.adj[,"bias"], 
                        CAA_rerand = tbl1.4.bias.adj[,"bias"], tbl1.3.bias.adj[,-1] )

ordd <- with( tbl1.bias.adj, order( outcome_marginal_prevalence, 
                                    prognostic_factor_prevalence,
                                    prognostic_factor_effect_size,
                                    treatment_assignment_effect_size))
tbl1.bias.adj.ord <- tbl1.bias.adj[ ordd, ]
tbl1.bias.adj.ord
tr <- tbl1.bias.adj.ord$trial_size == 32
look <- tbl1.bias.adj.ord[ tr, ]
look[13:24,]

#' Table 1d: Bias (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl1.bias.un <- cbind( CR = tbl1.1.bias.un[,"bias"], 
                       SBR = tbl1.2.bias.un[,"bias"], 
                       CAA_model = tbl1.3.bias.un[,"bias"], 
                       #                            CAA_rerand = tbl1.4.bias.un[,"coverage"], #' TODO(michael): run unadjusted, rerandomized simulations!
                       tbl1.3.bias.un[,-1] )

# --------------------------------------------------------------------------- #
# --------------------------- [ POWER ] ------------------------------------- #
# --------------------------------------------------------------------------- #
#' Adjusted analysis
tbl1.1.power.adj <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CR" & adjusted == 1 ) )
tbl1.2.power.adj <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "SBR" & adjusted == 1 ) )
tbl1.3.power.adj <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 1 ) )
tbl1.4.power.adj <- subset( metrics_bYbX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                            subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 1 ) )
#' Unadjusted analysis
tbl1.1.power.un <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CR" & adjusted == 0 ) )
tbl1.2.power.un <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "SBR" & adjusted == 0 ) )
tbl1.3.power.un <- subset( metrics_bYbX, select = c( power.pvalue, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == 0 ) )
tbl1.4.power.un <- subset( metrics_bYbX, select = c( power.rerand, trial_size, treatment_assignment_effect_size, prognostic_factor_effect_size, outcome_marginal_prevalence, prognostic_factor_prevalence ),
                           subset = ( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == 0 ) )

#' Table 1e: Power (adjusted tx effect). setting = (continuous Y, continuous X)
tbl1.power.adj <- cbind( CR = tbl1.1.power.adj[,"power.pvalue"], 
                         SBR = tbl1.2.power.adj[,"power.pvalue"], 
                         CAA_model = tbl1.3.power.adj[,"power.pvalue"], 
                         CAA_rerand = tbl1.4.power.adj[,"power.rerand"], tbl1.3.power.adj[,-1] )

#' Table 1f: Power (unadjusted tx effect). setting = (continuous Y, continuous X)
tbl1.power.un <- cbind( CR = tbl1.1.power.un[,"power.pvalue"], 
                        SBR = tbl1.2.power.un[,"power.pvalue"], 
                        CAA_model = tbl1.3.power.un[,"power.pvalue"], 
#                         CAA_rerand = tbl1.4.power.un[,"power.rerand"], #' TODO(michael): run unadjusted, rerandomized simulations!
                        tbl1.3.power.un[,-1] )



#' Notes from 10 July meeting:
#' 1) Have adjusted/unadjusted for each method (i.e. CR_adj, CR_un, SBR_adj, SBR_un, CAA_model_adj, CAA_model_un, etc)
#' 2) ordering is good, specifically ordering by:
#' >> outcome_marginal_prevalence, 
#' >> prognostic_factor_prevalence,
#' >> prognostic_factor_effect_size,
#' >> treatment_assignment_effect_size
#' 3) Bias is troubling (how it seems to be proportional to bZ)
#' >> investigate if this is due to 'segt1k'
#' 4) make same tables, excluding the SE greater than 1000 etc
#' >> include a row with #(simulations out of e.g. 5010 included)
#' 5) Make Markdown reports (knitR or markdown?)
#' >> History of .pdf reports
#' >> keeps track of (your work, history of results development, datasets you used etc.)
#' >> make this easily digestible (don't have to type code, can see it if you want to, can also visualize it easier).
#' 6) Histograms of \betahat by \segt1k for \allocationmethod \adjusted | \trialsize \bZ == 3.
#' 7) with( metrics_bYbX, by( segt1k, outcome_marginal_prevalence, summary ))