#' Author: Michael Flanagin
#' Date started: 29 August 2018 15:09
#' Date last modified: 29 August 2018 15:09
#' objective: Functions for creating LaTeX tables (via kable())
#' [ 1: kable_booktabs_linesep() ]
#' [ 2: assert_that() ]
#' [ 3: get_nsim_col() ]
#' [ 4: replace_large_value() & replace_values() ]
#' [ 5: make_summary_table() ] 
#' [ 6: make_summary_tables_adj_and_unadj_sorted() ] 
#' [ 7: batchX_make_summary_tables(), X = 1, 2, 3, 4 ]
#' [ 8: batchY_make_subset_tables(), Y = 1, 2 ]

# function kable_booktabs_linesep()
kable_booktabs_linesep <- function( nlines = 6 ){
  #' input: nlines := number of lines per block of table rows
  #' output: [character] linespace string to pass to kable()
  #' example: kable( foo_table, booktabs = TRUE, linesep = kable_booktabs_linesep( nlines = 6) )
  return(c(rep( '', nlines - 1 ), "\\addlinespace"))
}

#' [ assert_that( expr, errmsg ) returns `errmsg` if expr is NOT true ]
assert_that <- function( expr, errmsg ){
  if( !all( expr ) )
    stop( errmsg, call. = FALSE )
}

#' [ get_nsim_col() computes mean number of simulations and returns the proportion of included sims ]
#' [ if `include_percentage` is TRUE. ]
get_nsim_col <- function( tbl_nsim, include_percentage = TRUE ){
  method_cols <- grep( "un|adj$", names( tbl_nsim ), value = FALSE ) #' [ find all columns with metrics ]
  tbl_nsim_mean <- apply( tbl_nsim[, method_cols ], 1, function(.row){ round(mean( .row, na.rm = TRUE))} )
  tbl_proportion_sims <- paste0(" (",round( tbl_nsim_mean / 5010 * 100, 1 ), "%)")
  if( include_percentage ){
    return(mapply( paste0, tbl_nsim_mean, tbl_proportion_sims, MoreArgs = list(collapse = "") ))
  }else{
    return( tbl_nsim_mean )
  }
}

#' [ replace_large_values( value, threshold) replaces large values with strings: ]
#' [ returns ">/<`threshold` if value is large, `value` otherwise. ]
replace_large_value <- function( value, threshold = 1000 ){
  if( is.na( value ) ){
    return( "NA" )
  }else if( value > threshold ){
    return( paste0(">", threshold) )
  }else if( value < - threshold ){
    return( paste0("<", threshold) )
  }else{
    return( value )
  }
}
#' [ replace_values() is a vectorized version of replace_large_value() ]
replace_values <- Vectorize( replace_large_value )

#' make_summary_table()
make_summary_table <- function( metric_df,
                                metric = "coverage",
                                get_adj_ests = TRUE,
                                round_digits = 3,
                                parameter_names = c( "modelno","trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size", 
                                                     "outcome_marginal_prevalence", "prognostic_factor_prevalence" ),
                                table_colnames = c("CR", "SBR", "CAA_model", "CAA_rerand","modelno", "n", "bZ", "bX", "Pr( Y )", "Pr( X )"),
                                return_row_indices = FALSE,
                                verbose = FALSE )
{
  #' [1] Get row indices corresponding to alloc method, rerandomization type, and adjustment method
  row_indices <- list();
  row_indices[["CR"]] <- with( metric_df, which( alloc_method == "CR" & rerandomized == 0 & adjusted == get_adj_ests ));
  row_indices[["SBR"]] <- with( metric_df, which( alloc_method == "SBR" & rerandomized == 0 & adjusted == get_adj_ests ));
  row_indices[["CAA_model"]] <- with( metric_df, which( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 0 & adjusted == get_adj_ests ));
  row_indices[["CAA_rerand"]] <- with( metric_df, which( alloc_method == "CAA-MI-2-PBA-0.70" & rerandomized == 1 & adjusted == get_adj_ests ));
  
  #' [2] By (alloc_method, rerandomized, adjusted), view how many rows were selected.
  row_lengths <- sapply( row_indices, function( .indices ){ length( .indices ) })
  # Given variable row lengths, throw errors (or pad table)
  row_lengths_table <- table( row_lengths )
  if(length( row_lengths_table ) > 1 & verbose ){
    cat("WARNING: method row lengths are not identical! \n Methods by row: \n")
    print( row_lengths )
  }
  
  #' [3] Get statistics, round to 'round_digits'. Also get NA values.
  metrics_by_method <- lapply( row_indices, function( .indices ){
    if( length( .indices ) == 0 ){ # if no rows selected, return all NAs
      return(data.frame(metric = rep( NA, times = max( row_lengths )), modelno = metric_df$modelno[ row_indices[[which.max( row_lengths )]] ])) 
    }else{
      # if metric == 'power', select 'power.rerand' or 'power.pvalue' 
      #   depending on if rerandomized == TRUE or FALSE, respectively.
      if( metric == "power" ){ 
        if(any( metric_df[ .indices, "rerandomized" ] == 1 )){
          return(round( metric_df[ .indices, c("power.rerand", "modelno") ], round_digits ))
        }else{
          return(round( metric_df[ .indices, c("power.pvalue", "modelno") ], round_digits ))
        }
      }else{
        return(round( metric_df[ .indices, c(metric, "modelno") ], round_digits ))
      }
    }})
  mergeall <- function( x, y, by = "modelno", all = TRUE ){
    merge( x, y, by = by, all = all )
  }
  
  suppressWarnings( metrics_table <- Reduce( mergeall, metrics_by_method ) )
  names( metrics_table )[2:5] <- names( row_indices )
  #' [4] Get parameters corresponding to each model. NOTE: using max row lengths
  model_params_table <- metric_df[ row_indices[[which.max( row_lengths )]], parameter_names ];
  
  #' [5] Piece all method components together and (2) rename column names.
  output_table <- cbind( metrics_table[,-1], model_params_table ); 
  dimnames( output_table )[[2]] <- table_colnames;
  
  #' [6] For diagnostics: return row indices?
  if( !return_row_indices ){
    return( summary = output_table )
  }else{
    return(list( summary = output_table,
                 row_indices = row_indices ))
  }
}

#' [ make_summary_tables_adj_and_unadj_sorted() ]
make_summary_tables_adj_and_unadj_sorted <- 
  function( metric_df,
            metric,
            round_digits,
            parameter_names,
            table_colnames = c("CR", "SBR", "CAA_model", "CAA_rerand", "n", "bZ", "bX", "Pr( Y )", "Pr( X )"),
            sort_by_vars = c("n", "Pr( Y )", "Pr( X )", "bZ", "bX") )
  {
    #' use: [1] makes summary tables (with adjusted and unadjusted estimates side by side)
    #'      [2] sorts by sort_by_vars.
    #' [0] Assertions
    assert_that( sort_by_vars %in% table_colnames, 
                 errmsg = "All variables in `sort_by_vars` must be in `table_colnames`." )
    assert_that( parameter_names %in% names( metric_df ), 
                 errmsg = "All variables in `parameter_names` must be in data.frame `metric_df`." )
    
    #' [1] Get adjusted and unadjusted summary tables
    outdf_adj <- make_summary_table( metric_df = metric_df, metric = metric, get_adj_ests = TRUE, round_digits = round_digits,
                                     parameter_names = parameter_names, table_colnames = table_colnames )
    outdf_un <- make_summary_table( metric_df = metric_df, metric = metric, get_adj_ests = FALSE, round_digits = round_digits,
                                    parameter_names = parameter_names, table_colnames = table_colnames )
    
    #' [2] Sort rows by variables: 'sort_by_vars'
    col_indices_sort_variables <- sapply( sort_by_vars, function( .varname ){ which( table_colnames == .varname ) })
    row_indices_sorted <- do.call( order, outdf_adj[, col_indices_sort_variables ] )
    
    #' [3] Make table of [{model parameters, 'adjusted':'unadjusted' ests by randomization type.}]
    outdf_all <- cbind( outdf_adj[, col_indices_sort_variables ],
                        CR_adj = outdf_adj$CR, CR_un = outdf_un$CR,
                        SBR_adj = outdf_adj$SBR, SBR_un = outdf_un$SBR,
                        CAA_model_adj = outdf_adj$CAA_model, CAA_model_un = outdf_un$CAA_model,
                        CAA_rerand_adj = outdf_adj$CAA_rerand, CAA_rerand_un = outdf_un$CAA_rerand )
    #' [3b] add `modelno` to RHS if included in `table_colnames`.
    if( "modelno" %in% table_colnames ){
      outdf_all <- cbind( modelno = outdf_adj$modelno, outdf_all )
    }
    
    #' [4] Exclude certain parameters. 
    col_names_to_exclude <- c("CAA_rerand_un") # c("modelno", "CAA_rerand_un") 
    col_indices_to_exclude <- which( dimnames( outdf_all)[[2]] %in% col_names_to_exclude )
    
    #' [5] Return estimates in sorted order, excluding variables in `col_indices_to_exclude`.
    return( outdf_all[ row_indices_sorted, - col_indices_to_exclude ] )
  }

#' [ Define wrapper functions with defaults for 'parameter_names', 'table_colnames', and 'sort_by_vars' ]
batch1_make_summary_tables <- function( metric_df, metric, include_modelno = FALSE, sort_by_vars = c("n", "Pr( Y )", "Pr( X )", "bZ", "bX")){
  if( include_modelno ){
    .parameter_names <- c( "modelno","trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size", 
                           "outcome_marginal_prevalence", "prognostic_factor_prevalence" );
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand","modelno", "n", "bZ", "bX", "Pr( Y )", "Pr( X )");
  }else{
    .parameter_names <- c( "trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size", 
                           "outcome_marginal_prevalence", "prognostic_factor_prevalence" );
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand", "n", "bZ", "bX", "Pr( Y )", "Pr( X )");
  }
  return( make_summary_tables_adj_and_unadj_sorted(
    metric_df = metric_df,
    metric = metric,
    round_digits = 3,
    parameter_names =  .parameter_names,
    table_colnames = .col_names,
    sort_by_vars = sort_by_vars))
}

batch2_make_summary_tables <- function( metric_df, metric, include_modelno = FALSE, sort_by_vars = c("n", "Pr( Y )", "bZ", "bX")){
  if( include_modelno ){
    .parameter_names <- c( "modelno","trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size", 
                           "outcome_marginal_prevalence" )
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand","modelno", "n", "bZ", "bX", "Pr( Y )")
  }else{
    .parameter_names <- c( "trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size", 
                           "outcome_marginal_prevalence" )
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand", "n", "bZ", "bX", "Pr( Y )")
  }
  return( make_summary_tables_adj_and_unadj_sorted(
    metric_df = metric_df,
    metric = metric,
    round_digits = 3,
    parameter_names =  .parameter_names,
    table_colnames = .col_names,
    sort_by_vars = sort_by_vars))
}

batch3_make_summary_tables <- function( metric_df, metric, include_modelno = FALSE, sort_by_vars = c("n", "Pr( X )", "bZ", "bX")){
  if( include_modelno ){
    .parameter_names <- c( "modelno","trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size",
                           "prognostic_factor_prevalence" )
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand","modelno", "n", "bZ", "bX", "Pr( X )")
  }else{
    .parameter_names <- c( "trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size",
                           "prognostic_factor_prevalence" )
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand", "n", "bZ", "bX", "Pr( X )")
  }
  return( make_summary_tables_adj_and_unadj_sorted(
    metric_df = metric_df,
    metric = metric,
    round_digits = 3,
    parameter_names =  .parameter_names,
    table_colnames = .col_names,
    sort_by_vars = sort_by_vars))
}

batch4_make_summary_tables <- function( metric_df, metric, include_modelno = FALSE, sort_by_vars = c("n", "bZ", "bX")){
  if( include_modelno ){
    .parameter_names <- c( "modelno","trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size" );
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand","modelno", "n", "bZ", "bX")
  }else{
    .parameter_names <- c( "trial_size", "treatment_assignment_effect_size", "prognostic_factor_effect_size" );
    .col_names = c("CR", "SBR", "CAA_model", "CAA_rerand", "n", "bZ", "bX")
  }
  return( make_summary_tables_adj_and_unadj_sorted(
    metric_df = metric_df,
    metric = metric,
    round_digits = 3,
    parameter_names =  .parameter_names,
    table_colnames = .col_names,
    sort_by_vars = sort_by_vars))
}

batch1_subset_tables <- function( metric_df, metric ){
  #' [ 1. make summary tables ]
  batch1_tbl <- batch1_make_summary_tables( metric_df = metric_df, metric = metric)
  #' [ 2. compute average `nsims` and modify column names to include it. ]
  nsims_batch1_subset <- batch1_make_summary_tables( metric_df = metric_df, metric = "nsim")
  batch1_nsim_col <- get_nsim_col( tbl_nsim = nsims_batch1_subset )
  return(cbind.data.frame( avg_nsim = batch1_nsim_col, batch1_tbl ))
}

batch2_subset_tables <- function( metric_df, metric ){
  #' [ 1. make summary tables ]
  batch2_tbl <- batch2_make_summary_tables( metric_df = metric_df, metric = metric)
  #' [ 2. compute average `nsims` and modify column names to include it. ]
  nsims_batch2_subset <- batch1_make_summary_tables( metric_df = metric_df, metric = "nsim")
  batch2_nsim_col <- get_nsim_col( tbl_nsim = nsims_batch2_subset )
  return(cbind.data.frame( batch2_nsim_col, batch2_tbl ))
}


make_kable <- function( result_tbl, tbl_caption, kable_na_char = ' -- ', font_size = 7, model_params ){
  require('kableExtra')
  .adjustment.labels = c("adj", "unadj", "adj", "unadj", "adj", "unadj", "adj");
  ncol_pads <- length( model_params )  #' `ncol_pads` are #(cols) before we print method results.  
  .col_names <- c( model_params, .adjustment.labels )
  .align = c(rep("c", ncol_pads ), rep("r", 6), "c"); #' `.align` dictates LaTeX table alignment (`r`, `c`, `l`)
  .latex_options = c("striped", "bordered", "hold_position")
  .header.randomization.method = c(" " = ncol_pads, "CR" = 2, "SBR" = 2, "CAA" = 2, "CAA" = 1);
  .header.analysis.method = c(" " = ncol_pads, "Model-based" = 6, "Rerandomization" = 1);
  .linesep = kable_booktabs_linesep( nlines = 6 )
  
  options(knitr.kable.NA = kable_na_char) #' `kable_na_char` determines kable()'s NA behavior
  
  result_tbl %>% kable( caption = tbl_caption, row.names = FALSE, longtable = TRUE,
                        col.names = .col_names, align = .align, booktabs = TRUE, linesep = .linesep) %>%
    kable_styling( font_size = font_size, latex_options = .latex_options, full_width = FALSE ) %>%
    add_header_above( .header.randomization.method ) %>%
    add_header_above( .header.analysis.method )
}


