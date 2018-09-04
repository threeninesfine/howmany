# Load libraries necessary for functions used in this document
library('knitr')
library('xtable')
library('kableExtra')  # for additional table formatting


source("results_tables_functions.R")

round_digits <- 3  # digits to round to when presenting results
options( knitr.table.format = "latex")  # guarantees 'kableExtra' formatting handled properly
options( scipen = 999 )
setwd(getwd())  # set working directory to current file path.
#' [ `datadir_path` contains current results, `tmpdir_path` is for (test) results. ]
datadir_path <- "/Users/Moschops/Documents/MSThesis/results/"
tmpdir_path <- "/Users/Moschops/Documents/MSThesis/results/output_2018-08-28_15-07/"

#' -------------------------------------------------------------------------- #
#'                      [ Load datasets into batches ] 
#' -------------------------------------------------------------------------- #
#' [ (1) binary Y, binary X; (2) binary Y, continuous X ]
#' [ (3) continuous Y, binary X; (4) continuous Y, continuous X ] 
metrics_batch1 <- read.csv(paste0( datadir_path, "metrics-alloc-simulation-batch-1-of-4.csv" ))
metrics_batch2 <- read.csv(paste0( datadir_path, "metrics-alloc-simulation-batch-2-of-4.csv" ))
# metrics_batch3 <- read.csv(paste0( datadir_path, "metrics-alloc-simulation-batch-3-of-4.csv" ))
metrics_batch3 <- read.csv(paste0( tmpdir_path, "metrics-alloc-simulation-batch-3-of-4.csv" ))
metrics_batch4 <- read.csv(paste0( datadir_path, "metrics-alloc-simulation-batch-4-of-4.csv" ))

#' [ Binary outcome batches, subsetted: ]
metrics_batch1_sub <- read.csv(paste0( datadir_path, "metrics-subset-alloc-simulation-batch-1-of-4.csv" ))
metrics_batch2_sub <- read.csv(paste0( datadir_path, "metrics-subset-alloc-simulation-batch-2-of-4.csv" ))


#' -------------------------------------------------------------------------- #
#'                      [ Make summary tables ] 
#' -------------------------------------------------------------------------- #
#' [ Batch 1: Binary Y, binary X ]
b1_coverage <- batch1_make_summary_tables( metric_df = metrics_batch1, metric = "coverage")
b1_medbias <- batch1_make_summary_tables( metric_df = metrics_batch1, metric = "median_bias")
b1_power <- batch1_make_summary_tables( metric_df = metrics_batch1, metric = "power")
b1_bias <- batch1_make_summary_tables( metric_df = metrics_batch1, metric = "bias")

b1_sub_coverage <- batch1_subset_tables( metric_df = metrics_batch1_sub, metric = "coverage")
b1_sub_medbias <- batch1_subset_tables( metric_df = metrics_batch1_sub, metric = "median_bias")
b1_sub_power <- batch1_subset_tables( metric_df = metrics_batch1_sub, metric = "power")
b1_sub_bias <- batch1_subset_tables( metric_df = metrics_batch1_sub, metric = "bias")

 #' [ Batch 2: Binary Y, continuous X ]
b2_coverage <- batch2_make_summary_tables( metric_df = metrics_batch2, metric = "coverage")
b2_medbias <- batch2_make_summary_tables( metric_df = metrics_batch2, metric = "median_bias")
b2_power <- batch2_make_summary_tables( metric_df = metrics_batch2, metric = "power")
b2_bias <- batch2_make_summary_tables( metric_df = metrics_batch2, metric = "bias")

b2_sub_coverage <- batch2_subset_tables( metric_df = metrics_batch2_sub, metric = "coverage")
b2_sub_medbias <- batch2_subset_tables( metric_df = metrics_batch2_sub, metric = "median_bias")
b2_sub_power <- batch2_subset_tables( metric_df = metrics_batch2_sub, metric = "power")
b2_sub_bias <- batch2_subset_tables( metric_df = metrics_batch2_sub, metric = "bias")

#' [ Format large bias values to show ">1000" or "<1000" ] 
col_names_to_truncate <- c("CR_adj", "CR_un", "SBR_adj", "SBR_un", "CAA_model_adj", "CAA_model_un", "CAA_rerand_adj")
col_indices_to_truncate <- which( names( b2_bias ) %in% col_names_to_truncate )
b2_bias_truncated <- b2_bias;
b2_bias_truncated[, col_indices_to_truncate ] <- apply( b2_bias[, col_indices_to_truncate ], 2, replace_values )

col_indices_to_truncate <- which( names( b2_sub_bias ) %in% col_names_to_truncate )
b2_sub_bias_truncated <- b2_sub_bias;
b2_sub_bias_truncated[, col_indices_to_truncate] <- apply( b2_sub_bias[,col_indices_to_truncate], 2, replace_values )

#' [ Batch 3: Continuous Y, binary X ]
b3_coverage <- batch3_make_summary_tables( metric_df = metrics_batch3, metric = "coverage")
b3_medbias <- batch3_make_summary_tables( metric_df = metrics_batch3, metric = "median_bias")
b3_power <- batch3_make_summary_tables( metric_df = metrics_batch3, metric = "power")
b3_bias <- batch3_make_summary_tables( metric_df = metrics_batch3, metric = "bias")

#' [ Batch 4: Continuous Y, continuous X ]
b4_coverage <- batch4_make_summary_tables( metric_df = metrics_batch4, metric = "coverage")
b4_medbias <- batch4_make_summary_tables( metric_df = metrics_batch4, metric = "median_bias")
b4_power <- batch4_make_summary_tables( metric_df = metrics_batch4, metric = "power")
b4_bias <- batch4_make_summary_tables( metric_df = metrics_batch4, metric = "bias")

#' -------------------------------------------------------------------------- #
#'                      [ Print summary tables ] 
#' -------------------------------------------------------------------------- #
b1_mod_params <- c( "n", "Pr( Y )", "Pr( X )", "bZ", "bX" )
b2_mod_params <- c( "n", "Pr( Y )", "bZ", "bX" )
b3_mod_params <- c( "n", "Pr( X )", "bZ", "bX" )
b4_mod_params <- c( "n", "bZ", "bX" )

#' [ Batch 1: Binary Y, binary X ]
print( b1k_cov <- make_kable( b1_coverage, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Coverage probability" ) )
print( b1k_medbias <- make_kable( b1_medbias, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Median bias" ) )
print( b1k_power <- make_kable( b1_power, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Power" ) )

print( b1k_sub_cov <- make_kable( b1_sub_coverage, model_params = c("Avg. nsims", b1_mod_params), tbl_caption = "Batch 1 (Binary Y, Binary X): Coverage probability, subsetted" ) )
print( b1k_sub_medbias <- make_kable( b1_sub_medbias, model_params = c("Avg. nsims", b1_mod_params), tbl_caption = "Batch 1 (Binary Y, Binary X): Median bias, subsetted" ) )
print( b1k_sub_power <- make_kable( b1_sub_power, model_params = c("Avg. nsims", b1_mod_params), tbl_caption = "Batch 1 (Binary Y, Binary X): Power, subsetted" ) )

#' [ Batch 2: Binary Y, continuous X ]
print( b2k_cov <- make_kable( b2_coverage, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Coverage probability" ) )
print( b2k_medbias <- make_kable( b2_medbias, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Median bias" ) )
print( b2k_power <- make_kable( b2_power, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Power" ) )

print( b2k_sub_cov <- make_kable( b2_sub_coverage, model_params = c("Avg. nsims", b2_mod_params), tbl_caption = "Batch 2 (Binary Y, Continuous X): Coverage probability, subsetted" ) )
print( b2k_sub_medbias <- make_kable( b2_sub_medbias, model_params = c("Avg. nsims", b2_mod_params), tbl_caption = "Batch 2 (Binary Y, Continuous X): Median bias, subsetted" ) )
print( b2k_sub_power <- make_kable( b2_sub_power, model_params = c("Avg. nsims", b2_mod_params), tbl_caption = "Batch 2 (Binary Y, Continuous X): Power, subsetted" ) )

#' [ Batch 3: Continuous Y, binary X ]
print( b3k_cov <- make_kable( b3_coverage, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Coverage probability" ) )
print( b3k_medbias <- make_kable( b3_medbias, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Median bias" ) )
print( b3k_power <- make_kable( b3_power, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Power" ) )

#' [ Batch 4: Continuous Y, continuous X ]
print( b4k_cov <- make_kable( b4_coverage, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Continuous X): Coverage probability" ) )
print( b4k_medbias <- make_kable( b4_medbias, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Continuous X): Median bias" ) )
print( b4k_power <- make_kable( b4_power, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Continuous X): Power" ) )



#' -------------------------------------------------------------------------- #
#'                      [ Write summary tables to file ] 
#' -------------------------------------------------------------------------- #
output_tex_files <- TRUE

#' [ sink_tbl() takes kable table `kable_table` and writes it `file_name` in `out_dir`. ]
sink_tbl <- function( kable_table, file_name, out_dir = "/Users/Moschops/Documents/MSThesis/adaptive-allocation/tables/" ){
  sink(paste0( out_dir, file_name ));
  print( kable_table );
  sink( NULL );
}

if( output_tex_files ){
  mapply(sink_tbl, list(b1k_cov, b1k_medbias, b1k_power), 
         list("b1_coverage.tex", "b1_median_bias.tex", "b1_power.tex"))
  mapply(sink_tbl, list(b1k_sub_cov, b1k_sub_medbias, b1k_sub_power), 
         list("b1_sub_coverage.tex", "b1_sub_median_bias.tex", "b1_sub_power.tex"))
  
  mapply(sink_tbl, list(b2k_cov, b2k_medbias, b2k_power), 
         list("b2_coverage.tex", "b2_median_bias.tex", "b2_power.tex"))
  mapply(sink_tbl, list(b2k_sub_cov, b2k_sub_medbias, b2k_sub_power), 
         list("b2_sub_coverage.tex", "b2_sub_median_bias.tex", "b2_sub_power.tex"))
  
  mapply(sink_tbl, list(b3k_cov, b3k_medbias, b3k_power), 
         list("b3_coverage.tex", "b3_median_bias.tex", "b3_power.tex"))
  
  mapply(sink_tbl, list(b4k_cov, 
                        b4k_medbias, 
                        b4k_power), 
         list("b4_coverage.tex", 
              "b4_median_bias.tex", 
              "b4_power.tex"))
}

#' -------------------------------------------------------------------------- #
#'                      [ Results (data) visualizations ] 
#' -------------------------------------------------------------------------- #

library('ggplot2')
library('reshape2')
library('plyr')

setwd("/Users/Moschops/Documents/MSThesis/adaptive-allocation/figures")


#' -------------------------------------------------------------------------- #
#'                      [ Adjusted / unadjusted plots ] 
#' -------------------------------------------------------------------------- #
b1_id_vars <- c( "n", "Pr( Y )", "Pr( X )", "bZ", "bX" )
b2_id_vars <- c( "n", "Pr( Y )", "bZ", "bX" )
b3_id_vars <- c( "n", "Pr( X )", "bZ", "bX" )
b4_id_vars <- c( "n", "bZ", "bX" )


#' -------------------------------------------------------------------------- #
#'                      [ Batch 1 ] 
#' -------------------------------------------------------------------------- #
b1_coverage$metric <- 'coverage'
b1_medbias$metric <- 'median_bias'
b1_power$metric <- 'power'
b1 <- data.table(do.call( rbind, list( b1_coverage, b1_medbias, b1_power )))
b1_long <- melt( b1, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b1_id_vars, 'metric'), 
                 measure.vars = grep('adj$|un$', names( b1_coverage ), value = TRUE))

b1_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b1_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b1_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                 function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b1_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b1_long$metric ) ){
  if( .metric == "coverage" ){
    b1_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b1_long %>%
      subset( metric == .metric ) %>% 
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b1_long$metric ) ){
  if( .metric == "coverage" ){
    b1_long %>%
      subset( adjustment ==  'adjusted' ) %>% 
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric, ", adjusted estimates")) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b1_long %>%
      subset( metric == .metric & adjustment ==  'adjusted' ) %>% 
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric, ", adjusted estimates")) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b1_long$metric ) ){
  for( .alloc_type in unique( b1_long$alloc_type ) ){
    b1_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b1_long$metric ) ){
    b1_long %>%
      subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_", .metric,"_CR_to_CAA_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
}


#' -------------------------------------------------------------------------- #
#'                      [ Batch 1 subset ] 
#' -------------------------------------------------------------------------- #
b1_sub_coverage$metric <- 'coverage'
b1_sub_medbias$metric <- 'median_bias'
b1_sub_power$metric <- 'power'
b1_sub <- data.table(do.call( rbind, list( b1_sub_coverage, b1_sub_medbias, b1_sub_power )))
b1_sub_long <- melt( b1_sub, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b1_id_vars, 'metric', 'avg_nsim'), 
                 measure.vars = grep('adj$|un$', names( b1_sub_coverage ), value = TRUE))

b1_sub_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b1_sub_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b1_sub_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                 function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b1_sub_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b1_sub_long$metric ) ){
  if( .metric == "coverage" ){
    b1_sub_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_sub_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b1_sub_long %>%
      subset( metric == .metric ) %>% 
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_sub_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b1_sub_long$metric ) ){
  if( .metric == "coverage" ){
    b1_sub_long %>%
      subset( rerandomized == 'model' & metric == .metric & adjustment == 'adjusted' ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors (subset): ", .metric, ", adjusted estimates")) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_sub_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b1_sub_long %>%
      subset( metric == .metric & adjustment == 'adjusted' ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_sub_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b1_sub_long$metric ) ){
  for( .alloc_type in unique( b1_sub_long$alloc_type ) ){
    b1_sub_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b1_sub_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b1_sub_long$metric ) ){
  b1_sub_long %>%
    subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
    ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
    geom_line() +
    geom_point() + 
    facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
    ylab( .metric ) + 
    ggtitle(paste0("Binary outcome, binary predictors: ", .metric)) + 
    theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
    ggsave(paste0("b1_sub_", .metric,"_CR_to_CAA_adj_unadj.png"), 
           width = 30, height = 25, units = "cm")
}

#' -------------------------------------------------------------------------- #
#'                      [ Batch 2 adjusted/unadjusted plots ] 
#' -------------------------------------------------------------------------- #
b2_coverage$metric <- 'coverage'
b2_medbias$metric <- 'median_bias'
b2_power$metric <- 'power'
b2 <- data.table(do.call( rbind, list( b2_coverage, b2_medbias, b2_power )))
b2_long <- melt( b2, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b2_id_vars, 'metric'), 
                 measure.vars = grep('adj$|un$', names( b2_coverage ), value = TRUE))

b2_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b2_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b2_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                 function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b2_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b2_long$metric ) ){
  if( .metric == "coverage" ){
    b2_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b2_long %>%
      subset( metric == .metric ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b2_long$metric ) ){
  if( .metric == "coverage" ){
    b2_long %>%
      subset( rerandomized == 'model' & metric == .metric & adjustment == 'adjusted' ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b2_long %>%
      subset( metric == .metric & adjustment == 'adjusted' ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b2_long$metric ) ){
  for( .alloc_type in unique( b2_long$alloc_type ) ){
    b2_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b2_long$metric ) ){
  b2_long %>%
    subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
    ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
    geom_line() +
    geom_point() + 
    facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
    ylab( .metric ) + 
    ggtitle(paste0("Binary outcome, continuous predictors: ", .metric)) + 
    theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
    ggsave(paste0("b2_", .metric,"_CR_to_CAA_adj_unadj.png"), 
           width = 30, height = 25, units = "cm")
}



#' -------------------------------------------------------------------------- #
#'                      [ Batch 2 subset ] 
#' -------------------------------------------------------------------------- #
b2_sub_coverage$metric <- 'coverage'
b2_sub_medbias$metric <- 'median_bias'
b2_sub_power$metric <- 'power'
b2_sub <- data.table(do.call( rbind, list( b2_sub_coverage, b2_sub_medbias, b2_sub_power )))
b2_sub_long <- melt( b2_sub, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b2_id_vars, 'metric', 'avg_nsim'), 
                     measure.vars = grep('adj$|un$', names( b2_sub_coverage ), value = TRUE))

b2_sub_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b2_sub_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b2_sub_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                     function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b2_sub_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b2_long$metric ) ){
  if( .metric == "coverage" ){
    b2_sub_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_sub_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b2_sub_long %>%
      subset( metric == .metric ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_sub_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b2_sub_long$metric ) ){
  if( .metric == "coverage" ){
    b2_sub_long %>%
      subset( rerandomized == 'model' & metric == .metric & adjustment == 'adjusted' ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_sub_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b2_sub_long %>%
      subset( metric == .metric & adjustment == 'adjusted' ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_sub_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b2_sub_long$metric ) ){
  for( .alloc_type in unique( b2_sub_long$alloc_type ) ){
    b2_sub_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b2_sub_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b2_sub_long$metric ) ){
  b2_sub_long %>%
    subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
    ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
    geom_line() +
    geom_point() + 
    facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
    ylab( .metric ) + 
    ggtitle(paste0("Binary outcome, continuous predictors (subset): ", .metric)) + 
    theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
    ggsave(paste0("b2_sub_", .metric,"_CR_to_CAA_adj_unadj.png"), 
           width = 30, height = 25, units = "cm")
}


#' -------------------------------------------------------------------------- #
#'                      [ Batch 3 adjusted/unadjusted plots ] 
#' -------------------------------------------------------------------------- #
b3_coverage$metric <- 'coverage'
b3_medbias$metric <- 'median_bias'
b3_power$metric <- 'power'
b3 <- data.table(do.call( rbind, list( b3_coverage, b3_medbias, b3_power )))
b3_long <- melt( b3, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b3_id_vars, 'metric'), 
                 measure.vars = grep('adj$|un$', names( b3_coverage ), value = TRUE))

b3_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b3_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b3_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                 function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b3_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b3_long$metric ) ){
  if( .metric == "coverage" ){
    b3_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b3_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b3_long %>%
      subset( metric == .metric ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b3_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b3_long$metric ) ){
  if( .metric == "coverage" ){
    b3_long %>%
      subset( rerandomized == 'model' & metric == .metric & adjustment == 'adjusted' ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b3_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b3_long %>%
      subset( metric == .metric & adjustment == 'adjusted' ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b3_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b3_long$metric ) ){
  for( .alloc_type in unique( b3_long$alloc_type ) ){
    b3_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b3_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b3_long$metric ) ){
  b3_long %>%
    subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
    ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
    geom_line() +
    geom_point() + 
    facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
    ylab( .metric ) + 
    ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
    theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
    ggsave(paste0("b3_", .metric,"_CR_to_CAA_adj_unadj.png"), 
           width = 30, height = 25, units = "cm")
}


#' -------------------------------------------------------------------------- #
#'                      [ Batch 4 adjusted/unadjusted plots ] 
#' -------------------------------------------------------------------------- #
b4_coverage$metric <- 'coverage'
b4_medbias$metric <- 'median_bias'
b4_power$metric <- 'power'
b4 <- data.table(do.call( rbind, list( b4_coverage, b4_medbias, b4_power )))
b4_long <- melt( b4, variable.name = 'allocation_method', na.rm = TRUE, id.vars = c(b4_id_vars, 'metric'), 
                 measure.vars = grep('adj$|un$', names( b4_coverage ), value = TRUE))

b4_long[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
b4_long[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
b4_long[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                                 function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
b4_long[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

#' [ Single metric, all methods & adjustment: adj, unadj ]
for( .metric in unique( b4_long$metric ) ){
  if( .metric == "coverage" ){
    b4_long %>%
      subset( rerandomized == 'model' & metric == .metric ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n ~ bX, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b4_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b4_long %>%
      subset( metric == .metric ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n ~ bX, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b4_", .metric,"_all_methods_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ single metric, all methods, adjusted estimates only. ]
for( .metric in unique( b4_long$metric ) ){
  if( .metric == "coverage" ){
    b4_long %>%
      subset( rerandomized == 'model' & metric == .metric & adjustment == 'adjusted' ) %>%  #' [ exclude rerandomization-based CIs from coverage calcs]
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n ~ bX, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b4_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }else{
    b4_long %>%
      subset( metric == .metric & adjustment == 'adjusted' ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n ~ bX, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b4_", .metric,"_all_methods_adj_only.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, by single allocation type ]
for( .metric in unique( b4_long$metric ) ){
  for( .alloc_type in unique( b4_long$alloc_type ) ){
    b4_long %>%
      subset( metric == .metric & alloc_type == .alloc_type ) %>%
      ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
      geom_line() +
      geom_point() + 
      facet_grid( n ~ bX, labeller = label_context ) + 
      ylab( .metric ) + 
      ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
      theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
      ggsave(paste0("b4_", .metric,"_", .alloc_type, "_adj_unadj.png"), 
             width = 30, height = 25, units = "cm")
  }
}

#' [ Single metric, comparing `allocation type` CR vs. CAA ]
for( .metric in unique( b4_long$metric ) ){
  b4_long %>%
    subset( metric == .metric & alloc_type %in% c("CR", "CAA") ) %>%
    ggplot(aes( x = bZ, y = value, col = alloc_method, linetype = adjustment )) +
    geom_line() +
    geom_point() + 
    facet_grid( n ~ bX, labeller = label_context ) + 
    ylab( .metric ) + 
    ggtitle(paste0("Continuous outcome, binary predictors: ", .metric)) + 
    theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
    ggsave(paste0("b4_", .metric,"_CR_to_CAA_adj_unadj.png"), 
           width = 30, height = 25, units = "cm")
}


#' -------------------------------------------------------------------------- #
#'                      [ Older plots ] 
#' -------------------------------------------------------------------------- #
#' [ melt each data.table by `allocation_method` ]
m1 <- melt( b1_coverage, value.name = 'Coverage', variable.name = 'allocation_method', na.rm = TRUE,
            id.vars = b1_id_vars, measure.vars = grep('adj$|un$', names( b1_coverage ), value = TRUE))
m1[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
m1[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
m1[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                            function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
m1[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]

b1_medbias <- data.table( b1_medbias )
m2 <- melt( b1_medbias, value.name = 'Coverage', variable.name = 'allocation_method', na.rm = TRUE,
            id.vars = b1_id_vars, measure.vars = grep('adj$|un$', names( b1_coverage ), value = TRUE))
m2[, adjustment := ifelse(grepl( 'adj$', allocation_method ), 'adjusted', 'unadjusted') ]
m2[, alloc_type := sapply(strsplit(as.character( allocation_method ), split = "_" ), function(.x){.x[1]}) ] 
m2[, rerandomized := sapply(strsplit(as.character( allocation_method ), split = "_" ), 
                            function(.x){ifelse( .x[2] == 'rerand', 'rerand', 'model')}) ]
m2[, alloc_method := paste( alloc_type, rerandomized, sep = "_" ) ]


m1 %>% 
  subset( rerandomized == 'model' ) %>%
  ggplot(aes( x = bZ, y = Coverage, col = alloc_method, linetype = adjustment )) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
  ggtitle("Binary outcome, binary predictors: Coverage") + 
  theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
  ggsave("b1_coverage_adjustment_all.png", width = 30, height = 25, units = "cm")

m1 %>% 
  subset( rerandomized == 'model' & alloc_method == 'CR_model' ) %>%
  ggplot(aes( x = bZ, y = Coverage, col = alloc_method, linetype = adjustment )) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
  ggtitle("Binary outcome, binary predictors: Power") + 
  theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold')) +
  ggsave("b1_coverage_adjustment_all.png", width = 30, height = 25, units = "cm")

#' [ Batch 1 plots ]
b1_coverage %>%
  melt( value.name = "Coverage", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b1_coverage ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Coverage, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context )

# Coverage looks good -- exclude CAA_rerand_adj and rerun.

#' -------------------------------------------------------------------------- #
#'                      [ Batch 1 plots ] 
#' -------------------------------------------------------------------------- #
b1_coverage %>%
  melt( value.name = "Coverage", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b1_coverage ), value = TRUE)) %>%
  subset( allocation_method != "CAA_rerand_adj" ) %>%
  ggplot(aes(x = bZ, y = Coverage, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
  ggtitle("Binary outcome, binary predictors: Coverage probability") + 
  ggsave("b1_coverage.png", width = 30, height = 25, units = "cm")


b1_medbias %>%
  melt( value.name = "Median_bias", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b1_medbias ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Median_bias, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
  ggtitle("Binary outcome, binary predictors: Median bias") + 
  ggsave("b1_medbias.png", width = 30, height = 25, units = "cm")

b1_power %>%
  melt( value.name = "Power", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b1_power ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Power, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( Y )` + `Pr( X )`, labeller = label_context ) + 
  ggtitle("Binary outcome, binary predictors: Power") + 
  ggsave("b1_power.png", width = 30, height = 25, units = "cm")


#' -------------------------------------------------------------------------- #
#'                      [ Batch 2 plots ] 
#' -------------------------------------------------------------------------- #
b2_coverage %>%
  melt( value.name = "Coverage", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b2_coverage ), value = TRUE)) %>%
  subset( allocation_method != "CAA_rerand_adj" ) %>%
  ggplot(aes(x = bZ, y = Coverage, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( Y )`, labeller = label_context ) + 
  ggtitle("Binary outcome, continuous predictors: Coverage probability") + 
  ggsave("b2_coverage.png", width = 30, height = 25, units = "cm")


b2_medbias %>%
  melt( value.name = "Median_bias", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b2_medbias ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Median_bias, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( Y )`, labeller = label_context ) + 
  ggtitle("Binary outcome, continuous predictors: Median bias") + 
  ggsave("b2_medbias.png", width = 30, height = 25, units = "cm")

b2_power %>%
  melt( value.name = "Power", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( Y )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b2_power ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Power, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( Y )`, labeller = label_context ) + 
  ggtitle("Binary outcome, continuous predictors: Power") + 
  ggsave("b2_power.png", width = 30, height = 25, units = "cm")


#' -------------------------------------------------------------------------- #
#'                      [ Batch 3 plots ] 
#' -------------------------------------------------------------------------- #
b3_coverage %>%
  melt( value.name = "Coverage", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b3_coverage ), value = TRUE)) %>%
  subset( allocation_method != "CAA_rerand_adj" ) %>%
  ggplot(aes(x = bZ, y = Coverage, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
  ggtitle("Continuous outcome, binary predictors: Coverage probability") + 
  ggsave("b3_coverage.png", width = 30, height = 25, units = "cm")


b3_medbias %>%
  melt( value.name = "Median_bias", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b3_medbias ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Median_bias, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( X )`, labeller = label_context ) + 
  ggtitle("Continuous outcome, binary predictors: Median bias") + 
  ggsave("b3_medbias.png", width = 30, height = 25, units = "cm")

b3_power %>%
  melt( value.name = "Power", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "Pr( X )",  "bZ",  "bX"),
        measure.vars = grep("adj$", names( b3_power ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Power, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX  ~ `Pr( X )`, labeller = label_context ) + 
  ggtitle("Continuous outcome, binary predictors: Power") + 
  ggsave("b3_power.png", width = 30, height = 25, units = "cm")


#' -------------------------------------------------------------------------- #
#'                      [ Batch 4 plots ] 
#' -------------------------------------------------------------------------- #
b4_coverage %>%
  melt( value.name = "Coverage", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "bZ",  "bX"),
        measure.vars = grep("adj$", names( b4_coverage ), value = TRUE)) %>%
  subset( allocation_method != "CAA_rerand_adj" ) %>%
  ggplot(aes(x = bZ, y = Coverage, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n + bX ~ `Pr( X )`, labeller = label_context ) + 
  ggtitle("Continuous outcome, continuous predictors: Coverage probability") + 
  ggsave("b4_coverage.png", width = 30, height = 25, units = "cm")


# b4_medbias %>%
#   melt( value.name = "Median_bias", variable.name = "allocation_method", na.rm = TRUE,
#         id.vars = c("n", "bZ", "bX"),
#         measure.vars = grep("adj$", names( b4_medbias ), value = TRUE)) %>%
#   ggplot(aes(x = bZ, y = Median_bias, col = allocation_method)) +
#   geom_line() +
#   geom_point() + 
#   facet_grid( n ~ bX, labeller = label_context ) + 
#   ggtitle("Continuous outcome, continuous predictors: Median bias") + 
#   ggsave("b4_medbias.png", width = 30, height = 25, units = "cm")

b4_power %>%
  melt( value.name = "Power", variable.name = "allocation_method", na.rm = TRUE,
        id.vars = c("n", "bZ",  "bX"),
        measure.vars = grep("adj$", names( b4_power ), value = TRUE)) %>%
  ggplot(aes(x = bZ, y = Power, col = allocation_method)) +
  geom_line() +
  geom_point() + 
  facet_grid( n ~ bX, labeller = label_context ) + 
  ggtitle("Continuous outcome, continuous predictors: Power") + 
  theme( plot.title = element_text( hjust = 1, vjust = 0.5, face = 'bold'))
# ggsave("b4_power.png", width = 30, height = 25, units = "cm")



