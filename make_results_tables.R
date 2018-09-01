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
print( b4k_cov <- make_kable( b4_coverage, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Coverage probability" ) )
# print( b4k_medbias <- make_kable( b4_medbias, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Median bias" ) )
print( b4k_power <- make_kable( b4_power, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Power" ) )



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
#                        b4k_medbias, 
                        b4k_power), 
         list("b4_coverage.tex", 
#              "b4_median_bias.tex", 
              "b4_power.tex"))
}

#' -------------------------------------------------------------------------- #
#'                      [ Results (data) visualizations ] 
#' -------------------------------------------------------------------------- #

library('ggplot2')
library('reshape2')
library('plyr')

#' [ gimme_ggplot() NOTE: does not work! issues with defining .variable.name and .value.name. ]
gimme_ggplot <- function( result_tbl, 
                          .value.name = 'Coverage probability',
                          .variable.name = 'Allocation method',
                          .id.vars = c("n", "Pr( Y )", "Pr( X )",  "bZ",  "bX"),
                          .adjusted = TRUE )
  {
  grep_string <- ifelse( .adjusted, "adj$", "un$" );
  result_tbl %>%
    melt( value.name = .value.name, variable.name = .variable.name, na.rm = TRUE,
          id.vars = .id.vars,
          measure.vars = grep( grep_string, names( b1_coverage ), value = TRUE )) %>%
    ggplot(aes(x = bZ, y = eval(.value.name), col = eval(.variable.name) )) +
    geom_line() + geom_point() + 
    facet_grid( n + bX ~ `Pr( Y )` + `Pr( X )`, labeller = label_context )
}


setwd("/Users/Moschops/Documents/MSThesis/adaptive-allocation/figures")

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
  ggsave("b4_power.png", width = 30, height = 25, units = "cm")

