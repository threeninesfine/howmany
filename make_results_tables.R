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
#' [ Batch 1: Binary Y, binary X ]
b1_mod_params <- c( "n", "Pr( Y )", "Pr( X )", "bZ", "bX" )
make_kable( b1_coverage, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Coverage probability" )
make_kable( b1_medbias, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Median bias" )
make_kable( b1_power, model_params = b1_mod_params, tbl_caption = "Batch 1 (Binary Y, Binary X): Power" )

make_kable( b1_sub_coverage, model_params = c("Avg. nsims", b1_mod_params), 
            tbl_caption = "Batch 1 (Binary Y, Binary X): Coverage probability, subsetted" )
make_kable( b1_sub_medbias, model_params = c("Avg. nsims", b1_mod_params), 
            tbl_caption = "Batch 1 (Binary Y, Binary X): Median bias, subsetted" )
make_kable( b1_sub_power, model_params = c("Avg. nsims", b1_mod_params), 
            tbl_caption = "Batch 1 (Binary Y, Binary X): Power, subsetted" )

#' [ Batch 2: Binary Y, continuous X ]
b2_mod_params <- c( "n", "Pr( Y )", "bZ", "bX" )
make_kable( b2_coverage, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Coverage probability" )
make_kable( b2_medbias, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Median bias" )
make_kable( b2_power, model_params = b2_mod_params, tbl_caption = "Batch 2 (Binary Y, Continuous X): Power" )

make_kable( b2_sub_coverage, model_params = c("Avg. nsims", b2_mod_params), 
            tbl_caption = "Batch 2 (Binary Y, Continuous X): Coverage probability, subsetted" )
make_kable( b2_sub_medbias, model_params = c("Avg. nsims", b2_mod_params), 
            tbl_caption = "Batch 2 (Binary Y, Continuous X): Median bias, subsetted" )
make_kable( b2_sub_power, model_params = c("Avg. nsims", b2_mod_params), 
            tbl_caption = "Batch 2 (Binary Y, Continuous X): Power, subsetted" )

#' [ Batch 3: Continuous Y, binary X ]
b3_mod_params <- c( "n", "Pr( X )", "bZ", "bX" )
make_kable( b3_coverage, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Coverage probability" )
make_kable( b3_medbias, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Median bias" )
make_kable( b3_power, model_params = b3_mod_params, tbl_caption = "Batch 3 (Continuous Y, Binary X): Power" )

#' [ Batch 4: Continuous Y, continuous X ]
b4_mod_params <- c( "n", "bZ", "bX" )
make_kable( b4_coverage, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Coverage probability" )
make_kable( b4_medbias, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Median bias" )
make_kable( b4_power, model_params = b4_mod_params, tbl_caption = "Batch 4 (Continuous Y, Binary X): Power" )
