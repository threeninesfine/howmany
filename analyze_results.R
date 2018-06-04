## @knitr analyze_results
## author             = Michael Flanagin
## date started       = 1 Jun 2018
## date last modified = 3 Jun 2018
## advisor            = Amalia Magaret
## objective          = Analyze results of analysis

library("simulator") # this file was created under simulator version 0.2.0

metrics = list( coverage,
                power_p_value,
                power_ci,
                bias ),
parallel = list( socket_names = num_cores_parallel ))

#' method_names are found in:
alloc_method_names <- sapply( get_adjusted_tx_effect, function(.obj){ .obj@base_method@name} )
alloc_extended_method_names <- sapply( get_adjusted_tx_effect, function(.obj){ .obj@name } )

simulation %>%
  subset_simulation() %>%
  tabulate_eval(metric_name = "coverage",
                format_args = list(nsmall = 3, digits = 0))

simulation %>%
  subset_simulation() %>%
  tabulate_eval(metric_name = "power_p_value",
                format_args = list(nsmall = 3, digits = 0))

simulation %>%
  subset_simulation() %>%
  tabulate_eval(metric_name = "power_ci",
                format_args = list(nsmall = 3, digits = 0))

simulation %>%
  subset_simulation() %>%
  tabulate_eval(metric_name = "bias",
                format_args = list(nsmall = 3, digits = 0))
