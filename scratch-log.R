## @knitr main
## author             = Michael Flanagin
## date started       = 2018 August 12
## date last modified = 2018 August 12
## advisor            = Amalia Magaret
## objective          = Historical file to track edits and modifications.


# --------------------------------------------------------------------------- # 
#' [ 12 August 2018 (Sunday) ]
#' [ 16:30 ] BATCH 2: Investigating huge bias despite flagging for separation.
#' Key results are as follows:
#         alloc_method adjusted rerandomized power.pvalue power.rerand power.ci coverage         bias median_bias nsim modelno
# 17                CR        1            0        0.108        0.000    0.122    0.878 1.019121e+12  0.02879528 4445       4
# 19               SBR        1            0        0.109        0.000    0.125    0.875 5.085296e+11 -0.01944388 4580       4
# 21 CAA-MI-2-PBA-0.70        1            1        0.053        0.034    0.000    0.997 7.867960e+10  0.02669909 4462       5
# 22                CR        1            0        0.108        0.000    0.119    0.882 9.085824e+11  0.02778716 4444       5
# 24               SBR        1            0        0.105        0.000    0.121    0.882 4.513681e+11 -0.01024102 4573       5
# 26 CAA-MI-2-PBA-0.70        1            1        0.132        0.097    0.000    0.998 3.873593e+11  0.08380726 4238       6
# 29               SBR        1            0        0.175        0.000    0.196    0.882 1.831604e+11  0.13584332 4357       6
#' [1] Working with data frames in `dfs_by_model` to identify results (from calling `process_simulation_results.R`)