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
#' [ NOTE 1: all are adjusted for prognostic factors (`adjusted` == 1) ]
dftest <- dfs_by_model[[4]] 
str( dftest )
table( dftest$method_name )

df_cr <- dftest[ dftest$method_name == "CR_REG_ADJ", ]
str( df_cr );
summary( df_cr$est )
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -4.154e+14 -1.000e+00  0.000e+00  8.213e+11  1.000e+00  1.916e+15 
#' [ max is still large! where are the draws that this is happening? use old rule (est > 40)]
df_cr[ with( df_cr, which( est > 40 )), ]
#' [ some have separation_status == TRUE. what are the indices not flagged by current rule? ]
indices_large_ests <- with( df_cr, which( est > 40 & separation_status == FALSE ))
#' [ get row names of these guys (has format r.`index`.`drawnumber` e.g. r7.2161 ) ]
draw_names_large_ests <- rownames( df_cr[ indices_large_ests, ] )

#' [ get index and drawnumber ]
index_draw_pairs <- do.call("rbind", sapply( draw_names_large_ests, function( .string ){
  strsplit( substring( .string, first = 2 ), split = "[.]" )}) )
colnames( index_draw_pairs ) <- c("index", "draw")

#' [ get drawnumber as integer]
draws_look <- sort( as.integer( index_draw_pairs[,2] ))

#' [ load alloc method output to get (Z,Y) values ]
results_zy <- output( simulation, subset = 4, methods = "CR")
#' [ load draw results to get (X) values ]
results_x <- draws( simulation, subset = 4 )

#' [ get an example (first draws_look value) ]
df_look <- data.frame( results_zy@out[[ draws_look[1] ]][1:2] )
df_look$X <- results_x@draws[[ draws_look[1] ]]$X
df_look
# Y Z         X.1         X.2
# 1  0 1 -0.32711032  0.16469930
# 2  0 0  0.81659900 -0.41344070
# 3  0 1 -0.39590202 -0.08633056
# 4  0 0 -1.65862650 -1.93017172
# 5  0 0  1.34479756  0.22273685
# 6  0 0 -0.89153789 -0.23188284
# 7  0 1  1.78146681  0.51629952
# 8  0 0  1.01928412 -0.48781549
# 9  0 1  0.47097957 -0.19118787
# 10 0 1  0.81794009 -0.57186889
# 11 0 0 -0.11689430 -0.68579322
# 12 0 1  0.85552282  0.49906583
# 13 0 0 -0.43417234  0.21968083
# 14 1 1  0.85253405 -0.25715444
# 15 0 1 -0.11701137  0.06613837
# 16 0 0  1.59158075 -0.73459790
# 17 1 1  0.82910736 -1.23980317
# 18 0 1 -0.71753994 -2.54074258
# 19 0 1  1.55030832 -2.78129441
# 20 0 1 -0.77591711 -0.22870944
# 21 1 0  0.67292771  1.37028451
# 22 1 0  0.69501480 -0.40750230
# 23 0 1 -0.50319928  0.74014254
# 24 1 0 -0.35253991  1.91825369
# 25 0 1  0.26811917  0.87965413
# 26 0 1 -0.66350992  1.10245967
# 27 0 0 -0.37618877  0.52917633
# 28 0 0  0.67014456  0.51183086
# 29 1 1  0.16176286  0.22108412
# 30 0 0  1.45842902 -1.45036376
# 31 0 1 -0.79119458  1.39163296
# 32 0 0 -0.03671983  0.71283908
glm_look <- glm( Y ~ Z + X, family = quasibinomial(link = "logit"), data = df_look )
glm_look
# Call:  glm(formula = Y ~ Z + X, family = quasibinomial(link = "logit"), 
#            data = df_look)
# 
# Coefficients:
#   (Intercept)            Z           X1           X2  
# -1.738425    -0.008052     0.670487     0.657878  
# 
# Degrees of Freedom: 31 Total (i.e. Null);  28 Residual
# Null Deviance:	    30.88 
# Residual Deviance: 28.77 	AIC: NA
round( summary( glm_look )$coef, 4)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -1.7384     0.8140 -2.1356   0.0416
# Z            -0.0081     0.9799 -0.0082   0.9935
# X1            0.6705     0.6870  0.9759   0.3375
# X2            0.6579     0.5987  1.0989   0.2812

#' [ custom tryCatch() function -- TODO(michael): adapt to glm()! ]
# custom tryCatch to return result and warnings -- http://stackoverflow.com/a/24569739/2271856
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

#' [ NOTE2: returns valid results. what's going on? examine more draws.]
very_verbose <- FALSE;  # print all objects?
#' [ `results_look` is {est, se, t, p} from glm() output. ]
results_look <- matrix( nrow = length( draws_look ), ncol = 4 )
#' [ `glm_converged` is FALSE if glm.fit did not converge. ]
glm_converged <- rep( NA, length( draws_look ))
for( draw_i in seq_along( draws_look ) ){
  cat(paste0("Model 4: Examining draw number [ ", draws_look[ draw_i ], " ]: \n"))
  df_look <- data.frame( results_zy@out[[ draws_look[ draw_i ] ]][1:2] )
  df_look$X <- results_x@draws[[ draws_look[ draw_i ] ]]$X
  if( very_verbose ) print( df_look )
  # tryCatch( glm_look <- glm( Y ~ Z + X, family = quasibinomial(link = "logit"), data = df_look ),
  #           warning = function(w) list(draw = draws_look[ draw_i ], warning = w) )
  foo <- myTryCatch( glm_look <- glm( Y ~ Z + X, family = quasibinomial(link = "logit"), data = df_look ) )
  glm_converged[ draw_i ] <- glm_look$converged
  results_look[ draw_i, ] <- round( summary( glm_look )$coef["Z",], 4)
}


#' [ Key example: benchmark this and find best runtime. ]
f0 <- function( .nsim = 10 ){
  .glm_converge_status <- matrix( nrow = .nsim, ncol = length( output_alloc_names ))
  for( sim_i in seq_len( .nsim ) ){
    for( alloc_j in seq_along( output_alloc_names ) ){
      df_ij <- data.frame(output_allocs_only[[ alloc_j ]]@out[[ sim_i ]][1:2])
      df_ij$X <- draws_look@draws[[ sim_i ]]$X
      myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
      .glm_converge_status[ sim_i, alloc_j ] <- glm_test$converged
      if( !glm_test$converged ){
        cat(paste0("Simulation [ ", sim_i, " ] Alloc no. [", alloc_j ," ] DID NOT CONVERGE.\n"))
      }
    }
  }
  .glm_converge_status
}
f1 <- function( .nsim = 10 ){
  .glm_converge_status <- matrix( nrow = .nsim, ncol = length( output_alloc_names ))
  for( sim_i in seq_len( .nsim ) ){
    for( alloc_j in seq_along( output_alloc_names ) ){
      df_ij <- data.frame(output_allocs_only[[ alloc_j ]]@out[[ sim_i ]][1:2])
      df_ij$X <- draws_look@draws[[ sim_i ]]$X
      myTryCatch( .glm_converge_status[ sim_i, alloc_j ] <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij )$converged )
      if( .glm_converge_status[ sim_i, alloc_j ] ){
        cat(paste0("Simulation [ ", sim_i, " ] Alloc no. [", alloc_j ," ] DID NOT CONVERGE.\n"))
      }
    }
  }
  .glm_converge_status
}
f2 <- function( .nsim = 10 ){
  .glm_converge_status <- matrix( nrow = .nsim, ncol = length( output_alloc_names ))
  for( sim_i in seq_len( .nsim ) ){
    for( alloc_j in seq_along( output_alloc_names ) ){
      df_ij <- data.frame(output_allocs_only[[ alloc_j ]]@out[[ sim_i ]][1:2])
      df_ij$X <- draws_look@draws[[ sim_i ]]$X
      myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
      .glm_converge_status[ sim_i, alloc_j ] <- glm_test$converged
    }
  }
  .glm_converge_status
}
f3 <- function( .nsim = 10 ){
  .glm_converge_status <- matrix( nrow = .nsim, ncol = length( output_alloc_names ))
  for( sim_i in seq_len( .nsim ) ){
    for( alloc_j in seq_along( output_alloc_names ) ){
      df_ij <- data.frame(output_allocs_only[[ alloc_j ]]@out[[ sim_i ]][1:2])
      df_ij$X <- draws_look@draws[[ sim_i ]]$X
      myTryCatch( glm_test <- glm( Y ~ Z + X, family = quasibinomial(link="logit"), data = df_ij ) )
      .glm_converge_status[ sim_i, alloc_j ] <- glm_test$converged
      rm(list=c("df_ij", "glm_test"))
    }
  }
  .glm_converge_status
}
microbenchmark::microbenchmark( f0(), f1() )
# Unit: milliseconds
# expr      min       lq     mean   median       uq       max neval cld
# f0() 50.31465 53.01306 56.69882 54.52595 55.98490  99.56758   100  a 
# f1() 53.66540 55.66715 60.56367 56.55928 57.78203 176.80472   100   b
#' [ NOTE: not saving glm() full output doesn't save time. ]
microbenchmark::microbenchmark( f0(2), f1(2), f2(2), f3(2) )


