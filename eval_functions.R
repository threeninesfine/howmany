## @knitr metrics

coverage <- new_metric("coverage", "Coverage probability",
                       metric = function( model, out ){
                         return(( out$cilower <= model$bX ) & ( model$bX <= out$ciupper ))
                       })

power_p_value <- new_metric("power_p_value", "Power by P-value",
                            metric = function( model, out ){
                              return(( out$p < model$alpha ))
                            })

power_ci <- new_metric("power_ci", "Power by CI",
                       metric = function( model, out ){
                         return(( model$bX < out$cilower ) | ( model$bX > out$ciupper ))
                       })

bias <- new_metric("bias", "Bias",
                        metric = function(model, out){
                          return( model$bX - out$est )
                          })

mse <- new_metric("mse", "MSE",
                       metric = function(model, out){
                         return(( model$bX - out$est )^2)
                       })