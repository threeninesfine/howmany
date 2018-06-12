## @knitr metrics

coverage <- new_metric("coverage", "Coverage probability", metric = function( model, out ){
  return(( out$cilower <= model$bZ ) && ( model$bZ <= out$ciupper ))})

power_p_value <- new_metric("power_p_value", "Power by P-value", metric = function( model, out ){
  return(( out$p < model$alpha ))})

power_ci <- new_metric("power_ci", "Power by CI", metric = function( model, out ){
  if( out$rerandomized ){
    return(( out$cilower <= out$est ) && ( out$est <= out$ciupper ))
  }else{
    return(( 0 < out$cilower ) | ( 0 > out$ciupper ))}})

bias <- new_metric("bias", "Bias", metric = function(model, out){
  return( model$bZ - out$est ) })

mse <- new_metric("mse", "MSE", metric = function(model, out){
  return(( model$bZ - out$est )^2) })


