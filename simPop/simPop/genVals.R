genVals <- function(dataSample, dataPop, params, typ, response) {
  
  # unify level-set of predictors
  for ( i in params$predNames ) {
    dataSample[[i]] <- cleanFactor(dataSample[[i]])
    dataPop[[i]] <- cleanFactor(dataPop[[i]])
  }
  
  
  if ( !typ %in% c("multinom","lm","binary","poisson","xgboost") ) {
    stop("unsupported value for argument 'type' in genVals()\n")
  }
  # Check wheter all response values are the same
  if(length(unique(response))==1){
    res <- rep(unique(response),nrow(dataPop))
    return(res)
  }
  if ( typ=="binary") {
    res <- generateValues_binary(dataSample, dataPop, params)
  }else if ( typ=="lm") {
    res <- generateValues_lm(dataSample, dataPop, params)
  }else if ( typ=="multinom" ) {
    res <- generateValues_multinom(dataSample, dataPop, params)
  }else if ( typ=="poisson") {
    res <- generateValues_poisson(dataSample, dataPop, params)
  }else if ( typ=="xgboost") {
    res <- generateValues_xgboost(dataSample, dataPop, params)
  }
  res
}