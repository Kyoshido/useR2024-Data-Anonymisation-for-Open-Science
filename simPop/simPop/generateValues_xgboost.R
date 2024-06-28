generateValues_xgboost <- function(dataSample, dataPop, params) {
  
  res <- NULL
  
  predNames <- params$predNames
  name <- params$name
  typ <- params$typ
  formula <- params$formula
  weight <- params$weight
  command <- params$command
  residual <- params$residuals
  strata <- params$strata
  
  
  # remove strata from prediction, because xgboost can not handle factors with only one level
  predNames <- predNames[predNames != strata]
  
  # Check if a factor has only one level
  check_levels <- sapply (predNames, function(nam) {
    length(levels(dataSample[[nam]])) == 1
  })
  
  if(any(check_levels)){
    stop(paste0(predNames[which(check_levels)]),
         " has only one level, XGBoost can't work with categorical variables with one level")
  }
  
  mod <- eval(parse(text=command))
  
  # set sample factor levels to population
  ind <- match(colnames(dataPop), colnames(dataSample))
  for ( i in 1:length(ind) ) {
    if (is.factor(unlist(dataPop[,i,with=FALSE]))) {
      dataPop[,colnames(dataPop)[i]:=factor(as.character(unlist(dataPop[,colnames(dataPop)[i],with=FALSE])),levels(dataSample[[ind[i]]]))]
    }
  }
  
  new_data <- xgb.DMatrix(data = model.matrix(~.+0, data = model.frame(dataPop[, predNames, with = FALSE],  na.action=na.pass)), missing = NA)
  pred <- predict(mod,
                  newdata=new_data)
  
  # if residuals is true, calculate in-sample residuals and add an error term to the predictions
  if(residual){
    predSample <- predict(mod,
                          newdata=xgb.DMatrix(data = model.matrix(~.+0, data = model.frame(dataSample[, predNames, with = FALSE],  na.action=na.pass)),
                                              missing = NA,
                                              info = list(weight = as.numeric(dataSample[, weight, with = FALSE]))))
    resSample <- cbind(dataSample, predSample)
    
    resSample[, res := predSample - .SD, .SDcols = name]
    target <- resSample[, name, with = FALSE][[1]]
    
    error <- sample(resSample$res, nrow(dataPop), replace = TRUE)
    pred <- pred + error
  }
  
  return(pred)
}