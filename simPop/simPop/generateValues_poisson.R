generateValues_poisson <- function(dataSample, dataPop, params) {
  if ( !nrow(dataSample) ) {
    return(rep(0,nrow(dataPop)))
  }
  coef <- params$coef
  excludeLevels <- params$excludeLevels
  hasNewLevels <- params$hasNewLevels
  newLevels <- params$newLevels
  command <- params$command
  predNames <- params$predNames
  additional <- params$additional
  const <- params$const
  formula <- params$formula
  levels <- params$levels
  residuals <- params$residuals
  log <- params$log
  
  # fix: for each predictor, the level set must be equal in dataSample and dataPop
  for ( i in predNames ) {
    if(is.factor(dataSample[[i]])){
      both <- intersect(levels(dataSample[[i]]), levels(dataPop[[i]]))
      a <- as.character(dataSample[[i]])
      a[!a%in%both] <- NA
      b <- as.character(dataPop[[i]])
      b[!b %in%both] <- NA
      dataSample[[i]] <- factor(a, levels=both)
      dataPop[[i]] <- factor(b, levels=both)
    }
    if((is.factor(dataSample[[i]])&!is.factor(dataPop[[i]]))|(is.factor(dataPop[[i]])&!is.factor(dataSample[[i]]))){
      stop("Variable",i,"is a factor only in one of the sample and population data sets.")
    }
  }
  # unique combinations in the stratum of the population need to be computed for prediction
  indGrid <- split(1:nrow(dataPop), dataPop, drop=TRUE)
  grid <- dataPop[sapply(indGrid, function(i) i[1]), , drop=FALSE]
  grid <- as.data.frame(grid)
  
  # in sample, observations with NAs have been removed to fit the
  # model, hence population can have additional levels
  # these need to be removed since those probabilities cannot
  # be predicted from the model
  if ( excludeLevels ) {
    exclude <- mapply(function(pop, new) pop %in% new,
                      pop=grid[, hasNewLevels, drop=FALSE], new=newLevels[hasNewLevels]
    )
    if ( is.null(dim(exclude)) ) {
      exclude <- which(any(exclude))
    } else {
      exclude <- which(apply(exclude, 1, any))
    }
    if ( length(exclude) > 0 ) {
      grid <- grid[-exclude, , drop=FALSE]
    }
    for ( j in predNames[hasNewLevels] ) {
      # drop new factor levels
      grid[, j] <- factor(as.character(grid[, j]), levels=levels(dataSample[[j]]))
    }
  } else {
    exclude <- integer()
  }
  # fit linear model
  mod <- try(eval(parse(text=command)))
  # add coefficients from auxiliary model if necessary
  #tmp <- coef
  #coef[names(coef(mod))] <- coef(mod)
  #mod$coefficients <- coef
  # prediction
  # add 0 variable to combinations for use of 'model.matrix'
  if(!"try-error"%in%class(mod)){
    newdata <- cbind(grid, 0)
    names(newdata) <- c(predNames, additional[1])
    
    if ( length(exclude) == 0 ) {
      pred <- round(predict(mod, newdata=newdata,type="response"))
    } else {
      pred <- as.list(rep.int(NA, length(indGrid)))
      pred[-exclude] <- round(predict(mod, newdata=newdata,type="response"))
    }
    pred <- unsplit(pred, dataPop, drop=TRUE)  
  }else{
    pred <- rep(0,nrow(dataPop))
  }
  
  # add error terms
  # addition of an error term, not implemented for Poisson Regression yet
  #  if ( residuals ) {
  #    error <- sample(residuals(mod), size=nrow(dataPop), replace=TRUE)
  #  } else {
  #    mu <- median(residuals(mod))
  #    sigma <- mad(residuals(mod))
  #    error <- rnorm(nrow(dataPop), mean=mu, sd=sigma)
  #  }
  # return realizations
  return(pred)
  sim <- pred #+ error
  #return(sim)
}