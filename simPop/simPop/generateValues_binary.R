generateValues_binary <- function(dataSample, dataPop, params) {
  excludeLevels <- params$excludeLevels
  hasNewLevels <- params$hasNewLevels
  newLevels <- params$newLevels
  predNames <- params$predNames
  name <- params$name
  weight <- params$weight
  useAux <- params$useAux
  tol <- params$tol
  eps <- params$eps
  if ( !nrow(dataSample) ) {
    return(numeric())
  }
  # if all y values are the same return the same value for everybody
  if(length(unique(dataSample[[name]]))==1){
    if(params$verbose){
      cat("All values in the training data set are the same!\n")
    }
    return(rep(dataSample[[name]][1],nrow(dataPop)))
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
                      pop=grid[, hasNewLevels, drop=FALSE],
                      new=newLevels[hasNewLevels]
    )
    if ( is.null(dim(exclude)) ) {
      exclude <- which(any(exclude))
    } else {
      exclude <- which(apply(exclude, 1, any))
    }
    if ( length(exclude) > 0 ) {
      grid <- grid[exclude, , drop=FALSE]
    }
    for ( j in predNames[hasNewLevels] ) {
      # drop new factor levels
      grid[, j] <- factor(as.character(grid[, j]), levels=levels(dataSample[[j]]))
    }
  } else {
    exclude <- integer()
  }
  # add 0 variable to combinations for use of 'model.matrix'
  Xnew <- cbind(grid, 0)
  names(Xnew) <- c(predNames, name)
  Xnew <- model.matrix(params$command, data=Xnew)
  
  # fit logit model
  X <- model.matrix(params$command, data=dataSample)
  y <- dataSample[[name]]
  weights <- dataSample[[weight]]
  mod <- logitreg(X, y, weights=weights)
  # add parameters from auxiliary model if necessary
  if ( useAux ) {
    indPar <- abs(mod$par) < tol
    mod$par[indPar] <- params$par[indPar]
    
    # remove non-existing combinations from mod$par
    # reason: auxiliary model is estimated on total population
    ii <- setdiff(names(mod$par), colnames(Xnew))
    if ( length(ii) >0 ) {
      mod$par <- mod$par[!names(mod$par)%in%ii]
    }
  }
  
  # predict probabilities
  tmp <- exp(Xnew %*% mod$par)
  # avoid integer overflow
  p <- ifelse(is.infinite(tmp), 1, as.numeric(tmp / (1 + tmp)))
  # set too small probabilities to exactly 0
  if ( !is.null(eps) ) {
    p[p < eps] <- 0
  }
  # generate realizations for each combination
  if ( length(exclude) == 0 ) {
    ncomb <- as.integer(sapply(indGrid, length))
    sim <- lapply(1:length(ncomb), function(k) {
      spSample(ncomb[k], c(1-p[k], p[k])) - 1
    })
  } else {
    ncomb <- as.integer(sapply(indGrid[-exclude], length))
    sim <- as.list(rep.int(NA, length(indGrid)))
    sim[-exclude] <- lapply(1:length(ncomb), function(k) {
      spSample(ncomb[k], c(1-p[k], p[k])) - 1
    })
  }
  # return realizations
  if(params$verbose){
    cat("Summary of the predicted probabilites:")
    print(summary(unsplit(sim, dataPop, drop=TRUE)))
  }
  unsplit(sim, dataPop, drop=TRUE)
}