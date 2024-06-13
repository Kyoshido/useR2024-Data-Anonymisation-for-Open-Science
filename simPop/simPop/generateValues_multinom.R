generateValues_multinom <- function(dataSample, dataPop, params) {
  excludeLevels <- params$excludeLevels
  maxit <- params$maxit
  MaxNWts <- params$MaxNWts
  command <- params$command
  eps <- params$eps
  limit <- params$limit
  censor <- params$censor
  hasNewLevels  <- params$hasNewLevels
  newLevels <- params$newLevels
  name <- params$name
  response <- params$response
  
  # unique combinations in the stratum of the population need
  # to be computed for prediction
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
  } else {
    exclude <- integer()
  }
  # fit multinomial model
  mod <- eval(parse(text=command))  # fitted model
  # predict probabilities
  if ( length(exclude) == 0 ) {
    probs <- predict(mod, newdata=grid, type="probs")
  } else {
    probs <- predict(mod, newdata=grid[-exclude, , drop=FALSE], type="probs")
  }
  # set too small probabilities to exactly 0
  if ( !is.null(eps) ) {
    probs[probs < eps] <- 0
  }
  # ensure it works for missing levels of response
  ind <- as.integer(which(table(dataSample[[name]]) > 0))
  if ( length(ind) > 2 && (nrow(grid)-length(exclude)) == 1 ) {
    probs <- t(probs)
  }
  # account for structural zeros
  if ( (!is.null(limit) || !is.null(censor)) && !is.null(dim(probs)) ) {
    if ( length(exclude) == 0 ) {
      probs <- adjustProbs(probs, grid, names(indGrid), limit, censor)
    } else {
      probs <- adjustProbs(probs, grid[-exclude, , drop=FALSE], names(indGrid)[-exclude], limit, censor)
    }
  }
  # local function for sampling from probabilities
  if ( length(ind) == 1 ) {
    resample <- function(k, n, p) rep.int(1, n[k])
  } else if ( length(ind) == 2 ) {
    resample <- function(k, n, p) spSample(n[k], c(1-p[k],p[k]))
  } else {
    resample <- function(k, n, p) spSample(n[k], p[k,])
  }
  # generate realizations for each combination
  sim <- as.list(rep.int(NA, length(indGrid)))
  if ( length(exclude) == 0 ) {
    ncomb <- as.integer(sapply(indGrid, length))
    sim <- lapply(1:length(ncomb), resample, ncomb, probs)
  } else {
    ncomb <- as.integer(sapply(indGrid[-exclude], length))
    sim[-exclude] <- lapply(1:length(ncomb), resample, ncomb, probs)
  }
  sim <- unsplit(sim, dataPop, drop=TRUE)
  # return realizations
  levels(response)[ind][sim]
}