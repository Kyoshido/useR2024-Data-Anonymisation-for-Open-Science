rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)

library(simPop)

data(eusilc13puf, package = "simPop")
# ??eusilc13puf

df <- eusilc13puf[,c(1:6, 8:9,14, 16, 46)]
df$age <- as.numeric(df$age)
df$pid <- as.factor(df$pid)

vars <- c("hhid", "hsize", "region", "age", "sex", "pid", "eco_stat", "citizenship", 
          "pgrossIncome","hgrossIncome","weight")

colnames(df) <- vars

#df <- head(df, 1000)
# df$weight <- 1

inp <-
  specifyInput(
    data = df,
    hhid = "hhid",
    hhsize = "hsize",
    strata = "region",
    weight = "weight"
  )
## Calibrate sample weights
# Not going to this today
# data("totalsRG", package = "simPop")
# data("totalsRGtab", package = "simPop")
# 
# weights.df <- calibSample(inp, totalsRG)
# weights.tab <- calibSample(inp, totalsRGtab)
# identical(weights.df, weights.tab)
# addWeights(inp) <- calibSample(inp, totalsRGtab)

simPop <-
  simStructure(
    data = inp,
    method = "distribution",
    basicHHvars = c("age", "sex", "region") 
  )

simPop <-
  simCategorical(
    simPop,
    additional = c("eco_stat", "citizenship"),
    method = "multinom"
  )

###############################################################################

source("simPop/generateValues_binary.R")
source("simPop/generateValues_lm.R")
source("simPop/generateValues_multinom.R")
source("simPop/generateValues_poisson.R")
source("simPop/generateValues_xgboost.R")
source("simPop/genVals.R")
source("simPop/runModel.R")

source("simPop/regressionInput.R")
source("simPop/getCatName.R")
source("simPop/logitreg.R")

###############################################################################

# simContinuous <- function(
simPop <- simContinuous(
      simPop,
      method = "lm",
      additional = "pgrossIncome",
      regModel = ~ sex + hsize + eco_stat + citizenship + age,
      zeros = TRUE,
      log = FALSE,
      const = 1,
      alpha = NULL, #0.05,
      residuals = TRUE
    ) 

simPopObj = simPop
method = "lm"
additional = "pgrossIncome"
regModel = ~ sex + hsize + eco_stat + citizenship + age
zeros = TRUE
log = FALSE
const = 1
alpha = NULL #0.05
residuals = TRUE

# ----
breaks = NULL
lower = NULL
upper = NULL
equidist = TRUE
probs = NULL
gpd = TRUE
threshold = NULL
est = "moments"
limit = NULL
censor = NULL
keep = TRUE
maxit = 500
MaxNWts = 1500
tol = .Machine$double.eps^0.5
nr_cpus=NULL
eps = NULL
byHousehold=NULL
imputeMissings=FALSE
seed = 1
verbose=FALSE
by="strata"
model_params=NULL
    
    # ) {
  
  x <- hhid <- vals <- id <- V1 <- randId <- optional_params <- NULL
  
  if ( !is.null(byHousehold) ) {
    if ( !byHousehold %in% c("mean","sum","random") ) {
      stop("invalid value for argument 'byHousehold'. Allowed values are 'mean', 'sum' or 'random'!\n")
    }
  }
  
  samp <- simPopObj@sample
  pop <- simPopObj@pop
  basic <- simPopObj@basicHHvars
  if(by %in% c("strata", "none")){
    if(by == "none" & method[1] != "xgboost"){
      stop(paste0("by \"none\" not implemented for method ", method[1]))
    }
    strata <- samp@strata
  }else if(!is.null(by)){
    strata <- by
  }
  weight <- samp@weight
  
  dataS <- samp@data
  dataP <- pop@data
  
  if ( additional %in% names(dataP)) {
    stop(paste0("Variable '",additional,"' already available in the synthetic population!\n"))
  }
  
  ## initializations
  if ( !missing(seed) ) {
    set.seed(seed,"L'Ecuyer")  # set seed of random number generator
  }
  
  if ( length(additional) != 1 ) {
    stop("currently only one additional variable can be generated at a time")
  }
  if ( !additional %in% colnames(samp@data) ) {
    stop("variable 'additional' must be included in the sample of input 'simPopObj'!\n")
  }
  
  regInput <- regressionInput(simPopObj, 
                              additional=additional, 
                              regModel=regModel)
  predNames <- regInput[[1]]$predNames
  estimationModel <- regInput[[1]]$formula
  
  varNames <- unique(c(predNames, weight, additional, strata))
  if(!strata%in%colnames(dataS)){
    stop(strata," is defined as by variable, but not in the sample data set.")
  }
  dataS <- dataS[,varNames, with=FALSE]
  
  # method <- match.arg(method)
  zeros <- isTRUE(zeros)
  log <- isTRUE(log)
  if(log&&method %in% c("poisson", "xgboost")){
    log <- FALSE
    warning(paste0("For ", method," regression the log=TRUE parameter is ignored and the numeric variable is not transformed."))
  }
  if(!is.null(alpha) && method %in% c("poisson", "xgboost")){
    alpha <- NULL
    warning(paste0("For ", method ," regression the alpha!=NULL is not yet implemented and therefore set to NULL."))
  }
  if ( is.numeric(alpha) && length(alpha) > 0 ) {
    alpha <- rep(alpha, length.out=2)
    if ( !all(is.finite(alpha)) || any(alpha < 0) || sum(alpha) >= 1 ) {
      alpha <- NULL
      warning("invalid parameter 'alpha': trimming is not applied\n")
    }
  } else {
    alpha <- NULL
  }
  
  # observations with missings are excluded from simulation
  #exclude <- getExclude(dataS[,c(additional,predNames),with=F]) # fixes #31?
  #if ( length(exclude) ) {
  #  dataS <- dataS[-exclude,]
  #}
  
  # temporarily impute (using hotdeck) / or check (if imputeMissings=FALSE)
  # missing values in additional variables in the sample
  if ( is.null(strata) ) {
    modelVars <- setdiff(predNames, c(weight,basic,pop@hhsize))
  } else {
    modelVars <- setdiff(predNames, c(strata,weight,basic,pop@hhsize))
  }
  
  if ( length(modelVars) > 0 & imputeMissings ) {
    dataS_orig <- dataS[,modelVars,with=FALSE]
    dataS <- hotdeck(dataS, variable=modelVars, domain_var=strata, imp_var=FALSE)
  }
  
  # check for NAs and warn user
  if ( !imputeMissings) {
    naTab <- dataS[,lapply(.SD, is.na), .SDcols=c(additional,predNames)]
    perc.miss <- sum(rowSums(naTab)!=0) / nrow(dataS)
    if ( perc.miss > 0 ) {
      wm <- paste0("There are ~",formatC(100*perc.miss,format="f", digits=1),"% ")
      wm <- paste0(wm, "observations in the response/predictors with at least one missing variable.\n")
      wm <- paste0(wm, "If you get errors in the estimation procedure, consider to recode these missing ")
      wm <- paste0(wm, "values (e.g. by assigning an additional category) or try to specify a different model.\n\n")
      for ( z in 1:ncol(naTab) ) {
        vv <- colnames(naTab)[z]
        missv <- sum(naTab[[z]])
        missp <- formatC(100*missv/nrow(dataS),format="f", digits=1)
        if ( vv == additional ) {
          wm <- paste0(wm, "Variable '",vv,"' (response): ",missv," missing values (~",missp,"%).\n")
        } else {
          wm <- paste0(wm, "Variable '",vv,"' (predictor): ",missv," missing values (~",missp,"%).\n")
        }
      }
      if(verbose) warning(wm)
    }
  }
  
  # variables are coerced to factors
  select <- unique(c(predNames, strata)) # strata always included
  #  dataS <- checkFactor(dataS, select)
  if(!strata%in%colnames(dataP)){
    stop(strata," is defined as by variable, but not in the population data set.")
  }
  #  dataP <- checkFactor(dataP, select)
  
  # sample data of variable to be simulated
  additionalS <- dataS[[additional]]
  
  ## determine which models to fit and do further initialization
  haveBreaks <- !is.null(breaks)
  if ( method == "multinom" ) {
    useMultinom <- TRUE
    useLogit <- FALSE
    useLm <- FALSE
    usePoisson <- FALSE
    useXgboost <- FALSE
    # define break points (if missing)
    if ( haveBreaks ) {
      checkBreaks(breaks)
      breaks <- if(zeros) union(breaks, 0) else unique(breaks)
      breaks <- sort(breaks)
    } else {
      if ( is.null(upper) && gpd ) {
        upper <- Inf
      }
      weights <- NULL
      if(!is.null(weight)){
        weights <- dataS[[weight]]
      }
      breaks <- getBreaks(additionalS, weights=weights, zeros, lower, upper, equidist, probs)
    }
  } else {
    if(method=="lm"){
      useLm <- TRUE
      usePoisson <- FALSE
      useXgboost <- FALSE
    }else if(method=="poisson"){
      useLm <- FALSE
      usePoisson <- TRUE
      useXgboost <- FALSE
    }else if(method=="xgboost"){
      useMultinom <- FALSE
      useLm <- FALSE
      usePoisson <- FALSE
      useXgboost <- TRUE
    }
    
    
    if ( log ) {
      if ( is.null(const) ) {
        ## use log-transformation
        # check for negative values
        neg <- which(additionalS < 0)
        haveNeg <- length(neg) > 0
        if ( haveNeg ) {
          # define break points for negative values
          if ( haveBreaks ) {
            checkBreaks(breaks)
            breaks <- c(unique(breaks[breaks < 0]), 0)
          } else {
            breaks <- getBreaks(additionalS[neg], dataS[[weight]][neg], zeros=TRUE, lower, upper)
          }
          if ( zeros || length(breaks) > 2 ) {
            useMultinom <- TRUE
            breaks <- c(breaks, Inf)  # add Inf to breakpoints
          } else {
            useMultinom <- FALSE
          }
          useLogit <- !useMultinom
        } else {
          useLogit <- zeros || any(additionalS == 0)
          useMultinom <- FALSE
        }
      } else {
        # check constant
        if ( !is.numeric(const) || length(const) == 0 ) {
          stop("'const' must be numeric\n")
        } else {
          const <- const[1]
        }
        # set control parameters
        useLogit <- zeros || any(additionalS == 0)
        useMultinom <- FALSE
      } 
    } else if (method == "xgboost") {
      useLogit <- FALSE
      useMultinom <- FALSE
    } else {
      # logistic model is used in case of semi-continuous variable
      useLogit <- zeros
      # multinomial model is not needed
      useMultinom <- FALSE
    }
  }
  
  ## some general preparations for the simulation
  # list indStrata contains the indices of dataP split by strata
  N <- nrow(dataP)
  indP <- 1:N
  indStrata <- split(indP, dataP[[strata]])
  #fpred <- paste(predNames, collapse = " + ")  # for formula
  # check if population data contains factor levels that do not exist
  # in the sample
  newLevels <- lapply(predNames, function(nam) {
    levelsS <- levels(dataS[[nam]])
    levelsP <- levels(dataP[[nam]])
    levelsP[!(levelsP %in% levelsS)]
  })
  hasNewLevels <- sapply(newLevels, length) > 0
  excludeLevels <- any(hasNewLevels)
  
  ## preparations for multinomial or binomial logit model
  if ( useMultinom || useLogit ) {
    name <- getCatName(additional)
    estimationModel <- gsub(additional, name, estimationModel)
    # remove strata variable from estimation model if we are computing on multiple cores
    # else multinom fails because the variable has only one factor!
    if ( !is.null(dataS[[strata]]) ) {
      #      if ( parallelParameters(nr_cpus, length(levels(dataS[[strata]])))$nr_cores > 1 ) {
      estimationModel <- gsub(paste0("[+]",strata),"",estimationModel)
      #      }
    }
  }

  if ( useMultinom ) {
    ## some preparations
    dataS[[name]] <- getCat(additionalS, breaks, zeros, right=TRUE)
    response <- dataS[[name]]  # response variable
    # check threshold for GPD (if supplied)
    if ( !useLm && gpd && !is.null(threshold) && length(threshold) != 1 ) {
      stop("'threshold' must be a single numeric value")
    }
    
    ## simulate categories
    # TODO: share code with 'simCategorical'
    params <- list()
    params$excludeLevels <- excludeLevels
    # command needs to be constructed as string
    # this is actually a pretty ugly way of fitting the model
    params$command <- paste("suppressWarnings(multinom(", estimationModel,
                            ", weights=", weight, ", data=dataSample, trace=FALSE",
                            ", maxit=maxit, MaxNWts=MaxNWts))", sep="")
    params$maxit <- maxit
    params$MaxNWts <- MaxNWts
    params$eps <- eps
    params$limit <- limit
    params$censor <- censor
    params$hasNewLevels  <- hasNewLevels
    params$newLevels <- newLevels
    params$name <- name
    params$response <- response
    params$strata <- strata
    params$nr_cpus <- nr_cpus
    params$indStrata <- indStrata
    params$predNames <- predNames
    params$additional <- c(additional, weight)
    params$verbose <- verbose 
    params$by <- by
    if(verbose) cat("running multinom with the following model:\n")
    if(verbose) cat(gsub("))",")",gsub("suppressWarnings[(]","",params$command)),"\n")
    
    # run in parallel if possible
    valuesCat <- runModel(dataS, dataP, params, typ="multinom")
    
    ## simulate (semi-)continuous values
    tcat <- table(valuesCat)
    ncat <- length(tcat)
    
    icat <- 1:ncat
    values <- as.list(rep.int(NA, ncat))
    # zeros
    
    if ( zeros ) {
      # bug: missing 0 even though zeros is not null?
      izero <- which(breaks == 0)
      values[izero] <- 0
      tcat <- tcat[-izero]
      ncat <- length(tcat)
      icat <- icat[-izero]
    }
    
    # values to be simulated with linear model or draws from Pareto
    # distribution
    if ( useLm ) {
      # last breakpoint is Inf, the one before is 0
      nunif <- ncat - 1  # leave category of positive values out
    } else {
      nbreaks <- length(breaks)
      if ( gpd ) {
        if ( is.null(threshold) ) {
          if ( !haveBreaks && (!isTRUE(equidist) || !is.null(probs)) ) {
            ngpd <- nbreaks-2
          } else {
            ngpd <- nbreaks-1
          }
        } else if ( any(tmp <- breaks >= threshold) ) {
          ngpd <- min(which(tmp))
        } else {
          ngpd <- nbreaks
        }
      } else {
        ngpd <- nbreaks
      }
      if ( gpd && ngpd <= ncat ) {
        # adjust threshold and fit GPD
        threshold <- breaks[ngpd]  # adjust threshold
        estPar <- fitgpd(additionalS, threshold, est)  # fit GPD
        estPar <- estPar[["fitted.values"]]  # parameters of GPD
        # generalized pareto distribution
        igpd <- ngpd:ncat
        values[icat[igpd]] <- lapply(igpd, function(i) {
          truncPareto(tcat[i], loc=threshold, scale=estPar["scale"], shape=estPar["shape"], breaks[i], breaks[i+1])
        })
      }
      nunif <- ngpd - 1
    }
    # uniform distribution
    if ( nunif > 0 ) {
      iunif <- 1:nunif
      values[icat[iunif]] <- lapply(iunif, function(i) {
        runif(tcat[i], breaks[i], breaks[i+1])
      })
    }
    # turn list into vector of values
    values <- unsplit(values, valuesCat)
  }
  
  #############################################################################
  
  if ( useLogit ) {
    ## some preparations
    if ( log && is.null(const) && haveNeg ) {
      indS <- additionalS > 0
    } else {
      indS <- additionalS != 0
    }
    dataS[[name]] <- as.integer(indS)
    estimationModel <- as.formula(estimationModel)  # formula for model
    # auxiliary model for all strata (used in case of empty combinations)
    useAux <- !is.null(tol)
    if(method=="poisson"){
      useAux <- FALSE
    }
    if ( useAux ) {
      if ( length(tol) != 1 || tol <= 0 ) {
        stop("'tol' must be a single small positive value!\n")
      }
      #nas <- sum(!complete.cases(dataS))
      #if ( length(nas) > 0 ) {
      #  warning("\nwe Hotdeck-imputation of missing values sample data required!\n")
      #  dataS <- hotdeck(dataS, variable=predNames, domain_var=samp@strata, imp_var=FALSE)
      #}
      X <- model.matrix(estimationModel, data=dataS)
      y <- dataS[[name]]
      weights <- dataS[[weight]]
      mod <- logitreg(X, y, weights=weights)
      #########################################################################     
      
      # intercept is assumed to be included in 'x'
      # logitreg <- function(x, y, weights = rep(1, length(y)), start = rep(0, p), ...) {
        
      x=X
      y=y
      weights = weights

        # function to be minimized (log-likelihood)
        fmin <- function(beta, X, y, w) {
          p <- plogis(X %*% beta)
          -sum(2 * w * ifelse(y, log(p), log(1-p)))
        }
        # gradient
        gmin <- function(beta, X, y, w) {
          eta <- as.numeric(X %*% beta)
          p <- plogis(eta)
          -2 * (w * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))) %*% X
        }
        # some preparations
        if(is.null(dim(x))){
          dim(x) <- c(length(x), 1)
        }
        dn <- dimnames(x)[[2]]
        if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
        p <- ncol(x)
        if(is.factor(y)) y <- (unclass(y) != 1)
        # optimize and return result
        fit <- optim(start, fmin, gmin, X = x, y = y, w = weights, method = "BFGS", ...)
        names(fit$par) <- dn
        return(fit)
      }
      #########################################################################     
      par <- mod$par
    } else {
      par <- NULL
    }
    
    ## simulate binary vector
    params <- list()
    params$excludeLevels <- excludeLevels
    params$hasNewLevels <- hasNewLevels
    params$newLevels <- newLevels
    params$predNames <- predNames
    params$tol <- tol
    params$eps <- eps
    params$weight <- weight
    params$useAux <- useAux
    params$name <- name
    params$strata <- strata
    params$nr_cpus <- nr_cpus
    params$indStrata <- indStrata
    params$predNames <- predNames
    params$additional <- additional
    params$par <- par
    params$command <- estimationModel
    params$verbose <- verbose
    params$by <- by
    # run in parallel if possible
    
    valuesCat <- runModel(dataS, dataP, params, typ="binary")
  }
  
  ############################################################################
  
  if ( useLm || usePoisson || useXgboost) {
    ## some preparations
    if ( useMultinom ) {
      catLm <- names(tcat)[ncat]  # category for positive values
      dataS <- dataS[response == catLm, , drop=FALSE]
      indP <- valuesCat == catLm
    } else if( useLogit ) {
      dataS <- dataS[indS]  # select only non-zeros
      indP <- valuesCat == 1  # indicates non-zeros in population
    }
    if ( useMultinom || useLogit ) {
      # adjust population data
      if ( any(indP) ) {
        dataPop <- dataP[indP]
      } else {
        dataPop <- dataP
      }
      # list indStrata is adjusted so that it only contains
      # indices of persons in population with non-zero value
      indStrata <- split(1:nrow(dataPop), dataPop[[strata]])
    } else {
      dataPop <- dataP
    }
    ## trim data (if specified)
    if ( !is.null(alpha) ) {
      additional <- additional[1]
      additionalS <- dataS[[additional]]
      p <- c(alpha[1], 1-alpha[2])
      bounds <- quantileWt(additionalS, dataS[[weight]], p)
      select <- additionalS > bounds[1] & additionalS < bounds[2]
      dataSample <- dataS[select, , drop=FALSE]
      # check if all relevant levels of predictor variables are still
      # contained in sample after trimming
      # if not, trimming is not applied and a warning message is generated
      check <- unlist(sapply(predNames, function(i) {
        table(dataS[[i]]) > 0 & table(dataS[[i]]) == 0
      }))
      if ( any(check) ) {
        dataSample <- dataS
        warning("trimming could not be applied\n")
      }
    } else {
      dataSample <- dataS
    }
    
    ## fit linear model
    # formula for linear model
    if ( log ) {
      fname <- paste("log(", additional, if(!is.null(const)) " + const", ")", sep = "")
    } else {
      fname <- additional
    }
    fstring <- paste0(fname, " ~ ", tail(unlist(strsplit(as.character(estimationModel),"~")),1))
    formula <- as.formula(fstring)
    # auxiliary model for all strata (used in case of empty combinations)
    weights <- dataSample[[weight]]
    if(useLm){
      # TODO{Sironimo}: fix Fehler in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
      # contrasts can be applied only to factors with 2 or more levels
      # Question{Sironimo}: use of mod and coef? -> Not used at the moment, in my opinion
      mod <- lm(formula, weights=weights, data=dataSample,x=FALSE,y=FALSE,model=FALSE)
      coef <- coef(mod)
    }else if(usePoisson){
      mod <- glm(formula, weights=weights, data=dataSample,family=poisson(),model=FALSE,x=FALSE,y=FALSE)
      coef <- coef(mod)
    }
    
    
    # simulate values
    params <- list()
    params$coef <- coef
    if(useLm){
      params$command <- paste("lm(", fstring,", weights=", weight, ", data=dataSample,x=FALSE,y=FALSE,model=FALSE)", sep="")
    }else if(usePoisson){
      params$command <- paste("glm(", fstring,", weights=", weight, ", data=dataSample,family=poisson(),model=FALSE,x=FALSE,y=FALSE)", sep="")
    }else if(useXgboost){
      
      # simulation via xgboost
      if(verbose) cat("we are running xgboost:\n")
      
      # set xgb verbose level
      if(verbose){
        xgb_verbose <- 1
      }else{
        xgb_verbose <- 0
      }
      
      # set nr_cpus=1 for xgboost
      if(verbose){
        cat("\n Setting nr_cpus=1 when using xgboost\n")
      }
      nr_cpus <- 1
      
      if(TRUE){
        xgb_weight <- paste0(", info = list(\"weight\" = as.numeric(dataSample$", weight, "))")
      }else{
        xgb_weight <- ""
      }
      
      if( log ){
        log_transform <- "log"
      }else{
        log_transform <- ""
      }
      
      pred_names <- paste(predNames[predNames != strata], collapse = "\",\"")
      train <- paste0("xgb.DMatrix(data = model.matrix(~.+0,data = model.frame(setDT(dataSample)[,c(\"",pred_names,"\"), with=F],  na.action=na.pass)),
                                         missing = NA, label = ", log_transform, "(dataSample$",additional,")
                                        ", xgb_weight,")")
      
      # Default values
      nrounds <- 100
      early_stopping_rounds <- 10
      xgb_hyper_params <- "list(nthread = 6,
                                eta = 0.1,
                                max_depth = 32,
                                min_child_weight = 0,
                                gamma = 0,
                                subsample = 1,
                                lambda = 0,
                                objective = \"reg:squarederror\",
                                eval_metric = \"rmse\")"
      
      if(!is.null(model_params)){
        
        xgb_hyper_params <- "params$model_params"
        
        if(!is.null(optional_params$nrounds)){
          nrounds <- optional_params$nrounds
        }
        
        if(!is.null(optional_params$early_stopping_rounds)){
          early_stopping_rounds <- optional_params$early_stopping_rounds
        }
      }
      
      xgb_params <- paste0("nrounds = ", nrounds,",
                            watchlist = list(train = ", train, ",
                                             test = ", train, "), 
                            early_stopping_rounds = ", early_stopping_rounds,",
                            print_every_n = 10,")
      
      command <- paste0("xgb.train(",train, ", ",
                        xgb_params,
                        "verbose = ", xgb_verbose, ", ",
                        "params = ", xgb_hyper_params, ")")
      
      params$command <- command
    }
    
    #params$name <- fname
    params$name <- additional
    params$excludeLevels <- excludeLevels
    params$hasNewLevels <- hasNewLevels
    params$newLevels <- newLevels
    params$predNames <- predNames
    params$additional <- c(additional, weight)
    params$const <- const
    params$formula <- formula
    params$residuals <- residuals
    params$log <- log
    params$strata <- strata
    params$nr_cpus <- nr_cpus
    params$indStrata <- indStrata
    params$predNames <- predNames
    params$verbose <- verbose
    params$by <- by
    params$model_params <- model_params
    if(useLm){
      valuesTmp <- runModel(dataSample, dataPop, params, typ="lm")
    }else if(usePoisson){
      valuesTmp <- runModel(dataSample, dataPop, params, typ="poisson")
    }else if(useXgboost){
      valuesTmp <- runModel(dataSample, dataPop, params, typ="xgboost")
    }
    
    ## put simulated values together
    if ( useMultinom ) {
      values[which(indP == 1)] <- valuesTmp
    } else {
      if ( useLogit ) {
        if ( log && is.null(const) && haveNeg ) {
          # only one category for non-positive values (two breakpoints, one of them is 0)
          values <- rep.int(NA, N)
          values[indP] <- runif(sum(indP), breaks[1], breaks[2])
        } else {
          values <- ifelse(is.na(indP), NA, 0) # only zeros
        }
        values[indP] <- valuesTmp
      } else {
        values <- valuesTmp
      }
    }
  }
  
  # reset imputed variables in sample
  if ( imputeMissings ) {
    for ( i in 1:ncol(dataS_orig)) {
      cmd <- paste0("dataS[,",colnames(dataS_orig)[i],":=dataS_orig$",colnames(dataS_orig)[i],"]")
      eval(parse(text=cmd))
    }
  }
  
  # attach new variable(s) to population data
  if ( useMultinom && keep ) {
    dataP[[name]] <- valuesCat
  }
  
  # calculate mean of new variable by household
  if ( !is.null(byHousehold) ) {
    xx <- data.table(id=1:length(values), hhid=dataP[[pop@hhid]], vals=values)
    setkey(xx, hhid)
    
    if ( byHousehold=="mean" ) {
      yy <- xx[,mean(vals, na.rm=TRUE), by=key(xx)]
    }
    if ( byHousehold=="sum" ) {
      yy <- xx[,sum(vals, na.rm=TRUE), by=key(xx)]
    }
    if ( byHousehold=="random" ) {
      zz <- xx[,.N,by=key(xx)]
      ids1 <- zz[N==1]
      yy <- xx[hhid%in%ids1$hhid]
      yy[,id:=NULL]
      ids2 <- zz[N>1]
      if ( nrow(ids2) > 0 ) {
        xx2 <- xx[hhid %in% ids2$hhid]
        xx2[,randId:=sample(1:nrow(xx2))]
        setkey(xx2, hhid, randId)
        setkey(xx2, hhid)
        yy2 <- unique(xx2)
        yy2[,c("id","randId"):=NULL]
        yy <- rbind(yy, yy2)
      }
      setkey(yy, hhid)
      yy[,V1:=vals]
      yy[,vals:=NULL]
    }
    xx <- merge(xx, yy, all.x=TRUE)
    setkey(xx, id)
    xx[is.nan(V1), V1:=NA]
    values <- xx$V1
  }
  
  # return simulated data
  dataP[[additional]] <- values
  simPopObj@pop@data <- dataP
  invisible(simPopObj)
}


