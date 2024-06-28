# prepare input for regression-based estimation methods
# returns a list with required independent variables and the model formula
# regModel can be either 'basic', 'availabe' or a formula
regressionInput <- function(obj, additional, regModel) {
  cn_pop <- colnames(popData(obj))
  cn_samp <- colnames(sampleData(obj))
  
  if ( length(additional) == 1 & inherits(regModel,"formula") ) {
    regModel <- list(regModel)
  }
  
  if ( length(additional) != length(regModel) ) {
    stop("makeRegInput:: dimensions do not match!\n")
  }
  
  if ( length(additional) == 1 & inherits(regModel,"formula") ) {
    regModel <- list(regModel)
  }
  
  regInput <- list(); length(regInput) <- length(additional)
  names(regInput) <- additional
  for ( i in seq_along(additional) ) {
    if ( inherits(regModel[[i]], "formula" ) ){
      # check the formula
      fmt <- paste0(as.character(regModel[[i]]),collapse="")
      if ( substr(fmt,1,1)=="~" ) {
        regModel[[i]] <- paste0(additional[i], fmt)
        regf <- as.formula(regModel[[i]])
      }
      regvars <- all.vars(regf)
      if ( regvars[1] != additional[i] ) {
        stop("dependent variable in model-formula does not match with variable name specified in 'additional'!\n")
      }
      regvars <- regvars[-1]
      ii <- regvars %in% cn_pop
      if ( !all(ii) ) {
        stop(paste0("Some variables (", paste0(regvars[!ii], collapse=", "), ") required for the regression model are not available in the synthetic population!\n"))
      }
      ii <- regvars %in% cn_samp
      if ( !all(ii) ) {
        stop(paste0("Some variables (", paste0(regvars[!ii], collapse=", "), ") required for the regression model are not available in the sample!\n"))
      }
      regInput[[i]]$predNames <- regvars
      regInput[[i]]$formula <- regModel[[i]]
    } else {
      if ( !regModel[i] %in% c("basic","available") ) {
        stop("regModel[",i,"] is neither a formula nor 'basic' or 'available'!\n")
      }
      if ( regModel[[i]] == "basic" ) {
        regInput[[i]] <- list()
        # get basic household variables and hh sizes
        regInput[[i]]$predNames <- c(obj@basicHHvars, sampleObj(obj)@hhsize)
        regInput[[i]]$formula <- paste0(additional[i],"~", paste(regInput[[i]]$predNames, collapse="+"))
      }
      if ( regModel[[i]] == "available" ) {
        # get all available variables from sample/pop (excluding ids,...)
        regInput[[i]]$predNames <- setdiff(cn_pop, c(popObj(obj)@hhid, popObj(obj)@pid, popObj(obj)@weight))
        # we also use variables previously generated
        if ( i > 1 ) {
          regInput[[i]]$predNames <- c(regInput[[i]]$predNames, additional[1:(i-1)])
        }
        regInput[[i]]$formula <- paste0(additional[i],"~", paste(regInput[[i]]$predNames, collapse="+"))
      }
    }
  }
  return(regInput)
}
