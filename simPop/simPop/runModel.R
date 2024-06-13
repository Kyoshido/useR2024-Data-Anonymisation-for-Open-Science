runModel <- function(dataS, dataP, params, typ) {
  x <- NULL
  strata <- params$strata
  pp <- parallelParameters(nr_cpus=params$nr_cpus, nr_strata=length(levels(dataS[[strata]])))
  indStrata <- params$indStrata
  predNames <- params$predNames
  additional <- unique(c(params$additional, params$name, params$weight))
  by <- params$by
  
  if(is.null(by) |  by != "none"){
    if ( pp$parallel ) {
      # windows
      if ( pp$have_win ) {
        cl <- makePSOCKcluster(pp$nr_cores)
        registerDoParallel(cl,cores=pp$nr_cores)
        valuesCat <- foreach(x=levels(dataS[[strata]]), .options.snow=list(preschedule=TRUE)) %dopar% {
          genVals(
            dataSample=dataS[dataS[[strata]] == x,],
            dataPop=dataP[indStrata[[x]], predNames, with=FALSE],
            params,response=dataS[dataS[[strata]] == x,eval(parse(text=params$name))],
            typ=typ)
        }
        stopCluster(cl)
      }
      # linux/mac
      if ( !pp$have_win ) {
        valuesCat <- mclapply(levels(dataS[[strata]]), function(x) {
          genVals(
            dataSample=dataS[dataS[[strata]] == x,],
            dataPop=dataP[indStrata[[x]], predNames, with=FALSE],
            params,response=dataS[dataS[[strata]] == x,eval(parse(text=params$name))],
            typ=typ)
        },mc.cores=pp$nr_cores)
      }
    } else {
      if(params$verbose){
        cat("All values of the by group:","\n")
        print(levels(dataS[[strata]]))
      }
      valuesCat <- lapply(levels(dataS[[strata]]), function(x) {
        if(params$verbose){
          cat("Current by group for the binary model:",x,"\n")
        }
        genVals(
          dataSample=dataS[dataS[[strata]] == x,c(predNames, additional), with=FALSE],
          dataPop=dataP[indStrata[[x]], predNames, with=FALSE],
          params,response=dataS[dataS[[strata]] == x,eval(parse(text=params$name))],
          typ=typ)
      })
    }
    
    res <- sapply(valuesCat, class)
    if ( any(res=="try-error") ) {
      stop(paste0("Error in estimating the linear model. Try to specify a more simple model!\n"))
    }
    if ( typ=="multinom" ) {
      response <- dataS[[params$name]]
      valuesCat <- factor(unsplit(valuesCat, dataP[[strata]]), levels=levels(response))
    }
    if ( typ=="binary" ) {
      valuesCat <- unsplit(valuesCat, dataP[[strata]], drop=FALSE)
    }
    if ( typ%in%c("poisson","lm","xgboost") ) {
      valuesCat <- unsplit(valuesCat, dataP[[strata]], drop=FALSE)
    }
    
  }else{
    
    valuesCat <- genVals(dataSample=dataS,
                         dataPop=dataP,
                         params,
                         response=dataS[, eval(parse(text=params$name))],
                         typ=typ)
    
    res <- sapply(valuesCat, class)
    if ( any(res=="try-error") ) {
      stop(paste0("Error in estimating the linear model. Try to specify a more simple model!\n"))
    }
    if (!typ %in% "xgboost"){
      stop(paste0("Strata with type ", strata, "only implemented for typ xgboost, not",
                  typ))
    }
  }
  
  # check for errors
  
  
  return(valuesCat)
}