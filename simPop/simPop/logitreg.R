# intercept is assumed to be included in 'x'
logitreg <- function(x, y, weights = rep(1, length(y)), start = rep(0, p), ...) {
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