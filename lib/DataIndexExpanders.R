# function factory to produce ix.fun functions
ix.fun.factory = function(rmse.tol.f=c(upper=1, lower=2.5), guess.fun=NULL) {
  
  # determine which bounds to expand to
  bounds = names(rmse.tol.f)
  if (length(bounds) < 1 || !any(c('upper', 'lower') %in% bounds)) {
    stop('at least one expansion bound (upper,lower) must be specified')
  }
  
  if (!is.null(guess.fun) && !is.function(guess.fun)) {
    stop('guess.fun is not a function')
  }
  
  fun = function(xy, model, ...) {
    condition = NULL
    
    ix0 = which(xy$x >= 1 & xy$y <=0.6)  # typical area for growth phase
    if (is.function(guess.fun)) {
      ix0 = guess.fun(xy, ...)
    }
    
    ix = ix0
    
    # use initial guess to estimate the amount of error in the data via smoothing
    rmse = log(xy$y[ix]) - log(smooth.adaptive.loess(xy)$y[ix])
    rmse = sqrt(mean(rmse^2))
    
    if ('upper' %in% bounds && rmse.tol.f['upper'] > 0) {
      ix = expandix(xy, ix, model, rmse.tol.f=rmse.tol.f['upper'], bound='upper', verbose=T)
      pts.ub = attr(ix, 'npts')
      rmse.tol.ub = attr(ix, 'rmse.tol')
      attributes(ix) = NULL
    } else {
      pts.ub = NA
      rmse.tol.ub = NA
    }
    
    if ('lower' %in% bounds && rmse.tol.f['lower'] > 0) {
      ix = expandix(xy, ix, model, rmse.init = rmse, rmse.tol.f=rmse.tol.f['lower'], bound='lower', verbose=T)
      pts.lb = attr(ix, 'npts')
      rmse.tol.lb = attr(ix, 'rmse.tol')
      attributes(ix) = NULL
    } else {
      pts.lb = NA
      rmse.tol.lb = NA
    }
    
    attr(ix, 'init') = ix0
    attr(ix, 'rmse.tol') = c(lb=rmse.tol.lb, ub=rmse.tol.ub)
    attr(ix, 'expansion') = c(lb=pts.lb, ub=pts.ub)
    attr(ix, 'condition') = condition
    
    ix = ix[which(ix >= min(which(xy$x > 1)))]
  }
  
  return(fun)
}
