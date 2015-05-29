#' Expand growth curve fitting region
#' 
#' Iterative expands a fitting region based on rmse values of previous fit
#' @param xy A growth curve as an xy.coords compatible object
#' @param ix Initial x-indices to fit
#' @param model Growth curve model (ModelObject) to fit. If NULL a simple exponential is used
#' @param rmse.init Initial rmse value. If NULL is guessed
#' @param rmse.tol.f RMSE tolerance factor
#' @param bound Which way to expand the fitting region
#' @param n.ext Number of data points to expand the region in each iteration
#' @param iter.max Maximum number of expansion iterations to perform
#' @param verbose Flag to print messages to console
#' 
#' @import pangr
expandix = function(xy, ix, model=NULL, rmse.init=NULL, rmse.tol.f, bound=c('upper', 'lower'), n.ext=10, iter.max=100, verbose=F) {
  # uses local model fitting to expand the fitted data range of a growth curve
  require(pangr)  # provides smooth.adaptive.loess()
  
  if (is.null(model)) {
    # assume a simple exponential
    model = ModelObject(
      name = 'exp',
      expr = expression( y0*exp(u*(x-tl)) ),
      P = list(p0 = c(u=0.3, y0=0.1, tl=10), 
               lb = c(u=0,   y0=0,   tl=0), 
               ub = c(u=1,   y0=0.5, tl=Inf))
    )
  }
  
  # use initial guess to estimate the amount of error in the data via smoothing
  if (!is.null(rmse.init)) {
    rmse = rmse.init
  }
  else{
    rmse = log(xy$y[ix]) - log(smooth.adaptive.loess(xy)$y[ix])
    rmse = sqrt(mean(rmse^2))
    rmse.init = rmse
  }
  
  is.eod = FALSE
  ix.new = ix
  iter = 0
  
  rmse.tol = rmse * rmse.tol.f
  
  # expansion is done on log transformed data
  # ideally this transformation should be parameterized
  xy$y = log(xy$y)
  model = wrap(model, fun=log)
  
  bound = tolower(match.arg(bound))
  status = list(ok=TRUE, message='')
  
  if (verbose) {
    message('expanding fitted indices')
    message('[begin] rmse: ', sprintf('%.5f', rmse), '; rmse.tol: ', sprintf('%.5f', rmse.tol), '; bound: ', bound)
  }
  
  while(status$ok) {
    
    if (bound == 'upper') {
      ix.ext = max(ix.new):(max(ix.new)+n.ext)
      is.eod = max(ix.ext) > length(xy$x)
    } else if (bound == 'lower') {
      ix.ext = (min(ix.new)-n.ext):min(ix.new)
      is.eod = min(ix.ext) < 1
    } else {
      stop('unhandled condition')
    }
    
    if (is.eod) {
      
      ix.ext = switch(
        bound,
          upper = unique(max(ix):length(xy$x)),
          lower = unique(1:min(ix))
        )
      
      n.ext = length(ix.ext) - 1
      
      status$ok = FALSE
      status$message = '.. end of data reached'
      
      if (verbose) message(status$message)
    }
    
    ix.test = sort(union(ix.new, ix.ext))
    mf = ModelObjectFit(
      as.data.frame(xy)[ix.test,], model, optimizer='nlminb', 
      weightFun = function(data) {
        with(data,{
          sqrt(rev((1:length(x))/length(x)))
        })
      }
    )
    is.deviating = switch(
      bound,
        upper = {
          # check for negative deviations on the upper bound
          if (n.ext > 2) {
            sum(rev(mf$residuals)[1:n.ext] < -rmse.tol) > floor(n.ext/2)
          } else {
            any(rev(mf$residuals)[1:n.ext] < -rmse.tol)
          }
        },
        lower = {
          # check for positive deviations on the lower bound
          if (n.ext > 2) {
            sum(    mf$residuals[1:n.ext]  >  rmse.tol) > floor(n.ext/2)
          } else {
            any(    mf$residuals[1:n.ext]  >  rmse.tol)
          }
        }
      )
    
    if (is.deviating) {
      # try reducing the expansion
      if (verbose) message('.. reducing expansion')
      n.ext = n.ext - 1
      
      # status$ok = FALSE
      # status$message = 'extension is deviating'
      
    } else {
      ix.new = ix.test
      
      # adapt the rmse tolerance to the current fit
      rmse.new = sqrt(mean(mf$residuals^2))
      if (rmse.new <= rmse) {
        rmse = rmse.new
        rmse.tol = rmse*rmse.tol.f
        if (verbose) message('.. adapting rmse tolerance: ', sprintf('%.5f', rmse), ' (tol: ', sprintf('%.5f', rmse.tol), ')')
      }
      
      iter = iter + 1
    }
    
    if (n.ext < 1) {
      status$ok = FALSE
      status$message = '[stop] extension reduced to 0'
      if (verbose) message(status$message)
    }
    
    if(iter > iter.max) {
      status$ok = FALSE
      status$message = '[stop] iteration limit exceeded'
      if (verbose) message(status$message)
    }
  }
  
  attr(ix.new, 'rmse.tol') = rmse.tol
  attr(ix.new, 'iter') = iter
  attr(ix.new, 'npts') = length(ix.new)-length(ix)
  attr(ix.new, 'message') = status$message
  
  ix = ix.new
  return(ix)
}