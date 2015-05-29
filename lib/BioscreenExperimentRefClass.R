#' BioscreenExperiment ReferenceClass Object
#' 
#' @field file Raw data file the object is loaded from
#' @field expt Experiment details
#' @field settings Runtime settings information
#' @field info Culture information
#' @field data Growth curve data
#' @field params Fitted parameters, populated after a call to \code{$calcParams()}
#' 
#' @export BioscreenExperiment
#' @exportClass BioscreenExperiment
BioscreenExperiment = setRefClass('BioscreenExperiment',
  fields=list(
      srcPath='character',
      file='character',
      expt='data.frame',
      settings='data.frame',
      info='data.frame',
      data='data.frame',
      params='list'
    ),
  methods=list(
      load = function(...) {
        'Loads data from a file or data source'
        
        args = list(...)
        
        loader = NA
        if (!is.null(args$file)) {
          # determine file type based on extension
          file.ext = sub('^(.*?)\\.(.*?)$', '\\2', args$file)
          
          if (!nzchar(file.ext)) {
            stop('Could not determine file type')
          }
          
          file.loaders = list(xlsx=c('xlsx', 'xls'), 
                              text=c('csv', 'tsv', 'txt'))
          loader = names(which(sapply(file.loaders, function(types) { file.ext %in% types })))
          
          loader = switch(loader,
                          xlsx = .self$loadXlsx,
                          text = .self$loadText)
        }
        
        # if (!is.null(args$lims)) {
        #   
        # }
        
        if (!is.function(loader)) {
          stop('data type not supported')
        } else {
          loader(...)
        }
        
        invisible(NULL)
      },
      
      loadXlsx = function(file, time.unit=c('sec', 'min', 'hr'), ...) {
        'Loads data from an xlsx file'
        require(xlsx)
        
        args = list(...)
        
        message('Loading data from xls[x] file')
        # read compiled experiment xlsx file
        
        # validate file
        message('checking file ... ', appendLF=F)
        status = checkXlsxFile(file)
        if (!status$ok) {
          stop(status$message)
        }
        message('ok')
        
        .self$file = file
        
        ## process expt
        message('getting experiment info')
        .expt = read.xlsx(file, sheetName='expt', stringsAsFactors=F)
        rownames(.expt) = .expt$Name
        .self$expt = .expt
        
        equip.ids = unlist(strsplit(.self$expt['Equipment', 'Value'], ','))
        data.names = paste0('data-', equip.ids)  
        
        ## process settings
        message('getting settings info')
        .self$settings = read.xlsx(file, sheetName='settings', stringsAsFactors=F)
        
        ## process info
        message('getting well info')
        .info = read.xlsx(file, sheetName='info', stringsAsFactors=F)
        
        # create a well map to identify which row/column a well position is in
        well.map = matrix(paste0('Well.', 101:300), nrow=10, byrow=F)
        RC = t(sapply(.info$position, function(p) which(well.map == p, arr.ind=T), USE.NAMES=F))
        colnames(RC) = c('row', 'col')
        
        .self$info = cbind(id=sprintf('BS%d.%s', .info$bsid, .info$position), .info, RC, stringsAsFactors=F)
        
        ## process data sets
        message('getting well data')
        require(reshape)
        D = sapply(data.names, function(sheet.name) {
          
          if (length(equip.ids) == 1) {
            d = tryCatch({
              read.xlsx2(file, sheetName=sheet.name, colClasses=NA, stringsAsFactors=F)
            }, error = function(e) {
              message('.. one data set expected, but "', sheet.name, '" not found. trying "data" instead.')
              x = try(read.xlsx2(file, sheetName='data', colClasses=NA, stringsAsFactors=F), silent = T)
              
              if (inherits(x, 'try-error')) {
                stop('no data found')
              }
              
              return(x)
            })
          } else {
            d = read.xlsx2(file, sheetName=sheet.name, colClasses=NA, stringsAsFactors=F)  
          }
          
          # determine the scaling factor for time based on units
          # the typical default is `sec`
          time.divider = switch(time.unit[1],
                                sec = 3600,
                                min = 60,
                                1)
          
          # excel converts the time automatically
          d$Time = as.numeric(d$Time - d$Time[1])/time.divider
          
          # add a column to the data that indicates the equipment it was generated on
          equip.id = as.numeric(sub('(.*?)-([0-9])$', '\\2', sheet.name))
          colnames(d) = c('Time', sprintf('BS%d.%s', equip.id, colnames(d)[-1]))
          d = cbind(bsid=equip.id, d)
          
          # melt into records (long) format so that the entire dataset can be stacked
          d = melt(d, id.vars=c('bsid', 'Time'), variable_name='id')
          d$id = as.character(d$id)
          
          return(d)
        }, simplify=F)
        
        .self$data = do.call(rbind, D)
        
        message('DONE')
      },
      checkXlsxFile = function(file) {
        status = list(ok=T, message=NA)
        
        wb = loadWorkbook(file)
        sheets = getSheets(wb)
        sheet.names = tolower(names(sheets))
        
        testParts = list(hasExpt     = 'expt' %in% sheet.names,
                         hasSettings = 'settings' %in% sheet.names, 
                         hasInfo     = 'info' %in% sheet.names,
                         hasData     = any(grepl('data', sheet.names)))
        
        hasAllParts = all(sapply(testParts, identity))
        if (!hasAllParts) {
          status$ok = hasAllParts
          status$message = 'file is missing required parts'
          
          return(status)
        }
        
        # check for correct number of datasets
        # get a data.frame of named ranges and their sheets
        ranges = getRanges(wb)
        named.ranges = data.frame(name  = sapply(ranges, .jcall, 'S', 'getNameName'),
                                  sheet = sapply(ranges, .jcall, 'S', 'getSheetName'),
                                  stringsAsFactors=F)
        
        range.expt = subset(named.ranges, name == 'expt')
        range.id = as.numeric(rownames(range.expt))
        
        .expt = sapply(readRange(ranges[[range.id]], sheets[[range.expt$sheet[1]]]), as.character)
        colnames(.expt) = .expt[1,]
        rownames(.expt) = .expt[,1]
        .expt = .expt[-1,]
        
        equip = unlist(strsplit(.expt['Equipment','Value'], ','))
        
        # seems memory leaky to do it this way but this is the easiest
        # way to parse the expt table
        # expt = read.xlsx(file, sheetName='expt')
        
        testData = list(
          hasCorrectNumData  = length(equip) == length(grep('data', sheet.names)),
          hasCorrectNameData = {
            length(equip) == length(sapply(paste0('data-', equip), grep, names(sheets))) ||
              (length(equip) == 1 && any(grepl('data(-1)*', sheet.names)))
          }
        )
        hasAllData = all(sapply(testData, identity))
        
        if (!hasAllData) {
          status$ok = hasAllData
          status$message = 'file is missing required data'
          
          return(status)
        }
        
        return(status)
      },
      
      loadText = function(file, delim='\t', ...) {
        'Loads data from a delimited text file'
        
        message('Loading data from delimited text file')
        # not implemented yet ...
      },
      checkTextFile = function(file) {},
      
      loadLIMS = function(...) {
        'Loads data from LIMS'
        # currently not supported
      },
      
      getData = function(id=NULL, ...) {
        'Retrieves either the full dataset or data for specific well(s)'
        DS = merge(info, data, by=c('id', 'bsid'))
        ox = order(DS$id, DS$Time)
        DS = DS[ox,]
        if (is.null(id)) {
          # merge info and data
          return(DS)
        } else {
          if (sum(!id %in% DS$id) > 0) {
            warning('id(s) ', paste(id[!id %in% DS$id], collapse=', '), ' not found')
          }
          return(DS[which(DS$id %in% id), , drop=F])
        }
      },
      getIDs = function(type=c('s', 'm', 'e', 'n', 'c')) {
        'Retrieves sample IDs'
        if (is.null(type) || missing(type)) {
          return(unique(info$id))
        }
        
        type = match.arg(type)
        return(unique(info$id[info$type == type]))
      },
      
      calcParams = function(method='ModelFit', ...) {
        'Calculates growth parameters using various algorithms'
        
        method = match.arg(method)
        method.fun = switch(method,
                            MaxRSq = .self$calcParamsMaxRSq,
                            ModelFit = .self$calcParamsModelFit,
                            NA)
        
        if (!is.function(method.fun)) {
          stop('unrecognized parameter estimation method: ', method)
        }
        
        method.fun(...)
      },
      getParams = function(...) {
        'Retrieves calculated parameters aligned with sample info'
        if (is.null(.self$params$method)) {
          stop('parameters have not been calculated yet')
        }
        
        return(eval(parse(text=sprintf('.self$getParams%s(...)', .self$params$method))))
      },
      
      calcParamsModelFit = function(model, ids=NULL, ix.fun=NULL, xform.fun=identity, smooth.input = FALSE, ...){
        'Calculates growth curve parameters via non-linear model fitting'
        library(genoRSmallScale)
        library(genoRUtils)
        
        # source(file.path(.self$srcPath, 'NLMFit.R'), local=T)
        
        if (missing(model)) {
          stop('Model must be specified')
        }
        
        if (!inherits(model, 'ModelObject')) {
          stop('Model must be derived from the ModelObject class')
        }
        
        message('Computing growth parameters via ModelFit(', model$name, ')')
        
        if (is.null(ids)) {
          # if ids are not provided compute over all the 's' wells
          ids = getIDs('s')
        } else {
          if (!all(ids %in% getIDs())) {
            lIDsFound = ids %in% getIDs()
            # check that some ids have been found
            if (sum(lIDsFound) < 1) {
              stop('ids provided do not match any in dataset')
            } else {
              # exclude ids not found with a warning
              warning('ids not found: ', paste(ids[!lIDsFound], collapse=','))
              ids = ids[lIDsFound]
            }
          }
        }
        
        if (is.null(ix.fun) || !is.function(ix.fun)) {
          message('No index selection specified. Fitting all data points.')
          
          # default index selection function attempts to select all data points
          ix.fun = function(x, ...) {
            if (is.recursive(x)) {
              if ('x' %in% names(x)) {
                return(1:length(x$x))
              } else {
                stop('data is of type list() but not xy.coords compatible')
              }
              
            } else if (is.atomic(x)) {
              return(1:length(x))
            } else {
              stop('unhandled data type:', class(x))
            }
          }
        } else {
          message('Using user specified index selection.')
        }
        
        if (identical(identity, xform.fun)) {
          message('No data transformation specified. Using identity() on y-values.')
          xmodel = deepcopy(model)
        } else {
          message('Using user specified data transformation')
          xmodel = wrap_(model, fun = deparse(substitute(xform.fun)))
        }
        
        RES = sapply(ids, function(id) {
          message(id, ': ', appendLF = F)
          conditions = list()
          
          args = list(...)
          progress_callback = args$progress_callback
          if (!is.null(progress_callback) && is.function(progress_callback)) {
            progress_callback(id, ids)
          }
          
          # sanitize args of arguments not used by ModelObjetFit()
          args = args[names(args) %in% names(formals(ModelObjectFit))]
          
          .data = list(Y = NULL, smooth = NULL)
          gc = with(getData(id), {
            # ensure data is correctly ordered by time
            ox = order(Time)
            list(x=Time[ox], y=value[ox])
          })
          
          if (smooth.input) {
            message('(input smoothed) ', appendLF = F)
            ss = pangr::smooth.adaptive.loess(gc)
            .data$smooth = list(span = ss$span, objective = ss$objective)
            
            gc = list(x=ss$x, y=ss$y)
          }
          
          .data$Y = gc
          
          # if the default ix.fun fails, there are problems and the
          # fit should be allowed to stop.
          # user supplied ix.fun should return an ix range and an
          # optional condition attribute if there were problems.
          ix = ix.fun(gc, model, id=id)
          if (!is.null(attr(ix, 'condition'))) {
            conditions = c(conditions, list(attr(ix, 'condition')))
          }
          
          # apply data transformation
          xgc = with(gc, { list(x=x, y=xform.fun(y)) })
          
          fit = try({
              do.call(
                ModelObjectFit, 
                c(list(data=as.data.frame(xgc)[ix,], MODEL=xmodel), args)
              )
            }, silent = T)
          
          if (inherits(fit, 'try-error')) {
            fit = list(message=paste('[failed]', attr(fit, 'condition')$message))
          } else {
            message(sprintf('[success] rmsd: %f .. ', sqrt(mean(fit$residuals^2))), appendLF = length(fit$message) > 1)
            fit$MODEL = deepcopy(model)
            fit$MODEL$q = fit$par
          }
          
          if (length(fit$message) > 1) {
            lapply(paste('.. ', names(fit$message), fit$message, sep=': '), message)
          } else {
            message(fit$message)
          }
          
          
          out = list(data=.data, indices=ix, fit=fit, conditions=conditions)
          
          return(out)
        }, simplify=F)
        message('DONE')
        
        if (is.null(.self$params$results)) {
          # default: create new params list
          .self$params = list(method='ModelFit', results=RES)
          
        } else {
          # overwrite or append ids in/to existing params
          .self$params$results[ids] = RES
          
        }
        
      },
      getParamsModelFit = function(export=c('sample', 'full')) {
        'Retrieves calculated parameters (via ModelFit) aligned with sample info'
        
        RES = .self$params$results
        TAB = sapply(RES, function(res) {
          with(res, {
            # collect messages from fitting process
            msg = ''
            if (length(conditions) > 0) {
              msg = sapply(conditions, function(cond) {
                if (inherits(cond, 'error')) {
                  return(sprintf('error: %s', conditionMessage(cond)))
                }
                
                if (inherits(cond, 'warning')) {
                  return(sprintf('warning: %s', conditionMessage(cond)))
                }
                
                if (inherits(cond, 'message')) {
                  return(sprintf('message: %s', conditionMessage(cond)))
                }
              })
              
              msg = paste(msg, collapse='; ')
            }
            
            out = c(list(indices     = paste(indices, collapse=','),
                         model       = fit$MODEL$name,
                         equation    = as.character(fit$MODEL$expr),
                         rmsd        = sqrt(mean(fit$residuals^2)), 
                         optimizer   = fit$optimizer,
                         convergence = paste(paste0(sprintf('%s: ', names(fit$message)), fit$message), collapse='\n'),
                         messages    = msg), 
                    as.list(fit$par))
            
            data.frame(out, stringsAsFactors=F)
          })
          
        }, simplify=F)
        
        OUT = merge(.self$info, cbind(id=names(RES), rbind_all(TAB)), by='id') %>% tbl_df
        
        return(OUT)
      },
      plotResultModelFit = function(id, ytrans='log') {
        'Plots a ModelFit() result'
        
        # plot the original data
        # overlay the predicted model
        # highlight the points used to fit the model
        # plot the residuals
        # annotate the model used
        # annotate the RMSD
        
        library(ggplot2)
        
        res = .self$params$results[[id]]
        plt = with(res, {
          xy = cbind(.self$getData(id=id), isfitted=FALSE)
          xy = xy[order(xy$Time),]
          xy$isfitted[indices] = TRUE
          
          xyhat = data.frame(x=data$Y$x, y=fit$MODEL$value(as.list(fit$par), data$Y))
          
          ggp = ggplot() + ggtitle(id) +
            geom_point(data=xy, mapping=aes(x=Time, y=value, color=isfitted)) + 
            scale_color_manual(values=c(`FALSE`='black', `TRUE`='red')) +
            
            scale_y_continuous(
              breaks=pretty(seq(0.01, 2, length.out = 10), n = 8),
              limits=c(0.01, 2)
            ) +
            coord_trans(y=ytrans) + 
            
            geom_line(data=xyhat, mapping=aes(x=x, y=y), color='blue') +
            annotate('text', x=0, y=2, 
                     label=paste(sprintf('ModelFit: %s', fit$MODEL$name),
                                 sprintf('Optimizer: %s', fit$optimizer),
                                 sprintf('Convergence: %s', paste(paste0(sprintf('\n...%s: ', names(fit$message)), fit$message), collapse='')),
                                 sprintf('RMSD: %.4f', sqrt(mean(fit$residuals^2))),
                                 sep='\n'), 
                     hjust=0, vjust=1, size=3)
          
          ggp
        })
        
        return(plt)
        
      }
      
    ))