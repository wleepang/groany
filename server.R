
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# create input tags for parameter guess and bounds
param_inputs = function(mo) {
  b = sapply(names(mo$P$p0), function(n) {
    with(mo$P, {
      setNames(c(p0[n], lb[n], ub[n]), c('p0', 'lb', 'ub'))
    })
  }, simplify=F)
  
  list(
    fluidRow(
      column(
        width = 3,
        em('parameter')
      ),
      column(
        width = 3,
        em('initial')
      ),
      column(
        width = 3,
        em('lower')
      ),
      column(
        width = 3,
        em('upper')
      )
    ),
    
    lapply(names(b), function(n) {
      
      fluidRow(
        column(
          width = 3,
          div(n)
        ),
        
        # create a set of text inputs for each parameter
        lapply(c('p0', 'lb', 'ub'), function(v){
          column(
            width = 3,
            textInput(inputId = sprintf('mo_param_%s_%s', n, v), NULL, b[[n]][v])
          )
        })
      )
    })  
  )  
}

progress_callback = function(id, ids) {
  #print(id)
  #print(ids)
  #print(which(ids == id))
  #print(which(ids == id)/length(ids))
  
  i = which(ids == id)
  N = length(ids)
  setProgress(i/N, message=id, detail=sprintf('%d of %d', i, N))
}

plot_overview = function(bso, input) {
  
  facet = grepl('facet', tolower(input$sel_plot_overview_type))
  
  gg = ggplot(bso$getData()) +
    aes(x=Time, y=value, color=factor(lnum), group=id, linetype=type) + 
    geom_vline(xintercept=input$ix_fun_min_x, color='darkred') +
    geom_hline(yintercept=input$ix_fun_max_y, color='darkred') +
    geom_line()
  
  if (facet) {
    gg = gg + facet_wrap(~id, ncol=10)
  }
  
  gg = gg + 
    scale_linetype_manual(values=c('s'='solid', 'm'='dashed', 'e'='dotted')) +
    theme_minimal()
  
  return(gg)
}

plot_fit_results = function(bso, mo_growth, progress_callback = NULL) {
  x = bso$getParams()
  
  # parameter names
  pn = names(mo_growth$P$p0)
  
  pc = function(...){}
  if (!is.null(progress_callback) && is.function(progress_callback)) {
    pc = progress_callback
  }
  
  # construct a long data table of all raw and fitted data points
  y = lapply(x$id, function(.id) {
    pc(.id, x$id)
    
    # get input data (primary x values)
    xy = bso$getData(.id) %>% 
      select(x=Time, y=value)
    
    fd = (x %>% filter(id == .id))
    
    # get fitted parameters
    p = (fd[,pn] %>% as.list)
    
    # get fitted indices
    ix = strsplit(fd$indices[1], ',') %>%
      unlist %>%
      as.numeric
    
    l = (1:nrow(xy)) %in% ix
    
    # return fitted data by id
    data_frame(
      id=.id, 
      x = xy$x,
      y = xy$y,
      yhat=mo_growth$value(p = p, xy),
      fitted = l
    )
  }) %>% bind_rows
  
  gg = ggplot(y) +
    aes(x=x) +
    geom_point(aes(y=y, color=fitted), size=1.5) + 
    geom_line(aes(y=yhat), color='blue') +
    coord_trans(ytrans='log') + 
    facet_wrap(~id) + 
    scale_color_manual(values=c(`FALSE`='lightgray', `TRUE`='darkorange')) +
    scale_y_continuous(
      breaks=scales::pretty_breaks(n=6)(seq(0.05, 2, length.out = 100)),
      limits=c(0.05, 2)) + 
    theme_minimal()
  
  return(gg)
}

plot_fit_rmsd_trace = function(bso) {
  x = bso$getParams()
  
  gg = ggplot(x) +
    aes(x=id, y=rmsd) + 
    geom_bar(stat='identity', fill='lightgray') +
    geom_hline(yintercept=median(x$rmsd)) + 
    geom_hline(yintercept=median(x$rmsd) + sd(x$rmsd), linetype='dotted') + 
    geom_hline(yintercept=median(x$rmsd) - sd(x$rmsd), linetype='dotted') + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = 90, vjust=0.5)
    )
  
  return(gg)
}

bso = NULL

shinyServer(function(input, output, session) {
  observeEvent(
    input$btn_load_file, {
      print('loading')
      
      path = with(input$file, datapath[1])
      
      session$output$data_warning = renderUI({
        div()
      })
      
      tryCatch({
        withProgress({
          setProgress(0.5, message = 'Loading file')
          
          bso <<- BioscreenExperiment()
          bso$loadXlsx(file=path)
          
          setProgress(1)
        })
        
        session$output$data_warning = renderUI({
          div(
            class='panel panel-success',
            div(class='panel-body', 'Data successfully loaded.')
          )
        })
        
        
      }, error = function(e) {
        session$output$data_warning = renderUI({
          div(
            class='panel panel-danger',
            div(class='panel-heading', 'Error'),
            div(class='panel-body', e$message)
          )
        })
        bso <<- NULL
        
      })
      
    }
  )
  
  mo_growth = reactive({
    # combine the background and exp models
    mo_growth = mix(mo[[input$mo_bg]], mo$exp)
    
    # set the user defined bounds
    # need to roll through the input object for param names
    P = sapply(c('p0', 'lb', 'ub'), function(v) {
      sapply(names(mo_growth$P$p0), function(n) {
        inp_name = sprintf('mo_param_%s_%s', n, v)
        as.numeric(input[[inp_name]])
      })
    }, simplify=F)
    
    mo_growth$setP(
      p0 = P$p0,
      lb = P$lb,
      ub = P$ub
    )
    
    return(mo_growth)
  })
  
  observeEvent(
    input$btn_fit, {
      
      if (!is.null(bso)) {
        # construct an ix.fun from inputs
        ix.fun = ix.fun.factory(
          rmse.tol.f = c(
            lower=input$ix_fun_rmse_lower,
            upper=input$ix_fun_rmse_upper
          ),
          guess.fun = function(xy, ...) {
            which(
              xy$x >= input$ix_fun_min_x & xy$y <= input$ix_fun_max_y
            )
          }
        )
        
        # do the fit
        withProgress({
          bso$calcParams(
            'ModelFit', optimizer='nlminb', ids=NULL,
            model = mo_growth(), 
            smooth.input = T,
            ix.fun = ix.fun,
            weightFun = function(data) { with(data,{ sqrt(rev((1:length(x))/length(x))) }) },
            trust.bounded = T,
            progress_callback = progress_callback
          )  
        })
        
        bso
      }
    }
  )
  
  output$mo_bg_bounds = renderUI({
    param_inputs(mo[[input$mo_bg]])
  })
  
  output$mo_exp_bounds = renderUI({
    param_inputs(mo$exp)
  })
  
  output$status = renderPrint({
    input$btn_load_file
    
    if (!is.null(bso)) {
      # str(bso)
      cat('Compiled Experiment File: ', input$file$name, '\n')
      cat('Experiment Properties:\n')
      print(
        bso$expt %>%
          select(Value)
      )
      cat('\n')
      
      cat('Number of wells: ', length(unique(bso$data$id)), '\n')
      
      cult = bso$info %>% 
        filter(type == 's') %>% 
        select(media, lnum, vol, od0) %>%
        distinct
      
      cat('Number of unique cultures: ', nrow(cult), '\n')
    }
  }, width=160)
  
  output$plot_overview = renderPlot({
    input$btn_data_refresh
    
    if (!is.null(bso)) {
      withProgress({
        setProgress(0.3, message = 'building plot')
        gg = plot_overview(bso, input)
        
        setProgress(0.7, message = 'rendering')
        print(gg)
        
        setProgress(1)
      })
      
    }
    
  }, height=850)
  
  output$table_fit_results = renderDataTable({
    input$btn_fit
    
    if (!is.null(bso)) {
      bso$getParams() %>% 
        mutate(
          indices = sapply(
            strsplit(indices, ','), 
            function(x){x = as.numeric(x); sprintf('%d..%d (%d)', min(x), max(x), length(x))})
        ) %>%
        select(-type, -bsid, -position, -vol, -row, -col, -model, -equation, -optimizer)
    }
  }, options = list(pageLength = 10))
  
  output$plot_fit_rmsd_trace = renderPlot({
    input$btn_fit
    
    if (!is.null(bso)) {
      gg = plot_fit_rmsd_trace(bso)
      print(gg)
    }
    
  }, height=250)
  
  output$plot_fit_results = renderPlot({
    input$btn_fit
    mo_growth = isolate(mo_growth())
    
    if (!is.null(bso)) {
      withProgress({
        gg = plot_fit_results(bso, mo_growth, progress_callback=progress_callback)  
      })
      
      print(gg)
    }
  }, height=650)
  
  output$btn_dl_fit_results = downloadHandler(
    filename = function() {
      sub('^(.*)(\\.xlsx)$', '\\1__ModelFit\\2', input$file$name)
    },
    content = function(file) {
      #write.csv(bso$getParams(), file=file, row.names=F)
      
      wb = createWorkbook(type='xlsx')
      shInfo = createSheet(wb, 'info')
      shResults = createSheet(wb, 'results')
      
      #param_json = jsonlite::toJSON(
      #  pretty = T,
      #  sapply(names(mo_growth$P$p0), function(p) {
      #    sapply(names(mo_growth$P), function(v) {
      #      mo_growth$P[[v]][[p]]
      #    }, simplify=F)
      #  }, simplify=F)
      #)
      
      
      # collect the equation and initial parameter guesses that went into the model
      # collect the curve locator parameters
      
      info = bind_rows(
        data_frame(
          property = 'equation',
          value    = as.character(mo_growth()$expr)
        ),
        lapply(grep('^(mo_param|ix_fun)', names(input), value = T), function(n) {
          data_frame(
            property = n,
            value    = as.character(input[[n]])
          )  
        }) %>% bind_rows
      )
      
      addDataFrame(info %>% data.frame, shInfo, row.names=F)
      addDataFrame(bso$getParams() %>% data.frame, shResults, row.names=F)
      
      saveWorkbook(wb, file)
      
    },
    contentType = 'text/csv'
  )
  
})
