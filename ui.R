
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
models = sapply(mo, function(x){
  deparse(x[['expr']][[1]])
})

swap_names_values = function(x) {
  y = names(x)
  names(y) = x
  return(y)
}

shinyUI(bootstrapPage(
  br(),
  div(
    class = 'container-fluid',
    
    fluidRow(
      column(
        width=2,
        fileInput('file', 'Compiled Experiment File', accept = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
        actionButton('btn_load_file', 'Load', class="btn btn-primary", icon=icon('cog')),
        actionButton('btn_data_refresh', 'Refresh', icon=icon('refresh')),
        
        hr(),
        
        selectInput('mo_bg', 'Background Model', choices = swap_names_values(models[!names(models) %in% 'exp'])),
        uiOutput('mo_bg_bounds'),
        
        HTML('<label class="control-label" for="mo_exp">Growth Model</label>'),
        tags$input(type='text', id='mo_exp', class='form-control', value=models['exp'], disabled='disabled'),
        br(),
        uiOutput('mo_exp_bounds'),
        
        hr(),
        HTML('<label class="control-label">Curve Locator</label>'),
        fluidRow(
          column(
            width=8,
            em('min time (hr)')
          ),
          column(
            width=4,
            numericInput('ix_fun_min_x', NULL, value=2, min = 0)
          )
        ),
        fluidRow(
          column(
            width=8,
            em('max density (OD)')
          ),
          column(
            width=4,
            numericInput('ix_fun_max_y', NULL, value=0.5, step = 0.1, min = 0)
          )
        ),
        fluidRow(
          column(
            width=8,
            em('upper limit rmse tol')
          ),
          column(
            width=4,
            numericInput('ix_fun_rmse_upper', NULL, value=1.0)
          )
        ),
        fluidRow(
          column(
            width=8,
            em('lower limit rmse tol')
          ),
          column(
            width=4,
            numericInput('ix_fun_rmse_lower', NULL, value=2.5)
          )
        ),
        
        hr(),
        actionButton('btn_fit', 'Fit', class='btn btn-primary', icon=icon('play')),
        
        downloadButton('btn_dl_fit_results', label = 'Download Results')
      ),
      column(
        width=10,
        tabsetPanel(
          tabPanel(
            'Messages',
            br(),
            uiOutput('data_warning'),
            verbatimTextOutput('status')
          ),
          tabPanel(
            'Input Data Plot',
            div(
              div(
                style='display:inline-block;',
                div('show:'),
                selectInput('sel_plot_overview_type', NULL, choices = c('Facets', 'Single Plot'))
              )
            ),
            
            plotOutput('plot_overview', width = "100%", height = "100%")
          ),
          tabPanel(
            'Fit Result Plot',
            plotOutput('plot_fit_rmsd_trace', height='250px'),
            plotOutput('plot_fit_results', height='650px')
          ),
          tabPanel(
            'Fit Result Table',
            dataTableOutput('table_fit_results')
          )
        )
      )
    )
  ),
  tags$head(tags$style(type="text/css", ".form-control {padding: 6px 6px}"))
  
))
