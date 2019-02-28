# Filtragem dos estados, cidades e classificação taxon funcionando, segue abaixo o code para teste.

library(shiny)
library(DT)
library (RCurl)
library(dplyr)
#library(stringr)

download <- getURL("https://raw.githubusercontent.com/febr-team/febr-data/master/data/febr-superconjunto.csv")
dados <- read.csv(text = download, sep = ";")
estadoTipo <- unique(dados$estado_id)
#colShow <- dados %>% select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')

ui <- fluidPage(
  titlePanel("febr"), 
  tags$hr(),
  
  fluidRow(column(2,wellPanel(
    
    # Input: Seleciona um dataset ----
    selectInput(inputId = "est", label = "Estado:",
                choices = NULL),
    
    selectInput("cid", "Cidade:",
                choices =  NULL),
    
    selectInput("clasTox", "Classificação Taxonômica:",
                choices = NULL), 
    
    sliderInput("data", "Ano da pesquisa", min = 1950, max = 2019
                , value = c(1950, 2019)),
    
    sliderInput("profun", "Profundidade", min = -10, max = 300, value = c(-10,300)),
    
    helpText("Por favor, clique no botão 'Atualizar' para atualizar sua pesquisa "),
    
    actionButton("update", "Atualizar")
  )
  ),
  
  
  # main / tab-dados
  column(width = 9,
         tabsetPanel(id = 'maintabs',
                     tabPanel(title = tags$h3('tabDados'), value = 'Ipanel', tags$br(),
                              tags$p(class = 'lead'),tags$hr(),  DT::dataTableOutput("outDado")),
                     
                     # output display
                     tabPanel(title = tags$h3('Mapa'), value = 'map',
                              fluidRow(conditionalPanel('input.SID && input.splinetime',
                                                        column(10, offset = 1, tags$br(), 
                                                               plotOutput('splineplot')),
                                                        
                                                        fluidRow(column(width = 6, offset = 1, tags$br(),
                                                                        
                                                                        checkboxGroupInput(inputId = 'pick_geoms',label = NULL,                                  choices = list('Original' = 1,'Depth layers' = 2,                                        '1cm increments' = 3),  selected = c(1,2,3),inline = TRUE,width = '100%')),
                                                                 column(width = 4,downloadButton(outputId = 'dl_plot',
                                                                                                 label = 'Save plot as png',class = 'dlb-sml'),
                                                                        tags$head(tags$style(".dlb-sml{margin-top: 25px; width: 80%;}")))),
                                                        
                                                        fluidRow(column(width = 3, offset = 1,selectInput(inputId = 'plot_scale',                label   = 'Plot Scale', choices = list('Scale to site'= 1,
                                                                                                                                                                                        'Scale to quantile 0.75' = 2,
                                                                                                                                                                                        'Scale to quantile 0.95' = 3,
                                                                                                                                                                                        'Scale to whole dataset' = 4),
                                                                                                          selected = 1,multiple = FALSE,  width = '80%')))),
                                       
                                       tags$br()),
                              # end plot area
                              
                              
                              fluidRow(conditionalPanel('input.SID && input.splinetime', tags$hr(), 
                                                        tags$h3('Splined output values'),column(width = 6, tags$h4('Depth ranges'),              dataTableOutput('splinetable_sd')),column(width = 6, tags$h4('1cm intervals'),          dataTableOutput('splinetable_cm')))), conditionalPanel('input.SID && input.splinetime', tags$br(), tags$hr())),
                     
                     # settings
                     tabPanel(title = tags$h3('Settings'), value = 'Spanel',
                              fluidRow(tags$br(),column(width = 6,
                                                        wellPanel(tags$h4('Ajuda'),p("Exemplo",
                                                                                     style = 'color:#A9A9A9; font-weight: bold;'),
                                                                  radioButtons(inputId = 'depth_choice',label = NULL, 
                                                                               choices = list('Standard Depths' = 'sd', 'Custom...' = 'ns'), width = '100%'),
                                                                  
                                                                  # choosing custom output depth ranges:
                                                                  conditionalPanel("input.depth_choice == 'ns'", numericInput(inputId = 'cd',
                                                                                                                              label = 'Custom Depths', value = '', width = '100%',
                                                                                                                              min = 0, max = 1000, step = 1),
                                                                                   tags$i(textOutput(outputId = 'cd_vals')), tags$br(), 
                                                                                   actionButton(inputId = 'cd_add', label = 'Add', icon = icon('plus'), 
                                                                                                width = '100%'), tags$br(),tags$br(),
                                                                                   actionButton(inputId = 'cd_reset',label =  'Reset',icon = icon('undo'),
                                                                                                width = '100%'))), 
                                                        # end well Panel 1
                                                        
                                                        wellPanel(tags$h4('Ajuda 2'),tags$p('Exemplo 2',
                                                                                            style = 'color:#A9A9A9; font-weight: bold;'),
                                                                  numericInput(inputId = 'rnd',label = 'Rounding',
                                                                               value = 3,min = -10, max = 10,width = '50%'))),
                                       
                                       column(width = 6, wellPanel(tags$h4('Lambda'), tags$p('Control the degree of smoothing. Default is 0.1.',
                                                                                             style = 'color:#A9A9A9; font-weight: bold;'), numericInput(inputId = 'c_ld', label = 'Lambda',
                                                                                                                                                        value = 0.1, width = '50%')), 
                                              
                                              # end lambdapanel
                                              wellPanel(tags$h4('Output Limits'), tags$p('Set limits on spline output values. Defaults are 0, 1000.'),
                                                        numericInput(inputId = 'low_lim', label   = 'Lower Limit', value = 0,
                                                                     width = '50%'), numericInput(inputId = 'high_lim', label = 'Upper Limit', value = 1000,
                                                                                                  width = '50%'))))),
                     
                     tabPanel(title = tags$h3('Download'), value = 'Epanel',
                              fluidRow(tags$br(),
                                       column(width = 6, offset = 3,
                                              wellPanel(downloadButton(outputId = 'dl_sd',
                                                                       label = 'Resampled depth intervals as csv',
                                                                       class = 'dlb'),
                                                        tags$br(), tags$br(),
                                                        downloadButton(outputId = 'dl_1cm',
                                                                       label = '1cm depth intervals as csv',
                                                                       class = 'dlb'),
                                                        tags$br(), tags$br(),
                                                        downloadButton(outputId = 'dl_rds',
                                                                       label = 'Full output of GSIF::mpspline as rds',
                                                                       class = 'dlb'),
                                                        tags$head(tags$style(".dlb{width: 100%;}"))
                                              ) # end export wellpanel
                                       ))) # end export tabpanel
         )) #end main 
  )
)


server <- function(input, output, session) {
  
  ### select input aninhado
  observe({ 
    estados <- dados %>% select(estado_id)
    updateSelectInput(session,"est","Estados",choices = c("Todos", unique(estados)))
  }) 
  
  observe({ 
    #x <- dados$Product.Sub.Category[product_list$Product.Category == input$mainproduct] 
    cidades <- dados %>% filter(colShow$estado_id == input$est) %>% select(municipio_id)
    updateSelectInput(session,"cid","Cidade",choices = c("Todas", unique(cidades)))
  })
  
  observe({
      updateSliderInput(session,'profun', 'Profundidade', min = '-10', max = '300', value = c(input$profun[1], input$profun[2]))
  })
  
  observe({ 
    if(input$cid != 'Todas'){
      classificacao <- colShow %>% 
        filter((colShow$estado_id == input$est) & (colShow$municipio_id == input$cid )) %>% 
        select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", choices = c("Todas", unique(classificacao)))
    }else if(input$est == 'Todos'){
      classificacao <- colShow %>% select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", choices = c("Todas", unique(classificacao)))
    }else if(input$est != 'Todos'){
      classificacao <- colShow %>% 
        filter(colShow$estado_id == input$est) %>% 
        select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", choices = c("Todas", unique(classificacao)))
    }
  })
  #-----------------------------------------------------------------
  
  # Apresentação da tabela e filtragem
  
  colShow <- dados %>% select('dataSet' = 'dataset_id', 'observacao_data','coord_x', 'coord_y', 
                              'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
  
  #(dados$profund_sup > input$profun[1],dados$profund_inf < input$profun[2])
  
  filtroEst <- reactive({
    colShow <- dados %>% filter(colShow$estado_id == input$est) %>% 
      select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')
  })
  
  filtroCid <- reactive({
    colShow <- dados %>% filter((colShow$municipio_id == input$cid) & (colShow$estado_id == input$est)) %>%
      select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')
  })
  
  filtroEstTax <- reactive({
    colShow <- dados %>% filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs) %>% 
      select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')
  })
  
  filtroEstCidTax <- reactive({
    colShow <- dados %>%    
      filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs & colShow$municipio_id == input$cid)  %>% 
      select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')
  })
  
  filtroTax <- reactive({
    colShow <- dados %>%    
      filter(input$clasTox == dados$taxon_sibcs) %>% 
      select('dataset_id', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id', 'estado_id')
  })
  
  
  #tableOutput "dado"
  output$outDado <- DT::renderDataTable({
    if(input$est == 'Todos' && input$clasTox == 'Todas'){
      DT::datatable(colShow, options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEst(), options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroCid(), options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEstTax(), options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroEstCidTax(), options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
    }else if(((input$est == 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroTax(), options = list(lengthMenu = c(10, 30, 50), pageLength = 10), rownames = FALSE)
    }
  })
  #-------------------------------------------------------------------------------------------------------------------
  
  ### SIDEBAR
  # recieve data
  to_be_splined <- eventReactive(input$ld, {
    in_file <- input$file_1
    req(in_file)
    read_csv(in_file$datapath)
  })
  
  # Feed SID choices from input df to drop-down box
  observe({
    updateSelectInput(session, inputId = 'SID',
                      choices = levels(factor(to_be_splined()[[1]])),
                      selected = levels(factor(to_be_splined()[[1]]))[1])  
  })
  
  # display loaded data
  output$table_1 <- renderDataTable({
    req(input$SID)
    to_be_splined()[to_be_splined()[, 1] == input$SID, ] 
  },
  options = list(lengthChange = FALSE, pageLength = 10,
                 scrollX = FALSE, scrollY = '300px',
                 paging = FALSE, searching = FALSE, info = FALSE)
  )
  
  ### MAIN
  
  ## make soil profile collection for mpspline
  spline_in_spc <- reactive({
    cn <- names(to_be_splined())
    new('SoilProfileCollection',
        idcol     = cn[1],
        depthcols = c(cn[2:3]),
        # this class can't handle tbl, tbl-df :/
        horizons  = data.frame(to_be_splined(), 
                               stringsAsFactors = FALSE),
        site      = data.frame(unique(to_be_splined()[ , 1]),
                               stringsAsFactors = FALSE))
  })
  
  # Deal with default and custom depth ranges
  # expose mpspline inputs to user with defaults set (settings panel)
  out_ranges <- reactiveValues('default' = c(0,5,15,30,60,100,200))
} 


shinyApp(ui = ui, server = server)
