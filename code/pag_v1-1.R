library(shiny)
library(DT)
library(RCurl)
library(dplyr)
library(lubridate) 
library(leaflet)
library(stringr)

download <- getURL("https://raw.githubusercontent.com/febr-team/febr-data/master/data/febr-superconjunto.csv")
dados <- read.csv(text = download, sep = ";")
estadoTipo <- unique(dados$estado_id)
colShow <- dados


ui <- fluidPage(
  titlePanel(a(href = 'http://coral.ufsm.br/febr/', img(src = 'logo.PNG'))), 
  tags$hr(),
  
  
  fluidRow(
    column(2,
      wellPanel(
    
    # Input: Seleciona um dataset ----
        selectInput(inputId = "est", label = "Estado:",
                    choices = NULL),
        
        selectInput("cid", "Cidade:",
                    choices =  NULL),
        
        selectInput("clasTox", "Classificação Taxonômica:",
                    choices = NULL), 
        
        sliderInput("data", "Ano da pesquisa", min = 1900, max = 2019
                    , value = c(1900, 2019)),
        
        sliderInput("profun", "Profundidade", min = 0, max = 400, value = c(0, 1000)),
        
        helpText("Por favor, clique no botão 'Atualizar' para atualizar sua pesquisa ")
        )
    ),
  
  
  # main / tab-dados
    column(width = 9,
      tabsetPanel(id = 'maintabs',
         tabPanel(title = tags$h3('Tabela'), value = 'priTab', tags$br(),
              tags$p(class = 'lead'),tags$hr(),
              DT::dataTableOutput("outDados")),

         
         tabPanel(title = tags$h3('Segunda Tabela'), value = 'segTab', tags$br(),
              tags$p(class = 'lead'),tags$hr(),
              DT::dataTableOutput("outDadosSeg")),
         
             
   # output mapa
          tabPanel(title = tags$h3('Mapa'), value = 'map',
              fluidRow(tags$br(), tags$hr(),
                  leafletOutput('outMapa', width = '100%', height = '600'),
                   tags$br() )),
      

   # Ajuda
        # 
        # tabPanel(title = tags$h3('Ajuda'), value = 'ajuda',tags$br(),tags$hr(),
        #   fluidRow(tags$br(),
        #     column(width = 6,
        #       wellPanel(tags$h4('Estado'),p("Exemplo",
        #         style = 'color:#A9A9A9; font-weight: bold;')
        #       ), 
        #  
        #       wellPanel(tags$h4('Ajuda 2'),tags$p('Exemplo 2',
        #         style = 'color:#A9A9A9; font-weight: bold;'))),
        #  
        #     column(width = 6, 
        #       wellPanel(tags$h4('Ajuda 3'), 
        #         tags$p('Control the degree of smoothing. Default is 0.1.',
        #         style = 'color:#A9A9A9; font-weight: bold;')
        #       ),
        #       
        #        wellPanel(tags$h4('Output Limits'), 
        #           tags$p('Set limits on spline output values. Defaults are 0, 1000.')
        #         )))),
                   
   # Tab Download
                     
         tabPanel(title = tags$h3('Download'), value = 'download',tags$br(),tags$hr(),
            fluidRow(tags$br(),
              column(width = 6, offset = 3,
                wellPanel(
                  tags$br(), 
                  radioButtons('formato', h3('Escolha o formato do Download: '), tags$br(), 
                    inline = TRUE, choices = c('CSV' = 1, 'txt', 'Doc')), 
                    style = 'text-align:center',
                    tags$br(), 
                  downloadButton(outputId = 'download',
                    label = 'Download', class = 'dlb'),
                    tags$head(tags$style(".dlb{width: 100%;}"))
                )
              )
            )
          ) 
        )
      )  
    )
  )


server <- function(input, output, session) {
  
  ### UpdateInputs
  
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
    if(input$cid != 'Todas'){
      classificacao <- colShow %>% 
        filter((colShow$estado_id == input$est) & (colShow$municipio_id == input$cid )) %>% 
        select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", 
                        choices = c("Todas", unique(classificacao)))
    
      }else if(input$est == 'Todos'){
      classificacao <- colShow %>% select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", 
                        choices = c("Todas", unique(classificacao)))
    
      }else if(input$est != 'Todos'){
      classificacao <- colShow %>% 
        filter(colShow$estado_id == input$est) %>% 
        select(taxon_sibcs) 
      updateSelectInput(session,"clasTox","Classificação Taxonômica: ", 
                        choices = c("Todas", unique(classificacao)))
    }
  })
  
  observe({
    profun_max <- as.numeric(dados$profund_sup) %>% range(na.rm = TRUE) %>%  max()
    updateSliderInput(session,'profun', 'Profundidade', min = '0',
                      max = profun_max, value = c(input$profun[1], input$profun[2]))
  })
  
  observe({
    year_range <- dados$observacao_data %>% lubridate::year() %>% range(na.rm = TRUE)
    updateSliderInput(
      session, "data", "Ano da pesquisa", min = year_range[1], max = year_range[2],
      value = c(input$data[1], input$data[2]))
  })
  
  
  #----------------------------------------------------------------------------------------
  
  
  # Apresentação da tabela e filtragem
  
  filtroTodos <- reactive({
    colShow <- dados %>% 
      filter((((profund_sup %in% input$profun[1]:input$profun[2]) & 
                 (profund_inf %in% input$profun[1]:input$profun[2])) |
                (is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
               (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
    if(input$maintabs == 'segTab'){
      colShow %>% select('terrafina', 'argila','silte', 'areia', 
                         'carbono', 'ctc', 'ph') 
    }else{
      colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                 'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
    }
  })
  
  filtroEst <- reactive({
    colShow <- dados %>%
    filter((colShow$estado_id == input$est) & 
        ((profund_sup %in% input$profun[1]:input$profun[2]) & 
        (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf)) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        colShow %>% select('terrafina', 'argila','silte', 'areia', 
                           'carbono', 'ctc', 'ph') 
      }else{
        colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                           'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
      }
  })
  
  filtroCid <- reactive({
    colShow <- dados %>% 
    filter((colShow$municipio_id == input$cid) & (colShow$estado_id == input$est) &
        (((profund_sup %in% input$profun[1]:input$profun[2]) & 
        (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        colShow %>% select('terrafina', 'argila','silte', 'areia', 
                           'carbono', 'ctc', 'ph') 
      }else{
        colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                           'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
      }
  })
  
  filtroEstTax <- reactive({
    colShow <- dados %>% filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs 
         & (((profund_sup %in% input$profun[1]:input$profun[2]) & 
         (profund_inf %in% input$profun[1]:input$profun[2]) |
         is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
         (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        colShow %>% select('terrafina', 'argila','silte', 'areia', 
                           'carbono', 'ctc', 'ph') 
      }else{
        colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                           'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
      }
  })
  
  filtroEstCidTax <- reactive({
    colShow <- dados %>%    
    filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs &
          colShow$municipio_id == input$cid &
          (((profund_sup %in% input$profun[1]:input$profun[2]) & 
          (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        colShow %>% select('terrafina', 'argila','silte', 'areia', 
                           'carbono', 'ctc', 'ph') 
      }else{
        colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                           'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
      }
  })
  
  filtroTax <- reactive({
    colShow <- dados %>%    
    filter((input$clasTox == dados$taxon_sibcs) &
          (((profund_sup %in% input$profun[1]:input$profun[2]) & 
          (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
    if(input$maintabs == 'segTab'){
      colShow %>% select('terrafina', 'argila','silte', 'areia', 
                         'carbono', 'ctc', 'ph') 
    }else{
      colShow %>% select('dataset_id', 'observacao_data','coord_x', 'coord_y', 
                         'profund_sup', 'profund_inf', 'taxon_sibcs', 'municipio_id', 'estado_id') 
    }
  })
  
  #---------------------------------------------------------------------------------------------
  
  #tableOutput "Dados"
  output$outDados <- DT::renderDataTable({
    if(input$est == 'Todos' && input$clasTox == 'Todas'){
      DT::datatable(filtroTodos(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEst(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroCid(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEstTax(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroEstCidTax(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
    }else if(((input$est == 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroTax(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
    }
  })
  
  #-------------------------------------------------------------------------------------------------------------------
  
  # tableoutput segDados
  output$outDadosSeg <- DT::renderDataTable({
    if(input$est == 'Todos' && input$clasTox == 'Todas'){
      DT::datatable(filtroTodos(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEst(), options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroCid(), options = list(lengthMenu =  c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroEstTax(), options = list(lengthMenu =  c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid != 'Todas'))){
      DT::datatable(filtroEstCidTax(), options = list(lengthMenu =  c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
    }else if(((input$est == 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
      DT::datatable(filtroTax(), options = list(lengthMenu =  c(5, 10, 30, 50), pageLength = 5), rownames = FALSE)
    }
  })
  
  output$outMapa <- renderLeaflet({
    m <- leaflet() %>%
      setView(lng = -50.8663589, lat = -12.9214564, zoom = 4) %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>% 
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(
      baseGroups = c("Esri.WorldStreetMap", "CartoDB.Positron", "Esri.WorldImagery"),
      options = layersControlOptions(collapsed = TRUE)) 
    
    if(input$est == 'Todos' && input$clasTox == 'Todas'){
        m %>%  
          addAwesomeMarkers(
            lng = as.numeric(na.omit(sub(',','.',filtroTodos()$coord_x))),
            lat = as.numeric(na.omit(sub(',','.',filtroTodos()$coord_y))),
            icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
            clusterOptions = markerClusterOptions(),
            label = filtroTodos()$observacao_id)
      }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid == 'Todas'))){
        m %>%
          addAwesomeMarkers(
            lng = as.numeric(na.omit(sub(',','.',filtroEst()$coord_x))),
            lat = as.numeric(na.omit(sub(',','.',filtroEst()$coord_y))),
            icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
            clusterOptions = markerClusterOptions(),
            label = filtroEst()$observacao_id)
            
       }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid != 'Todas'))){
         m %>%
           addAwesomeMarkers(
             lng = as.numeric(na.omit(sub(',','.',filtroCid()$coord_x))),
             lat = as.numeric(na.omit(sub(',','.',filtroCid()$coord_y))),
             icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
             clusterOptions = markerClusterOptions(),
             label = filtroCid()$observacao_id)
      
       }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
         m %>%
           addAwesomeMarkers(
             lng = as.numeric(na.omit(sub(',','.',filtroEstTax()$coord_x))),
             lat = as.numeric(na.omit(sub(',','.',filtroEstTax()$coord_y))),
             icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
             clusterOptions = markerClusterOptions(),
             label = filtroEstTax()$observacao_id)
       
        }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid != 'Todas'))){
          m %>%
            addAwesomeMarkers(
              lng = as.numeric(na.omit(sub(',','.',filtroEstCidTax()$coord_x))),
              lat = as.numeric(na.omit(sub(',','.',filtroEstCidTax()$coord_y))),
              icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
              clusterOptions = markerClusterOptions(),
              label = filtroEstCidTax()$observacao_id)
       }else if(((input$est == 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
         m %>%
           addAwesomeMarkers(
             lng = as.numeric(na.omit(sub(',','.',filtroTax()$coord_x))),
             lat = as.numeric(na.omit(sub(',','.',filtroTax()$coord_y))),
             icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
             clusterOptions = markerClusterOptions(),
             label = filtroTax()$observacao_id)
       }
        
   })
  
  #------------------------------------------------------------------------------------------------------------------
  
  # Download
  
  observe({
    updateRadioButtons(session,'formato', selected = 1)
  })
  
  fileExt <- reactive({
    switch (input$formato,
      'CSV' = '.csv', 'txt' = '.txt', 'Doc' = '.doc')
  })
  
  output$download <- downloadHandler(
    filename = function(){
        paste('Dados-febr-',Sys.Date(), fileExt(), sep = '')
    },
    
    content = function(file){
      if(input$est == 'Todos' & input$clasTox == 'Todas'){
        write.table(filtroTodos(),file, sep = ';', row.names = FALSE)
      
        }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid == 'Todas'))){
          write.table(filtroEst(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox == 'Todas') && (input$cid != 'Todas'))){
          write.table(filtroCid(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
          write.table(filtroEstTax(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox != 'Todas') && (input$cid != 'Todas'))){
          write.table(filtroEstCidTax(),file, sep = ';', row.names = FALSE)
        }else if(((input$est == 'Todos') && (input$clasTox != 'Todas') && (input$cid == 'Todas'))){
          write.table(filtroTax(),file, sep = ';', row.names = FALSE)
       }
    }
  )
} 


shinyApp(ui = ui, server = server)
