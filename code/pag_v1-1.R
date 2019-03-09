library(shiny)
library(DT)
library(RCurl)
library(dplyr)
library(lubridate) 
library(leaflet)
library(stringr)

Sys.setlocale(category = "LC_TIME", locale = "Portuguese_Brazil.1252")
dados <- 
  getURL("https://raw.githubusercontent.com/febr-team/febr-data/master/data/febr-superconjunto.csv") %>% 
  read.csv(text = ., sep = ";", dec = ",", stringsAsFactors = FALSE, header = TRUE)
profun_max <- dados$profund_inf %>% max(na.rm = TRUE)


ui <- fluidPage(
  titlePanel(a(href = 'http://coral.ufsm.br/febr/', 
               img(src = '../../../../../img/logo.png')), 'febr'),         
  tags$hr(),  
  fluidRow(
    column(2,
      wellPanel(
    
    # Input: Seleciona um dataset ----
        selectInput(inputId = "est", label = "UF", choices = NULL),
        
        selectInput("cid", "Município", choices =  NULL),
        
        selectInput("clasTox", "Taxonomia", choices = NULL), 
        
        sliderInput("data", "Ano", min = 1900, max = 2019, value = c(1900, 2019), sep=''),
        
        sliderInput("profun", "Profundidade (cm)", sep = '', min = 0, max = profun_max, value = c(0, profun_max))
        )
    ),
  
  
  # main / tab-dados
    column(width = 9,
      tabsetPanel(id = 'maintabs',
         tabPanel(title = tags$h3('Localização'), value = 'priTab', tags$br(),
              tags$p(class = 'lead'),tags$hr(),
              DT::dataTableOutput("outDados")),

         
         tabPanel(title = tags$h3('Dados analíticos'), value = 'segTab', tags$br(),
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
                     
         tabPanel(title = tags$h3('Descarregar'), value = 'download',tags$br(),tags$hr(),
            fluidRow(tags$br(),
              column(width = 6, offset = 3,
                wellPanel(
                  tags$br(), 
                  radioButtons('formato', h3('Escolha o formato do arquivo: '), tags$br(), 
                    inline = TRUE, choices = c('CSV' , 'TXT', 'TSV')), 
                    style = 'text-align:center',
                    tags$br(), 
                  downloadButton(outputId = 'download',
                    label = 'Descarregar', class = 'dlb'),
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
    estados <- dados %>% arrange(dados$estado_id) %>% select(estado_id) 
    updateSelectInput(session,"est", "UF", choices = c("Todos", unique(estados)))
  }) 
  
  observe({ 
    cidades <- dados %>% filter(dados$estado_id == input$est) %>% 
               select(municipio_id) %>% arrange(-desc(municipio_id))
    
    updateSelectInput(session,"cid", "Município", choices = c("Todos", unique(cidades)))
  })
  
  observe({ 
    if(input$cid != 'Todos'){
      classificacao <- dados %>% 
        filter((dados$estado_id == input$est) & (dados$municipio_id == input$cid )) %>% 
        select(taxon_sibcs) %>% arrange(-desc(taxon_sibcs))
      updateSelectInput(session,"clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
    
      }else if(input$est == 'Todos'){
      classificacao <- dados %>% select(taxon_sibcs) %>% arrange(-desc(taxon_sibcs))
      updateSelectInput(session,"clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
    
      }else if(input$est != 'Todos'){
      classificacao <- dados %>% 
        filter(dados$estado_id == input$est) %>% 
        select(taxon_sibcs) %>% arrange(-desc(taxon_sibcs))
      updateSelectInput(session,"clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
    }
  })
  
  observe({
    updateSliderInput(session, 'profun', 'Profundidade (cm)', min = '0',
                      max = profun_max, value = c(input$profun[1], input$profun[2]))
  })
  
  observe({
    year_range <- dados$observacao_data %>% lubridate::year() %>% range(na.rm = TRUE) 
    updateSliderInput(
      session, "data", "Ano", min = year_range[1], max = year_range[2],
      value = c(input$data[1], input$data[2]) )
  })
  
  
  #----------------------------------------------------------------------------------------
  
  
  # Apresentação da tabela e filtragem
  vars_localizacao <- 
    c('dataset_id', 'observacao_id', 'observacao_data', 'coord_x', 'coord_y', 'profund_sup', 'profund_inf', 
      'taxon_sibcs', 'municipio_id', 'estado_id')
  
  filtroTodos <- reactive({
    dados <- dados %>% 
      filter((((profund_sup %in% input$profun[1]:input$profun[2]) & 
                 (profund_inf %in% input$profun[1]:input$profun[2])) |
                (is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
               (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
    if(input$maintabs == 'segTab'){
      dados %>% 
        select(
          'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
           Carbono = carbono, CTC = ctc, pH = ph, CE = ce, DSI = dsi)
    }else{
      dados %>% select(vars_localizacao) 
    }
  })
  
  filtroEst <- reactive({
    dados <- dados %>%
    filter((dados$estado_id == input$est) & 
        ((profund_sup %in% input$profun[1]:input$profun[2]) & 
        (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf)) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        dados %>% 
          select(
            'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
            Carbono = carbono, CTC = ctc, pH = ph, CE = ce,DSI = dsi)
      }else{
        dados %>% select(vars_localizacao)
      }
  })
  
  filtroCid <- reactive({
    dados <- dados %>% 
    filter((dados$municipio_id == input$cid) & (dados$estado_id == input$est) &
        (((profund_sup %in% input$profun[1]:input$profun[2]) & 
        (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        dados %>% 
          select(
            'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
            Carbono = carbono, CTC = ctc, pH = ph, CE = ce,DSI = dsi)
      }else{
        dados %>% select(vars_localizacao)
      }
  })
  
  filtroEstTax <- reactive({
    dados <- dados %>% filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs 
         & (((profund_sup %in% input$profun[1]:input$profun[2]) & 
         (profund_inf %in% input$profun[1]:input$profun[2]) |
         is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
         (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        dados %>% 
          select(
            'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
            Carbono = carbono, CTC = ctc, pH = ph, CE = ce,DSI = dsi)
      }else{
        dados %>% select(vars_localizacao)
      }
  })
  
  filtroEstCidTax <- reactive({
    dados <- dados %>%    
    filter(input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs &
          dados$municipio_id == input$cid &
          (((profund_sup %in% input$profun[1]:input$profun[2]) & 
          (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
      if(input$maintabs == 'segTab'){
        dados %>% 
          select(
            'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
             Carbono = carbono, CTC = ctc, pH = ph, CE = ce,DSI = dsi)
      }else{
        dados %>% select(vars_localizacao)
      }
  })
  
  filtroTax <- reactive({
    dados <- dados %>%    
    filter((input$clasTox == dados$taxon_sibcs) &
          (((profund_sup %in% input$profun[1]:input$profun[2]) & 
          (profund_inf %in% input$profun[1]:input$profun[2]) | is.na(dados$profund_sup) | is.na(dados$profund_inf))) &
          (year(dados$observacao_data) %in% input$data[1]:input$data[2] | is.na(dados$observacao_data)))
    if(input$maintabs == 'segTab'){
      dados %>% 
        select(
          'Terra fina' = terrafina, Argila = argila, Silte = silte, Areia = areia,
           Carbono = carbono, CTC = ctc, pH = ph, CE = ce,DSI = dsi)
    }else{
      dados %>% select(vars_localizacao)
    }
  })
  
  #---------------------------------------------------------------------------------------------
  
  #tableOutput "Dados"
  output$outDados <- DT::renderDataTable({
    if(input$est == 'Todos' && input$clasTox == 'Todos'){
      DT::datatable(filtroTodos(),
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroEst(), 
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))){
      DT::datatable(filtroCid(),
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))        
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroEstTax(),
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))){
      DT::datatable(filtroEstCidTax(), 
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    
    }else if(((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroTax(),
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    }
  })
  
  #-------------------------------------------------------------------------------------------------------------------
  
  # tableoutput segDados
  output$outDadosSeg <- DT::renderDataTable({
    if(input$est == 'Todos' && input$clasTox == 'Todos'){
      DT::datatable(filtroTodos(),
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroEst(), 
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
      
    }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))){
      DT::datatable(filtroCid(),
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))        
      
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroEstTax(),
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    
    }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))){
      DT::datatable(filtroEstCidTax(), 
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    
    }else if(((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
      DT::datatable(filtroTax(),
          options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5, rownames = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
    }
  })
  
  output$outMapa <- renderLeaflet({
    m <- leaflet() %>%
      # setView(lng = -50.8663589, lat = -12.9214564, zoom = 4) %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>% 
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(
      baseGroups = c("Esri.WorldStreetMap", "CartoDB.Positron", "Esri.WorldImagery"),
      options = layersControlOptions(collapsed = TRUE)) 
    
    if(input$est == 'Todos' && input$clasTox == 'Todos'){
        m %>%  
          addAwesomeMarkers(
            lng = as.numeric(na.omit(filtroTodos()$coord_x)),
            lat = as.numeric(na.omit(filtroTodos()$coord_y)),
            icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
            clusterOptions = markerClusterOptions(),
            label = filtroTodos()$observacao_id)
      }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))){
        m %>%
          addAwesomeMarkers(
            lng = as.numeric(na.omit(sub(',','.',filtroEst()$coord_x))),
            lat = as.numeric(na.omit(sub(',','.',filtroEst()$coord_y))),
            icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
            clusterOptions = markerClusterOptions(),
            label = filtroEst()$observacao_id)
            
       }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))){
         m %>%
           addAwesomeMarkers(
             lng = as.numeric(na.omit(sub(',','.',filtroCid()$coord_x))),
             lat = as.numeric(na.omit(sub(',','.',filtroCid()$coord_y))),
             icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
             clusterOptions = markerClusterOptions(),
             label = filtroCid()$observacao_id)
      
       }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
         m %>%
           addAwesomeMarkers(
             lng = as.numeric(na.omit(sub(',','.',filtroEstTax()$coord_x))),
             lat = as.numeric(na.omit(sub(',','.',filtroEstTax()$coord_y))),
             icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
             clusterOptions = markerClusterOptions(),
             label = filtroEstTax()$observacao_id)
       
        }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))){
          m %>%
            addAwesomeMarkers(
              lng = as.numeric(na.omit(sub(',','.',filtroEstCidTax()$coord_x))),
              lat = as.numeric(na.omit(sub(',','.',filtroEstCidTax()$coord_y))),
              icon = awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
              clusterOptions = markerClusterOptions(),
              label = filtroEstCidTax()$observacao_id)
       }else if(((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
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
    updateRadioButtons(session,'formato', selected = 'CSV')
  })
  
  fileExt <- reactive({
    switch (input$formato,
      'CSV' = 'csv', 'TXT' = 'txt', 'TSV' = 'txt')
  })
  
  output$download <- downloadHandler(
    filename = function(){
        paste('Dados-febr-',Sys.Date(), fileExt(), sep = '.')
    },
    
    content = function(file){
        #sep1 <- switch (input$formato, 'CSV' = ',', 'TSV' = '\t', "TXT" = ';')
          
        
        if(input$est == 'Todos' & input$clasTox == 'Todos'){
          write.table(filtroTodos(),file, sep = ';', row.names = FALSE)
      
        }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))){
          write.table(filtroEst(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))){
          write.table(filtroCid(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
          write.table(filtroEstTax(),file, sep = ';', row.names = FALSE)

        }else if(((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))){
          write.table(filtroEstCidTax(),file, sep = ';', row.names = FALSE)
        
        }else if(((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))){
          write.table(filtroTax(),file, sep = ';', row.names = FALSE)
       }
    }
  )
} 


shinyApp(ui = ui, server = server)
