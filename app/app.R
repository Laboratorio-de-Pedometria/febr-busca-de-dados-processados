#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(magrittr)

# Descarregar super conjunto de dados
url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
temp <- tempfile(fileext = '.zip')
download.file(url = url, destfile = temp)
superconjunto <- read.table(unzip(temp), sep = ";", dec = ",", stringsAsFactors = FALSE, header = TRUE)
superconjunto$observacao_data <- as.Date(superconjunto$observacao_data, format = "%Y-%m-%d")
superconjunto[, c("coord_precisao", "terrafina", "argila", "silte", "areia")] <- 
  lapply(superconjunto[, c("coord_precisao", "terrafina", "argila", "silte", "areia")], as.numeric)

# Documentação da aplicação
ajuda <- readLines('www/ajuda.html')

# Definir UI para aplicação
ui <- fluidPage(
  
  tags$head(
    tags$style(".btn{width: 100%;}"),
    tags$style(".well{padding:10px;}")
  ),
   
  # Título da aplicação
  titlePanel(title = a(href = 'https://www.pedometria.org/projeto/febr/'), windowTitle = 'FEBR'),
  
  fluidRow(
    column(
      width = 2,
      wellPanel(
        htmltools::HTML('<h3>FEBR</h3><h4>Dados Processados</h4>'), 
        
        tags$hr(),
        
        # Selecionar variáveis
        tags$h4('Tabela'),
        varSelectInput(
          inputId = 'variables', 
          label = 'Variáveis selecionadas', 
          data = superconjunto, 
          multiple = TRUE, 
          selected = c('dataset_id', 'observacao_id', 'camada_id')
        ),
        
        tags$hr(),
        
        # Opções do mapa
        tags$h4('Mapa'),
        sliderInput(inputId = 'radius', label = 'Raio de agrupamento', value = 80, min = 0, max = 100)#,
        # https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
        # fileInput(
        #   inputId = 'vector', 
        #   label = 'Área de interesse',
        #   accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'), 
        #   multiple = TRUE, 
        #   placeholder = "Nenhum arquivo selecionado",
        #   buttonLabel = "Navegar..."
        # )
      ),
      HTML(paste('©', format(Sys.time(), "%Y"), 'A. Samuel-Rosa<br>')),
      HTML(paste('This work is licensed under', 
                 '<a href = "https://creativecommons.org/licenses/by-nc-nd/4.0/">CC BY NC ND 4.0</a>')),
      HTML(paste('<br>Powered by <a href = "https://shiny.rstudio.com/">Shiny</a>,',
                 '<a href = "https://www.datatables.net/">DataTables</a> and',
                 '<a href = "https://leafletjs.com/">Leaflet</a>'))
    ),
    
    column(
      width = 10,
      tabsetPanel(
        id = 'maintabs',
        
        # Tabela
        tabPanel(
          title = tags$h3('Tabela'),
          DT::dataTableOutput("outDados")
        ),
        
        # Mapa
        tabPanel(
          title = tags$h3('Mapa'),
          leafletOutput('outMapa', width = '100%', height = '840')
        ),
        
        # Descarregar
        tabPanel(
          title = tags$h3('Descarregar'),
          column(
            width = 3, offset = 3, tags$br(),
            
            # Dados filtrados
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFiltered",
                class = 'btn',
                label = HTML('Dados filtrados<br>(*.txt)'))
            ),
            
            # Conjunto(s) de dados filtrado(s)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFilteredAll", 
                class = 'btn',
                label = HTML('Conjunto(s) de dados filtrado(s)<br>(*.txt)'))
            ),
            
            # Conjunto(s) de dados original(is)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outRawAll",
                class = 'btn',
                label = HTML('Conjunto(s) de dados original(is)<br>(*.zip)'))
            ),

            # Todos os dados
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outAll",
                class = 'btn',
                label = HTML('Todos os dados do repositório<br>(*.zip)')
              )
            )
          )
        ),
        
        tabPanel(
          title = tags$h3('Ajuda'),
          HTML(ajuda)
        )
      )
    )
  )
)

# server ####
server <- function (input, output) {
  
  # Tabela de dados
  output$outDados <-
    DT::renderDT(
      dplyr::select(superconjunto, !!!input$variables),
      filter = 'top', 
      escape = FALSE, 
      # rownames = FALSE, 
      rownames = paste0(
        '<a href="https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F', superconjunto$dataset_id, 
        '" target=_blank title = "Acessar conjunto de dados original">', superconjunto$dataset_id, '</a>'),
      # extensions = 'Buttons',
      # Plug-in accent-neutralise.js ainda não foi implementado
      # https://github.com/rstudio/DT/issues/822
      # plugins = 'accent-neutralise', 
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json'), 
        pageLength = 5, 
        # dom = 'B<"clear">lfrtip', 
        search = list(regex = TRUE, caseInsensitive = TRUE)#,
        # buttons = list(
          # list(extend = 'copy', buttons = 'copy', text = 'Copiar'),
          # list(extend = 'csv', buttons = 'csv', text = 'CSV'),
          # list(extend = 'print', buttons = 'print', text = 'Imprimir')
        # )
      )
    )
  
  # Mapa
  # https://yihui.shinyapps.io/DT-info/
  observe({
    filtered <- input$outDados_rows_all
    unique <- !duplicated(superconjunto[filtered, c("dataset_id", "observacao_id")])
    m <- 
      leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldStreetMap", "Esri.WorldImagery"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addMiniMap() %>% 
      addAwesomeMarkers(
        lng = superconjunto[filtered, ][unique, 'coord_x'],
        lat = superconjunto[filtered, ][unique, 'coord_y'], 
        icon = awesomeIcons(
          icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
        label = paste(superconjunto[filtered, ][unique, 'observacao_id'], '@',
                      superconjunto[filtered, ][unique, 'dataset_id'], sep = ''),
        popup = 
          paste0('<b>', superconjunto[filtered, ][unique, 'dataset_id'], '</b><br>',
                 superconjunto[filtered, ][unique, 'dataset_titulo'], '<br>',
                 '<i>', superconjunto[filtered, ][unique, 'dataset_licenca'], '</i><br>',
                 '<a href = "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F', 
                 superconjunto[filtered, ][unique, 'dataset_id'], '" target=_blank>',
                 'Acessar conjunto de dados original na nuvem</a><br>',
                 '<a href = "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F',
                 superconjunto[filtered, ][unique, 'dataset_id'], '/',
                 superconjunto[filtered, ][unique, 'dataset_id'], '.xlsx" target=_blank>',
                 'Descarregar conjunto de dados original (*.xlsx)</a>'),
        clusterOptions = markerClusterOptions(maxClusterRadius = input$radius)) %>% 
      leafem::addHomeButton(
        ext = raster::extent(
          sf::st_as_sf(superconjunto[filtered, ][unique, c('coord_x', 'coord_y')], 
                       coords = c('coord_x', 'coord_y'), na.fail = FALSE)),
        position = "bottomleft", group = 'Ver tudo')
    output$outMapa <- renderLeaflet(m)
  })
  
  # Descarregar
  
  # Dados filtrados
  output$outFiltered <-
    downloadHandler(
      filename = paste0('febr-dados-filtrados-', format(Sys.time(), "%Y-%m-%d"), '.txt'), 
      content = function (file) {
        write.table(
          superconjunto[input$outDados_rows_all, ], file, sep = '\t', dec = ',', row.names = FALSE)
      })
  
  # Conjunto(s) de dados filtrado(s)
  output$outFilteredAll <-
    downloadHandler(
      filename = paste0('febr-conjuntos-de-dados-filtrados-', format(Sys.time(), "%Y-%m-%d"), '.txt'), 
      content = function (file) {
        idx <- unique(superconjunto[input$outDados_rows_all, 'dataset_id'])
        idx <- superconjunto$dataset_id %in% idx
        write.table(superconjunto[idx, ], file, sep = '\t', dec = ',', row.names = FALSE)
      })
  
  # Conjunto(s) de dados original(is)
  output$outRawAll <-
    downloadHandler(
      filename = paste0('febr-conjuntos-de-dados-originais-', format(Sys.time(), "%Y-%m-%d"), '.zip'), 
      content = function (file) {
        ctb <- unique(superconjunto[input$outDados_rows_all, 'dataset_id'])
        setwd(tempdir())
        for (i in ctb) {
          url <- paste0(
            'https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F', i, '/', i, '.xlsx')
          download.file(url = url, destfile = paste0(i, '.xlsx'))
        }
        zip(zipfile = file, files = paste0(ctb, '.xlsx'))
      }
    )
  
  # Todos os dados do respositório
  output$outAll <-
    downloadHandler(
      filename = paste0('febr-todos-os-dados-do-repositorio-', format(Sys.time(), "%Y-%m-%d"), '.zip'), 
      content = function (file) {
        browseURL("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download")
      }
    )
}

# Executar a aplicação
shinyApp(ui = ui, server = server)
