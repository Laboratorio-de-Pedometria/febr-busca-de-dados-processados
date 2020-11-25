# Title: Repositório Brasileiro Livre para Dados Abertos do Solo | Dados Processados
# Version: 1.2.0
# Date: 2020-11-25
# Authors: Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br),
#           Matheus Ferreira Ramos (matheusramos@alunos.utfpr.edu.br)
# License: GPL (>= 2)
# Encoding: UTF-8

# Pacotes
library(shiny)
library(leaflet)
library(shinythemes)

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

# Descarregar todos os conjuntos de dados
download <- readLines('www/download.html')

# Definir UI para aplicação
ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  
  tags$head(
    tags$style(".btn{width: 100%;}"),
    tags$style(".well{padding:10px;}")
  ),
  
  # Título da aplicação
  titlePanel(title = a(href = 'https://www.pedometria.org/febr/'), windowTitle = 'FEBR'),
  
  fluidRow(
    column(
      width = 2,
      wellPanel(
        htmltools::HTML('<h3>FEBR</h3><h4>Busca de Dados Processados</h4>'), 
        
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
      HTML(paste0(
        '<div><p>Desenvolvido por <a href="https://www.pedometria.org/alessandro-samuel-rosa/">',
        'Alessandro Samuel-Rosa</a>.</p>')),
      HTML(paste0(
        '<p>Exceto quando proveniente de outras fontes ou onde especificado o contrário, o ',
        'conteúdo desta página está licenciado sob uma licença ',
        '<a href = "https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY NC SA 4.0</a>.</p>')),
      HTML(paste(
        '<p>Página construída com <a href = "https://shiny.rstudio.com/">Shiny</a>,',
        '<a href = "https://www.datatables.net/">DataTables</a> e',
        '<a href = "https://leafletjs.com/">Leaflet</a>.</p></div>'))
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
            width = 3, offset = 2, tags$br(),
            
            tags$h4("Dados processados"),
            
            # Amostra(s) filtrada(s)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFiltered",
                class = 'btn',
                label = HTML('Amostras filtradas<br>(*.txt)'))
            ),
            
            # Local(is) filtrado(is)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFilteredSites",
                class = 'btn',
                label = HTML('Locais filtrados<br>(*.txt)'))
            ),
            # Conjunto(s) de dados filtrado(s)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFilteredAll", 
                class = 'btn',
                label = HTML('Conjuntos de dados filtrados<br>(*.txt)'))
            )),
          column(
            width = 3, offset = 0, tags$br(),
            tags$h4("Dados não processados (originais)"),
            
            # Conjunto(s) de dados original(is)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outRawAll",
                class = 'btn',
                label = HTML('Conjuntos de dados filtrados<br>(*.zip)'))
            ),
            
            # Todos os conjuntos de dados
            HTML(download)
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
      # https://github.com/rstudio/DT/issues/822
      plugins = 'accent-neutralise',
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
    map <- leaflet()
    map <- addProviderTiles(map, provider = "Esri.WorldStreetMap", group = "Esri.WorldStreetMap")
    map <- addProviderTiles(map, provider = "Esri.WorldImagery", group = "Esri.WorldImagery")
    map <- 
      addLayersControl(
        map, baseGroups = c("Esri.WorldStreetMap", "Esri.WorldImagery"),
        options = layersControlOptions(collapsed = TRUE))
    map <- addMiniMap(map)
    map <- 
      addAwesomeMarkers(
        map,
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
        clusterOptions = markerClusterOptions(maxClusterRadius = input$radius))
    map <- 
      leafem::addHomeButton(
        map,
        ext = raster::extent(
          sf::st_as_sf(superconjunto[filtered, ][unique, c('coord_x', 'coord_y')], 
                       coords = c('coord_x', 'coord_y'), na.fail = FALSE)),
        position = "bottomleft", group = 'Ver tudo')
    output$outMapa <- renderLeaflet(map)
  })
  
  # Descarregar
  
  # Amostra(s) filtrada(s)
  output$outFiltered <-
    downloadHandler(
      filename = paste0('febr-amostras-filtradas-', format(Sys.time(), "%Y-%m-%d"), '.txt'), 
      content = function (file) {
        write.table(
          superconjunto[input$outDados_rows_all, ], file, sep = '\t', dec = ',', row.names = FALSE)
      })
  
  # Local(is) filtrado(s)
  output$outFilteredSites <-
    downloadHandler(
      filename = paste0('febr-locais-filtrados-', format(Sys.time(), "%Y-%m-%d"), '.txt'),
      content = function (file) {
        idx1 <- unique(superconjunto[input$outDados_rows_all, 'dataset_id'])
        idx2 <- unique(superconjunto[input$outDados_rows_all, 'observacao_id'])
        idx <- (
          superconjunto$dataset_id %in% idx1
        ) & (
          superconjunto$observacao_id %in% idx2
        )
        write.table(superconjunto[idx, ], file, sep = '\t', dec = ',', row.names = FALSE)
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
}

# Executar a aplicação
shinyApp(ui = ui, server = server)
