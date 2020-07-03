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
        sliderInput(inputId = 'radius', label = 'Raio de agrupamento', value = 80, min = 0, max = 100)
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
            width = 6, offset = 3,
            
            # Dados filtrados
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFiltered",
                class = 'btn',
                label = HTML('Dados filtrados<br><br><pre>(ext: ".txt", sep: "\\t", dec: ",")</pre>'))
            ),
            
            # Conjunto(s) de dados filtrado(s)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outFilteredAll", 
                class = 'btn',
                label = HTML('Conjunto(s) de dados filtrado(s)<br><br><pre>(ext: ".txt", sep: "\\t", dec: ",")</pre>'))
            ),
            
            # Conjunto(s) de dados bruto(s)
            wellPanel(
              style = 'text-align:center',
              downloadButton(
                outputId = "outRawAll",
                class = 'btn',
                label = HTML('Conjunto(s) de dados bruto(s)<br><br><pre>(ext: ".zip")</pre>'))
            )
          )
        ),
        
        tabPanel(
          title = tags$h3('Ajuda'),
          h4('Painel'),
          HTML(
            '<p style="font-size:16px">',
            '<b>Tabela – Variáveis selecionadas</b><br>',
            'Selecione as variáveis que você deseja que sejam mostradas na tabela de dados. Você pode fazer ',
            'isso (1) pressionando sobre o nome das variáveis no menu suspenso ou (2) digitando o nome das ',
            'variáveis na caixa de seleção. Para remover variáveis da seleção, basta posicionar o cursor do ',
            'mouse junto do nome da mesma e pressionar as teclas <code>Delete</code> ou <code>Backspace',
            '</code>.',
            '</p>',
            '<p style="font-size:16px">',
            '<b>Mapa – Raio de agrupamento</b><br>',
            'Defina o modo de apresentação espacial das observações do solo. Você pode utilizar a barra de ',
            'rolagem para selecionar um valor entre 0 e 100 pixels. Quanto maior o valor do raio, mais ',
            'observações serão agregadas sob um mesmo agrupamento e, assim, um número menor de agrupamentos ',
            'será formado. Defina o raio como zero para reduzir ao máximo o agrupamento de observações. ',
            'Note que a alteração do raio de agrupamento força o retorno do mapa à sua extensão original. ',
            '</p>'
          ),
          tags$hr(),
          h4('Tabela'),
          HTML(
            '<p style="font-size:16px">',
            '<b>Visão geral</b><br>',
            'As variáveis selecionadas no painel lateral são usadas para construir a tabela de dados. Três ',
            'variáveis de identificação são mostradas por padrão: <code>dataset_id</code>, o código de ', 
            'identificação do conjunto de dados, <code>observacao_id</code>, o código de identificação das ',
            'observações do solo, e <code>camada_id</code>, o código de identificação das camadas/amostras ',
            'do solo. A disposição inicial dos dados é feita seguindo a ordem crescente do <code>dataset_id',
            '</code>, com cinco linhas, onde cada linha representa uma camada/amostra do solo. O número de ',
            'linhas mostradas pode ser alterada no topo da tabela junto à opção "resultados por página". Na ',
            'parte inferior da tabela está indicado o número total de registros. Para navegar ao longo dos ',
            'registros, basta utilizar os controles localizados no canto inferior direito abaixo da tabela. ',
            'Caso muitas variáveis sejam selecionadas, a tabela ficará maior do que o espaço disponível em ',
            'tela. Nesse caso, será preciso usar a barra de rolagem horizontal do navegador para visualizar ',
            'todas as colunas da tabela.',
            '</p>',
            '<p style="font-size:16px">',
            '<b>Filtros de coluna</b><br>',
            'Em construção!',
            '</p>'
          ),
          tags$hr(),
          h4('Mapa'),
          HTML(
            '<p style="font-size:16px">',
            '<b>Visão geral</b><br>',
            'Em construção!',
            '</p>'
          ),
          tags$hr(),
          h4('Descarregar'),
          HTML(
            '<p style="font-size:16px">',
            '<b>Visão geral</b><br>',
            'Em construção!',
            '</p>'
          )
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
        '" target=_blank title = "Acessar conjunto de dados brutos">', superconjunto$dataset_id, '</a>'),
      # extensions = 'Buttons',
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json'), 
        pageLength = 5, 
        # dom = 'B<"clear">lfrtip', 
        search = list(regex = TRUE, caseInsensitive = FALSE)#,
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
    output$outMapa <-
      renderLeaflet(
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
                     'Acessar conjunto de dados brutos</a><br>',
                     '<a href = "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F',
                     superconjunto[filtered, ][unique, 'dataset_id'], '/',
                     superconjunto[filtered, ][unique, 'dataset_id'], '.xlsx" target=_blank>',
                     'Descarregar conjunto de dados brutos (.xlsx)</a>'),
            clusterOptions = markerClusterOptions(maxClusterRadius = input$radius)) %>% 
          leafem::addHomeButton(
            ext = raster::extent(
              sf::st_as_sf(superconjunto[filtered, ][unique, c('coord_x', 'coord_y')], 
                           coords = c('coord_x', 'coord_y'), na.fail = FALSE)),
            position = "bottomleft", group = 'Ver tudo')
      )
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
  
  # Conjunto(s) de dados brutos(s)
  output$outRawAll <-
    downloadHandler(
      filename = paste0('febr-conjuntos-de-dados-brutos-', format(Sys.time(), "%Y-%m-%d"), '.zip'), 
      content = function (file) {
        ctb <- unique(superconjunto[input$outDados_rows_all, 'dataset_id'])
        setwd(tempdir())
        for (i in ctb) {
          url <- paste0(
            'https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F', i, '/', i, '.xlsx')
          download.file(url = url, destfile = paste0(i, '.xlsx'))
        }
        zip(zipfile = file, files = paste0(ctb, '.xlsx'))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
