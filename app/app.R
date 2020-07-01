# Title: Repositório Brasileiro Livre para Dados Abertos do Solo | Aplicação Shiny
# Version: 0.2.6
# Date: 2020-06-30
# Authors: Matheus Ferreira Ramos (matheusramos@alunos.utfpr.edu.br),
#          Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)
# License: GPL (>= 2)
# Encoding: UTF-8

# Bibliotecas -------------------------------------------------------------

library(shiny)
library(RCurl)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(magrittr)

# Definições --------------------------------------------------------------------------------------------------
sep_dec <- ','  # Separador decimal
sep_col <- '\t' # Separador de colunas 
febr_catalog <- "http://coral.ufsm.br/febr/catalog/"

# Definição de língua a ser utilizada nas tabelas geradas usando DT::datatable
dt_lang <- '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json'

# Descarregamento dos dados ####
# Dados são descarregados do servidor ownCloud da UTFPR
# Usar arquivo ZIP acelera o descarregamento dos dados e a inicialização da página
# dados <- "https://cloud.utfpr.edu.br/index.php/s/nEXaoXIE0nZ1AqG/download"
# dados <- "https://raw.githubusercontent.com/febr-team/febr-data/master/data/febr-superconjunto.txt"
# dados <- "data/febr-superconjunto.zip"
url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
temp <- tempfile(fileext = '.zip')
download.file(url = url, destfile = temp)
dados <- read.table(unzip(temp), sep = ";", dec = ",", stringsAsFactors = FALSE, header = TRUE)

# Definindo variáveis -----------------------------------------------------------------------------------------

# Variáveis para apresentação da tabela 'Dados Gerais'
vars_info <- 
  c('dataset_id', 'observacao_id', 'observacao_data', 'coord_x', 'coord_y', 'taxon_sibcs', 'municipio_id',
    'estado_id')
vars_info_name <- 
  c("Código de identificação do conjunto de dados no repositório",
    "Código de identificação da observação do solo no conjunto de dados",
    "Data de observação do solo",
    "Longitude (SIRGAS 2000, graus)",
    "Latitude (SIRGAS 2000, graus)",
    "Classificação taxonômica pelo Sistema Brasileiro de Classificação de Solos",
    "Nome do município onde a observação do solo foi realizada",
    "Sigla da unidade federativa onde a observação do solo foi realizada") %>% 
  paste("<code>", vars_info, "</code>", ": ", ., ". ", sep = "", collapse = " ")

# Variáveis para apresentação da tabela 'Dados Analíticos'
vars_lab <-
  c('dataset_id', 'observacao_id', 'profund_sup', 'profund_inf',
    'terrafina', 'argila', 'silte', 'areia', 'carbono', 'ctc', 'ph', 'ce', 'dsi')
vars_lab_name <- 
  c("Código de identificação do conjunto de dados no repositório",
    "Código de identificação da observação do solo no conjunto de dados",
    "Profundidade superior da camada (cm)",
    "Profundidade inferior da camada (cm)",
    "Conteúdo de terra fina (g/kg)",
    "Conteúdo de argila (g/kg)",
    "Conteúdo de silte (g/kg)",
    "Conteúdo de areia (g/kg)",
    "Conteúdo de carbono (g/kg)",
    "Capacidade de troca de cátions potencial (cmol<sub>c</sub>/kg)",
    "pH em água (adimensional)",
    "Condutividade elétrica (mS/cm)",
    "Densidade do solo inteiro (kg/dm<sup>3</sup>)") %>% 
  paste("<code>", vars_lab, "</code>", ": ", ., ". ", sep = "", collapse = " ")

# Variáveis para descarregamento
vars_download <-
  c('dataset_id', 'observacao_id', 'sisb_id', 'ibge_id', 'observacao_data', 'coord_x', 'coord_y', 
    'coord_precisao', 'coord_fonte', 'pais_id', 'estado_id', 'municipio_id', 'amostra_tipo', 'amostra_quanti',
    'amostra_area', 'taxon_sibcs', 'taxon_st', 'taxon_wrb', 'camada_id', 'amostra_id', 'camada_nome',
    'profund_sup', 'profund_inf', 'terrafina', 'argila', 'silte', 'areia', 'carbono', 'ctc', 'ph', 'dsi', 'ce')

# Valor máximo da profundidade
profun_max <- max(dados$profund_inf, na.rm = TRUE)

# Início ------------------------------------------------------------------------------------------------------
ui <- 
  fluidPage(
    titlePanel(
      # title = a(href = 'https://www.pedometria.org/projeto/febr/', img(src = 'logo.png')),
      title = a(href = 'https://www.pedometria.org/projeto/febr/'), windowTitle = 'FEBR'), #tags$hr(),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          selectInput(inputId = "est", label = "UF", choices = NULL),
          selectInput(inputId = "cid", label = "Município", choices =  NULL),
          selectInput(inputId = "clasTox", label = "Taxonomia", choices = NULL), 
          sliderInput(inputId = "data", label = "Ano", min = 1900, max = 2019, value = c(1900, 2019), sep = ''),
          sliderInput(inputId = "profun", label = "Profundidade (cm)", sep = '', min = 0, max = profun_max,
                      value = c(0, profun_max)))
      ),
      
      # Principal: abas
      column(
        width = 9,
        tabsetPanel(
          id = 'maintabs',
          
          # Aba 'Apresentação'
          tabPanel(
            title = tags$h3('Apresentação'), value = 'intro', tags$br(),
            fluidRow(
              column(
                width = 12, 
                h2('Olá, tudo bem?'),
                htmltools::HTML(
                  '<img src = "logo.png">'
                ),
                p(
                  'Esperamos que você tenha gostado da nova ferramenta de busca e visualização de dados.', 
                  'Mas nós sabemos que você deve ter ótimas ideias para deixá-la ainda melhor.',
                  'Acesse o formulário que preparamos em e deixe a sua opinião.',
                  'São apenas 5 minutinhos!'
                )))),
          
          # Aba "Dados Gerais"
          tabPanel(
            title = tags$h3('Dados Gerais'), value = 'priTab', tags$p(class = 'lead'), 
            DT::dataTableOutput("outDados"), tags$br(), tags$hr(), HTML(vars_info_name)
          ),
          
          # Aba "Dados Analíticos"
          tabPanel(
            title = tags$h3('Dados Analíticos'), value = 'segTab', tags$p(class = 'lead'),
            DT::dataTableOutput("outDadosSeg"), tags$br(), tags$hr(), HTML(vars_lab_name)
          ),
          
          # Aba "Localização"
          tabPanel(
            title = tags$h3('Localização'), value = 'map',
            fluidRow(
              column(
                width = 12, tags$br(), leafletOutput('outMapa', width = '100%', height = '800'),
                actionButton("reset_button", "Ver tudo"),
                tags$style("#reset_button{float:left;margin-top:-45px;margin-left:20px;position:relative;}")))
          ),
          
          # Aba 'Descarregar'
          tabPanel(
            title = tags$h3('Descarregar'), value = 'download', tags$br(), 
            fluidRow(
              column(
                width = 6, offset = 3,
                wellPanel(
                  tags$br(), h3('Descarregue dados selecionados'), tags$br(), 
                  style = 'text-align:center', tags$br(),
                  downloadButton(outputId = 'outDown', label = 'Descarregar', class = 'dlb'),
                  tags$head(tags$style(".dlb{width: 100%;}"))))))
          ))))

# Servidor ----------------------------------------------------------------------------------------------------
server <- 
  function (input, output, session) {
    
    # Função para apresentação das tabelas
    # Inclui alteração do separador decimal
    dataTables <-
      function (x) {
        if (input$maintabs == 'priTab') {
          DT::datatable(
            data = x, filter = 'none', escape = FALSE, rownames = FALSE, selection = 'none',
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 15, rownames = FALSE, 
                           language = list(url = dt_lang))) %>%
            DT::formatCurrency(., c('coord_x', 'coord_y'), currency = "", digits = 6, dec.mark = ',')
        } else if (input$maintabs == 'segTab') {
          DT::datatable(
            data = x, filter = 'none', escape = FALSE, rownames = FALSE, selection = 'none',
            options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 15, rownames = FALSE,
                           language = list(url = dt_lang))) %>%
            DT::formatCurrency(., c('carbono', 'ctc', 'ph', 'ce', 'dsi'), currency = "", digits = 1, 
                               dec.mark = ',')
        }
      }
    
    # Função para adicionar marcadores/pontos no mapa. Primeiro são removidas as observações sem coordenadas
    # espacias.
    marks <-
      function (my.map, my.points) {
        my.points <- dplyr::filter(my.points, !is.na(coord_x) | !is.na(coord_y))
        my.map %>%
          leaflet::addAwesomeMarkers(
            lng = my.points$coord_x, lat = my.points$coord_y,
            icon = leaflet::awesomeIcons(icon = "info-sign", markerColor = "#b22222", iconColor = "#fffff0"),
            clusterOptions = leaflet::markerClusterOptions(),
            label = glue::glue('{my.points$observacao_id}@{my.points$dataset_id}'),
            popup = glue::glue(
              "<a href={febr_catalog}{my.points$dataset_id} target='_blank'> Mais informações?</a>"))
      }
    
    ### UpdateInputs ---------------------------------------------------------
    
    # função reativa para atualizar o selectInput do estado e ordenar alfabeticamente
    observe({ 
      estados <- dados %>% arrange(estado_id) %>% select(estado_id) %>% unique() %>% na.exclude()
      shiny::updateSelectInput(session, "est", "UF", choices = c("Todos", estados))
    })
    
    # função reativa para atualizar o selectInput da cidade para apresentar
    # somente as cidades do estado dentro do superconjunto e ordenar alfabeticamente
    observe({ 
      cidades <- dados %>% filter(dados$estado_id == input$est) %>% select(municipio_id) %>% 
        arrange(-desc(municipio_id))
      shiny::updateSelectInput(session, "cid", "Município", choices = c("Todos", unique(cidades)))
    })
    
    # função reativa para atualizar o selectInput da classificacao taxonomica apresentando
    # somente as taxonomia que tem no estado ou cidade selecionado e ordenar alfabeticamente 
    observe({ 
      if (input$cid != 'Todos') {
        classificacao <- dados %>% 
          filter((dados$estado_id == input$est) & (dados$municipio_id == input$cid )) %>% 
          select(taxon_sibcs) %>% arrange(-desc(taxon_sibcs))
        shiny::updateSelectInput(session, "clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
        
      } else if (input$est == 'Todos') {
        classificacao <- dados %>% select(taxon_sibcs) %>% arrange(-desc(taxon_sibcs))
        shiny::updateSelectInput(session, "clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
        
      } else if (input$est != 'Todos') {
        classificacao <- dados %>% filter(dados$estado_id == input$est) %>% select(taxon_sibcs) %>% 
          arrange(-desc(taxon_sibcs))
        shiny::updateSelectInput(session, "clasTox", "Taxonomia", choices = c("Todos", unique(classificacao)))
      }
    })
    
    # Atualizar a profundida maxima
    observe({
      updateSliderInput(
        session, 'profun', 'Profundidade (cm)', min = '0', max = profun_max, 
        value = c(input$profun[1], input$profun[2]))
    })
    
    # Atualizar o ano mínimo e máximo
    observe({
      year_range <- dados$observacao_data %>% lubridate::year() %>% range(na.rm = TRUE) 
      updateSliderInput(
        session, "data", "Ano", min = year_range[1], max = year_range[2],
        value = c(input$data[1], input$data[2]) )
    })
    
    # Update do botão do mapa "home button"
    observe({
      input$reset_button
      leaflet::leafletProxy("outMapa") %>% setView(lng = -50.8663589, lat = -12.9214564, zoom = 4)
    })
    
    # filtragem -------------------------------------------------------------------
    
    # filtroTodos, ,ultilizado quando o usuario nao altera os estado, cidade e taxonomia
    filtroTodos <- 
      reactive({
        # Esse filter é usado em todos filtros, ele diz se a profundade esta entre o input profun
        # filtra tambem, os anos da observacao_data, se estao entre o input data
        # essa filtragem de profundidade e ano tambem eh aplicada nos outros filtros abaixo
        my.data <-
          dados %>%
          dplyr::filter(
            (
              profund_sup %in% input$profun[1]:input$profun[2] & 
                profund_inf %in% input$profun[1]:input$profun[2] | 
                is.na(profund_sup) | 
                is.na(profund_inf)
            ) & (
              year(observacao_data) %in% input$data[1]:input$data[2] | 
                is.na(observacao_data)
            )
          )
        
        #Condicoes para apresentacao das abas
        if (input$maintabs == 'priTab') {
          # Para a tabela localizacao, remove-se as observacoes repetidas 
          select(my.data, all_of(vars_info)) %>% 
            distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
            mutate(
              dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
          
        } else if (input$maintabs == 'segTab') {
          # Para a tabela analitica, apresenta em condicao de ordem crescente da profundidade 
          select(my.data, !!!vars_lab) %>%
            mutate(
              dataset_id = 
                glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
            group_by(dataset_id, observacao_id) %>% 
            arrange(profund_sup, .by_group = TRUE)
          
        } else if (input$maintabs == 'download') {
          # Para a aba de download, seleciona a variavel que contem as informacoes para download definida no 
          # comeco do codigo
          select(my.data, all_of(vars_download))
          
        } else {
          # Para a aba do mapa, Apresenta as variaveis para plotagem no mapa, apresentar o label e o popup 
          # corretamente
          # removendo tambem, as observacoes repetidas
          select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
        }
      })
    
    # Filtro de UF ----
    # filtroEst, filtra os estados, 
    # ultilizado quando o usuario altera somente o estado (input est)
    filtroEst <- 
      reactive({ 
        my.data <-
          dados %>%
          dplyr::filter(
            (
              estado_id == input$est 
            ) & (
              profund_sup %in% input$profun[1]:input$profun[2] &
                profund_inf %in% input$profun[1]:input$profun[2] |
                is.na(profund_sup) | 
                is.na(profund_inf)
            ) & (
              year(observacao_data) %in% input$data[1]:input$data[2] | 
                is.na(observacao_data)
            )
          )
        
        if (input$maintabs == 'priTab') {
          select(my.data, all_of(vars_info)) %>% 
            distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
            mutate(
              dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
          
        } else if (input$maintabs == 'segTab') {
          select(my.data, !!!vars_lab) %>%
            mutate(
              dataset_id = 
                glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
            group_by(dataset_id, observacao_id) %>% 
            arrange(profund_sup, .by_group = TRUE)
          
        } else if (input$maintabs == 'download') {
          select(my.data, all_of(vars_download))
          
        } else {
          select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
        }
      })
    
    # filtroCid, filtra as cidades que contem dentro do estado selecionado
    filtroCid <- reactive({
      my.data <-
        dados %>%
        dplyr::filter((dados$municipio_id == input$cid & dados$estado_id == input$est) & 
                        (
                          profund_sup %in% input$profun[1]:input$profun[2] &
                            profund_inf %in% input$profun[1]:input$profun[2] |
                            is.na(profund_sup) | 
                            is.na(profund_inf)
                        ) & (
                          year(observacao_data) %in% input$data[1]:input$data[2] | 
                            is.na(observacao_data)
                        )
        )
      
      
      if (input$maintabs == 'priTab') {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
          mutate(
            dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
        
      } else if (input$maintabs == 'segTab') {
        select(my.data, !!!vars_lab) %>%
          mutate(
            dataset_id = 
              glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
          group_by(dataset_id, observacao_id) %>% arrange(profund_sup, .by_group = TRUE)
        
      } else if (input$maintabs == 'download') {
        select(my.data, all_of(vars_download))
        
      } else {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
      }
    })
    
    # filtroEstTax, filtra a classificacao taxonomica pelo estado, 
    # ultilizado quando o usuario altera somente a taxonomia e o estado
    filtroEstTax <- reactive({
      my.data <-
        dados %>%
        dplyr::filter((input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs) & 
                        (
                          profund_sup %in% input$profun[1]:input$profun[2] &
                            profund_inf %in% input$profun[1]:input$profun[2] |
                            is.na(profund_sup) | 
                            is.na(profund_inf)
                        ) & (
                          year(observacao_data) %in% input$data[1]:input$data[2] | 
                            is.na(observacao_data)
                        )
        )
      
      if (input$maintabs == 'priTab') {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
          mutate(
            dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
        
      } else if (input$maintabs == 'segTab') {
        select(my.data, !!!vars_lab) %>%
          mutate(
            dataset_id = 
              glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
          group_by(dataset_id, observacao_id) %>% arrange(profund_sup, .by_group = TRUE)
        
      } else if (input$maintabs == 'download') {
        select(my.data, all_of(vars_download))
        
      } else {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
      }
    })
    
    # filtroEstCidTax, filtra ultilizado quando o usuario altera todos inputs
    # taxonomia, cidade e estado
    
    filtroEstCidTax <- reactive({
      my.data <-
        dados %>%
        dplyr::filter((input$est == dados$estado_id & input$clasTox == dados$taxon_sibcs & 
                         dados$municipio_id == input$cid) & 
                        (
                          profund_sup %in% input$profun[1]:input$profun[2] &
                            profund_inf %in% input$profun[1]:input$profun[2] |
                            is.na(profund_sup) | 
                            is.na(profund_inf)
                        ) & (
                          year(observacao_data) %in% input$data[1]:input$data[2] | 
                            is.na(observacao_data)
                        )
        )
      
      if (input$maintabs == 'priTab') {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
          mutate(
            dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
        
      } else if (input$maintabs == 'segTab') {
        select(my.data, !!!vars_lab) %>%
          mutate(
            dataset_id = 
              glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
          group_by(dataset_id, observacao_id) %>% arrange(profund_sup, .by_group = TRUE)
        
      } else if (input$maintabs == 'download') {
        select(my.data, all_of(vars_download))
        
      } else {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
      }
    })
    
    # filtroTax, filtra a classificacao taxonomica, 
    # ultilizado quando o usuario altera somente a taxonomia (input clasTox)
    filtroTax <- reactive({
      my.data <-
        dados %>%
        dplyr::filter((input$clasTox == dados$taxon_sibcs) & 
                        (
                          profund_sup %in% input$profun[1]:input$profun[2] &
                            profund_inf %in% input$profun[1]:input$profun[2] |
                            is.na(profund_sup) | 
                            is.na(profund_inf)
                        ) & (
                          year(observacao_data) %in% input$data[1]:input$data[2] | 
                            is.na(observacao_data)
                        )
        )
      
      if (input$maintabs == 'priTab') {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE) %>% 
          mutate(
            dataset_id = glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>"))
        
      } else if (input$maintabs == 'segTab') {
        select(my.data, !!!vars_lab) %>%
          mutate(
            dataset_id = 
              glue::glue("<a href={febr_catalog}{dataset_id} target='_blank'>{dataset_id}</a>")) %>% 
          group_by(dataset_id, observacao_id) %>% arrange(profund_sup, .by_group = TRUE)
        
      } else if (input$maintabs == 'download') {
        select(my.data, all_of(vars_download))
        
      } else {
        select(my.data, all_of(vars_info)) %>% distinct(dataset_id, observacao_id, .keep_all = TRUE)
      }
    })
    
    # Tabela de localização -----------------------------------------------------------------------------------
    
    # Apresentacao da tabela localizacao conforme for filtrado. 
    output$outDados <- 
      DT::renderDataTable({
        if (input$est == 'Todos' && input$clasTox == 'Todos') {
          filtroTodos() %>% dataTables()
        } else if (input$est != 'Todos' && input$clasTox == 'Todos' && input$cid == 'Todos') {
          filtroEst() %>% dataTables()
        } else if (input$est != 'Todos' && input$clasTox == 'Todos' && input$cid != 'Todos') {
          filtroCid() %>% dataTables()
        } else if (input$est != 'Todos' && input$clasTox != 'Todos' && input$cid == 'Todos') {
          filtroEstTax() %>% dataTables()
        } else if (input$est != 'Todos' && input$clasTox != 'Todos' && input$cid != 'Todos') {
          filtroEstCidTax() %>% dataTables()
        } else if (input$est == 'Todos' && input$clasTox != 'Todos' && input$cid == 'Todos') {
          filtroTax() %>% dataTables()
        }
      })
    
    # Tabela Analitica ----------------------------------------------------------------------------------------
    
    # Apresentacao da tabela analitica conforme for filtrado. 
    output$outDadosSeg <- DT::renderDataTable({
      if (input$est == 'Todos' && input$clasTox == 'Todos') {
        filtroTodos() %>% dataTables()
      } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))) {
        filtroEst() %>% dataTables()
      } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))) {
        filtroCid() %>% dataTables()
      } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
        filtroEstTax() %>% dataTables()
      } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))) {
        filtroEstCidTax() %>% dataTables()
      } else if (((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
        filtroTax() %>% dataTables()
      }
    })
    
    
    # Mapa -------------------------------------------------------------------------------------------------    
    
    # Apresentacao do mapa conforme for filtrado. 
    output$outMapa <- renderLeaflet({
      
      #Variavel "m" esta recebendo as informacoes de providerTiles e miniMap
      m <- leaflet() %>% 
        addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>% 
        addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
        addLayersControl(
          baseGroups = c("Esri.WorldStreetMap", "Esri.WorldImagery"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        addMiniMap() 
      
      if (input$est == 'Todos' && input$clasTox == 'Todos') {
        # tmp e uma variavel temporaria para nao precisar ativar o filtro com muita frequencia
        tmp <- filtroTodos()
        
        # Aqui passamos a varia "m" e "tmp" para a funcao "marks" 
        # para adicionar os marcadores do mapa conforme for filtrado
        m %>% 
          marks(tmp)
      } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))) {
        tmp <- filtroEst()
        m %>% marks(., tmp)
      } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))) {
        tmp <- filtroCid()    
        m %>% marks(., tmp)
      } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
        tmp <- filtroEstTax()      
        m %>% marks(., tmp)
      } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))) {
        tmp <- filtroEstCidTax()       
        m %>% marks(., tmp)
      } else if (((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
        tmp <- filtroTax()      
        m %>% marks(., tmp)
      }
    })
    
    # Download ------------------------------------------------------------------------------------------------
    
    output$outDown <- downloadHandler(
      
      # funcao para o nome do arquivo que esta sendo descarregado
      filename = function () {
        paste('dados-febr-', Sys.Date(), ".", "txt", sep = '')
      },
      
      # funcao para escreve arquivo que sera descarregado aplicado a filtragem
      content = function (file) {
        if (input$est == 'Todos' & input$clasTox == 'Todos') {
          write.table(filtroTodos(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
          
        } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid == 'Todos'))) {
          write.table(filtroEst(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
          
        } else if (((input$est != 'Todos') && (input$clasTox == 'Todos') && (input$cid != 'Todos'))) {
          write.table(filtroCid(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
          
        } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
          write.table(filtroEstTax(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
          
        } else if (((input$est != 'Todos') && (input$clasTox != 'Todos') && (input$cid != 'Todos'))) {
          write.table(filtroEstCidTax(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
          
        } else if (((input$est == 'Todos') && (input$clasTox != 'Todos') && (input$cid == 'Todos'))) {
          write.table(filtroTax(), file, sep = sep_col, dec = sep_dec, row.names = FALSE)
        }
      }
    )
  }

# Servir aplicação --------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
