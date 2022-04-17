library(shiny)
library(shinydashboard)
library(ECharts2Shiny)
library(readr)
library(dplyr)
library(DT)
library(plotly)
library(purrr)
library(wordcloud2)



df <- read.csv("notifica_sindrome_gripal.csv",
                 sep = ";", encoding = 'UTF-8')

View(df)


#Calculando o total de notificacoes
total <- paste0(substr(toString(nrow(df)),1,3), '.', 
                substr(toString(nrow(df)),4,6))

#Calculando o total de notificacoes de profissionais da saude
num_prof_saude <- df %>% group_by(profissionalSaude) %>% 
  tally(sort = T) %>% 
  slice(2) %>% 
  pull(n)

#Construindo rank de notificacoes por municipio
rank <- df %>% group_by(municipioNotificacao) %>% 
  tally(sort = T) %>% rename(Municípios = municipioNotificacao)


#Construindo serie visao diaria
agrup_data_dia <- df %>% group_by(dataNotificacao) %>% 
  tally(sort = F)

visao_Dia <- plot_ly(agrup_data_dia, x = ~dataNotificacao, 
               y = ~n, type = 'scatter', mode = 'lines') %>% 
  layout(title = "N?mero de notificações de Síndrome gripal por dia",
         xaxis = list(title = "Dia"),
         yaxis = list (title = "Frequ?ncia"))

#Construindo serie historica visao mensal
agrup_data_mes <- df %>% group_by(substr(df$dataNotificacao, 1, 7)) %>% 
  tally(sort = F) %>% rename(anomes = `substr(df$dataNotificacao, 1, 7)`)

visao_Mes <- plot_ly(agrup_data_mes, x = ~anomes,
                     y = ~n, type = 'scatter', mode = 'lines') %>% 
  layout(title = "Número de notificações de Síndrome gripal por dia",
         xaxis = list(title = "Mês"),
         yaxis = list (title = "Frequência"))

#Extraindo todos os sintomas para construcao da nuvem de palavras
list_sintomas <- df$sintomas %>% map(function(x) strsplit(x, split = ', '))
vetor_sintomas <- c()

df_sintomas <- data.frame('word' = as.factor(rapply(list_sintomas, function(x) x)),
                          'id' = 1)

df_sintomas <- df_sintomas %>%
  group_by(word) %>% 
  summarise(freq = sum(id))
  

################################ / * SHINY */ ##################################
ui <- dashboardPage(
  dashboardHeader(
    title = 'Notificações de Síndrome Gripal para o Estado de São Paulo.',
    titleWidth = 600
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    tags$style(type = 'text/css', 'alinhanento {color: black;height:40px;display:flex;align-itens: center:justify-content: center:}'),
    tags$style(type = 'text/css', '.cor (background-color: white:'),
    loadEChartsLibrary(),
    loadEChartsTheme('shine'),
    loadEChartsTheme('vintage'),
    loadEChartsTheme('roma'),
    loadEChartsTheme('infographic'),
    loadEChartsTheme('jazz'),
    loadEChartsTheme('london'),
    loadEChartsTheme('caravan'),
    loadEChartsTheme('macarons'),
    loadEChartsTheme('dark-digerati'),
    tabsetPanel(
      id = 'tabs',
      tabPanel(
        title = 'Contagem',
        value = 'page1',
        fluidRow(
          valueBoxOutput('Total', width = 4),
          valueBoxOutput('Saude', width = 4)),
        fluidRow(tags$p()),
        fluidRow(column(12,
                        tags$h4(class = 'alinhamento',
                                'Rank de cidades notificadas')),
        fluidRow(DT::dataTableOutput('Tabela'))
        )),
      tabPanel(
        title = 'An?lise por Munic?pio de notifica??o',
        sidebarLayout(
          sidebarPanel(width = 3,
                       textInput(inputId = 'cidade',
                                 label = strong('Digite a cidade'), 'São Carlos'),
                       selectizeInput(inputId = 'var',
                                   label = strong('Variável'),
                                   c('Idade'='1','Cor'='2', 'Gênero' = '3'))),
          mainPanel(plotlyOutput('Distrib')))),
      tabPanel(
        title = 'Série histórica',
        sidebarLayout(
          sidebarPanel(width = 2,
                       selectInput(inputId = 'tipo',
                                   label = strong('Visâo'),
                                   c('Dia'='1','M?s'='2'))),
        mainPanel(class = 'cor',
                  plotlyOutput('Serie')))),
      tabPanel(
        title = 'Sintomas',
        sidebarLayout(
          sidebarPanel(width = 3,
                       sliderInput(inputId = 'freq',
                                   label = strong('Frequência mínima'),
                                   min = 1, max = 65000, value = 10000)),
          mainPanel(wordcloud2Output('Nuvem')))),
      tabPanel(
        title = 'Sobre este app',
        fluidRow(tags$h4('Fonte dos dados: https://opendatasus.saude.gov.br/dataset/notificacoes-de-sindrome-gripal-leve-2022')),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$h4('Estes dados são preliminares, sujeitos a avaliação.As bases estão sendo avaliadas e harmonizadas, com o objetivo de ser verificada sua consistência, principalmente em relação a atualização dos dados.')),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$p()),
        fluidRow(tags$h6('Produzido por Giovanni Vicente Batista e Gabriela Pereira Soares')
      )))))
  

back <- function(input, output){
  output$Total <- renderValueBox(
    valueBox(total, 'Total de notificações', 
             icon = icon('atom'), color = 'purple'))
  
  output$Saude <- renderValueBox(
    valueBox(num_prof_saude, 'Notifica??es de profissionais da saúde', 
             icon = icon('list-alt'), color = 'blue'))
  
  output$Tabela <- DT::renderDataTable(
    DT::datatable(rank,
                  options = list(pageLength = 15)))
  
  output$Distrib <- renderPlotly({
    
    filtro_cidade <- df %>% filter(municipioNotificacao ==  input$cidade)
    
    
    if(input$var == '1'){
      
    plot_final <- plot_ly(filtro_cidade, x = ~idade, type = 'histogram') %>% 
      layout(title = paste0('Distribuição por idade na cidade de ', input$cidade),
             xaxis = list(title = 'Idade'))}
    
    if(input$var == '2'){
    
    plot_final <- count(filtro_cidade, racaCor) %>% plot_ly(y = ~n,
                                                         x = ~factor(racaCor),
                                                         type = "bar") %>% 
      layout(title = paste0('Distribuição por cor na cidade de ', input$cidade),
             xaxis = list(title = 'Cor'),
             yaxis = list(title = "Frequência"))}
    
    if(input$var == '3'){
    plot_final <- count(filtro_cidade, sexo) %>% plot_ly(values = ~n,
                                                         labels = ~factor(sexo),
                                                         marker = list(colors=c("purple","blue")),
                                                         type = "pie") %>% 
      layout(title = paste0('Distribui??o por g?nero na cidade de ', input$cidade))}
    
    plot_final})
  
  output$Serie <- renderPlotly({
    
    atual <- visao_Dia
    
    
    if(input$tipo == '2'){
      
      atual <- visao_Mes
      }
    atual})
  
  output$Nuvem <- renderWordcloud2({
    wordcloud2(as.data.frame(df_sintomas[order(-df_sintomas$freq),]) %>% 
                      filter(freq >= input$freq),
               size = .5)
  })
  
  }

shinyApp(ui, back)
