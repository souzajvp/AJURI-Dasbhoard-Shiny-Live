suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(stringr)
  library(forcats)
  library(lubridate)
  library(gt)
  library(gtsummary)
  library(labelled)
  library(plotly)
  library(scales)
  library(bsicons)
})
options(shiny.maxRequestSize = 50 * 1024^2,
        sass.cache = file.path(tempdir(), "ajuri-dashboard-sass"))
source("data-import.R", local = TRUE)
source("dashboard.R", local = TRUE)

ajuri_green <- "#00A85A"
ajuri_dark_green <- "#14533A"
ajuri_lime <- "#A9D63F"
ajuri_soft_green <- "#EAF7EF"
ajuri_mint <- "#CFEBDD"
ajuri_ink <- "#17212B"
warning_red <- "#B22222"
warning_soft_red <- "#F6DEDE"
open_yellow <- "#F2C94C"
fmt_blue <- duke_blue <- ajuri_green
fmt_light_green <- ajuri_soft_green
target_per_site <- 10
meta_semanal <- 3

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = ajuri_green,
                  success = ajuri_lime, danger = warning_red,
                  base_font = c("Arial", "sans-serif"))

ui <- page_fluid(
  title = "AJURI Dashboard · Shinylive", theme = theme, lang = "pt-BR",
  tags$head(tags$link(rel = "stylesheet", href = "onboarding.css")),
  # Load plotly/crosstalk scripts with the page; when they arrive with the dynamic
  # dashboard UI, the first widgets can render before the scripts run (blank plots).
  tags$div(style = "display:none", `aria-hidden` = "true",
           plotly::plot_ly(type = "scatter", mode = "markers", x = 0, y = 0)),
  uiOutput("workspace")
)

source_chooser <- function() {
  tags$main(class = "data-start",
    tags$img(src = "logo_ajuri_longa.jpeg", alt = "AJURI", class = "start-logo"),
    tags$h1("Carregar dados"),
    tags$p("Selecione a fonte de dados do estudo AJURI.", class = "start-intro"),
    tags$div(class = "source-options",
      tags$section(class = "csv-option", `aria-labelledby` = "csv-title",
        bs_icon("filetype-csv", size = "2rem"),
        tags$h2(id = "csv-title", "Carregar arquivo CSV"),
        tags$p("Selecione um arquivo AJURI para explorar inclusão, seguimento e eventos adversos."),
        fileInput("csv", "Arquivo CSV (até 50 MB)", accept = c(".csv", "text/csv"),
                  buttonLabel = "Selecionar CSV", placeholder = "Nenhum arquivo selecionado"),
        dateInput("reference_date", "Data de referência para o seguimento", value = Sys.Date(),
                  language = "pt-BR", format = "yyyy-mm-dd"),
        actionButton("open_dashboard", "Abrir dashboard", class = "btn-primary"),
        tags$div(role = "status", `aria-live` = "polite", uiOutput("import_status")),
        tags$details(tags$summary("Formato esperado do CSV"),
          tags$p("Envie diretamente a exportação do REDCap (todos os instrumentos e eventos), em ",
                 "\"CSV (labels)\" ou \"CSV (raw data)\", com nomes de variáveis no cabeçalho. ",
                 "Vírgula ou ponto e vírgula; datas AAAA-MM-DD ou DD/MM/AAAA."),
          tags$p("Braço do estudo é opcional: inclua uma coluna trial_arm (ex.: Intervencao/Controle ",
                 "ou códigos cegos A/B) para ver a divisão por braço. Sem ela, essa divisão é ocultada. ",
                 "Cada polo_base é um cluster, a menos que o arquivo tenha cluster_id."),
          tags$a(href = "ajuri-csv-template.csv", download = "ajuri-csv-template.csv", "Baixar modelo de colunas"),
          tags$p("Também é aceito um CSV de análise com uma linha por participante. O modelo contém ",
                 "apenas o cabeçalho; deixe em branco os valores desconhecidos."))
      ),
      tags$section(class = "api-option", `aria-labelledby` = "api-title",
        bs_icon("cloud-download", size = "2rem"),
        tags$h2(id = "api-title", "Conectar via API"),
        tags$span("Em breve · Coming soon", class = "coming-soon"),
        tags$p("Use suas credenciais de API para buscar e baixar dados do GitHub."),
        tags$button(type = "button", disabled = NA, class = "btn btn-outline-secondary",
                    "API indisponível por enquanto")
      )
    ),
    tags$p(class = "local-note", bs_icon("laptop"),
           " Os dados selecionados são processados neste navegador e não são enviados ao servidor. ",
           "Ao recarregar a página, selecione o arquivo novamente.")
  )
}

server <- function(input, output, session) {
  loaded <- reactiveVal(NULL)
  import_error <- reactiveVal(NULL)
  output$import_status <- renderUI({
    if (!is.null(import_error())) tags$p(class = "import-error", import_error())
  })
  output$workspace <- renderUI({
    dashboard <- loaded()
    if (is.null(dashboard)) return(source_chooser())
    tagList(
      tags$div(class = "data-toolbar",
        tags$span(dashboard$filename),
        tags$span(paste(dashboard$rows, "participantes")),
        tags$span(paste("Referência:", dashboard$date)),
        tags$button(type = "button", class = "btn btn-outline-primary btn-sm",
                    onclick = "window.location.reload()", "Trocar dados")
      ),
      dashboard$view
    )
  })
  observeEvent(input$open_dashboard, {
    if (!is.null(loaded())) return()
    import_error(NULL)
    if (is.null(input$csv)) {
      import_error("Selecione um arquivo CSV para continuar.")
      return()
    }
    if (!grepl("\\.csv$", input$csv$name, ignore.case = TRUE) || input$csv$size > 50 * 1024^2) {
      import_error("Selecione um arquivo .csv de até 50 MB.")
      return()
    }
    tryCatch({
      reference_date <- as.Date(input$reference_date)
      if (length(reference_date) != 1 || is.na(reference_date)) stop("Selecione uma data de referência válida.")
      withProgress(message = "Carregando dados…", value = 0, {
        data <- read_dashboard_csv(input$csv$datapath)
        incProgress(0.5)
        dashboard <- build_dashboard(data, reference_date)
        dashboard$server(input, output, session)
        loaded(list(view = dashboard$ui, filename = input$csv$name,
                    rows = nrow(data), date = format(reference_date, "%Y-%m-%d")))
      })
    }, error = function(error) import_error(conditionMessage(error)))
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
