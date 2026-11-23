# Dashboard calculations and views, adapted from the original app.R.
# Each upload gets its own closure; no participant data is loaded globally.
build_dashboard <- function(dados, reference_date) {
trial_target_sample <- first(na.omit(dados$trial_target_sample)) %||% 190
trial_target_per_arm <- first(na.omit(dados$trial_target_per_arm)) %||% 96
baseline_target_without_inflation <- first(na.omit(dados$baseline_target_without_inflation)) %||% 168

enrollment_summary <- dados |>
  filter(!is.na(record_id), !is.na(polo_base), !is.na(cluster_id)) |>
  count(trial_arm, cluster_id, polo_base, name = "Enrolled") |>
  mutate(
    Target = coalesce(dados$cluster_target[match(cluster_id, dados$cluster_id)], target_per_site),
    Percent = round(100 * Enrolled / Target, 1)
  )

unit_first_case <- dados |>
  filter(!is.na(cluster_id), !is.na(polo_base), !is.na(data_atendimento_polo)) |>
  group_by(cluster_id, polo_base, trial_arm) |>
  summarise(first_case_date = min(data_atendimento_polo), .groups = "drop")

total_enrolled <- sum(enrollment_summary$Enrolled)
total_sites <- n_distinct(enrollment_summary$cluster_id)
current_active_units <- unit_first_case |>
  filter(first_case_date <= reference_date) |>
  nrow()
target_total <- trial_target_sample
polo_choices <- sort(unique(na.omit(dados$polo_base)))
default_cumulative_polos <- head(polo_choices, 5)
# Arm breakdowns appear only when the upload supplies trial_arm (labels may be blinded codes).
has_arm <- any(!is.na(dados$trial_arm))
arm_display <- function(arm) {
  recode(arm, Intervencao = "Intervenção", .missing = "Não informado")
}
arm_counts <- dados |>
  count(trial_arm, name = "n") |>
  arrange(is.na(trial_arm), trial_arm)
arm_allocation_value <- tags$div(
  purrr::pmap(arm_counts, \(trial_arm, n) {
    target <- if (is.na(trial_arm)) "" else paste0(" / ", trial_target_per_arm)
    tags$div(tags$span(arm_display(trial_arm)), tags$strong(paste0(n, target)))
  }),
  class = "arm-kpi"
)
kpi_boxes <- list(
  value_box(
    title = tags$span(
      "Pacientes inseridos",
      bslib::tooltip(
        tags$span("i", class = "followup-tooltip", tabindex = "0"),
        "190 pacientes com inflação, 168 sem inflação.",
        placement = "top"
      )
    ),
    value = paste0(total_enrolled, " / ", target_total),
    showcase = bsicons::bs_icon("people-fill"),
    theme = fmt_light_green
  ),
  if (has_arm) value_box(
    title = "Pacientes por braço",
    value = arm_allocation_value,
    showcase = bsicons::bs_icon("clipboard-data-fill"),
    theme = fmt_light_green
  ),
  value_box(
    title = "N. Unidades Ativas",
    value = paste0(current_active_units, " / ", total_sites),
    showcase = bsicons::bs_icon("hospital-fill"),
    theme = fmt_light_green
  )
) |> purrr::compact()
stratify_choices <- c("None", "polo_base", if (has_arm) "trial_arm")
explore_defaults <- c("idade_paciente", "sexo_paciente", "polo_base", if (has_arm) "trial_arm")
explore_choices <- setdiff(names(dados), if (!has_arm) "trial_arm")

followup_specs <- tibble(
  followup = factor(c("1 mês", "3 meses", "6 meses"), levels = c("1 mês", "3 meses", "6 meses")),
  months_after_baseline = c(1, 3, 6),
  collection_col = c("data_coleta_t2", "data_coleta_t3", "data_coleta_t4"),
  completion_col = c("completo_t2", "completo_t3", "completo_t4")
)

build_followup_data <- function(data, reference_date = Sys.Date()) {
  purrr::pmap_dfr(followup_specs, \(followup, months_after_baseline, collection_col, completion_col) {
    baseline_date <- as.Date(data$data_atendimento_polo)
    due_date <- baseline_date %m+% months(months_after_baseline)
    collection_date <- as.Date(data[[collection_col]])
    completion_status <- data[[completion_col]]

    tibble(
      record_id = data$record_id,
      cluster_id = data$cluster_id,
      polo_base = data$polo_base,
      trial_arm = data$trial_arm,
      followup = followup,
      baseline_date = baseline_date,
      due_date = due_date,
      window_start = due_date - 7,
      window_end = due_date + 7,
      collection_date = collection_date,
      completion_status = completion_status,
      days_from_due = as.integer(collection_date - due_date),
      status = case_when(
        completion_status == "Completo" & collection_date >= window_start & collection_date <= window_end ~ "Concluído no prazo",
        completion_status == "Completo" & collection_date < window_start ~ "Concluído cedo",
        completion_status == "Completo" & collection_date > window_end ~ "Concluído atrasado",
        completion_status %in% "Perda de seguimento" ~ "Perdido",
        # Not completed (Incompleto or form not started): open inside the window,
        # lost once the window has closed by the reference date.
        !completion_status %in% "Completo" & reference_date >= window_start &
          reference_date <= window_end ~ "Janela aberta",
        !completion_status %in% "Completo" & reference_date > window_end ~ "Perdido",
        TRUE ~ "Aguardando janela"
      )
    )
  }) |>
    mutate(
      status = factor(
        status,
        levels = c(
          "Aguardando janela",
          "Janela aberta",
          "Concluído no prazo",
          "Concluído cedo",
          "Concluído atrasado",
          "Perdido"
        )
      )
    )
}

followup_overview <- build_followup_data(dados, reference_date)
# One height for the three follow-up cards, sized to the longest polo list.
followup_plot_height <- followup_overview |>
  filter(status %in% c("Janela aberta", "Perdido")) |>
  distinct(followup, polo_base) |>
  count(followup) |>
  pull(n) |>
  c(0) |>
  max() |>
  (\(n) paste0(max(300, 24 * n + 120), "px"))()
followups_in_window <- followup_overview |>
  filter(status == "Janela aberta") |>
  nrow()
followups_overdue <- followup_overview |>
  filter(status == "Perdido") |>
  nrow()
followups_completed_on_time <- followup_overview |>
  filter(status == "Concluído no prazo") |>
  nrow()
followup_stage_summary <- followup_overview |>
  mutate(
    should_follow = window_start <= reference_date,
    completed = completion_status == "Completo",
    open = status == "Janela aberta",
    missed = status == "Perdido"
  ) |>
  group_by(followup) |>
  summarise(
    completed_n = sum(completed, na.rm = TRUE),
    should_follow_n = sum(should_follow, na.rm = TRUE),
    open_n = sum(open, na.rm = TRUE),
    missed_n = sum(missed, na.rm = TRUE),
    completion_percent = if_else(should_follow_n > 0, completed_n / should_follow_n, NA_real_),
    .groups = "drop"
  )

followup_stage_count <- function(stage, column) {
  row <- followup_stage_summary |>
    filter(as.character(followup) == stage)

  if (nrow(row) == 0) {
    return(0L)
  }

  row[[column]][[1]]
}

followup_stage_due_text <- function(stage) {
  row <- followup_stage_summary |>
    filter(as.character(followup) == stage)

  if (nrow(row) == 0) {
    return("0 devem ser seguidos")
  }

  paste0(row$should_follow_n, " devem ser seguidos")
}

followup_stage_label <- function(stage) {
  case_when(
    stage == "1 mês" ~ "mês 1 (T1)",
    stage == "3 meses" ~ "mês 3 (T2)",
    stage == "6 meses" ~ "mês 6 (T3)",
    TRUE ~ stage
  )
}

followup_completed_value <- function(stage) {
  paste0(followup_stage_count(stage, "completed_n"), " / ", total_enrolled)
}

followup_metric_box <- function(title, value, status = c("complete", "open", "missed"), tooltip_text = NULL) {
  status <- match.arg(status)

  tags$div(
    tags$div(
      tags$span(title, class = "followup-metric-title"),
      if (!is.null(tooltip_text)) {
        bslib::tooltip(
          tags$span("i", class = "followup-tooltip", tabindex = "0"),
          tooltip_text,
          placement = "top"
        )
      },
      class = "followup-metric-heading"
    ),
    tags$div(value, class = "followup-metric-value"),
    class = paste("followup-metric", paste0("followup-", status))
  )
}

dashboard_plot_theme <- (
  theme_minimal(base_family = "Arial", base_size = 13) +
    theme(
      axis.text = element_text(size = 11, color = "#30363d"),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 11),
      plot.margin = margin(12, 18, 12, 18),
      panel.grid.major = element_line(color = "#E6ECE8"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", color = ajuri_dark_green)
    )
)

weekly_counts <- function(data, group_col = NULL) {
  out <- data |>
    filter(!is.na(data_atendimento_polo), !is.na(polo_base)) |>
    mutate(
      date = as.Date(data_atendimento_polo),
      week_start = lubridate::floor_date(date, unit = "week")
    )

  if (is.null(group_col)) {
    out |>
      count(week_start, name = "n")
  } else {
    out |>
      count(.data[[group_col]], week_start, name = "n")
  }
}

ggplot2::theme_set(dashboard_plot_theme)

pt_months <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
# Portuguese month labels independent of the browser/webR locale.
pt_month_year <- function(x) ifelse(is.na(x), NA, paste(pt_months[lubridate::month(x)], lubridate::year(x)))
pt_day_month <- function(x) ifelse(is.na(x), NA, paste(lubridate::day(x), pt_months[lubridate::month(x)]))
# Patient counts: whole-number ticks only.
integer_breaks <- function(limits) {
  breaks <- pretty(limits)
  breaks[breaks == round(breaks)]
}

# Month labels for long ranges; day + month when ticks fall within the same month.
pt_date_labels <- function(x) {
  labels <- pt_month_year(x)
  if (anyDuplicated(stats::na.omit(labels))) pt_day_month(x) else labels
}

# Plotly renders text in the browser, so it stays crisp under webR (unlike renderPlot).
as_plotly <- function(plot, tooltip = "all", legend = TRUE) {
  ggplotly(plot, tooltip = tooltip) |>
    layout(
      font = list(size = 13, family = "Arial, sans-serif"),
      margin = list(l = 10, r = 20, t = 20, b = 10),
      xaxis = list(automargin = TRUE),
      yaxis = list(automargin = TRUE),
      showlegend = legend,
      legend = list(title = list(text = ""), orientation = "h",
                    x = 0, xanchor = "left", y = -0.12, yanchor = "top")
    ) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

empty_plot <- function(message = "Sem dados para exibir") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5) +
    theme_void()
}

has_value <- function(x, value = "Sim") {
  coalesce(as.character(x) == value, FALSE)
}

contains_any <- function(x, pattern) {
  stringr::str_detect(coalesce(as.character(x), ""), stringr::regex(pattern, ignore_case = TRUE))
}

numeric_value <- function(x) {
  suppressWarnings(as.numeric(x))
}

first_event_date <- function(...) {
  dates <- list(...)
  out <- as.Date(rep(NA, length(dates[[1]])))

  for (date_value in dates) {
    parsed_date <- as.Date(date_value)
    out <- if_else(is.na(out) & !is.na(parsed_date), parsed_date, out)
  }

  out
}

build_adverse_events <- function(data) {
  allergic_severe_pattern <- "choque|hipotens|dificuldade respirat|chiado|rouquid|ru[ií]do na garganta"
  allergic_event <- data |>
    mutate(
      event_present = has_value(reacao_soro_1) | has_value(reclas_reacao_soro),
      severe_sign = contains_any(tipo_reacao_soro_1, allergic_severe_pattern) |
        contains_any(reclas_tipo_reacao_soro, allergic_severe_pattern),
      event_date = first_event_date(data_hora_reacao_soro_1, reclas_dt_hora_reacao_soro, data_atendimento_polo),
      severity_criteria = case_when(
        severe_sign ~ "Ameaça ou risco de vida",
        TRUE ~ "EA esperado"
      )
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Reação alérgica/aguda",
      event_date,
      severity_criteria,
      source_variable = "reacao_soro_1 / reclas_reacao_soro; tipo_reacao_soro_1 / reclas_tipo_reacao_soro",
      gravidade = if_else(severe_sign, "EAG", "EA")
    )

  serum_sickness <- data |>
    mutate(
      event_present = !is.na(doenca_soro_t2) &
        !contains_any(doenca_soro_t2, "^nenhum|nenhuma"),
      event_date = first_event_date(data_coleta_t2, data_atendimento_polo),
      severity_criteria = "EA esperado"
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Doença do soro",
      event_date,
      severity_criteria,
      source_variable = "doenca_soro_t2",
      gravidade = "EA"
    )

  local_damage_t1 <- data |>
    mutate(
      event_present = has_value(necrose_loc_picada_1) | has_value(reclas_necrose_loc_picada),
      event_date = as.Date(data_atendimento_polo),
      severity_criteria = "EA esperado"
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Dano tecidual local",
      event_date,
      severity_criteria,
      source_variable = "necrose_loc_picada_1 / reclas_necrose_loc_picada",
      gravidade = "EA"
    )

  local_damage_t2 <- data |>
    mutate(
      event_present = has_value(necrose_cut_t2) | has_value(limpeza_teci_necro_t2) | has_value(amputacao_t2),
      surgical = has_value(limpeza_teci_necro_t2) | has_value(amputacao_t2),
      event_date = first_event_date(data_coleta_t2, data_atendimento_polo),
      severity_criteria = if_else(surgical, "Intervenção médica/cirúrgica", "EA esperado")
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Dano tecidual local",
      event_date,
      severity_criteria,
      source_variable = "necrose_cut_t2 / limpeza_teci_necro_t2 / amputacao_t2",
      gravidade = if_else(surgical, "EAG", "EA")
    )

  local_damage_t3 <- data |>
    mutate(
      event_present = has_value(necrose_cut_t3) | has_value(limpeza_teci_necro_t3) | has_value(amputacao_t3),
      surgical = has_value(limpeza_teci_necro_t3) | has_value(amputacao_t3),
      event_date = first_event_date(data_coleta_t3, data_atendimento_polo),
      severity_criteria = if_else(surgical, "Intervenção médica/cirúrgica", "EA esperado")
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Dano tecidual local",
      event_date,
      severity_criteria,
      source_variable = "necrose_cut_t3 / limpeza_teci_necro_t3 / amputacao_t3",
      gravidade = if_else(surgical, "EAG", "EA")
    )

  hypotension <- data |>
    mutate(
      event_present = contains_any(tipo_reacao_soro_1, "hipotens") |
        contains_any(reclas_tipo_reacao_soro, "hipotens") |
        coalesce(numeric_value(pa_sist_soro_1) < 90, FALSE) |
        coalesce(numeric_value(reclas_pa_sist_soro) < 90, FALSE),
      event_date = first_event_date(data_hora_reacao_soro_1, reclas_dt_hora_reacao_soro, data_atendimento_polo),
      severity_criteria = "Ameaça ou risco de vida"
    ) |>
    filter(event_present) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Hipotensão",
      event_date,
      severity_criteria,
      source_variable = "tipo_reacao_soro_1 / reclas_tipo_reacao_soro; pa_sist_soro_1 / reclas_pa_sist_soro",
      gravidade = "EAG"
    )

  deaths <- data |>
    mutate(
      death = tipo_alta_1 == "Óbito" | !is.na(obito_alta_1) | !is.na(obito_alta_2) | !is.na(obito_alta_3) |
        !is.na(data_obito_t2) | !is.na(data_obito_t3) | !is.na(data_obito_t4),
      event_date = first_event_date(data_obito_t2, data_obito_t3, data_obito_t4, data_atendimento_polo)
    ) |>
    filter(death) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Óbito",
      event_date,
      severity_criteria = "Morte",
      source_variable = "tipo_alta_1 / obito_alta_1 / obito_alta_2 / obito_alta_3 / data_obito_t2-t4",
      gravidade = "EAG"
    )

  transfers <- data |>
    mutate(
      transfer = has_value(tansferencia_1) | has_value(reclas_tansferencia),
      event_date = as.Date(data_atendimento_polo)
    ) |>
    filter(transfer) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Hospitalização/transferência",
      event_date,
      severity_criteria = "Hospitalização/transferência",
      source_variable = "tansferencia_1 / reclas_tansferencia",
      gravidade = "EAG"
    )

  permanent_damage <- data |>
    mutate(
      permanent_damage = !is.na(sequela_alta_1) | !is.na(sequela_alta_2) | !is.na(sequela_alta_3) |
        has_value(sequela_cro_acidente_t4),
      event_date = first_event_date(data_coleta_t4, data_coleta_t3, data_coleta_t2, data_atendimento_polo)
    ) |>
    filter(permanent_damage) |>
    transmute(
      record_id, polo_base, trial_arm,
      event_type = "Sequela/dano permanente",
      event_date,
      severity_criteria = "Incapacidade ou dano permanente",
      source_variable = "sequela_alta_1 / sequela_alta_2 / sequela_alta_3 / sequela_cro_acidente_t4",
      gravidade = "EAG"
    )

  bind_rows(
    allergic_event,
    serum_sickness,
    local_damage_t1,
    local_damage_t2,
    local_damage_t3,
    hypotension,
    deaths,
    transfers,
    permanent_damage
  ) |>
    arrange(record_id, event_type, desc(gravidade == "EAG"), event_date) |>
    group_by(record_id, event_type) |>
    summarise(
      polo_base = first(polo_base),
      trial_arm = first(trial_arm),
      event_date = coalesce(first(event_date[!is.na(event_date)]), as.Date(NA)),
      severity_criteria = first(severity_criteria),
      source_variable = paste(unique(source_variable), collapse = "; "),
      gravidade = first(gravidade),
      .groups = "drop"
    ) |>
    mutate(
      event_date = as.Date(event_date),
      event_week = lubridate::floor_date(event_date, unit = "week"),
      event_month = lubridate::floor_date(event_date, unit = "month"),
      gravidade = factor(gravidade, levels = c("EAG", "EA")),
      event_type = factor(
        event_type,
        levels = c(
          "Óbito",
          "Hipotensão",
          "Reação alérgica/aguda",
          "Dano tecidual local",
          "Sequela/dano permanente",
          "Hospitalização/transferência",
          "Doença do soro"
        )
      )
    )
}

adverse_events <- build_adverse_events(dados)
adverse_totals <- adverse_events |>
  summarise(
    eag = sum(gravidade == "EAG", na.rm = TRUE),
    ea = sum(gravidade == "EA", na.rm = TRUE),
    patients_with_eag = n_distinct(record_id[gravidade == "EAG"])
  )

ui <- navset_bar(
  title = "AJURI · Shinylive",
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML("
      html,
      body {
        min-height: 100%;
        height: auto;
        overflow-y: auto;
        background: #F7FAF8;
        color: #17212B;
      }

      .navbar {
        background: #ffffff !important;
        border-bottom: 3px solid #A9D63F;
        box-shadow: 0 2px 10px rgba(20, 83, 58, 0.08);
      }

      .navbar .container-fluid {
        gap: 0.75rem;
      }

      .navbar-brand,
      .navbar-nav .nav-link {
        color: #14533A !important;
        font-weight: 700;
      }

      .navbar-nav {
        flex-wrap: wrap;
        gap: 0.55rem;
        padding: 0.45rem;
        border-radius: 8px;
        background: #EAF7EF;
      }

      .navbar-nav .nav-link {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-height: 42px;
        min-width: 88px;
        padding: 0.65rem 1.35rem !important;
        border: 1px solid #CFEBDD;
        border-radius: 6px;
        background: #FFFFFF;
        font-size: 0.98rem;
        line-height: 1.3;
        white-space: nowrap;
        transition: background-color 120ms ease, border-color 120ms ease, color 120ms ease;
      }

      .navbar-nav .nav-link:hover,
      .navbar-nav .nav-link:focus-visible {
        color: #0F6A43 !important;
        border-color: #00A85A;
        background: #F4FAF5;
      }

      .navbar-nav .nav-link.active {
        color: #FFFFFF !important;
        border-color: #14533A;
        background: #14533A;
        box-shadow: 0 2px 5px rgba(20, 83, 58, 0.18);
      }

      .bslib-page-navbar,
      .bslib-page-fill {
        min-height: auto !important;
        height: auto !important;
        overflow: visible !important;
      }

      .navbar + .container-fluid,
      .tab-content,
      .tab-pane {
        height: auto !important;
        min-height: auto !important;
        overflow: visible !important;
      }

      .tab-pane {
        padding-top: 1.25rem;
        padding-bottom: 1.75rem;
      }

      .bslib-grid {
        margin-bottom: 1.25rem;
        align-items: stretch;
      }

      .value-box,
      .bslib-value-box {
        min-height: 136px;
        height: 100%;
        border: 1px solid #CFEBDD;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(20, 83, 58, 0.07);
      }

      .bslib-value-box > .card-body,
      .value-box > .card-body {
        min-height: 136px;
        display: flex;
        align-items: center;
      }

      .value-box-title,
      .bslib-value-box .value-box-title {
        color: #14533A;
        font-weight: 750;
        line-height: 1.15;
      }

      .value-box-value,
      .bslib-value-box .value-box-value {
        color: #17212B;
        font-weight: 800;
      }

      .card {
        margin-bottom: 1rem;
        overflow: visible;
        border: 1px solid #D8E9DD;
        border-radius: 8px;
        box-shadow: 0 1px 5px rgba(20, 83, 58, 0.06);
      }

      .bslib-grid > .card,
      .bslib-grid > .value-box,
      .bslib-grid > .bslib-value-box {
        height: 100%;
      }

      .card-header {
        font-weight: 650;
        color: #14533A;
        background: #F4FAF5;
        border-bottom: 1px solid #D8E9DD;
      }

      .selectize-control { margin-bottom: 0; }

      .shiny-plot-output,
      .plotly {
        width: 100%;
      }

      .js-plotly-plot .plotly .hoverlayer,
      .js-plotly-plot .hoverlayer {
        z-index: 10000;
      }

      .js-plotly-plot .plotly .hoverlayer .hovertext path,
      .js-plotly-plot .plotly .hoverlayer .hovertext rect,
      .js-plotly-plot .hoverlayer .hovertext path,
      .js-plotly-plot .hoverlayer .hovertext rect {
        fill: #17212b !important;
        stroke: #17212b !important;
        fill-opacity: 0.96 !important;
        stroke-opacity: 1 !important;
      }

      .js-plotly-plot .plotly .hoverlayer .hovertext text,
      .js-plotly-plot .plotly .hoverlayer .hovertext tspan,
      .js-plotly-plot .hoverlayer .hovertext text,
      .js-plotly-plot .hoverlayer .hovertext tspan {
        fill: #ffffff !important;
        font-family: Arial, sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
      }

      .js-plotly-plot .plotly .hoverlayer .spikeline {
        stroke: #14533A !important;
      }

      #summary_table {
        overflow-x: auto;
      }

      .followup-metric-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 0.75rem;
        margin-bottom: 0.75rem;
      }

      .followup-metric {
        min-height: 108px;
        height: 100%;
        border: 1px solid #D8E9DD;
        border-radius: 8px;
        background: #fff;
        padding: 0.85rem 0.95rem;
        box-shadow: 0 1px 5px rgba(20, 83, 58, 0.06);
      }

      .followup-metric-heading {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 0.5rem;
        margin-bottom: 0.4rem;
      }

      .followup-metric-title {
        font-size: 0.82rem;
        line-height: 1.15;
        font-weight: 650;
        color: #14533A;
      }

      .followup-metric-value {
        font-size: 1.65rem;
        line-height: 1;
        font-weight: 750;
        color: #17212B;
      }

      .followup-complete { border-left: 4px solid #00A85A; }
      .followup-open { border-left: 4px solid #F2C94C; }
      .followup-missed { border-left: 4px solid #B22222; }

      .followup-tooltip {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 auto;
        width: 18px;
        height: 18px;
        border-radius: 50%;
        background: #EAF7EF;
        color: #14533A;
        font-size: 0.72rem;
        font-weight: 700;
        cursor: help;
      }

      .tooltip-inner {
        max-width: 260px;
        text-align: left;
        background: #17212b;
        color: #ffffff;
        opacity: 1;
      }

      .tooltip {
        opacity: 1 !important;
        z-index: 10050;
      }

      .polo-filter-panel .control-label {
        font-size: 0.86rem;
        font-weight: 650;
        color: #14533A;
        margin-bottom: 0.5rem;
      }

      .polo-filter-panel .shiny-options-group {
        max-height: 240px;
        overflow-y: auto;
        padding-right: 0.35rem;
      }

      .polo-filter-panel .checkbox {
        margin-top: 0;
        margin-bottom: 0.35rem;
      }

      .polo-filter-panel label {
        font-size: 0.86rem;
        line-height: 1.15;
      }

      .polo-filter-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.4rem;
        margin-top: 0.75rem;
      }

      .polo-filter-actions .btn {
        padding: 0.25rem 0.55rem;
        font-size: 0.78rem;
      }

      .arm-kpi {
        display: grid;
        gap: 0.25rem;
        min-width: 150px;
      }

      .arm-kpi > div {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 0.75rem;
        font-size: 1rem;
      }

      .arm-kpi span {
        font-weight: 650;
        color: #14533A;
      }

      .arm-kpi strong {
        font-size: 1.25rem;
        color: #17212B;
      }

      .ajuri-footer {
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 1rem 1rem 1.25rem;
        margin-top: 1rem;
        border-top: 1px solid #E3E8E4;
        background: #F7F7F7;
      }

      .ajuri-footer img {
        display: block;
        width: min(280px, 62vw);
        height: auto;
      }

      @media (max-width: 768px) {
        .navbar-nav {
          gap: 0.35rem;
          padding: 0.35rem;
        }

        .navbar-nav .nav-link {
          min-height: 38px;
          min-width: 76px;
          padding: 0.5rem 0.9rem !important;
          font-size: 0.9rem;
        }

        .card-header {
          font-size: 0.95rem;
        }

        .value-box,
        .bslib-value-box,
        .bslib-value-box > .card-body,
        .value-box > .card-body {
          min-height: 116px;
        }

        .followup-metric-grid {
          grid-template-columns: 1fr;
          gap: 0.55rem;
        }

        .followup-metric {
          min-height: 78px;
          padding: 0.7rem 0.75rem;
        }

        .followup-metric-title {
          font-size: 0.78rem;
        }

        .followup-metric-value {
          font-size: 1.35rem;
        }

        .ajuri-footer img {
          width: min(230px, 70vw);
        }
      }
    "))
  ),
  footer = tags$footer(
    class = "ajuri-footer",
    tags$img(
      src = "logo_ajuri_longa.jpeg",
      alt = "AJURI - Decentralization of Antivenoms for Indigenous Communities"
    )
  ),
  nav_panel(
    "Geral",
    do.call(layout_columns, c(list(
      col_widths = rep(12 / length(kpi_boxes), length(kpi_boxes)),
      fill = FALSE,
      fillable = FALSE
    ), kpi_boxes)),
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      sidebar = sidebar(
        title = "Comparar polos",
        width = 330,
        open = "always",
        div(
          checkboxGroupInput(
            "cumulative_polos",
            "Polos no gráfico",
            choices = polo_choices,
            selected = default_cumulative_polos
          ),
          div(
            actionButton("cumulative_default", "Padrão", class = "btn-outline-primary btn-sm"),
            actionButton("cumulative_all", "Todos", class = "btn-outline-secondary btn-sm"),
            actionButton("cumulative_clear", "Limpar", class = "btn-outline-secondary btn-sm"),
            class = "polo-filter-actions"
          ),
          class = "polo-filter-panel"
        )
      ),
      card(
        card_header("Número cumulativo de pacientes por polo, por mês"),
        plotlyOutput("cumulativeByPolo", height = "430px")
      )
    ),
    layout_columns(
      col_widths = c(12),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("Percentual do total de pacientes inseridos no estudo, por polo"),
        plotlyOutput("percentByPolo", height = paste0(max(320, 26 * length(polo_choices) + 90), "px"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("Variáveis com alto % de dados incompletos"),
        plotlyOutput("missingVars", height = "380px")
      ),
      card(
        card_header("Número de pacientes inseridos por semana"),
        plotlyOutput("weeklyEnrollment", height = "380px")
      )
    ),
    tags$div()
  ),
  nav_panel(
    "Análise Exploratória",
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      sidebar = sidebar(
        selectInput(
          "x",
          "Selecione uma ou mais variáveis",
          choices = explore_choices,
          selected = explore_defaults,
          multiple = TRUE
        ),
        selectInput("y", "Estratificação", choices = stratify_choices, selected = "None"),
        downloadButton("download_summary_table", "Exportar tabela", class = "btn-primary btn-sm")
      ),
      card(
        card_header("Resumo das variáveis selecionadas"),
        htmlOutput("summary_table"),
        min_height = "500px"
      )
    )
  ),
  nav_panel(
    "Projeções",
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("Número de pacientes inseridos no estudo por semana"),
        plotlyOutput("weeklyStudy", height = "400px")
      ),
      card(
        card_header("Projeção de pacientes por semana"),
        plotlyOutput("projection", height = "400px")
      )
    ),
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      sidebar = sidebar(
        title = "Polo de interesse",
        width = 330,
        open = "always",
        selectInput(
          "polo",
          NULL,
          choices = sort(unique(na.omit(dados$polo_base))),
          selected = sort(unique(na.omit(dados$polo_base)))[[1]],
          multiple = FALSE
        )
      ),
      card(
        card_header("Número de pacientes inseridos no estudo por semana, por polo"),
        plotlyOutput("weeklyPolo", height = "380px")
      )
    )
  ),
  nav_panel(
    "Seguimento",
    tags$div(
      followup_metric_box("Completos mês 1 (T1)", followup_completed_value("1 mês"), "complete"),
      followup_metric_box("Completos mês 3 (T2)", followup_completed_value("3 meses"), "complete"),
      followup_metric_box("Completos mês 6 (T3)", followup_completed_value("6 meses"), "complete"),
      class = "followup-metric-grid"
    ),
    tags$div(
      followup_metric_box(
        "Aberto mês 1 (T1)",
        followup_stage_count("1 mês", "open_n"),
        "open",
        tooltip_text = "Janela de seguimento: 7 dias antes até 7 dias depois da data devida."
      ),
      followup_metric_box(
        "Aberto mês 3 (T2)",
        followup_stage_count("3 meses", "open_n"),
        "open",
        tooltip_text = "Janela de seguimento: 7 dias antes até 7 dias depois da data devida."
      ),
      followup_metric_box(
        "Aberto mês 6 (T3)",
        followup_stage_count("6 meses", "open_n"),
        "open",
        tooltip_text = "Janela de seguimento: 7 dias antes até 7 dias depois da data devida."
      ),
      class = "followup-metric-grid"
    ),
    tags$div(
      followup_metric_box(
        "Perdido mês 1 (T1)",
        followup_stage_count("1 mês", "missed_n"),
        "missed",
        tooltip_text = "Marcado como perda de seguimento, ou a janela fechou (7 dias após a data devida) sem o seguimento completo."
      ),
      followup_metric_box(
        "Perdido mês 3 (T2)",
        followup_stage_count("3 meses", "missed_n"),
        "missed",
        tooltip_text = "Marcado como perda de seguimento, ou a janela fechou (7 dias após a data devida) sem o seguimento completo."
      ),
      followup_metric_box(
        "Perdido mês 6 (T3)",
        followup_stage_count("6 meses", "missed_n"),
        "missed",
        tooltip_text = "Marcado como perda de seguimento, ou a janela fechou (7 dias após a data devida) sem o seguimento completo."
      ),
      class = "followup-metric-grid"
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("Janela aberta e perdido por unidade - T1"),
        plotlyOutput("followupUnitStatusT1", height = followup_plot_height)
      ),
      card(
        card_header("Janela aberta e perdido por unidade - T2"),
        plotlyOutput("followupUnitStatusT2", height = followup_plot_height)
      ),
      card(
        card_header("Janela aberta e perdido por unidade - T3"),
        plotlyOutput("followupUnitStatusT3", height = followup_plot_height)
      )
    )
  ),
  nav_panel(
    "Eventos Adversos",
    layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      fillable = FALSE,
      value_box(
        title = "Eventos adversos graves",
        value = adverse_totals$eag,
        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
        theme = warning_soft_red
      ),
      value_box(
        title = "Eventos adversos não graves",
        value = adverse_totals$ea,
        showcase = bsicons::bs_icon("activity"),
        theme = fmt_light_green
      ),
      value_box(
        title = "Pacientes com pelo menos 1 EAG",
        value = paste0(adverse_totals$patients_with_eag, " / ", total_enrolled),
        showcase = bsicons::bs_icon("person-lines-fill"),
        theme = warning_soft_red
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("EAG por semana e tipo de evento"),
        plotlyOutput("eagTimePlot", height = "380px")
      ),
      card(
        card_header("EA e EAG por evento esperado"),
        plotlyOutput("aeTypePlot", height = "380px")
      )
    ),
    layout_columns(
      col_widths = c(12),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header("Critérios de gravidade dos EAG"),
        plotlyOutput("eagCriteriaPlot", height = "360px")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$cumulative_default, {
    updateCheckboxGroupInput(session, "cumulative_polos", selected = default_cumulative_polos)
  }, ignoreInit = TRUE)

  observeEvent(input$cumulative_all, {
    updateCheckboxGroupInput(session, "cumulative_polos", selected = polo_choices)
  }, ignoreInit = TRUE)

  observeEvent(input$cumulative_clear, {
    updateCheckboxGroupInput(session, "cumulative_polos", selected = character(0))
  }, ignoreInit = TRUE)

  output$cumulativeByPolo <- renderPlotly({
    selected_polos <- input$cumulative_polos
    if (is.null(selected_polos)) {
      selected_polos <- default_cumulative_polos
    }

    monthly <- dados |>
      filter(!is.na(data_atendimento_polo), !is.na(polo_base), polo_base %in% selected_polos) |>
      mutate(month = lubridate::floor_date(as.Date(data_atendimento_polo), unit = "month")) |>
      count(polo_base, month, name = "n")

    if (nrow(monthly) == 0) {
      return(as_plotly(legend = FALSE, empty_plot("Selecione pelo menos um polo")))
    }

    # Every month from the first enrollment to the last (or the reference month),
    # so each polo shows a continuous cumulative line, starting at 0 before activation.
    last_month <- max(max(monthly$month), lubridate::floor_date(reference_date, unit = "month"))
    months <- seq(min(monthly$month), last_month, by = "month")
    plot_data <- monthly |>
      tidyr::complete(polo_base, month = months, fill = list(n = 0)) |>
      arrange(polo_base, month) |>
      group_by(polo_base) |>
      mutate(
        cum_enrolled = cumsum(n),
        hover_text = paste0(
          "Polo: ", polo_base,
          "<br>Mês: ", pt_month_year(month),
          "<br>Novos no mês: ", n,
          "<br>Pacientes cumulativos: ", cum_enrolled
        )
      ) |>
      ungroup()
    month_breaks <- if (length(months) <= 12) months else scales::breaks_pretty(8)

    plot <- plot_data |>
      ggplot(aes(
        x = month,
        y = cum_enrolled,
        color = polo_base,
        group = polo_base,
        text = hover_text
      )) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.4) +
      labs(y = "Pacientes cumulativos", x = NULL, color = "Polo") +
      scale_x_date(labels = pt_month_year, breaks = month_breaks,
                   expand = expansion(add = c(12, 12))) +
      scale_y_continuous(labels = comma, limits = c(0, NA), breaks = integer_breaks,
                         expand = expansion(mult = c(0, 0.08))) +
      theme(legend.position = "bottom")

    as_plotly(plot, tooltip = "text")
  })

  output$percentByPolo <- renderPlotly({
    plot_data <- enrollment_summary |>
      mutate(hover_text = paste0("Polo: ", polo_base, "<br>Pacientes: ", Enrolled, " / ", Target,
                                 "<br>", Percent, "% da meta"))
    plot <- plot_data |>
      ggplot(aes(x = reorder(polo_base, Percent), y = Percent, text = hover_text)) +
      geom_col(aes(fill = Percent >= 100), width = 0.72) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "#7A8580") +
      coord_flip() +
      scale_fill_manual(values = c(`TRUE` = ajuri_dark_green, `FALSE` = ajuri_green), guide = "none") +
      scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = scales::breaks_width(25),
                         limits = c(0, max(110, max(plot_data$Percent) * 1.05)),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "% da meta por polo")
    as_plotly(plot, tooltip = "text", legend = FALSE)
  })

  output$missingVars <- renderPlotly({
    missing_data <- dados |>
      summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
      pivot_longer(everything(), names_to = "Variable", values_to = "Missing") |>
      filter(Missing > 0) |>
      arrange(desc(Missing)) |>
      slice_head(n = 10) |>
      mutate(hover_text = paste0(Variable, "<br>", round(Missing, 1), "% incompleto"))

    if (nrow(missing_data) == 0) {
      return(as_plotly(empty_plot("Sem dados incompletos"), legend = FALSE))
    }

    plot <- missing_data |>
      ggplot(aes(x = reorder(Variable, Missing), y = Missing, text = hover_text)) +
      geom_col(fill = warning_red, width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 100),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "% de registros sem valor")
    as_plotly(plot, tooltip = "text", legend = FALSE)
  })

  output$weeklyEnrollment <- renderPlotly({
    plot <- weekly_counts(dados) |>
      mutate(hover_text = paste0("Semana de ", format(week_start, "%d/%m/%Y"), "<br>Pacientes: ", n)) |>
      ggplot(aes(x = week_start, y = n, text = hover_text)) +
      geom_col(fill = ajuri_green) +
      scale_x_date(labels = pt_date_labels, breaks = scales::breaks_pretty(6)) +
      scale_y_continuous(breaks = scales::breaks_pretty(), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Pacientes por semana")
    as_plotly(plot, tooltip = "text", legend = FALSE)
  })

  summary_table <- reactive({
    req(input$x)
    selected_columns <- intersect(input$x, names(dados))
    req(length(selected_columns) > 0)

    if (is.null(input$y) || input$y == "None") {
      dados |>
        select(all_of(selected_columns)) |>
        tbl_summary(
          type = list(
            all_continuous() ~ "continuous2",
            ends_with("_num") ~ "continuous2",
            all_dichotomous() ~ "categorical"
          ),
          statistic = list(
            all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
            all_categorical() ~ "{n} / {N} ({p}%)"
          ),
          missing_text = "Missing"
        ) |>
        bold_labels()
    } else {
      dados |>
        select(all_of(selected_columns), all_of(input$y)) |>
        tbl_summary(
          by = input$y,
          type = list(
            all_continuous() ~ "continuous2",
            ends_with("_num") ~ "continuous2",
            all_dichotomous() ~ "categorical"
          ),
          statistic = list(
            all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
            all_categorical() ~ "{n} / {N} ({p}%)"
          ),
          missing_text = "Missing"
        ) |>
        bold_labels()
    }
  })

  output$summary_table <- renderUI({
    summary_table() |>
      as_gt() |>
      gt::as_raw_html(inline_css = FALSE) |>
      HTML()
  })

  output$download_summary_table <- downloadHandler(
    filename = function() {
      paste0("ajuri_analise_exploratoria_", Sys.Date(), ".csv")
    },
    content = function(file) {
      summary_table() |>
        as_tibble(col_labels = TRUE) |>
        readr::write_csv(file, na = "")
    }
  )

  output$weeklyStudy <- renderPlotly({
    plot <- weekly_counts(dados) |>
      ggplot(aes(x = week_start, y = n)) +
      geom_col(linewidth = 1.2, fill = duke_blue) +
      geom_hline(aes(yintercept = meta_semanal, linetype = "Meta semanal"), color = "darkgray") +
      scale_linetype_manual(name = NULL, values = "dashed") +
      labs(y = NULL, x = NULL) +
      scale_x_date(labels = pt_date_labels, breaks = scales::breaks_pretty(6)) +
      theme(legend.position = "bottom")

    as_plotly(plot)
  })

  output$weeklyPolo <- renderPlotly({
    req(input$polo)

    plot_data <- dados |>
      filter(!is.na(data_atendimento_polo), polo_base == input$polo) |>
      mutate(week_start = lubridate::floor_date(as.Date(data_atendimento_polo), unit = "week")) |>
      count(week_start, name = "n")

    plot <- plot_data |>
      ggplot(aes(x = week_start, y = n)) +
      geom_col(linewidth = 1.2, fill = duke_blue) +
      labs(y = NULL, x = NULL) +
      scale_x_date(labels = pt_date_labels, breaks = scales::breaks_pretty(6)) +
      theme(legend.position = "bottom")

    as_plotly(plot)
  })

  output$projection <- renderPlotly({
    current_enrollment <- weekly_counts(dados) |>
      rename(date = week_start) |>
      mutate(
        status = "Observado",
        error = 0,
        nmax = n,
        nmin = n
      )

    req(nrow(current_enrollment) > 0)

    mean_enrollment <- current_enrollment |>
      summarise(
        mean = mean(n, na.rm = TRUE),
        sd = coalesce(sd(n, na.rm = TRUE), 0)
      )

    future_weeks <- 20
    future_dates <- seq(max(current_enrollment$date) + 7, by = "week", length.out = future_weeks)

    future_enrollment <- tibble(
      date = future_dates,
      n = rep(mean_enrollment$mean, future_weeks),
      nmax = rep(mean_enrollment$mean + mean_enrollment$sd, future_weeks),
      nmin = pmax(rep(mean_enrollment$mean - mean_enrollment$sd, future_weeks), 0),
      status = "Projetado",
      error = mean_enrollment$sd
    )

    projection_data <- current_enrollment |>
      bind_rows(future_enrollment) |>
      arrange(date) |>
      mutate(
        cum_enrolled = cumsum(n),
        cum_enrolled_max = cumsum(nmax),
        cum_enrolled_min = cumsum(nmin)
      )

    plot <- projection_data |>
      ggplot(aes(x = date, y = cum_enrolled, color = status, fill = status)) +
      geom_line(linewidth = 1.2) +
      geom_ribbon(aes(ymin = cum_enrolled_min, ymax = cum_enrolled_max), alpha = 0.25, color = NA,
                  show.legend = FALSE) +
      geom_hline(yintercept = target_total, linetype = "dashed", color = "#7A8580") +
      labs(y = NULL, x = NULL) +
      scale_color_manual(values = c("Observado" = "black", "Projetado" = duke_blue)) +
      scale_fill_manual(values = c("Observado" = "black", "Projetado" = duke_blue)) +
      scale_x_date(labels = pt_date_labels, breaks = scales::breaks_pretty(6)) +
      scale_y_continuous(labels = comma, breaks = scales::breaks_pretty(6)) +
      guides(fill = "none") +
      labs(y = "Pacientes cumulativos")

    plotly_projection <- as_plotly(plot)
    # ggplotly adds one legend item per ribbon; keep only the line traces.
    for (i in seq_along(plotly_projection$x$data)) {
      trace <- plotly_projection$x$data[[i]]
      if (!identical(trace$mode, "lines") || identical(trace$fill, "toself")) {
        plotly_projection$x$data[[i]]$showlegend <- FALSE
      }
    }
    plotly_projection
  })

  followup_unit_status_plot <- function(stage) {
    unit_data <- followup_overview |>
      filter(as.character(followup) == stage, status %in% c("Janela aberta", "Perdido")) |>
      count(polo_base, status, name = "n") |>
      mutate(
        polo_ordered = forcats::fct_reorder(polo_base, n),
        hover_text = paste0(
          "Polo: ", polo_base,
          "<br>Seguimento: ", followup_stage_label(stage),
          "<br>Status: ", status,
          "<br>Pacientes: ", n
        )
      )

    if (nrow(unit_data) == 0) {
      return(as_plotly(legend = FALSE, empty_plot("Nenhum seguimento em janela ou perdido")))
    }

    plot <- unit_data |>
      ggplot(aes(x = polo_ordered, y = n, fill = status, text = hover_text)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(
        values = c("Janela aberta" = open_yellow, "Perdido" = warning_red),
        drop = FALSE
      ) +
      scale_y_continuous(breaks = breaks_width(1), expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
      )

    as_plotly(plot, tooltip = "text")
  }

  output$followupUnitStatusT1 <- renderPlotly({
    followup_unit_status_plot("1 mês")
  })

  output$followupUnitStatusT2 <- renderPlotly({
    followup_unit_status_plot("3 meses")
  })

  output$followupUnitStatusT3 <- renderPlotly({
    followup_unit_status_plot("6 meses")
  })

  output$eagTimePlot <- renderPlotly({
    plot_data <- adverse_events |>
      filter(gravidade == "EAG", !is.na(event_week)) |>
      count(event_week, event_type, name = "n") |>
      mutate(
        hover_text = paste0(
          "Semana: ", format(event_week, "%d/%m/%Y"),
          "<br>Evento: ", event_type,
          "<br>EAG: ", n
        )
      )

    if (nrow(plot_data) == 0) {
      return(as_plotly(legend = FALSE, empty_plot("Nenhum EAG identificado")))
    }

    plot <- plot_data |>
      ggplot(aes(x = event_week, y = n, fill = event_type, text = hover_text)) +
      geom_col() +
      scale_x_date(labels = pt_date_labels, breaks = scales::breaks_pretty(6)) +
      scale_y_continuous(breaks = breaks_width(2), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.position = "bottom"
      )

    as_plotly(plot, tooltip = "text")
  })

  output$aeTypePlot <- renderPlotly({
    plot_data <- adverse_events |>
      filter(!is.na(event_type)) |>
      count(event_type, gravidade, name = "n") |>
      mutate(
        hover_text = paste0(
          "Evento: ", event_type,
          "<br>Gravidade: ", gravidade,
          "<br>Pacientes/eventos: ", n
        )
      )

    if (nrow(plot_data) == 0) {
      return(as_plotly(legend = FALSE, empty_plot("Nenhum evento adverso identificado")))
    }

    plot <- plot_data |>
      mutate(event_type_wrapped = stringr::str_wrap(as.character(event_type), width = 24)) |>
      ggplot(aes(x = reorder(event_type_wrapped, n), y = n, fill = gravidade, text = hover_text)) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_fill_manual(values = c("EAG" = warning_red, "EA" = open_yellow), drop = FALSE) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11)
      )

    as_plotly(plot, tooltip = "text")
  })

  output$eagCriteriaPlot <- renderPlotly({
    plot_data <- adverse_events |>
      filter(gravidade == "EAG", !is.na(severity_criteria)) |>
      count(severity_criteria, name = "n") |>
      arrange(n) |>
      mutate(
        hover_text = paste0(
          "Critério: ", severity_criteria,
          "<br>EAG: ", n
        )
      )

    if (nrow(plot_data) == 0) {
      return(as_plotly(legend = FALSE, empty_plot("Nenhum critério de EAG identificado")))
    }

    plot <- plot_data |>
      mutate(severity_criteria_wrapped = stringr::str_wrap(severity_criteria, width = 28)) |>
      ggplot(aes(x = reorder(severity_criteria_wrapped, n), y = n, text = hover_text)) +
      geom_col(fill = warning_red) +
      coord_flip() +
      scale_y_continuous(breaks = breaks_width(1), expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11)
      )

    as_plotly(plot, tooltip = "text", legend = FALSE)
  })
}


  list(ui = ui, server = server, total_enrolled = total_enrolled, followup = followup_overview)
}
