suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(lubridate)
})

set.seed(20260706)

# Run from the repository root. Keep synthetic CSVs outside shinylive/ so
# the browser export does not bundle participant-shaped test data.
proj <- normalizePath(".")
output_dir <- file.path(proj, "data")
output_file <- Sys.getenv("AJURI_SIMULATED_OUTPUT", unset = file.path(output_dir, "Simulated_Data.csv"))
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

current_date <- as.Date(Sys.getenv("AJURI_REFERENCE_DATE", unset = "2026-08-31"))

trial_target_sample <- 190
trial_target_per_arm <- 96
clusters_per_arm_with_inflation <- 10
baseline_clusters_per_arm <- 7
baseline_target_per_arm <- 84
average_patients_per_unit <- 10

n <- trial_target_sample

dictionary_dirs <- c(file.path(proj, "data"), file.path(proj, "shinylive", "data"))
dictionary_files <- unlist(lapply(dictionary_dirs[dir.exists(dictionary_dirs)], function(path) {
  list.files(path, pattern = "^AJURI_DataDictionary_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$", full.names = TRUE)
}))
if (!length(dictionary_files)) stop("No AJURI data dictionary found.", call. = FALSE)
latest_dictionary <- dictionary_files[which.max(as.Date(str_extract(basename(dictionary_files), "[0-9]{4}-[0-9]{2}-[0-9]{2}")))]

dict <- readr::read_csv(latest_dictionary, show_col_types = FALSE) |>
  transmute(
    field_name = `Variable / Field Name`,
    form_name = `Form Name`,
    field_type = tolower(`Field Type`),
    field_label = `Field Label`,
    choices = `Choices, Calculations, OR Slider Labels`,
    validation = `Text Validation Type OR Show Slider Number`,
    validation_min = `Text Validation Min`,
    validation_max = `Text Validation Max`,
    branching = `Branching Logic (Show field only if...)`
  ) |>
  filter(!is.na(field_name), field_name != "")

parse_choices <- function(choice_text) {
  if (is.na(choice_text) || !nzchar(choice_text)) {
    return(tibble(code = character(), label = character()))
  }

  stringr::str_split(choice_text, "\\s*\\|\\s*")[[1]] |>
    purrr::map_dfr(\(choice) {
      pieces <- stringr::str_match(choice, "^\\s*([^,]+),\\s*(.*?)\\s*$")
      if (is.na(pieces[1, 1])) {
        tibble(code = stringr::str_trim(choice), label = stringr::str_trim(choice))
      } else {
        tibble(code = stringr::str_trim(pieces[1, 2]), label = stringr::str_trim(pieces[1, 3]))
      }
    }) |>
    filter(label != "")
}

choice_lookup <- dict |>
  mutate(choice_table = purrr::map(choices, parse_choices)) |>
  select(field_name, choice_table) |>
  deframe()

choice_labels <- function(field_name) {
  choices <- choice_lookup[[field_name]]
  if (is.null(choices) || nrow(choices) == 0) {
    character()
  } else {
    choices$label
  }
}

choice_label_for_code <- function(field_name, code) {
  choices <- choice_lookup[[field_name]]
  if (is.null(choices) || nrow(choices) == 0) {
    return(code)
  }

  label <- choices$label[choices$code == code]
  if (length(label) == 0) code else label[[1]]
}

build_cluster_plan <- function() {
  polo_labels <- choice_labels("polo_base")
  if (length(polo_labels) < clusters_per_arm_with_inflation * 2) {
    stop("The data dictionary needs at least 20 polo_base choices for cluster generation.", call. = FALSE)
  }

  cluster_count <- clusters_per_arm_with_inflation * 2
  intervention_total <- trial_target_per_arm
  control_total <- trial_target_sample - intervention_total

  arm_cluster_sizes <- function(total) {
    sizes <- rep(floor(total / clusters_per_arm_with_inflation), clusters_per_arm_with_inflation)
    remainder <- total %% clusters_per_arm_with_inflation
    if (remainder > 0) {
      sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1
    }
    sizes
  }

  cluster_sizes <- c(arm_cluster_sizes(intervention_total), arm_cluster_sizes(control_total))

  tibble(
    cluster_id = sprintf("CL%02d", seq_len(cluster_count)),
    cluster_sequence = seq_len(cluster_count),
    polo_base = polo_labels[seq_len(cluster_count)],
    trial_arm = rep(c("Intervencao", "Controle"), each = clusters_per_arm_with_inflation),
    cluster_target = cluster_sizes,
    cluster_activation_date = seq(as.Date("2026-01-05"), by = "1 week", length.out = cluster_count),
    dsei = if_else(str_detect(polo_base, "^Alto Solimões"), "Alto Rio Solimões", "Manaus")
  )
}

cluster_plan <- build_cluster_plan()
patient_cluster <- cluster_plan[rep(seq_len(nrow(cluster_plan)), cluster_plan$cluster_target), ] |>
  arrange(cluster_sequence) |>
  mutate(
    patient_index_in_cluster = row_number(),
    enrollment_offset_days = sort(sample(0:35, n(), replace = TRUE)),
    data_atendimento_polo = cluster_activation_date + enrollment_offset_days,
    .by = cluster_id
  ) |>
  ungroup()

sample_date <- function(size, min_value = NA_character_, max_value = NA_character_) {
  min_date <- suppressWarnings(as.Date(min_value))
  max_date <- suppressWarnings(as.Date(max_value))

  if (is.na(min_date)) min_date <- as.Date("2022-01-01")
  if (is.na(max_date)) max_date <- min(current_date, as.Date("2030-01-01"))
  if (max_date < min_date) max_date <- min_date + 365

  sample(seq(min_date, max_date, by = "day"), size, replace = TRUE)
}

sample_datetime <- function(size) {
  dates <- sample_date(size)
  times <- sprintf(
    "%02d:%02d:00",
    sample(0:23, size, replace = TRUE),
    sample(c(0, 15, 30, 45), size, replace = TRUE)
  )
  as.POSIXct(paste(dates, times), tz = "America/Sao_Paulo")
}

sample_time <- function(size) {
  sprintf(
    "%02d:%02d",
    sample(0:23, size, replace = TRUE),
    sample(c(0, 15, 30, 45), size, replace = TRUE)
  )
}

sample_integer <- function(size, min_value = NA_character_, max_value = NA_character_) {
  min_num <- suppressWarnings(as.integer(min_value))
  max_num <- suppressWarnings(as.integer(max_value))

  if (is.na(min_num)) min_num <- 0L
  if (is.na(max_num)) max_num <- max(min_num + 10L, 100L)

  sample(seq(min_num, max_num), size, replace = TRUE)
}

sample_number <- function(size, min_value = NA_character_, max_value = NA_character_) {
  min_num <- suppressWarnings(as.numeric(min_value))
  max_num <- suppressWarnings(as.numeric(max_value))

  if (is.na(min_num)) min_num <- 0
  if (is.na(max_num)) max_num <- max(min_num + 10, 100)

  round(runif(size, min_num, max_num), 1)
}

sample_text <- function(row, size) {
  name <- row$field_name
  label <- tolower(coalesce(row$field_label, ""))
  validation <- coalesce(row$validation, "")

  # Dictionary validation takes precedence over variable-name heuristics.
  if (stringr::str_detect(validation, "^datetime")) {
    return(format(sample_datetime(size), "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo"))
  }

  if (stringr::str_detect(validation, "date")) {
    return(as.character(sample_date(size, row$validation_min, row$validation_max)))
  }

  if (validation == "time") {
    return(sample_time(size))
  }

  if (validation == "integer") {
    return(sample_integer(size, row$validation_min, row$validation_max))
  }

  if (validation == "number") {
    return(sample_number(size, row$validation_min, row$validation_max))
  }

  if (name == "record_id") {
    return(sprintf("REC_%03d", seq_len(size)))
  }

  if (stringr::str_detect(name, "record_id|_id$")) {
    return(replicate(
      size,
      paste0(sample(c(LETTERS, 0:9), 10, replace = TRUE), collapse = "")
    ))
  }

  if (stringr::str_detect(name, "nome|name")) {
    return(sample(
      c("Ana Silva", "Bruno Costa", "Carla Santos", "Daniel Oliveira", "Elisa Pereira"),
      size,
      replace = TRUE
    ))
  }

  if (stringr::str_detect(name, "telefone")) {
    return(sprintf(
      "(%02d) 9%04d-%04d",
      sample(11:99, size, replace = TRUE),
      sample(0:9999, size, replace = TRUE),
      sample(0:9999, size, replace = TRUE)
    )
    )
  }

  if (stringr::str_detect(name, "lote")) {
    return(paste0("L", sample(100000:999999, size, replace = TRUE)))
  }

  if (stringr::str_detect(name, "dose")) {
    return(sample(c("5 mg", "10 mg", "20 mg", "500 mg"), size, replace = TRUE))
  }

  if (stringr::str_detect(name, "peso")) {
    return(sample_number(size, row$validation_min, row$validation_max))
  }

  if (stringr::str_detect(name, "altura")) {
    return(sample_integer(size, row$validation_min, row$validation_max))
  }

  if (stringr::str_detect(name, "temperatura|temp|febre")) {
    return(sample_number(size, row$validation_min, row$validation_max))
  }

  if (stringr::str_detect(name, "press_art|pa_sist|pa_diast|pressao|press.o")) {
    return(sample_integer(size, row$validation_min, row$validation_max))
  }


  sample(
    c(
      "Sem intercorrencias",
      "Acompanhamento em andamento",
      "Informacao coletada no atendimento",
      "Paciente orientado"
    ),
    size,
    replace = TRUE
  )
}

generate_field <- function(row, size) {
  labels <- choice_labels(row$field_name)

  if (row$field_type %in% c("radio", "dropdown") && length(labels) > 0) {
    return(sample(labels, size, replace = TRUE))
  }

  # An empty choice list is a dictionary issue; leave it missing instead
  # of inventing values (e.g. outro_reclas_doenca_conco in the August dictionary).
  if (row$field_type %in% c("checkbox", "radio", "dropdown") && !length(labels)) {
    return(rep(NA_character_, size))
  }

  if (row$field_type == "checkbox" && length(labels) > 0) {
    return(map_chr(seq_len(size), \(i) {
      selected <- sample(labels, sample(seq_along(labels), 1), replace = FALSE)
      paste(selected, collapse = ", ")
    }))
  }

  if (row$field_type == "file") {
    return(paste0(row$field_name, "_", sample(10000:99999, size, replace = TRUE), ".pdf"))
  }

  if (row$field_type == "notes") {
    return(sample(
      c(
        "Observacao registrada durante o acompanhamento.",
        "Paciente orientado e sem novas queixas.",
        "Revisao clinica documentada."
      ),
      size,
      replace = TRUE
    ))
  }

  sample_text(row, size)
}

branch_mask <- function(data, branching) {
  if (is.na(branching) || !nzchar(branching)) {
    return(rep(TRUE, nrow(data)))
  }

  checkbox_match <- stringr::str_match(
    branching,
    "\\[([A-Za-z0-9_]+)\\(([^\\)]+)\\)\\]\\s*=\\s*['\"]([^'\"]+)['\"]"
  )

  if (!is.na(checkbox_match[1, 1])) {
    field <- checkbox_match[1, 2]
    code <- checkbox_match[1, 3]
    label <- choice_label_for_code(field, code)

    if (!field %in% names(data)) return(rep(TRUE, nrow(data)))
    return(stringr::str_detect(coalesce(as.character(data[[field]]), ""), stringr::fixed(label)))
  }

  comparison_match <- stringr::str_match(
    branching,
    "\\[([A-Za-z0-9_]+)\\]\\s*(=|<>)\\s*['\"]?([^'\"]*)['\"]?"
  )

  if (is.na(comparison_match[1, 1])) {
    return(rep(TRUE, nrow(data)))
  }

  field <- comparison_match[1, 2]
  operator <- comparison_match[1, 3]
  value <- comparison_match[1, 4]

  if (!field %in% names(data)) return(rep(TRUE, nrow(data)))

  current <- coalesce(as.character(data[[field]]), "")
  target <- if (value == "") "" else choice_label_for_code(field, value)

  if (operator == "=") {
    current == target
  } else {
    current != target
  }
}

data <- tibble(.row = seq_len(n))

for (i in seq_len(nrow(dict))) {
  row <- dict[i, ]
  values <- generate_field(row, n)
  mask <- branch_mask(data, row$branching)
  values[!mask] <- NA
  data[[row$field_name]] <- values
}

sample_followup_status <- function(due_date) {
  if (due_date > current_date + 7) {
    return("Incompleto")
  }

  if (current_date > due_date + 7) {
    return(sample(c("Completo", "Perda de seguimento"), size = 1, prob = c(0.90, 0.10)))
  }

  sample(
    c("Completo", "Incompleto"),
    size = 1,
    prob = c(0.70, 0.30)
  )
}

sample_followup_collection <- function(due_date, status) {
  if (status != "Completo") {
    return(as.Date(NA))
  }

  if (current_date > due_date + 7) {
    timing <- sample(c("on_time", "late"), size = 1, prob = c(0.82, 0.18))
    if (timing == "late") {
      return(due_date + sample(seq.int(8L, min(28L, as.integer(current_date - due_date))), 1))
    }
  }

  due_date + sample(seq.int(-7L, min(7L, as.integer(current_date - due_date))), 1)
}

data <- data |>
  mutate(
    record_id = sprintf("REC_%03d", seq_len(n)),
    record_id_dsei = sprintf("%s-%03d", patient_cluster$cluster_id, patient_cluster$patient_index_in_cluster),
    dsei = patient_cluster$dsei,
    polo_base = patient_cluster$polo_base,
    data_atendimento_polo = patient_cluster$data_atendimento_polo,
    data_acidente = data_atendimento_polo - sample(0:3, n, replace = TRUE),
    data_ava_cli_1 = data_atendimento_polo,
    dt_ava_cli_1 = data_atendimento_polo,
    data_coleta_t2_due = data_atendimento_polo %m+% months(1),
    data_coleta_t3_due = data_atendimento_polo %m+% months(3),
    data_coleta_t4_due = data_atendimento_polo %m+% months(6),
    completo_t2 = map_chr(data_coleta_t2_due, sample_followup_status),
    completo_t3 = map_chr(data_coleta_t3_due, sample_followup_status),
    completo_t4 = map_chr(data_coleta_t4_due, sample_followup_status),
    data_coleta_t2 = as.Date(unlist(map2(data_coleta_t2_due, completo_t2, sample_followup_collection)), origin = "1970-01-01"),
    data_coleta_t3 = as.Date(unlist(map2(data_coleta_t3_due, completo_t3, sample_followup_collection)), origin = "1970-01-01"),
    data_coleta_t4 = as.Date(unlist(map2(data_coleta_t4_due, completo_t4, sample_followup_collection)), origin = "1970-01-01"),
    seguimento_dia = as.integer(current_date - data_atendimento_polo),
    trial_arm = patient_cluster$trial_arm,
    cluster_id = patient_cluster$cluster_id,
    cluster_sequence = patient_cluster$cluster_sequence,
    cluster_activation_date = patient_cluster$cluster_activation_date,
    cluster_target = patient_cluster$cluster_target,
    trial_target_sample = trial_target_sample,
    trial_target_per_arm = trial_target_per_arm,
    clusters_per_arm_with_inflation = clusters_per_arm_with_inflation,
    baseline_target_without_inflation = baseline_target_per_arm * 2,
    baseline_clusters_per_arm = baseline_clusters_per_arm,
    average_patients_per_unit = average_patients_per_unit
  ) |>
  select(-data_coleta_t2_due, -data_coleta_t3_due, -data_coleta_t4_due)

if (all(c("data_acidente", "data_atendimento_polo") %in% names(data))) {
  data <- data |>
    mutate(
      data_acidente = as.Date(data_acidente),
      data_atendimento_polo = pmax(
        as.Date(data_atendimento_polo),
        data_acidente,
        na.rm = TRUE
      )
    )
}

phq_cols <- paste0("phq9_", 1:9, "_t3")
if (all(phq_cols %in% names(data))) {
  phq_scores <- data |>
    select(all_of(phq_cols)) |>
    mutate(across(everything(), \(x) case_when(
      stringr::str_detect(x, "^V[aá]rios dias$") ~ 1L,
      x == "Mais da metade dos dias" ~ 2L,
      x == "Quase todos os dias" ~ 3L,
      TRUE ~ 0L
    )))

  data <- data |>
    mutate(
      soma_phq9_vd_t3 = rowSums(phq_scores == 1L, na.rm = TRUE),
      soma_phq9_md_t3 = rowSums(phq_scores == 2L, na.rm = TRUE),
      soma_phq9_qd_t3 = rowSums(phq_scores == 3L, na.rm = TRUE),
      soma_total_phq9_t3 = rowSums(phq_scores, na.rm = TRUE)
    )
}

set_col <- function(data, field, value) {
  if (field %in% names(data)) {
    data[[field]] <- value
  }

  data
}

set_date_col <- function(data, field, value) {
  if (field %in% names(data)) {
    data[[field]] <- as.Date(value)
  }

  data
}

yes_no <- function(flag) {
  if_else(flag, "Sim", "Não")
}

reaction_flag <- runif(n) < 0.08
severe_reaction_flag <- reaction_flag & runif(n) < 0.20
serum_sickness_flag <- runif(n) < 0.06
necrosis_t1_flag <- runif(n) < 0.04
necrosis_t2_flag <- runif(n) < 0.035
necrosis_t3_flag <- runif(n) < 0.025
debridement_t2_flag <- necrosis_t2_flag & runif(n) < 0.45
debridement_t3_flag <- necrosis_t3_flag & runif(n) < 0.35
amputation_t2_flag <- rep(FALSE, n)
amputation_t3_flag <- rep(FALSE, n)
if (sum(necrosis_t2_flag) > 0) {
  amputation_t2_flag[sample(which(necrosis_t2_flag), min(1, sum(necrosis_t2_flag)))] <- TRUE
}
sequela_flag <- runif(n) < 0.03
death_flag <- rep(FALSE, n)
death_flag[sample(seq_len(n), 1)] <- TRUE
transfer_flag <- severe_reaction_flag | debridement_t2_flag | debridement_t3_flag | death_flag | (runif(n) < 0.03)
hypotension_flag <- severe_reaction_flag | (reaction_flag & runif(n) < 0.12)

reaction_type <- case_when(
  severe_reaction_flag & hypotension_flag ~ "Hipotensão, Dificuldade respiratória",
  severe_reaction_flag ~ "Dificuldade respiratória, Rouquidão",
  reaction_flag ~ "Prurido (coceira), Urticária (manchas avermelhadas pelo corpo)",
  TRUE ~ NA_character_
)

serum_sickness_type <- if_else(
  serum_sickness_flag,
  sample(
    c(
      "Coceira ou manchas vermelhas na pele",
      "Dor nas articulações",
      "Febre, Dor nas articulações",
      "Mal-estar"
    ),
    n,
    replace = TRUE
  ),
  "Nenhum dos anteriores"
)

data <- data |>
  mutate(
    reacao_soro_1 = yes_no(reaction_flag),
    tipo_reacao_soro_1 = reaction_type,
    data_hora_reacao_soro_1 = if_else(
      reaction_flag,
      as.POSIXct(data_atendimento_polo, tz = "America/Sao_Paulo") + hours(sample(1:6, n, replace = TRUE)),
      as.POSIXct(NA)
    ),
    soro_interrom_1 = yes_no(severe_reaction_flag),
    trat_soro_interrom_1 = if_else(severe_reaction_flag, "Sim", NA_character_),
    tipo_trat_soro_interrom_1 = if_else(severe_reaction_flag, choice_label_for_code("tipo_trat_soro_interrom_1", "1"), NA_character_),
    pa_sist_soro_1 = if_else(hypotension_flag, sample(70:89, n, replace = TRUE), sample(95:140, n, replace = TRUE)),
    pa_diast_soro_1 = if_else(hypotension_flag, sample(45:60, n, replace = TRUE), sample(60:90, n, replace = TRUE)),
    tipo_alta_1 = case_when(
      death_flag ~ "Óbito",
      sequela_flag ~ "Alta com sequela",
      TRUE ~ "Alta sem sequela"
    ),
    sequela_alta_1 = if_else(sequela_flag, "Déficit motor", NA_character_),
    obito_alta_1 = if_else(death_flag, "Ocorreu no hospital", NA_character_),
    necrose_loc_picada_1 = yes_no(necrosis_t1_flag),
    doenca_soro_t2 = serum_sickness_type,
    necrose_cut_t2 = yes_no(necrosis_t2_flag),
    limpeza_teci_necro_t2 = yes_no(debridement_t2_flag),
    amputacao_t2 = yes_no(amputation_t2_flag),
    tipo_alta_2 = case_when(
      death_flag ~ "Óbito",
      sequela_flag ~ "Alta com sequela",
      TRUE ~ "Alta sem sequela"
    ),
    sequela_alta_2 = if_else(sequela_flag, "Déficit motor", NA_character_),
    obito_alta_2 = if_else(death_flag, "Ocorreu no hospital", NA_character_),
    necrose_cut_t3 = yes_no(necrosis_t3_flag),
    limpeza_teci_necro_t3 = yes_no(debridement_t3_flag),
    amputacao_t3 = yes_no(amputation_t3_flag),
    sequela_alta_3 = if_else(sequela_flag, "Déficit motor", NA_character_),
    obito_alta_3 = if_else(death_flag, "Ocorreu no hospital", NA_character_),
    sequela_cro_acidente_t4 = yes_no(sequela_flag),
    sim_sequela_cro_t4 = if_else(sequela_flag, "Déficit motor", NA_character_),
    reclas_reacao_soro = "Não",
    reclas_tipo_reacao_soro = NA_character_,
    reclas_soro_interrom = "Não",
    reclas_trat_soro_interrom = NA_character_,
    reclas_tansferencia = "Não",
    reclas_necrose_loc_picada = "Não",
    reclas_pa_sist_soro = pa_sist_soro_1,
    reclas_pa_diast_soro = pa_diast_soro_1,
    reclas_press_art_sis = pa_sist_1
  )

data <- data |>
  set_col("transferencia_1", yes_no(transfer_flag)) |>
  set_col("tansferencia_1", yes_no(transfer_flag)) |>
  set_col("razao_transf_1", if_else(transfer_flag, "Necessidade de avaliação hospitalar", NA_character_)) |>
  set_col("servico_transf_1", if_else(transfer_flag, "Hospital de referência", NA_character_)) |>
  set_col("data_hora_transf_1", if_else(
    transfer_flag,
    as.POSIXct(data$data_atendimento_polo, tz = "America/Sao_Paulo") + hours(sample(2:8, n, replace = TRUE)),
    as.POSIXct(NA)
  )) |>
  set_col("compartimental_1", yes_no(debridement_t2_flag | debridement_t3_flag | amputation_t2_flag | amputation_t3_flag)) |>
  set_col("sim_sindrome_compar_1", if_else(
    debridement_t2_flag | debridement_t3_flag | amputation_t2_flag | amputation_t3_flag,
    "Dor intensa, edema progressivo e necessidade de avaliação cirúrgica",
    NA_character_
  )) |>
  set_col("reclas_compartimental", "Não") |>
  set_col("sim_sindrome_compar_2", NA_character_) |>
  set_col("paci_vivo_t2", yes_no(!death_flag)) |>
  set_col("paci_vivo_t3", yes_no(!death_flag)) |>
  set_col("paci_vivo_t4", yes_no(!death_flag)) |>
  set_col("morte_asso_acidente_t2", if_else(death_flag, "Sim", NA_character_)) |>
  set_col("morte_asso_acidente_t3", if_else(death_flag, "Sim", NA_character_)) |>
  set_col("morte_asso_acidente_t4", if_else(death_flag, "Sim", NA_character_)) |>
  set_col("causa_morte_t2", if_else(death_flag, "Complicações associadas ao acidente ofídico", NA_character_)) |>
  set_date_col("data_obito_t2", if_else(death_flag, data$data_coleta_t2, as.Date(NA))) |>
  set_date_col("data_obito_t3", if_else(death_flag, data$data_coleta_t3, as.Date(NA))) |>
  set_date_col("data_obito_t4", if_else(death_flag, data$data_coleta_t4, as.Date(NA))) |>
  set_col("fonte_info_obito_t2", if_else(death_flag, "Familiar", NA_character_)) |>
  set_col("fonte_info_obito_t3", if_else(death_flag, "Familiar", NA_character_)) |>
  set_col("fonte_info_obito_t4", if_else(death_flag, "Familiar", NA_character_))

# Blank uncollected follow-up forms while preserving their status flags.
for (stage in c("t2", "t3", "t4")) {
  form <- dict$form_name[match(paste0("completo_", stage), dict$field_name)]
  fields <- setdiff(dict$field_name[dict$form_name == form], paste0("completo_", stage))
  uncollected <- data[[paste0("completo_", stage)]] != "Completo"
  for (field in fields) data[[field]][uncollected] <- NA
}

# Re-evaluate after overrides and propagate parent changes until stable.
for (pass in seq_len(nrow(dict))) {
  changed <- FALSE
  for (i in seq_len(nrow(dict))) {
    field <- dict$field_name[[i]]
    mask <- branch_mask(data, dict$branching[[i]])
    remove <- !mask & !is.na(data[[field]])
    if (any(remove)) {
      data[[field]][remove] <- NA
      changed <- TRUE
    }
  }
  if (!changed) break
}

data <- data |>
  select(-.row) |>
  select(all_of(dict$field_name), everything())

# Fail before writing if dictionary choices or numeric bounds are violated.
for (i in seq_len(nrow(dict))) {
  row <- dict[i, ]
  field <- row$field_name
  value <- data[[field]]
  present <- !is.na(value)
  if (row$field_type %in% c("radio", "dropdown")) {
    if (any(present & !value %in% choice_labels(field))) {
      stop("Invalid dictionary category in ", field, call. = FALSE)
    }
  }
  if (row$field_type == "checkbox") {
    selected <- unlist(strsplit(value[present], ", ", fixed = TRUE))
    if (any(!selected %in% choice_labels(field))) {
      stop("Invalid checkbox choice in ", field, call. = FALSE)
    }
  }
  if (row$validation %in% c("integer", "number")) {
    number <- suppressWarnings(as.numeric(value))
    lower <- suppressWarnings(as.numeric(row$validation_min))
    upper <- suppressWarnings(as.numeric(row$validation_max))
    if (any(present & (is.na(number) | !is.finite(number))) ||
        (row$validation == "integer" && any(number[present] != trunc(number[present]))) ||
        (!is.na(lower) && any(number[present] < lower)) ||
        (!is.na(upper) && any(number[present] > upper))) {
      stop("Invalid numeric value in ", field, call. = FALSE)
    }
  }
}

readr::write_csv(data, output_file)
message("Dictionary: ", basename(latest_dictionary))
message("Rows: ", nrow(data), " Columns: ", ncol(data))
