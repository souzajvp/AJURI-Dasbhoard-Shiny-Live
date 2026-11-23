# CSV imports accept either a REDCap export (labels or raw codes; any delimiter;
# longitudinal/repeating rows) or a one-row-per-participant analysis CSV.
# Trim and drop blank lines: the Shinylive bundle can add a trailing empty line.
required_columns <- trimws(readLines("required-columns.txt", warn = FALSE))
required_columns <- c(required_columns[nzchar(required_columns)], "tansferencia_1")
# Retired from the REDCap project (2026-11 dictionary) or not stored in REDCap.
# Filled with NA when absent; trial_arm/cluster_id are handled separately below.
optional_columns <- c("sequela_alta_2", "obito_alta_2")
optional_targets <- c(trial_target_sample = 190, trial_target_per_arm = 96,
                      baseline_target_without_inflation = 168, cluster_target = 10)
redcap_meta_columns <- c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
date_columns <- c("data_atendimento_polo", "data_coleta_t2", "data_coleta_t3", "data_coleta_t4",
                  "data_obito_t2", "data_obito_t3", "data_obito_t4",
                  "data_hora_reacao_soro_1", "reclas_dt_hora_reacao_soro")

latest_dictionary_file <- function(dir = "data") {
  files <- list.files(dir, pattern = "^AJURI_DataDictionary_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$", full.names = TRUE)
  if (!length(files)) stop("Dicionário de dados AJURI não encontrado.", call. = FALSE)
  files[order(basename(files), decreasing = TRUE)][[1]]
}

read_dictionary <- function(path = latest_dictionary_file()) {
  dictionary <- readr::read_csv(path, show_col_types = FALSE,
                                col_types = readr::cols(.default = readr::col_character()))
  names(dictionary)[1:6] <- c("field", "form", "section", "type", "label", "choices")
  dictionary
}

# "1, Sim | 0, Não" -> c(`1` = "Sim", `0` = "Não")
parse_choices <- function(choices) {
  if (is.na(choices) || !nzchar(trimws(choices))) return(character())
  parts <- trimws(strsplit(choices, "|", fixed = TRUE)[[1]])
  parts <- parts[grepl(",", parts, fixed = TRUE)]
  codes <- trimws(sub(",.*$", "", parts))
  stats::setNames(trimws(sub("^[^,]*,", "", parts)), codes)
}

detect_delimiter <- function(header) {
  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(d) lengths(regmatches(header, gregexpr(d, header, fixed = TRUE))), 1L)
  if (max(counts) == 0) "," else candidates[[which.max(counts)]]
}

read_delimited_text <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  text <- rawToChar(bytes[bytes != as.raw(0)])
  encoding <- if (validUTF8(text)) "UTF-8" else "latin1"
  header <- readLines(path, n = 1, warn = FALSE, encoding = encoding)
  readr::read_delim(path, delim = detect_delimiter(if (length(header)) header else ""), show_col_types = FALSE,
                    name_repair = "minimal", locale = readr::locale(encoding = encoding),
                    col_types = readr::cols(.default = readr::col_character()),
                    na = c("", "NA"), trim_ws = TRUE)
}

# Last non-missing value per record, in file order (events, then repeat instances).
collapse_by_record <- function(data) {
  ids <- unique(data$record_id)
  out <- tibble::tibble(record_id = ids)
  for (column in setdiff(names(data), "record_id")) {
    x <- data[[column]]
    keep <- !is.na(x)
    rows <- which(keep)[!duplicated(data$record_id[keep], fromLast = TRUE)]
    value <- rep(NA_character_, length(ids))
    value[match(data$record_id[rows], ids)] <- x[rows]
    out[[column]] <- value
  }
  out
}

# Convert REDCap's export shape into one labelled row per participant.
normalize_redcap_export <- function(data, dictionary) {
  if (all(c("redcap_event_name", "redcap_repeat_instance") %in% names(data))) {
    instance <- suppressWarnings(as.integer(data$redcap_repeat_instance))
    event_order <- match(data$redcap_event_name, unique(data$redcap_event_name))
    data <- data[order(event_order, dplyr::coalesce(instance, 0L), method = "radix"), , drop = FALSE]
  }
  # Checkboxes: field___code columns -> one comma-separated label column.
  checked_values <- c("1", "Marcado", "Checked")
  checkbox_columns <- grep("___", names(data), fixed = TRUE, value = TRUE)
  for (field in unique(sub("___.*$", "", checkbox_columns))) {
    columns <- checkbox_columns[startsWith(checkbox_columns, paste0(field, "___"))]
    choices <- parse_choices(dictionary$choices[match(field, dictionary$field)])
    choice_keys <- tolower(gsub("[^A-Za-z0-9]", "_", names(choices)))
    codes <- sub("^.*___", "", columns)
    labels <- dplyr::coalesce(unname(choices[match(tolower(codes), choice_keys)]), codes)
    values <- as.matrix(data[columns])
    answered <- rowSums(!is.na(values)) > 0
    ticked <- matrix(values %in% checked_values, nrow = nrow(values))
    combined <- apply(ticked, 1, function(row) paste(labels[row], collapse = ", "))
    # "" marks an answered row with nothing ticked, so later instances can clear earlier ones.
    data[[field]] <- ifelse(answered, combined, NA_character_)
    data[columns] <- NULL
  }
  data <- collapse_by_record(data)
  data[intersect(redcap_meta_columns, names(data))] <- NULL
  for (field in intersect(dictionary$field[dictionary$type == "checkbox"], names(data))) {
    data[[field]][data[[field]] %in% ""] <- NA_character_
  }
  # Raw-code exports: map codes to labels where a field holds only valid codes.
  coded <- dictionary$type %in% c("radio", "dropdown", "yesno", "truefalse")
  for (i in which(coded & dictionary$field %in% names(data))) {
    field <- dictionary$field[[i]]
    choices <- switch(dictionary$type[[i]],
      yesno = c(`1` = "Sim", `0` = "Não"),
      truefalse = c(`1` = "Verdadeiro", `0` = "Falso"),
      parse_choices(dictionary$choices[[i]]))
    observed <- unique(stats::na.omit(data[[field]]))
    if (length(choices) && length(observed) && all(observed %in% names(choices)) &&
        !all(observed %in% choices)) {
      data[[field]] <- unname(choices[data[[field]]])
    }
  }
  # Form status columns: raw exports use 0/1/2.
  for (column in grep("_complete$", names(data), value = TRUE)) {
    if (all(stats::na.omit(data[[column]]) %in% c("0", "1", "2"))) {
      data[[column]] <- c(`0` = "Incomplete", `1` = "Unverified", `2` = "Complete")[data[[column]]] |> unname()
    }
  }
  data
}

# REDCap lets users pick YYYY-MM-DD, DD/MM/YYYY or MM/DD/YYYY at export time.
parse_dates <- function(data) {
  slash <- unlist(lapply(intersect(date_columns, names(data)), function(column) {
    grep("^\\d{1,2}/\\d{1,2}/\\d{4}", data[[column]], value = TRUE)
  }))
  month_first <- length(slash) &&
    any(as.integer(sub("^\\d{1,2}/(\\d{1,2})/.*$", "\\1", slash)) > 12)
  slash_format <- if (month_first) "%m/%d/%Y" else "%d/%m/%Y"
  for (column in date_columns) {
    value <- data[[column]]
    day_part <- sub("[ T].*$", "", value)
    is_slash <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", day_part)
    parsed <- as.Date(rep(NA_character_, length(value)))
    parsed[!is_slash] <- suppressWarnings(as.Date(day_part[!is_slash], format = "%Y-%m-%d"))
    parsed[is_slash] <- suppressWarnings(as.Date(day_part[is_slash], format = slash_format))
    iso_ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", day_part) & !is.na(parsed) &
      format(parsed, "%Y-%m-%d") == day_part
    invalid <- !is.na(value) & (is.na(parsed) | (!is_slash & !iso_ok))
    if (any(invalid)) stop(paste0("Datas inválidas em ", column, ". Use AAAA-MM-DD ou DD/MM/AAAA."), call. = FALSE)
    data[[column]] <- parsed
  }
  data
}

read_dashboard_csv <- function(path, dictionary_path = latest_dictionary_file()) {
  data <- read_delimited_text(path)
  if (nrow(readr::problems(data)) > 0) {
    stop("CSV malformado. Confira as aspas e o número de colunas em cada linha.", call. = FALSE)
  }
  if (anyDuplicated(names(data)) || any(!nzchar(names(data)))) {
    stop("O CSV deve ter nomes de colunas únicos e não vazios.", call. = FALSE)
  }
  if (!"record_id" %in% names(data)) {
    stop(paste0("Coluna record_id não encontrada. No REDCap, exporte com nomes de variáveis ",
                "no cabeçalho (\"CSV / Microsoft Excel (raw data)\" ou \"(labels)\" com cabeçalhos ",
                "de variáveis), não com rótulos dos campos."), call. = FALSE)
  }
  if (nrow(data) == 0) stop("O CSV não contém registros.", call. = FALSE)
  if (anyNA(data$record_id)) stop("Cada linha deve ter um record_id preenchido.", call. = FALSE)
  dictionary <- read_dictionary(dictionary_path)
  is_redcap <- any(redcap_meta_columns %in% names(data)) || any(grepl("___", names(data), fixed = TRUE))
  if (is_redcap) {
    data <- normalize_redcap_export(data, dictionary)
  } else if (anyDuplicated(data$record_id)) {
    stop("Cada registro deve ter um record_id único (uma linha por participante).", call. = FALSE)
  }
  if ("transferencia_1" %in% names(data) && !"tansferencia_1" %in% names(data)) {
    data$tansferencia_1 <- data$transferencia_1
  }
  missing <- setdiff(required_columns, names(data))
  if (length(missing)) {
    stop(paste0("Colunas obrigatórias ausentes: ", paste(missing, collapse = ", "),
                ". Exporte o projeto REDCap completo ou use o modelo CSV."), call. = FALSE)
  }
  for (column in setdiff(optional_columns, names(data))) data[[column]] <- NA_character_
  if (anyNA(data$polo_base)) stop("Preencha polo_base em todos os registros.", call. = FALSE)
  # Each polo is its own cluster unless the file supplies cluster_id.
  if (!"cluster_id" %in% names(data)) data$cluster_id <- NA_character_
  data$cluster_id <- dplyr::coalesce(data$cluster_id, data$polo_base)
  # Arm labels are optional and may be blinded codes (e.g. A/B); breakdowns hide when absent.
  if (!"trial_arm" %in% names(data)) data$trial_arm <- NA_character_
  data$trial_arm <- trimws(data$trial_arm)
  data$trial_arm[data$trial_arm %in% ""] <- NA_character_
  for (column in c("completo_t2", "completo_t3", "completo_t4")) {
    if (any(!is.na(data[[column]]) & !data[[column]] %in%
            c("Completo", "Incompleto", "Perda de seguimento"))) {
      stop(paste0(column, ": use Completo, Incompleto ou Perda de seguimento."), call. = FALSE)
    }
  }
  data <- parse_dates(data)
  if (all(is.na(data$data_atendimento_polo))) {
    stop("Informe ao menos uma data_atendimento_polo para exibir o dashboard.", call. = FALSE)
  }
  # Preserve types for exploratory analysis while keeping identifiers and arm as text.
  text_columns <- c(date_columns, "record_id", "cluster_id", "polo_base", "trial_arm")
  other <- setdiff(names(data), text_columns)
  data[other] <- lapply(data[other], function(x) readr::parse_guess(x, na = c("", "NA")))
  for (column in names(optional_targets)) {
    if (!column %in% names(data)) data[[column]] <- optional_targets[[column]]
    value <- suppressWarnings(as.numeric(data[[column]]))
    if (any(!is.na(data[[column]]) & (is.na(value) | !is.finite(value) | value <= 0))) {
      stop(paste0(column, " deve conter números positivos."), call. = FALSE)
    }
    data[[column]] <- dplyr::coalesce(value, optional_targets[[column]])
  }
  labels <- stats::setNames(dictionary$label, dictionary$field)
  labels <- labels[intersect(names(labels), names(data))]
  labelled::var_label(data)[names(labels)] <- as.list(labels)
  data
}
