library(testthat)
repo <- normalizePath(".")
setwd(file.path(repo, "shinylive"))
app <- source("app.R")$value
# Synthetic test data; no participant dataset is committed.
raw <- as.data.frame(setNames(lapply(required_columns, function(x) rep(NA_character_, 4)), required_columns))
raw$record_id <- c("001", "002", "003", "004")
raw$polo_base <- c("Polo A", "Polo A", "Polo B", "Polo B")
raw$cluster_id <- c("01", "01", "02", "02")
raw$trial_arm <- c("Intervencao", "Intervencao", "Controle", "Controle")
raw$data_atendimento_polo <- c("2026-06-01", "2026-06-08", "2026-07-01", "2026-07-08")
raw$idade_paciente <- c("20", "30", "40", "50")
raw$sexo_paciente <- c("Masculino", "Feminino", "Masculino", "Feminino")
raw$transferencia_1 <- rep("Não", 4)
raw$tansferencia_1 <- NULL
raw$completo_t2 <- rep("Incompleto", 4)
raw$completo_t3 <- rep("Incompleto", 4)
raw$completo_t4 <- rep("Incompleto", 4)
fixture <- tempfile(fileext = ".csv")
readr::write_csv(raw, fixture, na = "")
temp_csv <- function(data) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(data, path, na = "")
  path
}

test_that("CSV importer validates the data contract", {
  data <- read_dashboard_csv(fixture)
  expect_equal(nrow(data), nrow(raw))
  expect_s3_class(data$data_atendimento_polo, "Date")
  expect_type(data$record_id, "character")
  expect_true("tansferencia_1" %in% names(data))
  expect_error(read_dashboard_csv(temp_csv(raw[0, ])), "não contém registros")
  expect_error(read_dashboard_csv(temp_csv(raw[, setdiff(names(raw), "polo_base")])), "polo_base")
  duplicate <- raw
  duplicate$record_id[2] <- duplicate$record_id[1]
  expect_error(read_dashboard_csv(temp_csv(duplicate)), "único")
  bad <- raw
  bad$data_atendimento_polo[1] <- "2026-02-30"
  expect_error(read_dashboard_csv(temp_csv(bad)), "Datas inválidas")
  bad$data_atendimento_polo[1] <- "31/08/2026"
  expect_equal(read_dashboard_csv(temp_csv(bad))$data_atendimento_polo[1], as.Date("2026-08-31"))
  blinded <- raw
  blinded$trial_arm <- c("A", "A", "B", "B")
  expect_equal(read_dashboard_csv(temp_csv(blinded))$trial_arm, blinded$trial_arm)
  no_arm <- raw[, setdiff(names(raw), c("trial_arm", "cluster_id", "sequela_alta_2", "obito_alta_2"))]
  imported <- read_dashboard_csv(temp_csv(no_arm))
  expect_true(all(is.na(imported$trial_arm)))
  expect_equal(imported$cluster_id, raw$polo_base)
  expect_true(all(c("sequela_alta_2", "obito_alta_2") %in% names(imported)))
  bad <- raw
  bad$completo_t2[1] <- "2"
  expect_error(read_dashboard_csv(temp_csv(bad)), "Completo, Incompleto")
  targets <- raw[, setdiff(names(raw), names(optional_targets))]
  expect_equal(unique(read_dashboard_csv(temp_csv(targets))$trial_target_sample), 190)
})

# Build a synthetic REDCap export (no real data): header from the dictionary,
# checkbox fields split into field___code, longitudinal rows plus a repeating form.
dictionary <- read_dictionary()
redcap_header <- function(dictionary) {
  fields <- dictionary[dictionary$type != "descriptive", ]
  cols <- unlist(lapply(seq_len(nrow(fields)), function(i) {
    if (fields$type[[i]] == "checkbox") paste0(fields$field[[i]], "___", names(parse_choices(fields$choices[[i]])))
    else fields$field[[i]]
  }))
  forms <- unique(fields$form)
  c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",
    setdiff(cols, "record_id"), paste0(forms, "_complete"))
}
make_redcap_export <- function(codes = FALSE) {
  header <- redcap_header(dictionary)
  rows <- as.data.frame(matrix(NA_character_, nrow = 5, ncol = length(header),
                               dimnames = list(NULL, header)), check.names = FALSE)
  yes <- if (codes) "1" else "Sim"
  no <- if (codes) "0" else "Não"
  tick <- if (codes) "1" else "Marcado"
  untick <- if (codes) "0" else "Desmarcado"
  rows$record_id <- c("1", "1", "1", "2", "2")
  rows$redcap_event_name <- c("T1: Primeiro contato", "T1: Primeiro contato", "T2: Após 1 mês",
                              "T1: Primeiro contato", "T2: Após 1 mês")
  rows$redcap_repeat_instrument <- c(NA, "Avaliação Clínica Atualizada", NA, NA, NA)
  rows$redcap_repeat_instance <- c(NA, "1", NA, NA, NA)
  t1 <- c(1, 4)
  rows$polo_base[t1] <- if (codes) c("21", "23") else c("Alto Solimões-Betânia", "Alto Solimões-Feijoal")
  rows$sexo_paciente[t1] <- if (codes) c("1", "2") else c("Masculino", "Feminino")
  rows$idade_paciente[t1] <- c("34", "51")
  rows$data_atendimento_polo[t1] <- c("2026-09-02", "2026-09-10")
  rows$reacao_soro_1[t1] <- c(yes, no)
  rows$necrose_loc_picada_1[t1] <- no
  rows$transferencia_1[t1] <- c(no, yes)
  rows$tipo_alta_1[t1] <- if (codes) "1" else "Alta sem sequela"
  reaction <- grep("^tipo_reacao_soro_1___", header, value = TRUE)
  rows[t1, reaction] <- untick
  rows[1, reaction[[1]]] <- tick
  reclas <- grep("^reclas_tipo_reacao_soro___", header, value = TRUE)
  rows[2, reclas] <- untick
  rows$reclas_reacao_soro[2] <- yes
  serum <- grep("^doenca_soro_t2___", header, value = TRUE)
  rows[c(3, 5), serum] <- untick
  rows[3, serum[[2]]] <- tick
  rows$data_coleta_t2[c(3, 5)] <- c("2026-10-02", "2026-10-11")
  rows$completo_t2[c(3, 5)] <- if (codes) c("1", "0") else c("Completo", "Incompleto")
  rows
}
write_redcap <- function(data, delim = ";") {
  path <- tempfile(fileext = ".csv")
  readr::write_delim(data, path, delim = delim, na = "")
  path
}

test_that("REDCap exports (labels or raw codes, any delimiter) import directly", {
  serum_label <- parse_choices(dictionary$choices[dictionary$field == "doenca_soro_t2"])[[2]]
  reaction_label <- parse_choices(dictionary$choices[dictionary$field == "tipo_reacao_soro_1"])[[1]]
  for (codes in c(FALSE, TRUE)) for (delim in c(";", ",")) {
    data <- labelled::remove_var_label(read_dashboard_csv(write_redcap(make_redcap_export(codes), delim)))
    expect_equal(nrow(data), 2)
    expect_equal(data$record_id, c("1", "2"))
    expect_equal(data$polo_base, c("Alto Solimões-Betânia", "Alto Solimões-Feijoal"))
    expect_equal(data$cluster_id, data$polo_base)
    expect_true(all(is.na(data$trial_arm)))
    expect_equal(as.character(data$reacao_soro_1), c("Sim", "Não"))
    expect_equal(as.character(data$reclas_reacao_soro), c("Sim", NA))
    expect_equal(as.character(data$tansferencia_1), c("Não", "Sim"))
    expect_equal(as.character(data$tipo_reacao_soro_1), c(reaction_label, NA))
    expect_equal(as.character(data$doenca_soro_t2), c(serum_label, NA))
    expect_equal(as.character(data$completo_t2), c("Completo", "Incompleto"))
    expect_equal(data$data_coleta_t2, as.Date(c("2026-10-02", "2026-10-11")))
    expect_false(any(grepl("___|^redcap_", names(data))))
    dashboard <- build_dashboard(data, as.Date("2026-10-15"))
    expect_false(grepl("Pacientes por braço", as.character(dashboard$ui)))
  }
  with_arm <- make_redcap_export()
  with_arm$trial_arm <- c("A", NA, NA, "B", NA)
  data <- read_dashboard_csv(write_redcap(with_arm))
  expect_equal(data$trial_arm, c("A", "B"))
  expect_true(grepl("Pacientes por braço", as.character(build_dashboard(data, as.Date("2026-10-15"))$ui)))
  labelled_header <- make_redcap_export()
  names(labelled_header)[1] <- "Record ID"
  expect_error(read_dashboard_csv(write_redcap(labelled_header)), "nomes de variáveis")
})

test_that("follow-up status uses the reference date", {
  data <- read_dashboard_csv(fixture)
  # Baselines: 2026-06-01/06-08/07-01/07-08 -> 1-month windows close 07-08 .. 08-15.
  data$completo_t2 <- c("Completo", "Incompleto", NA, "Perda de seguimento")
  data$data_coleta_t2 <- as.Date(c("2026-07-01", NA, NA, NA))
  t1 <- function(date) {
    out <- build_dashboard(data, as.Date(date))$followup
    as.character(out$status[out$followup == "1 mês"])
  }
  # 07-05: record 2 in window; record 3 (form not started) still before its window.
  expect_equal(t1("2026-07-05"), c("Concluído no prazo", "Janela aberta", "Aguardando janela", "Perdido"))
  # 08-05: record 2's window closed -> lost; record 3 (blank) now in window.
  expect_equal(t1("2026-08-05"), c("Concluído no prazo", "Perdido", "Janela aberta", "Perdido"))
  # 09-30: every window closed; only the completed visit is not lost.
  expect_equal(t1("2026-09-30"), c("Concluído no prazo", "Perdido", "Perdido", "Perdido"))
})

test_that("dashboard waits for a valid CSV and recovers from invalid imports", {
  shiny::testServer(server, {
    session$flushReact()
    expect_null(loaded())
    session$setInputs(open_dashboard = 1)
    expect_match(import_error(), "Selecione um arquivo")
    session$setInputs(csv = data.frame(name = "bad.csv", size = 10, datapath = temp_csv(raw[0, ])))
    session$setInputs(open_dashboard = 2, reference_date = "2026-08-31")
    expect_null(loaded())
    expect_match(import_error(), "não contém registros")
    session$setInputs(csv = data.frame(name = "data.csv", size = file.info(fixture)$size, datapath = fixture))
    session$setInputs(open_dashboard = 3)
    expect_null(import_error())
    expect_equal(loaded()$rows, nrow(raw))
    expect_match(as.character(loaded()$view), "Seguimento")
    session$setInputs(x = c("idade_paciente", "sexo_paciente"), y = "None", polo = raw$polo_base[1])
    expect_true(nchar(output$summary_table$html) > 0)
  })
  shiny::testServer(server, {
    session$flushReact()
    expect_null(loaded())
  })
})
cat("Shinylive tests passed.\n")
