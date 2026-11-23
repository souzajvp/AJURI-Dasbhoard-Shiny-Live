library(testthat)
repo <- normalizePath(".")
preview <- tempfile(fileext = ".csv")
state <- new.env()
withr::with_envvar(c(AJURI_SIMULATED_OUTPUT = preview, AJURI_REFERENCE_DATE = "2026-08-31"), {
  source("scripts/generate_data.R", local = state)
})
data <- readr::read_csv(preview, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
dict <- state$dict

test_that("all dictionary fields and trial metadata are present", {
  expect_equal(nrow(data), 190L)
  expect_identical(head(names(data), nrow(dict)), dict$field_name)
  expect_false(anyDuplicated(data$record_id) > 0)
  expect_equal(dplyr::n_distinct(data$cluster_id), 20L)
  expect_equal(sum(data$trial_arm == "Intervencao"), 96L)
  expect_equal(sum(data$trial_arm == "Controle"), 94L)
})

test_that("categorical and numeric fields respect dictionary definitions", {
  for (i in seq_len(nrow(dict))) {
    field <- dict$field_name[[i]]
    value <- stats::na.omit(data[[field]])
    type <- dict$field_type[[i]]
    labels <- state$choice_labels(field)
    if (type %in% c("radio", "dropdown")) {
      expect_true(all(value %in% labels), info = field)
    }
    if (type == "checkbox") {
      expect_true(all(unlist(strsplit(value, ", ", fixed = TRUE)) %in% labels), info = field)
    }
    validation <- dict$validation[[i]]
    if (!is.na(validation) && validation %in% c("integer", "number")) {
      number <- suppressWarnings(as.numeric(value))
      expect_true(all(is.finite(number)), info = field)
      if (validation == "integer") expect_true(all(number == trunc(number)), info = field)
      low <- suppressWarnings(as.numeric(dict$validation_min[[i]]))
      high <- suppressWarnings(as.numeric(dict$validation_max[[i]]))
      if (!is.na(low)) expect_true(all(number >= low), info = field)
      if (!is.na(high)) expect_true(all(number <= high), info = field)
    }
    if (!is.na(validation) && grepl("^date", validation)) {
      expect_false(anyNA(as.Date(substr(value, 1, 10), format = "%Y-%m-%d")), info = field)
      if (grepl("^datetime", validation)) {
        expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}", value)), info = field)
      }
    }
  }
  # This checkbox has no choices defined in the supplied dictionary.
  expect_true(all(is.na(data$outro_reclas_doenca_conco)))
})

test_that("conditional fields are blank when their dictionary condition is false", {
  # Decode the simple comparison expressions used by this dictionary independently.
  choices <- lapply(state$choice_lookup, function(x) stats::setNames(x$label, x$code))
  for (i in which(!is.na(dict$branching) & nzchar(dict$branching))) {
    branch <- dict$branching[[i]]
    parsed <- stringr::str_match(branch,
      "^\\[([a-zA-Z0-9_]+)(?:\\(([^)]+)\\))?\\]\\s*(=|<>)\\s*['\"]?(.*?)['\"]?$")
    expect_false(is.na(parsed[1, 1]), info = branch)
    parent <- parsed[1, 2]
    code <- parsed[1, 3]
    operator <- parsed[1, 4]
    target <- parsed[1, 5]
    current <- dplyr::coalesce(data[[parent]], "")
    if (!is.na(code)) {
      label <- choices[[parent]][[code]]
      allowed <- vapply(strsplit(current, ", ", fixed = TRUE), function(x) label %in% x, logical(1))
    } else {
      if (nzchar(target) && target %in% names(choices[[parent]])) target <- choices[[parent]][[target]]
      allowed <- if (operator == "=") current == target else current != target
    }
    expect_true(all(is.na(data[[dict$field_name[[i]]]][!allowed])), info = dict$field_name[[i]])
  }
})

test_that("uncollected follow-ups do not contain results or future collection dates", {
  for (stage in c("t2", "t3", "t4")) {
    status <- data[[paste0("completo_", stage)]]
    collection <- as.Date(data[[paste0("data_coleta_", stage)]])
    form <- dict$form_name[match(paste0("completo_", stage), dict$field_name)]
    fields <- setdiff(dict$field_name[dict$form_name == form], paste0("completo_", stage))
    expect_true(all(is.na(collection[status != "Completo"])))
    expect_true(all(collection[!is.na(collection)] <= as.Date("2026-08-31")))
    for (field in fields) expect_true(all(is.na(data[[field]][status != "Completo"])), info = field)
  }
})

test_that("generation is reproducible and the Shinylive dashboard accepts the CSV", {
  second <- tempfile(fileext = ".csv")
  withr::with_envvar(c(AJURI_SIMULATED_OUTPUT = second, AJURI_REFERENCE_DATE = "2026-08-31"), {
    source("scripts/generate_data.R", local = new.env())
  })
  expect_identical(unname(tools::md5sum(preview)), unname(tools::md5sum(second)))
  previous <- getwd()
  setwd(file.path(repo, "shinylive"))
  tryCatch({
    source("app.R", local = TRUE)
    imported <- read_dashboard_csv(preview)
    dashboard <- build_dashboard(imported, as.Date("2026-08-31"))
    expect_equal(dashboard$total_enrolled, 190)
    expect_true(all(imported$trial_target_sample == 190))
  }, finally = setwd(previous))
})
cat("Simulated data validation passed.\n")
