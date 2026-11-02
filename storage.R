# Compact storage: two CSV registries; comparison state lives outside output.
rw_registry_files <- c(minjust = "foreign_agents.csv", rosfinmonitoring = "terrorists_extremists.csv")
rw_registry_fields <- c("name", "birth_date", "status", "included_on", "excluded_on", "aliases",
                        "section", "source_number", "source_marker", "updated_at", "source_date", "source_mode")

rw_data_paths <- function(output = file.path(rw_root, "output")) {
  output <- rw_absolute(output)
  state <- if (basename(output) == "output") file.path(dirname(output), ".state") else
    file.path(dirname(output), paste0(".", basename(output), "-state"))
  list(output = output, state = state, file = file.path(state, "state.json"))
}

rw_csv_atomic <- function(path, rows, fields) {
  temporary <- tempfile(".registry-", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  rw_csv(temporary, rows, fields)
  fs::file_move(temporary, path)
}

rw_store_registry <- function(records, metadata, path) {
  rows <- lapply(records, function(record) c(record, list(updated_at = metadata$updated_at,
    source_date = rw_text(metadata$document_as_of), source_mode = rw_text(metadata$mode))))
  rw_csv_atomic(path, rows, rw_registry_fields)
}

rw_read_registry <- function(path, source, metadata = NULL) {
  if (!file.exists(path)) stop("Нет таблицы ", basename(path), "; выполните refresh")
  if (!is.null(metadata$csv_sha256) && rw_file_hash(path) != metadata$csv_sha256)
    stop("Таблица ", basename(path), " изменена вручную; выполните refresh для проверки по источнику")
  data <- read.csv(path, fileEncoding = "UTF-8-BOM", colClasses = "character", check.names = FALSE,
                   stringsAsFactors = FALSE, na.strings = character())
  if (!all(rw_registry_fields %in% names(data))) stop("Изменилась структура таблицы ", basename(path))
  if (!nrow(data) || any(!data$status %in% c("active", "excluded"))) stop("Повреждена таблица ", basename(path))
  lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]; aliases <- row$aliases
    aliases <- if (nzchar(aliases)) as.list(strsplit(aliases, " | ", fixed = TRUE)[[1]]) else list()
    key <- if (source == "minjust") row$source_number else
      substr(rw_hash(rw_json_array(c(rw_norm(row$name), row$birth_date, row$section))), 1, 24)
    rw_record(source, key, row$name, aliases, rw_date(row$birth_date), rw_date(row$included_on),
              rw_date(row$excluded_on), row$status, row$section, row$source_number, row$source_marker)
  })
}

rw_print_result <- function(result) {
  run <- result$manifest
  cat("\n", if (isTRUE(run$cached)) "Проверка по сохранённым таблицам" else "Проверка реестров", "\n", sep = "")
  for (source in names(rw_pages)) {
    item <- run$sources[[source]]
    if (item$status == "error") {
      cat("  ", rw_labels[[source]], ": ОШИБКА — ", item$error, "\n", sep = "")
    } else {
      cat("  ", rw_labels[[source]], ": ", item$persons, " записей; таблица обновлена ", item$updated_at, "\n", sep = "")
      for (warning in item$warnings) cat("    ", warning, "\n", sep = "")
    }
  }
  if (!nrow(result$checks)) {
    cat("\nТаблицы: ", result$output, "\n", sep = "")
    return(invisible(result))
  }
  labels <- c(active_name_match = "Есть действующие кандидаты", excluded_name_match = "Есть исключённая запись",
    birth_date_conflict_only = "Имя совпало, дата рождения другая", not_found_in_snapshot = "Совпадений нет",
    source_error = "Не удалось проверить")
  cat("\nРезультаты:\n")
  for (i in seq_len(nrow(result$checks))) {
    row <- result$checks[i, ]
    cat("  ", row$query, " / ", rw_labels[[row$registry]], ": ", labels[[row$check_status]], "\n", sep = "")
  }
  if (nrow(result$results)) {
    cat("\nПодходящие записи:\n")
    rows <- result$results
    levels <- c(surname_candidate = "фамилия: кандидат", name_candidate = "часть имени: кандидат",
      full_name_match = "полное имя", full_name_birth_date_match = "полное имя и дата рождения")
    accuracy <- unname(levels[rows$match_level])
    accuracy[rows$birth_date_check == "conflict"] <- "ДРУГАЯ дата рождения"
    accuracy[rows$birth_date_check == "unavailable"] <- paste0(accuracy[rows$birth_date_check == "unavailable"], "; дата неизвестна")
    display <- data.frame(rows$query, ifelse(rows$registry == "minjust", "Иноагенты", "Перечень РФМ"), rows$name,
      rows$birth_date, ifelse(rows$status == "active", "в реестре", "исключён(а)"), accuracy, check.names = FALSE)
    names(display) <- c("Запрос", "Реестр", "ФИО", "Дата рождения", "Статус", "Совпадение")
    print(display, row.names = FALSE, max = 200)
    cat("Совпадение имени — кандидат для сверки, а не подтверждение личности.\n")
  }
  if (nrow(result$changes)) {
    cat("\nИзменения с прошлой проверкой:\n")
    events <- c(baseline_match = "первое наблюдение", new_match = "новое совпадение",
      status_changed = "изменился статус", record_updated = "изменились сведения",
      not_seen_in_current_snapshot = "не найден в новой выгрузке")
    display <- result$changes[, c("query", "registry", "name")]
    display$event <- unname(events[result$changes$event])
    names(display) <- c("Запрос", "Реестр", "ФИО", "Изменение")
    print(display, row.names = FALSE, max = 120)
    cat("Первое наблюдение не означает новое включение в реестр.\n")
  }
  invisible(result)
}

rw_run <- function(watchlist = NULL, output = file.path(rw_root, "output"), cached = FALSE,
                   minjust_file = NULL, rosfin_file = NULL, timeout = 45) {
  rw_dependencies()
  watches <- if (is.null(watchlist)) list() else rw_load_watchlist(rw_absolute(watchlist, must_work = TRUE))
  paths <- rw_data_paths(output)
  dir.create(paths$output, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)
  lock <- filelock::lock(file.path(paths$state, "registry.lock"), timeout = 0)
  if (is.null(lock)) stop("Другая проверка ещё выполняется")
  on.exit(filelock::unlock(lock), add = TRUE)
  state <- if (file.exists(paths$file)) rw_json_read(paths$file) else
    list(version = 2L, snapshots = list(), baselines = list())
  if (!identical(state$version, 2L)) stop("Неподдерживаемая версия служебной базы")
  run <- list(started_at = rw_utc(), cached = cached, sources = list())
  matches <- changes <- list()
  raw <- tempfile("firsthand-download-"); dir.create(raw)
  on.exit(unlink(raw, recursive = TRUE), add = TRUE)
  downloader <- if (cached) NULL else rw_downloader(raw, timeout)
  if (!is.null(downloader)) on.exit(downloader$close(), add = TRUE)
  for (source in names(rw_pages)) {
    previous <- state$snapshots[[source]]
    tryCatch({
      path <- file.path(paths$output, rw_registry_files[[source]])
      if (cached) {
        if (is.null(previous)) stop("Нет сведений об источнике таблицы; выполните refresh")
        records <- rw_read_registry(path, source, previous)
        metadata <- previous
      } else {
        cat("Обновляю: ", rw_labels[[source]], "...\n", sep = ""); flush.console()
        local_file <- if (source == "minjust") minjust_file else rosfin_file
        if (!is.null(local_file)) {
          file <- rw_absolute(local_file, must_work = TRUE)
          parsed <- if (source == "minjust") rw_parse_minjust(file) else rw_parse_rosfin(file)
          parsed$metadata$mode <- "local_import"
        } else {
          parsed <- if (source == "minjust") rw_download_minjust(downloader) else
            rw_parse_rosfin(downloader$get(rw_pages[[source]], "rosfin.html"))
          parsed$metadata$mode <- "live_download"
        }
        records <- parsed$records; metadata <- parsed$metadata
        if (!is.null(previous) && length(records) < previous$persons * .85)
          stop("Число записей уменьшилось более чем на 15%; прежняя таблица сохранена")
        if (nzchar(rw_text(metadata$document_as_of)) && nzchar(rw_text(previous$document_as_of)) &&
            metadata$document_as_of < previous$document_as_of)
          stop("Дата документа раньше предыдущего снимка; прежняя таблица сохранена")
        metadata$updated_at <- run$started_at
        metadata$persons <- length(records)
        rw_store_registry(records, metadata, path)
        metadata$csv_sha256 <- rw_file_hash(path)
        state$snapshots[[source]] <- metadata
      }
      warnings <- list()
      if (metadata$mode == "local_import") warnings <- list("Локальная выгрузка: свежая загрузка с сайта не подтверждена.")
      if (cached) warnings <- c(warnings, list("Сайты не запрашивались: используются сохранённые таблицы."))
      if (nzchar(rw_text(metadata$document_as_of))) {
        age <- as.integer(as.Date(Sys.time(), tz = "UTC") - as.Date(substr(metadata$document_as_of, 1, 10)))
        if (age > 14L) warnings <- c(warnings, list(paste("Документ старше 14 дней:", age)))
      }
      run$sources[[source]] <- c(list(status = "ok"), metadata, list(warnings = warnings))
      if (length(watches)) {
        found <- rw_match(watches, records)
        changed <- rw_changes(state$baselines[[source]], watches, found)
        state$baselines[[source]] <- list(checked_at = run$started_at,
          watch_keys = as.list(vapply(watches, `[[`, "", "key")), matches = found)
        matches <- c(matches, found); changes <- c(changes, changed)
      }
    }, error = function(error) {
      run$sources[[source]] <<- list(status = "error", error = conditionMessage(error),
        previous_success_at = rw_text(previous$updated_at))
    })
  }
  checks <- if (length(watches)) rw_summary(run, watches, matches) else list()
  rw_json_write(paths$file, state)
  result <- list(manifest = run, checks = rw_table(checks, rw_fields$checks),
    results = rw_table(matches, rw_fields$results), changes = rw_table(changes, rw_fields$changes), output = paths$output,
    exit_code = if (any(vapply(run$sources, function(s) s$status == "error", logical(1)))) 2L else 0L)
  rw_print_result(result)
  invisible(result)
}

rw_check <- function(watchlist = file.path(rw_root, "watchlist.txt"), output = file.path(rw_root, "output"),
                     minjust_file = NULL, rosfin_file = NULL, timeout = 45, cached = FALSE) {
  rw_run(watchlist, output, cached, minjust_file, rosfin_file, timeout)
}

rw_refresh <- function(output = file.path(rw_root, "output"), minjust_file = NULL, rosfin_file = NULL, timeout = 45) {
  rw_run(NULL, output, FALSE, minjust_file, rosfin_file, timeout)
}
