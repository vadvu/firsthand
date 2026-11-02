# Terminal menu and a small line editor; works in Rscript and with piped stdin.
firsthand_prompt <- function(text, input) {
  cat(text); flush.console()
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) NULL else trimws(line)
}

firsthand_watchlist <- function(path = file.path(rw_root, "watchlist.txt")) {
  if (!file.exists(path)) stop("Нет списка: выполните firsthand setup")
  if (tolower(tools::file_ext(path)) == "csv") {
    watches <- rw_load_watchlist(path)
    print(rw_table(watches, c("name", "birth_date", "aliases")), row.names = FALSE)
    return(invisible(NULL))
  }
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  names <- lines[nzchar(trimws(lines)) & !grepl("^\\s*#", lines)]
  cat("\nСписок людей:\n")
  if (!length(names)) cat("  Список пуст\n") else
    for (i in seq_along(names)) cat(sprintf("  %d. %s\n", i, names[i]))
  cat("Файл: ", rw_absolute(path), "\n", sep = "")
}

firsthand_edit <- function(path = file.path(rw_root, "watchlist.txt"), input) {
  if (tolower(tools::file_ext(path)) == "csv") stop("Редактор терминала предназначен для TXT; CSV откройте в табличном редакторе")
  lines <- if (file.exists(path)) readLines(path, encoding = "UTF-8", warn = FALSE) else "# Фамилия или ФИО на строке"
  repeat {
    indices <- which(nzchar(trimws(lines)) & !grepl("^\\s*#", lines))
    cat("\nРедактор списка\n")
    if (!length(indices)) cat("  Список пуст\n") else
      for (i in seq_along(indices)) cat(sprintf("  %d. %s\n", i, lines[indices[i]]))
    cat("+ ФИО — добавить | - номер — удалить | s — сохранить | q — отменить\n")
    answer <- firsthand_prompt("> ", input)
    if (is.null(answer) || answer %in% c("q", "0")) return(invisible(FALSE))
    if (grepl("^\\+\\s*.+", answer)) {
      name <- trimws(sub("^\\+", "", answer))
      if (rw_norm(name) %in% rw_norm(lines[indices])) cat("Этот запрос уже есть в списке.\n") else lines <- c(lines, name)
    } else if (grepl("^-\\s*[0-9]+$", answer)) {
      number <- suppressWarnings(as.integer(sub("^-\\s*", "", answer)))
      if (!is.na(number) && number >= 1L && number <= length(indices)) lines <- lines[-indices[number]] else cat("Нет такого номера.\n")
    } else if (answer %in% c("s", "save")) {
      path <- rw_absolute(path)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile(".watchlist-", tmpdir = dirname(path), fileext = ".txt")
      writeLines(enc2utf8(lines), temporary, useBytes = TRUE)
      error <- if (!length(indices)) NULL else tryCatch({ rw_load_watchlist(temporary); NULL }, error = identity)
      if (!is.null(error)) { unlink(temporary); cat("Ошибка: ", conditionMessage(error), "\n", sep = ""); next }
      fs::file_move(temporary, path)
      cat("Список сохранён: ", path, "\n", sep = "")
      return(invisible(TRUE))
    } else if (nzchar(answer)) cat("Используйте + ФИО, - номер, s или q.\n")
  }
}

firsthand_tables <- function(output = file.path(rw_root, "output")) {
  paths <- rw_data_paths(output)
  cat("\nОбработанные реестры:\n")
  for (file in rw_registry_files) {
    path <- file.path(paths$output, file)
    cat("  ", path, if (!file.exists(path)) " (ещё не скачан)" else "", "\n", sep = "")
  }
  invisible(NULL)
}

firsthand_terminal <- function(command, args = character(), input = NULL) {
  rw_dependencies()
  own_input <- is.null(input)
  if (command %in% c("menu", "edit") && own_input) {
    input <- file("stdin", open = "r")
    on.exit(close(input), add = TRUE)
  }
  if (command == "refresh") {
    options <- rw_arguments(args, list(output = file.path(rw_root, "output"), minjust_file = NULL, rosfin_file = NULL, timeout = 45))
    options$timeout <- as.numeric(options$timeout)
    if (!is.finite(options$timeout) || options$timeout <= 0) stop("timeout должен быть положительным")
    return(do.call(rw_refresh, options)$exit_code)
  }
  if (command == "tables") {
    options <- rw_arguments(args, list(output = file.path(rw_root, "output")))
    firsthand_tables(options$output); return(0L)
  }
  options <- rw_arguments(args, list(watchlist = file.path(rw_root, "watchlist.txt")))
  if (command == "watchlist") { firsthand_watchlist(options$watchlist); return(0L) }
  if (command == "edit") { firsthand_edit(options$watchlist, input); return(0L) }
  repeat {
    cat("\nFirsthand\n",
        "  1  Показать список людей\n",
        "  2  Изменить список\n",
        "  3  Обновить реестры\n",
        "  4  Проверить список с обновлением реестров\n",
        "  5  Проверить по сохранённым таблицам\n",
        "  6  Где находятся таблицы\n",
        "  0  Выход\n", sep = "")
    answer <- firsthand_prompt("Выбор: ", input)
    if (is.null(answer) || answer %in% c("0", "q")) return(0L)
    tryCatch(switch(answer,
      "1" = firsthand_watchlist(options$watchlist),
      "2" = firsthand_edit(options$watchlist, input),
      "3" = rw_refresh(),
      "4" = rw_check(watchlist = options$watchlist),
      "5" = rw_check(watchlist = options$watchlist, cached = TRUE),
      "6" = firsthand_tables(),
      cat("Выберите номер из меню.\n")), error = function(error) cat("Ошибка: ", conditionMessage(error), "\n", sep = ""))
  }
}
