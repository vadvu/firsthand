#!/usr/bin/env Rscript
# Prepare/install a user-local launchd or Windows Task Scheduler job.
local({
  files <- as.character(unlist(lapply(sys.frames(), function(f) f$ofile)))
  files <- files[basename(files) == "schedule.R"]
  if (!length(files)) files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  files <- gsub("~+~", " ", files, fixed = TRUE)
  directory <- if (length(files)) dirname(normalizePath(tail(files, 1L), winslash = "/", mustWork = TRUE)) else normalizePath("firsthand", winslash = "/")
  source(file.path(directory, "monitor.R"), local = parent.env(environment()))
})

rw_find_rscript <- function() {
  candidates <- c(unname(Sys.which("Rscript")), unname(Sys.which("Rscript.exe")),
                  file.path(R.home("bin"), "Rscript.exe"), file.path(R.home("bin"), "Rscript"))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) stop("Не найден Rscript")
  rw_absolute(candidates[1], must_work = TRUE)
}

rw_schedule_plist <- function(watchlist, output, hours = 168, rscript = rw_find_rscript()) {
  if (!nzchar(rscript)) stop("Не найден Rscript")
  hours <- as.numeric(hours)
  if (length(hours) != 1L || !is.finite(hours) || hours != as.integer(hours) || hours < 1 || hours > 8784)
    stop("every-hours: целое число от 1 до 8784")
  list(Label = paste0("org.vadvu.firsthand.", substr(rw_hash(output), 1L, 10L)),
    ProgramArguments = as.list(c(rscript, file.path(rw_root, "firsthand.R"), "check", "--watchlist", watchlist, "--output", output)),
    WorkingDirectory = rw_root, StartInterval = as.integer(hours * 3600), RunAtLoad = TRUE,
    StandardOutPath = file.path(rw_data_paths(output)$state, "scheduler.stdout.log"), StandardErrorPath = file.path(rw_data_paths(output)$state, "scheduler.stderr.log"),
    ProcessType = "Background")
}

rw_write_plist <- function(plist, path) {
  root <- xml2::xml_new_root("plist", version = "1.0")
  dictionary <- xml2::xml_add_child(root, "dict")
  for (key in names(plist)) {
    xml2::xml_add_child(dictionary, "key", key)
    value <- plist[[key]]
    if (is.list(value)) {
      array <- xml2::xml_add_child(dictionary, "array")
      for (item in value) xml2::xml_add_child(array, "string", item)
    } else if (is.logical(value)) xml2::xml_add_child(dictionary, if (value) "true" else "false")
    else xml2::xml_add_child(dictionary, if (is.numeric(value)) "integer" else "string", as.character(value))
  }
  body <- sub("^<\\?xml[^>]+\\?>\\s*", "", as.character(root), perl = TRUE)
  lines <- c('<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">',
    body)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

rw_launchctl <- function(arguments, absent_ok = FALSE) {
  result <- suppressWarnings(system2("/bin/launchctl", shQuote(arguments), stdout = TRUE, stderr = TRUE))
  status <- rw_or(attr(result, "status"), 0L)
  if (status != 0L && !(absent_ok && any(grepl("No such process|Could not find service", result))))
    stop(paste(result, collapse = "\n"), call. = FALSE)
  result
}

rw_windows_quote <- function(value) {
  if (any(grepl('["\r\n]', value))) stop("Недопустимая кавычка или перенос строки в аргументе Windows")
  paste0('"', gsub("(\\\\+)$", "\\1\\1", value, perl = TRUE), '"')
}

rw_windows_task <- function(watchlist, output, hours = 168, user, rscript = rw_find_rscript(),
                            start = format(Sys.time() + 60, "%Y-%m-%dT%H:%M:%S")) {
  hours <- as.numeric(hours)
  if (!is.finite(hours) || hours != as.integer(hours) || hours < 1 || hours > 744)
    stop("Для Windows every-hours должен быть целым числом от 1 до 744 (31 день)")
  if (!nzchar(user)) stop("Не определён пользователь Windows")
  native <- function(path) gsub("/", "\\", path, fixed = TRUE)
  script <- file.path(rw_root, "firsthand.R")
  id <- paste0("Firsthand-", substr(rw_hash(output), 1L, 10L))
  owner <- paste0("Firsthand R | ", script)
  root <- xml2::xml_new_root("Task", version = "1.2", xmlns = "http://schemas.microsoft.com/windows/2004/02/mit/task")
  info <- xml2::xml_add_child(root, "RegistrationInfo")
  xml2::xml_add_child(info, "Description", owner)
  xml2::xml_add_child(info, "URI", paste0("\\", id))
  triggers <- xml2::xml_add_child(root, "Triggers")
  trigger <- xml2::xml_add_child(triggers, "TimeTrigger")
  repetition <- xml2::xml_add_child(trigger, "Repetition")
  xml2::xml_add_child(repetition, "Interval", paste0("PT", hours, "H"))
  xml2::xml_add_child(repetition, "StopAtDurationEnd", "false")
  xml2::xml_add_child(trigger, "StartBoundary", start)
  xml2::xml_add_child(trigger, "Enabled", "true")
  principals <- xml2::xml_add_child(root, "Principals")
  principal <- xml2::xml_add_child(principals, "Principal", id = "Author")
  xml2::xml_add_child(principal, "UserId", user)
  xml2::xml_add_child(principal, "LogonType", "InteractiveToken")
  xml2::xml_add_child(principal, "RunLevel", "LeastPrivilege")
  settings <- xml2::xml_add_child(root, "Settings")
  for (key in names(values <- c(MultipleInstancesPolicy = "IgnoreNew", DisallowStartIfOnBatteries = "false",
    StopIfGoingOnBatteries = "false", StartWhenAvailable = "true", Enabled = "true", ExecutionTimeLimit = "PT1H")))
    xml2::xml_add_child(settings, key, values[[key]])
  actions <- xml2::xml_add_child(root, "Actions", Context = "Author")
  exec <- xml2::xml_add_child(actions, "Exec")
  xml2::xml_add_child(exec, "Command", native(rscript))
  args <- c(native(script), "check", "--watchlist", native(watchlist), "--output", native(output),
            "--log-file", native(file.path(rw_data_paths(output)$state, "scheduler.log")))
  xml2::xml_add_child(exec, "Arguments", paste(rw_windows_quote(args), collapse = " "))
  xml2::xml_add_child(exec, "WorkingDirectory", native(rw_root))
  list(id = id, owner = owner, xml = root)
}

rw_schtasks <- function(arguments, stdout_file = NULL) {
  executable <- unname(Sys.which("schtasks.exe"))
  if (!nzchar(executable)) stop("Не найден schtasks.exe")
  error_file <- tempfile(); on.exit(unlink(error_file), add = TRUE)
  output_file <- if (is.null(stdout_file)) tempfile() else stdout_file
  if (is.null(stdout_file)) on.exit(unlink(output_file), add = TRUE)
  status <- suppressWarnings(system2(executable, shQuote(arguments, type = "cmd"),
                                    stdout = output_file, stderr = error_file))
  list(status = status, error = paste(readLines(error_file, warn = FALSE), collapse = "\n"))
}

rw_windows_schedule <- function(options, watchlist, output) {
  id <- paste0("Firsthand-", substr(rw_hash(output), 1L, 10L))
  owner <- paste0("Firsthand R | ", file.path(rw_root, "firsthand.R"))
  query_file <- tempfile(fileext = ".xml"); on.exit(unlink(query_file), add = TRUE)
  query <- rw_schtasks(c("/Query", "/TN", id, "/XML"), stdout_file = query_file)
  exists <- query$status == 0L
  if (exists) {
    existing <- xml2::read_xml(query_file, options = "NONET")
    description <- xml2::xml_text(xml2::xml_find_first(existing,
      "//*[local-name()='RegistrationInfo']/*[local-name()='Description']"))
    if (!identical(description, owner)) stop("Существующее задание принадлежит другой программе")
  }
  if (options$uninstall) {
    if (!exists) stop("Задание отсутствует или недоступно; удаление не выполнялось: ", query$error)
    result <- rw_schtasks(c("/Delete", "/TN", id, "/F"))
    if (result$status != 0L) stop("Не удалось удалить задание: ", result$error)
    message("Автозапуск выключен: ", id)
    return(0L)
  }
  user <- trimws(system2("whoami.exe", stdout = TRUE))
  task <- rw_windows_task(watchlist, output, options$every_hours, user)
  path <- file.path(rw_data_paths(output)$state, paste0(task$id, ".task.xml"))
  xml2::write_xml(task$xml, path, encoding = "UTF-16")
  args <- c("/Create", "/TN", id, "/XML", path, if (exists) "/F")
  result <- rw_schtasks(args)
  if (result$status != 0L) stop("Не удалось создать задание: ", result$error)
  verification <- rw_schtasks(c("/Query", "/TN", id, "/XML"), stdout_file = query_file)
  if (verification$status != 0L) stop("Задание создано, но проверка состояния не удалась: ", verification$error)
  message("Автозапуск включён: ", id, "\nИнтервал, часов: ", options$every_hours,
          "\nЛог: ", file.path(rw_data_paths(output)$state, "scheduler.log"))
  0L
}

rw_schedule_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if ("--help" %in% args) {
    cat("firsthand schedule [--watchlist FILE] [--output DIR]\n",
        "[--every-hours 168] [--install | --uninstall]\nБез install создаётся только файл задания.\n")
    return(0L)
  }
  options <- rw_arguments(args, list(watchlist = file.path(rw_root, "watchlist.txt"),
    output = file.path(rw_root, "output"), every_hours = 168, install = FALSE, uninstall = FALSE),
    flags = c("install", "uninstall"))
  if (options$install && options$uninstall) stop("Выберите install или uninstall")
  output <- rw_absolute(options$output)
  watchlist <- rw_absolute(options$watchlist)
  if (!options$uninstall) {
    # Validate input before stopping/replacing an existing job.
    rw_load_watchlist(watchlist)
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- rw_absolute(output, must_work = TRUE)
    watchlist <- rw_absolute(watchlist, must_work = TRUE)
    dir.create(rw_data_paths(output)$state, recursive = TRUE, showWarnings = FALSE)
  }
  if (.Platform$OS.type == "windows") {
    if (options$install || options$uninstall) return(rw_windows_schedule(options, watchlist, output))
    user <- trimws(system2("whoami.exe", stdout = TRUE))
    task <- rw_windows_task(watchlist, output, options$every_hours, user)
    path <- file.path(rw_data_paths(output)$state, paste0(task$id, ".task.xml"))
    xml2::write_xml(task$xml, path, encoding = "UTF-16")
    message("Подготовлено задание, автозапуск НЕ включён: ", path)
    return(0L)
  }
  plist <- rw_schedule_plist(watchlist, output, options$every_hours)
  target <- file.path(path.expand("~/Library/LaunchAgents"), paste0(plist$Label, ".plist"))
  if (options$install || options$uninstall) {
    if (Sys.info()[["sysname"]] != "Darwin") stop("Автозапуск launchd предназначен для macOS")
    uid <- trimws(system2("/usr/bin/id", "-u", stdout = TRUE))
    if (length(uid) != 1L || !grepl("^[0-9]+$", uid)) stop("Не удалось определить UID")
    domain <- paste0("gui/", uid)
    if (file.exists(target)) {
      existing <- xml2::read_xml(target, options = "NONET")
      script <- xml2::xml_text(xml2::xml_find_first(existing,
        "//key[text()='ProgramArguments']/following-sibling::array[1]/string[2]"))
      if (!script %in% file.path(rw_root, c("monitor.R", "firsthand.R"))) stop("Файл задания принадлежит другому скрипту")
      rw_launchctl(c("bootout", paste0(domain, "/", plist$Label)), absent_ok = TRUE)
    }
    if (options$uninstall) {
      if (file.exists(target)) unlink(target)
      message("Автозапуск выключен (либо не был установлен): ", plist$Label)
      return(0L)
    }
  }
  if (!options$install) {
    path <- file.path(rw_data_paths(output)$state, paste0(plist$Label, ".plist"))
    rw_write_plist(plist, path)
    message("Подготовлено задание, автозапуск НЕ включён: ", path)
    return(0L)
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  rw_write_plist(plist, target)
  rw_launchctl(c("bootstrap", domain, target))
  status <- rw_launchctl(c("print", paste0(domain, "/", plist$Label)))
  message("Автозапуск включён: ", target, "\nИнтервал, часов: ", options$every_hours)
  cat(paste(head(status, 25L), collapse = "\n"), "\n")
  0L
}

if (sys.nframe() == 0L) {
  code <- tryCatch(rw_schedule_main(), error = function(error) { message("Ошибка: ", conditionMessage(error)); 1L })
  quit(save = "no", status = code)
}
