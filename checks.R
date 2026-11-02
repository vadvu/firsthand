# Offline regression checks; no personal data and no requests to registry sites.
files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
files <- gsub("~+~", " ", files, fixed = TRUE)
directory <- if (length(files)) dirname(normalizePath(files[1], winslash = "/")) else getwd()
source(file.path(directory, "monitor.R"))
source(file.path(directory, "schedule.R"))
rw_dependencies()

checks <- 0L
ok <- function(condition) {
  stopifnot(isTRUE(condition)); checks <<- checks + 1L
}
fails <- function(expression, pattern) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  ok(inherits(error, "error") && grepl(pattern, conditionMessage(error)))
}
temporary <- tempfile("registry-checks-"); dir.create(temporary)
copy <- file.path(temporary, "папка с пробелами"); dir.create(copy)
file.copy(file.path(directory, "firsthand.R"), copy)
help <- system2(rw_find_rscript(), c(shQuote(file.path(copy, "firsthand.R")), "help"), stdout = TRUE, stderr = TRUE)
ok(is.null(attr(help, "status")) && any(grepl("firsthand check", help)))

person <- function(...) {
  values <- modifyList(list(registry = "minjust", key = "123", name = "Петров Пётр Петрович",
    birth_date = "1980-01-15", included_on = "2026-10-30", section = "national"), list(...))
  values$source_number <- values$key
  if (values$registry == "rosfinmonitoring")
    values$key <- substr(rw_hash(rw_json_array(c(rw_norm(values$name), values$birth_date, values$section))), 1, 24)
  do.call(rw_record, values)
}
quiet <- function(expression) { invisible(capture.output(value <- force(expression))); value }

watch <- rw_watch("Петров Пётр Петрович", "1980-01-15")
ok(watch$key == "7e616dfb481e2e2af16dfa49")
ok(rw_norm("ПЁТР‑ИВАН") == "петр-иван")
matches <- rw_match(lapply(c("Петров", "Петрова", "Пётр"), rw_watch), list(person()))
ok(length(matches) == 1L && matches[[1]]$match_level == "surname_candidate")
matched <- rw_match(list(watch), list(person(), person(key = "2", birth_date = "1980-01-16"), person(key = "3", birth_date = "")))
ok(setequal(vapply(matched, `[[`, "", "birth_date_check"), c("match", "conflict", "unavailable")))
ok(matched[[1]]$match_level == "full_name_birth_date_match")
alias <- rw_watch("Петров Пётр Петрович", aliases = "Прежнев Пётр Петрович")
ok(length(rw_match(list(alias), list(person(aliases = list("Прежнев Пётр Петрович"))))) == 1L)
fails(rw_date("31.02.1980"), "дату")

input <- file.path(temporary, "names.csv")
writeLines(c("name;birth_date;aliases", "Петров Пётр Петрович;15.01.1980;Прежнев Пётр Петрович"), input, useBytes = TRUE)
ok(rw_load_watchlist(input)[[1]]$birth_date == "1980-01-15")
writeLines("", input); fails(rw_load_watchlist(input), "пуст")
input <- file.path(temporary, "имена с пробелами.txt")
writeLines("Иванов И.И.", input, useBytes = TRUE); fails(rw_load_watchlist(input), "Инициалы")
writeLines(watch$name, input, useBytes = TRUE)
file <- file.path(temporary, "история.json")
rw_json_write(file, list(number = 1L, text = "Имя"))
rw_json_write(file, list(number = 2L, text = "Другое имя"))
ok(identical(rw_json_read(file), list(number = 2L, text = "Другое имя")))
ok(!any(grepl("история.json\\.", list.files(temporary))))
ok(rw_absolute("C:/Users/a b/имена.txt") == "C:/Users/a b/имена.txt" || .Platform$OS.type == "windows")

matches <- rw_match(list(watch), list(person()))
previous <- list(watch_keys = list(watch$key), matches = matches)
ok(rw_changes(NULL, list(watch), matches)[[1]]$event == "baseline_match")
ok(length(rw_changes(previous, list(watch), matches)) == 0L)
ok(rw_changes(list(watch_keys = list(watch$key), matches = list()), list(watch), matches)[[1]]$event == "new_match")
ok(rw_changes(previous, list(watch), list())[[1]]$event == "not_seen_in_current_snapshot")
excluded <- rw_match(list(watch), list(person(status = "excluded", excluded_on = "2026-11-01")))
ok(rw_changes(previous, list(watch), excluded)[[1]]$event == "status_changed")
renumbered <- matches; renumbered[[1]]$source_number <- "999"
ok(length(rw_changes(previous, list(watch), renumbered)) == 0L)
summary <- rw_summary(list(sources = list(minjust = list(status = "ok"), rosfinmonitoring = list(status = "error"))),
  list(watch), rw_match(list(watch), list(person(birth_date = "1980-01-16"))))
ok(summary[[1]]$check_status == "birth_date_conflict_only" && summary[[1]]$active_compatible_matches == 0L)
ok(summary[[2]]$check_status == "source_error" && is.null(summary[[2]]$active_compatible_matches))

downloader <- rw_downloader(temporary)
fails(downloader$get("http://www.fedsfm.ru/list", "bad"), "HTTPS")
fails(downloader$get("https://example.org/list", "bad"), "HTTPS")
downloader$close()

# Exercise real state writes across repeated runs, with controlled source failures.
local({
  env <- environment(rw_check)
  originals <- mget(c("rw_downloader", "rw_download_minjust", "rw_parse_rosfin"), env)
  on.exit(list2env(originals, env))
  assign("rw_downloader", function(...) list(get = function(...) "unused", receipts = list(value = list()), close = function() NULL), env)
  records <- lapply(seq_len(20), function(i) person(key = as.character(i)))
  assign("rw_download_minjust", function(...) list(records = records, metadata = list(document_as_of = "2026-10-30", persons = 20L)), env)
  assign("rw_parse_rosfin", function(...) list(records = list(person(registry = "rosfinmonitoring")), metadata = list(document_as_of = "", persons = 1L)), env)
  output <- file.path(temporary, "result")
  result <- quiet(rw_check(input, output))
  ok(result$exit_code == 0L)
  result <- quiet(rw_check(input, output))
  ok(result$exit_code == 0L && nrow(result$changes) == 0L)
  cached <- quiet(rw_check(input, output, cached = TRUE))
  ok(cached$exit_code == 0L && nrow(cached$changes) == 0L)
  ok(setequal(list.files(output, all.files = TRUE, no.. = TRUE), unname(rw_registry_files)))
  ok(all(file.exists(file.path(output, rw_registry_files))))
  before <- rw_json_read(rw_data_paths(output)$file)$baselines$minjust
  for (failure in c("network", "shrink", "older")) {
    assign("rw_download_minjust", function(...) {
      if (failure == "network") stop("Сайт недоступен")
      list(records = if (failure == "shrink") head(records, 16L) else records,
           metadata = list(document_as_of = if (failure == "older") "2026-10-29" else "2026-10-30", persons = 20L))
    }, env)
    result <- quiet(rw_check(input, output))
    ok(result$exit_code == 2L && result$checks$check_status[1] == "source_error")
    ok(identical(rw_json_read(rw_data_paths(output)$file)$baselines$minjust, before))
    ok(is.na(result$checks$active_compatible_matches[1]))
  }
})

source(file.path(directory, "terminal.R"))
source(file.path(directory, "command.R"))
editor_file <- file.path(temporary, "editable.txt")
writeLines(c("# keep this comment", "Тестов Иван Петрович"), editor_file, useBytes = TRUE)
input_connection <- textConnection(c("+ Примеров Иван Петрович", "- 1", "s"))
quiet(firsthand_edit(editor_file, input_connection)); close(input_connection)
ok(identical(readLines(editor_file, encoding = "UTF-8"), c("# keep this comment", "Примеров Иван Петрович")))
previous_text <- readLines(editor_file)
input_connection <- textConnection(c("+ Другов Иван Петрович", "q"))
quiet(firsthand_edit(editor_file, input_connection)); close(input_connection)
ok(identical(readLines(editor_file), previous_text))
input_connection <- textConnection("0")
ok(quiet(firsthand_terminal("menu", input = input_connection)) == 0L); close(input_connection)
user_bin <- file.path(temporary, "command bin")
command <- rw_install_command(directory, command_dir = user_bin, update_path = FALSE)
output <- system2(command, "help", stdout = TRUE, stderr = TRUE)
ok(is.null(attr(output, "status")) && any(grepl("firsthand edit", output)))
command2 <- rw_install_command(directory, command_dir = user_bin, update_path = FALSE)
ok(identical(command, command2))
windows_bin <- file.path(temporary, "Windows launcher")
windows_command <- rw_install_command(directory, command_dir = windows_bin, update_path = FALSE, windows = TRUE)
ok(all(as.integer(readBin(windows_command, "raw", n = file.info(windows_command)$size)) < 128L))
entry <- file.path(windows_bin, "firsthand-entry.R")
entry_help <- system2(rw_find_rscript(), c(shQuote(entry), "help"), stdout = TRUE, stderr = TRUE)
ok(is.null(attr(entry_help, "status")) && any(grepl("firsthand edit", entry_help)))
if (.Platform$OS.type != "windows") {
  home <- file.path(temporary,"fake home"); dir.create(home)
  quiet(rw_install_command(directory, command_dir = file.path(home,".local","bin"), user_home = home))
  ok(any(file.exists(file.path(home,c(".zshrc",".bash_profile")))))
}

plist <- rw_schedule_plist("/some folder/имена.txt", "/some folder/output", 168, "/an R interpreter")
ok(plist$StartInterval == 604800L && "/some folder/имена.txt" %in% unlist(plist$ProgramArguments))
path <- file.path(temporary, "job.plist"); rw_write_plist(plist, path)
ok(length(xml2::xml_find_all(xml2::read_xml(path, options = "NONET"), "//array/string")) == 7L)
if (Sys.info()[["sysname"]] == "Darwin") {
  lint <- system2("/usr/bin/plutil", c("-lint", shQuote(path)), stdout = TRUE, stderr = TRUE)
  ok(is.null(attr(lint, "status")))
}
user <- if (.Platform$OS.type == "windows") trimws(system2("whoami.exe", stdout = TRUE)) else "TEST-PC\\test-user"
task <- rw_windows_task("C:/a b/имена.csv", "C:/data/output", 168, user, "C:/Program Files/R/bin/Rscript.exe")
path <- file.path(temporary, "job.task.xml"); xml2::write_xml(task$xml, path, encoding = "UTF-16")
doc <- xml2::read_xml(path, options = "NONET")
element <- function(name) xml2::xml_text(xml2::xml_find_first(doc, paste0("//*[local-name()='", name, "']")))
ok(element("Interval") == "PT168H")
ok(element("LogonType") == "InteractiveToken" && element("RunLevel") == "LeastPrivilege")
ok(grepl('"C:\\a b\\имена.csv"', element("Arguments"), fixed = TRUE))
ok(element("Command") == "C:\\Program Files\\R\\bin\\Rscript.exe")
fails(rw_windows_task("C:/names.txt", "C:/out", 745, user, "Rscript.exe"), "744")
fails(rw_windows_quote('a"b'), "кавычка")

# Windows validates the XML with Task Scheduler COM without registering a job.
if (.Platform$OS.type == "windows") {
  powershell <- file.path(temporary, "validate-task.ps1")
  writeLines(c('param([string]$TaskFile)', "$ErrorActionPreference = 'Stop'",
    "$service = New-Object -ComObject 'Schedule.Service'", "$service.Connect()", "$task = $service.NewTask(0)",
    "$task.XmlText = [System.IO.File]::ReadAllText($TaskFile)",
    "if ($task.Actions.Count -ne 1) { throw 'Invalid action count' }", "Write-Output 'WINDOWS_TASK_SCHEMA_OK'"), powershell)
  validation <- system2("powershell.exe", shQuote(c("-NoProfile", "-NonInteractive", "-File", powershell, path), type = "cmd"),
                        stdout = TRUE, stderr = TRUE)
  ok(is.null(attr(validation, "status")) && any(grepl("WINDOWS_TASK_SCHEMA_OK", validation)))
}
unlink(temporary, recursive = TRUE)
cat("PASSED:", checks, "checks on", Sys.info()[["sysname"]], "\n")
