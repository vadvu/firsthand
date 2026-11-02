#!/usr/bin/env Rscript
# One entry point for setup, checks and local schedules on macOS and Windows.
firsthand_root <- local({
  files <- as.character(unlist(lapply(sys.frames(), function(f) f$ofile)))
  files <- files[basename(files) == "firsthand.R"]
  if (!length(files)) files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  files <- gsub("~+~", " ", files, fixed = TRUE)
  if (length(files)) dirname(normalizePath(tail(files, 1L), winslash = "/", mustWork = TRUE)) else getwd()
})

firsthand_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  command <- if (length(args)) args[1] else "menu"
  args <- if (length(args)) args[-1] else character()
  if (command %in% c("help", "--help", "-h")) {
    cat("Firsthand — проверка двух официальных реестров\n\n",
        "firsthand                 меню\n",
        "firsthand edit            изменить список людей\n",
        "firsthand watchlist       показать список\n",
        "firsthand refresh         обновить две таблицы\n",
        "firsthand check           обновить реестры и проверить список\n",
        "firsthand check --cached  проверить уже сохранённые таблицы\n",
        "firsthand tables          показать расположение таблиц\n",
        "firsthand schedule --every-hours 168 --install\n",
        "firsthand unschedule      отключить расписание\n\n",
        "Установка команды: Rscript install.R\n", sep = "")
    return(0L)
  }
  if (command == "setup") {
    if (any(!args %in% "--no-command")) stop("У setup есть только флаг --no-command")
    old <- options(registry.watch.no.command = "--no-command" %in% args)
    on.exit(options(old), add = TRUE)
    source(file.path(firsthand_root, "install.R"))
    return(0L)
  }
  if (command %in% c("menu", "edit", "watchlist", "tables", "refresh")) {
    source(file.path(firsthand_root, "monitor.R"))
    source(file.path(firsthand_root, "terminal.R"))
    return(firsthand_terminal(command, args))
  }
  if (command == "check") {
    source(file.path(firsthand_root, "monitor.R"))
    position <- which(args == "--log-file")
    if (length(position)) {
      if (length(position) != 1L || position == length(args)) stop("Укажите один --log-file FILE")
      path <- rw_absolute(args[position + 1L])
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      connection <- file(path, open = "at", encoding = "UTF-8")
      old_output <- sink.number(); old_messages <- sink.number(type = "message")
      sink(connection, type = "output"); sink(connection, type = "message")
      on.exit({
        if (sink.number(type = "message") != old_messages) sink(type = "message")
        while (sink.number() > old_output) sink()
        close(connection)
      }, add = TRUE)
      message("\nПроверка: ", rw_utc())
      args <- args[-c(position, position + 1L)]
    }
    return(tryCatch(rw_main(args), error = function(error) { message("Ошибка: ", conditionMessage(error)); 1L }))
  }
  if (command %in% c("schedule", "unschedule")) {
    source(file.path(firsthand_root, "schedule.R"))
    rw_dependencies()
    if (command == "unschedule") args <- c(args, "--uninstall")
    return(rw_schedule_main(args))
  }
  stop("Неизвестная команда: ", command, ". Справка: firsthand help")
}

if (sys.nframe() == 0L) {
  code <- tryCatch(firsthand_main(), error = function(error) { message("Ошибка: ", conditionMessage(error)); 1L })
  quit(save = "no", status = code)
}
