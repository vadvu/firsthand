# Install only missing packages in a project-local library; no Python dependency.
files <- as.character(unlist(lapply(sys.frames(), function(f) f$ofile)))
files <- files[basename(files) == "install.R"]
if (!length(files)) files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
files <- gsub("~+~", " ", files, fixed = TRUE)
root <- if (length(files)) dirname(normalizePath(tail(files, 1L), winslash = "/", mustWork = TRUE)) else normalizePath("firsthand", winslash = "/")
if (getRversion() < "4.2.0") stop("Установите R 4.2 или новее")
lib <- file.path(root, "rlib"); dir.create(lib, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
packages <- c("curl", "httr2", "xml2", "readxl", "stringi", "jsonlite", "digest", "filelock", "fs")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = lib, repos = "https://cloud.r-project.org")
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
watchlist <- file.path(root, "watchlist.txt")
if (!file.exists(watchlist)) {
  if (!file.copy(file.path(root, "watchlist.example.txt"), watchlist)) stop("Не удалось создать watchlist.txt")
  message("Создан watchlist.txt: впишите фамилии или ФИО перед первой проверкой.")
}
message("Зависимости готовы. Папка приложения: ", root)
if (!isTRUE(getOption("registry.watch.no.command", FALSE)) && !"--no-command" %in% commandArgs(trailingOnly = TRUE)) {
  source(file.path(root, "command.R"))
  rw_install_command(root)
}
