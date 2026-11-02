# Install a user-local command pointing to this folder; no administrator rights.
rw_install_command <- function(root, command_dir = NULL, update_path = TRUE, user_home = path.expand("~"),
                                windows = .Platform$OS.type == "windows") {
  if (is.null(command_dir)) command_dir <- if (windows) {
    base <- Sys.getenv("LOCALAPPDATA")
    if (!nzchar(base)) base <- file.path(user_home, "AppData", "Local")
    file.path(base, "Firsthand", "bin")
  } else file.path(user_home, ".local", "bin")
  dir.create(command_dir, recursive = TRUE, showWarnings = FALSE)
  command_dir <- normalizePath(command_dir, winslash = "/", mustWork = TRUE)
  target <- file.path(command_dir, if (windows) "firsthand.cmd" else "firsthand")
  marker <- if (windows) "rem firsthand user launcher" else "# firsthand user launcher"
  if (file.exists(target) && !any(readLines(target, warn = FALSE) == marker))
    stop("Команда уже занята другим файлом: ", target)
  if (windows) {
    # Keep the batch file ASCII; R reads the Unicode application path itself.
    entry <- file.path(command_dir, "firsthand-entry.R")
    entry_marker <- "# firsthand user entry"
    if (file.exists(entry) && !any(readLines(entry, warn = FALSE) == entry_marker))
      stop("Имя файла уже занято: ", entry)
    path <- encodeString(file.path(root, "firsthand.R"), quote = '"')
    pointer <- c(entry_marker, paste0("source(", path, ", encoding = 'UTF-8')"),
      "code <- tryCatch(firsthand_main(commandArgs(trailingOnly = TRUE)), error = function(e) { message(conditionMessage(e)); 1L })",
      "quit(save = 'no', status = code)")
    writeLines(enc2utf8(pointer), entry, useBytes = TRUE)
    lines <- readLines(file.path(root, "firsthand.cmd"), warn = FALSE)
    lines <- sub("%~dp0firsthand.R", "%~dp0firsthand-entry.R", lines, fixed = TRUE)
    lines <- append(lines, marker, after = 1L)
    writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), target)
  } else {
    lines <- c("#!/bin/sh", marker, paste("exec /bin/sh", shQuote(file.path(root, "firsthand.sh")), '"$@"'))
    writeLines(lines, target, useBytes = TRUE)
    Sys.chmod(target, "0755")
  }
  directories <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  in_path <- tolower(command_dir) %in% tolower(gsub("\\", "/", directories, fixed = TRUE))
  if (update_path && !in_path) {
    if (windows) {
      script <- tempfile(fileext = ".ps1"); on.exit(unlink(script), add = TRUE)
      writeLines(c('param([string]$Bin)', "$ErrorActionPreference = 'Stop'",
        "$old = [Environment]::GetEnvironmentVariable('Path', 'User')",
        "$parts = @($old -split ';' | Where-Object { $_ })",
        "if ($parts -notcontains $Bin) {",
        "  $new = (@($Bin) + $parts) -join ';'",
        "  [Environment]::SetEnvironmentVariable('Path', $new, 'User')",
        "}"), script)
      result <- suppressWarnings(system2("powershell.exe", shQuote(c("-NoProfile", "-NonInteractive", "-File", script,
        gsub("/", "\\", command_dir, fixed = TRUE)), type = "cmd"), stdout = TRUE, stderr = TRUE))
      if (!is.null(attr(result, "status"))) stop("Команда создана, но не удалось обновить PATH: ", paste(result, collapse = "\n"))
    } else {
      shell <- basename(Sys.getenv("SHELL"))
      profile <- if (shell == "bash") file.path(user_home, ".bash_profile") else {
        zdot <- Sys.getenv("ZDOTDIR")
        file.path(if (nzchar(zdot) && identical(user_home, path.expand("~"))) zdot else user_home, ".zshrc")
      }
      old <- if (file.exists(profile)) readLines(profile, warn = FALSE) else character()
      line <- paste0("export PATH=", shQuote(command_dir), ':"$PATH"')
      if (!line %in% old) {
        dir.create(dirname(profile), recursive = TRUE, showWarnings = FALSE)
        cat(paste(c("", "# firsthand command", line, ""), collapse = "\n"), file = profile, append = TRUE)
      }
    }
    message("Откройте новое окно терминала, чтобы команда появилась в PATH.")
  }
  message("Команда установлена: ", target)
  message("Запуск: firsthand | Справка: firsthand help")
  invisible(target)
}
