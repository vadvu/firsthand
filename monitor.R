#!/usr/bin/env Rscript
# Firsthand: checks against official public registers. In RStudio, call
# rw_check(watchlist = "firsthand/watchlist.txt"). No Python is required.

rw_root <- local({
  files <- as.character(unlist(lapply(sys.frames(), function(f) f$ofile)))
  files <- files[basename(files) == "monitor.R"]
  if (!length(files)) {
    files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
    files <- gsub("~+~", " ", files, fixed = TRUE)
    files <- files[basename(files) == "monitor.R"]
  }
  if (length(files)) dirname(normalizePath(tail(files, 1L), winslash = "/", mustWork = TRUE))
  else normalizePath("firsthand", winslash = "/", mustWork = TRUE)
})
if (dir.exists(file.path(rw_root, "rlib"))) .libPaths(c(file.path(rw_root, "rlib"), .libPaths()))

rw_pages <- c(
  minjust = "https://www.minjust.gov.ru/ru/pages/reestr-inostryannykh-agentov/",
  rosfinmonitoring = "https://www.fedsfm.ru/documents/terrorists-catalog-portal-act"
)
rw_labels <- c(minjust = "Минюст: иностранные агенты", rosfinmonitoring = "Росфинмониторинг: перечень")
rw_fields <- list(
  results = c("query", "query_birth_date", "registry", "name", "aliases", "birth_date", "status",
              "included_on", "excluded_on", "match_level", "birth_date_check", "candidate_count",
              "matched_variant", "section", "key", "source_number", "source_marker", "watch_key"),
  checks = c("query", "query_birth_date", "registry", "check_status", "name_matches",
             "active_compatible_matches", "excluded_compatible_matches", "birth_date_conflicts"),
  changes = c("query", "query_birth_date", "registry", "name", "event", "before_status",
              "after_status", "birth_date_check", "record_key")
)

rw_dependencies <- function() {
  packages <- c("curl", "httr2", "xml2", "readxl", "stringi", "jsonlite", "digest", "filelock", "fs")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Нужны R-пакеты: ", paste(missing, collapse = ", "),
                            ". Выполните setup (Rscript firsthand.R setup).", call. = FALSE)
  invisible(TRUE)
}

rw_or <- function(x, fallback) if (is.null(x) || !length(x)) fallback else x
rw_text <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1])
rw_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
rw_hash <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)
rw_file_hash <- function(path) digest::digest(file = path, algo = "sha256")
rw_json_read <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
rw_json_write <- function(path, value) {
  temporary <- tempfile(paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  # libuv replaces an existing file on both Windows and Unix.
  fs::file_move(temporary, path)
}

rw_absolute <- function(path, must_work = FALSE) {
  path <- path.expand(path)
  absolute <- grepl("^(?:/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path, perl = TRUE)
  if (!absolute) path <- file.path(getwd(), path)
  normalizePath(path, winslash = "/", mustWork = must_work)
}

rw_norm <- function(x) {
  x <- stringi::stri_trans_casefold(stringi::stri_trans_nfkc(x))
  x <- stringi::stri_replace_all_fixed(x, "ё", "е")
  x <- stringi::stri_replace_all_regex(x, "[‐‑‒–—−]", "-")
  vapply(stringi::stri_extract_all_regex(x, "\\p{L}+(?:-\\p{L}+)*", omit_no_match = TRUE),
         paste, character(1), collapse = " ", USE.NAMES = FALSE)
}

rw_date <- function(value) {
  if (inherits(value, "Date") || inherits(value, "POSIXt")) return(format(value, "%Y-%m-%d", tz = "UTC"))
  value <- trimws(rw_text(value))
  if (value %in% c("", "-", "—")) return("")
  formats <- c("%d.%m.%Y", "%Y-%m-%d", "%d/%m/%Y")
  for (fmt in formats) {
    parsed <- suppressWarnings(as.Date(value, format = fmt))
    if (!is.na(parsed) && identical(format(parsed, fmt), value)) return(format(parsed, "%Y-%m-%d"))
  }
  stop("Не удалось прочитать дату: ", value, call. = FALSE)
}

# Stable identifiers use canonical arrays of normalized query/record fields.
rw_json_array <- function(x) paste0("[", paste(vapply(x, function(v)
  as.character(jsonlite::toJSON(v, auto_unbox = TRUE)), character(1)), collapse = ", "), "]")
rw_watch <- function(name, birth_date = "", aliases = character()) {
  aliases <- as.character(unlist(aliases, use.names = FALSE))
  key_input <- paste0("[", jsonlite::toJSON(rw_norm(name), auto_unbox = TRUE), ", ",
                      jsonlite::toJSON(birth_date, auto_unbox = TRUE), ", ",
                      rw_json_array(sort(rw_norm(aliases), method = "radix")), "]")
  list(name = name, birth_date = birth_date, aliases = as.list(aliases), key = substr(rw_hash(key_input), 1, 24))
}

rw_load_watchlist <- function(path) {
  text <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(text)) text[1] <- sub("^\ufeff", "", text[1])
  if (tolower(tools::file_ext(path)) == "csv") {
    if (!any(nzchar(trimws(text)))) stop("Список CSV пуст: ", path, call. = FALSE)
    header <- text[which(nzchar(trimws(text)))[1]]
    separators <- c(",", ";", "\t")
    hits <- vapply(separators, function(s) sum(strsplit(header, s, fixed = TRUE)[[1]] == "birth_date") +
                     sum(strsplit(header, s, fixed = TRUE)[[1]] == "name") +
                     stringi::stri_count_fixed(header, s), numeric(1))
    sep <- separators[which.max(hits)]
    data <- read.table(text = paste(text, collapse = "\n"), header = TRUE, sep = sep, quote = '"',
                       comment.char = "", colClasses = "character", check.names = FALSE,
                       stringsAsFactors = FALSE, na.strings = NULL, fill = FALSE)
    if (!"name" %in% names(data)) stop("В CSV нужна колонка name; необязательные: birth_date, aliases")
    data <- data[nzchar(trimws(data$name)), , drop = FALSE]
    watches <- lapply(seq_len(nrow(data)), function(i) rw_watch(trimws(data$name[i]),
      rw_date(if ("birth_date" %in% names(data)) data$birth_date[i] else ""),
      if ("aliases" %in% names(data)) Filter(nzchar, trimws(strsplit(data$aliases[i], "|", fixed = TRUE)[[1]])) else character()))
  } else {
    text <- trimws(text)
    watches <- lapply(text[nzchar(text) & !startsWith(text, "#")], rw_watch)
  }
  if (!length(watches)) stop("Список пуст: добавьте фамилии/ФИО в ", path, call. = FALSE)
  for (watch in watches) for (name in c(watch$name, unlist(watch$aliases))) {
    if (!nzchar(rw_norm(name)) || stringi::stri_detect_regex(name, "\\p{N}"))
      stop("Впишите только имя; дату рождения вынесите в birth_date: ", name)
    if (stringi::stri_detect_regex(name, "\\b\\p{L}\\s*\\."))
      stop("Инициалы не поддерживаются: укажите фамилию или полные имена: ", name)
  }
  if (anyDuplicated(vapply(watches, `[[`, "", "key"))) stop("В списке есть повторяющиеся запросы")
  watches
}

rw_name_variants <- function(raw) {
  primary <- sub('["«(].*$', "", raw, perl = TRUE)
  primary <- gsub("^[ ,;]+|[ ,;]+$", "", primary)
  found <- stringi::stri_match_all_regex(raw, '["«]([^"»]+)["»]|\\(([^()]+)\\)')[[1]]
  aliases <- if (!nrow(found) || is.na(found[1, 1])) character() else
    ifelse(is.na(found[, 2]), found[, 3], found[, 2])
  aliases <- trimws(stringi::stri_replace_first_regex(aliases, "(?i)^(?:ранее|прежн\\w*\\s+фио)\\s*:?\\s*", ""))
  aliases <- unique(aliases[nzchar(rw_norm(aliases))])
  list(name = if (nzchar(primary)) primary else trimws(raw), aliases = as.list(aliases))
}

rw_record <- function(registry, key, name, aliases = list(), birth_date = "", included_on = "",
                      excluded_on = "", status = "active", section = "", source_number = "", source_marker = "") {
  record <- list(registry = registry, key = key, name = name, aliases = as.list(aliases), birth_date = birth_date,
       included_on = included_on, excluded_on = excluded_on, status = status,
       section = section, source_number = source_number, source_marker = source_marker)
  for (field in setdiff(names(record), "aliases")) record[[field]] <- unname(record[[field]])
  record$aliases <- unname(lapply(record$aliases, unname))
  record
}

rw_parse_minjust <- function(path) {
  if (!identical(readBin(path, "raw", n = 2), charToRaw("PK")))
    stop("Вместо XLSX получен другой документ (возможны ошибка сайта или CAPTCHA)")
  data <- as.matrix(readxl::read_xlsx(path, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  data[is.na(data)] <- ""
  header_row <- which(apply(data[seq_len(min(30L, nrow(data))), , drop = FALSE], 1, function(row)
    any(grepl("фио", tolower(row), fixed = TRUE)) && any(grepl("основан", tolower(row), fixed = TRUE))))[1]
  if (is.na(header_row)) stop("Не найдена структура реестра Минюста")
  header <- stringi::stri_replace_all_regex(tolower(data[header_row, ]), "\\s+", " ")
  column <- function(pattern, label, exclude = NULL) {
    found <- grepl(pattern, header, fixed = TRUE)
    if (!is.null(exclude)) found <- found & !grepl(exclude, header, fixed = TRUE)
    if (sum(found) != 1L) stop("Неоднозначная/отсутствующая колонка Минюста: ", label)
    which(found)
  }
  cols <- c(name = column("псевдоним", "ФИО"), type = column("тип иностранного агента", "тип"),
            included = column("решения о включении", "включение", "опубликования"),
            excluded = column("решения об исключении", "исключение"),
            birth = column("дата рождения", "дата рождения"), number = column("№ п/п", "номер"))
  rows <- data[seq.int(header_row + 1L, nrow(data)), , drop = FALSE]
  rows <- rows[nzchar(rows[, cols["number"]]) & nzchar(rows[, cols["name"]]), , drop = FALSE]
  total <- nrow(rows)
  rows <- rows[grepl("физическ", tolower(rows[, cols["type"]]), fixed = TRUE), , drop = FALSE]
  if (nrow(rows) < 300L || total < 500L) stop("Слишком короткая выгрузка Минюста: ", nrow(rows), " физических лиц")
  if (anyDuplicated(rows[, cols["number"]])) stop("Повтор номера Минюста")
  records <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]; variants <- rw_name_variants(row[cols["name"]])
    excluded <- rw_date(row[cols["excluded"]])
    rw_record("minjust", row[cols["number"]], variants$name, variants$aliases,
               rw_date(row[cols["birth"]]), rw_date(row[cols["included"]]), excluded,
               if (nzchar(excluded)) "excluded" else "active", "Физические лица", row[cols["number"]])
  })
  stamp <- if (header_row > 1L) data[2, 1] else ""
  if (grepl("^[0-9]{4,5}(\\.[0-9]+)?$", stamp)) {
    stamp <- format(as.POSIXct(round(as.numeric(stamp) * 86400), origin = "1899-12-30", tz = "UTC"),
                    "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  } else if (nzchar(stamp)) stamp <- paste0(rw_date(stamp), "T00:00:00")
  list(records = records, metadata = list(document_as_of = stamp, total_entries = total,
    persons = length(records), scope = "Физические лица, включая исключённых"))
}

rw_parse_rosfin <- function(path) {
  doc <- xml2::read_html(path, encoding = "UTF-8")
  if (length(xml2::xml_find_all(doc, "//*[@id='NationalPart' or @id='InternationalPart']")) != 2L)
    stop("Не найдены обе части полного перечня Росфинмониторинга")
  titles <- xml2::xml_text(xml2::xml_find_all(doc, "//h1|//h2"))
  if (!any(grepl("весь список", titles, ignore.case = TRUE))) stop("Ожидался полный перечень, а не страница изменений/поиска")
  records <- list(); counts <- list()
  for (section in c("national", "international")) {
    id <- if (section == "national") "russianFL" else "foreignFL"
    container <- xml2::xml_find_all(doc, paste0("//*[@id='", id, "']"))
    if (length(container) != 1L) stop("Отсутствует блок физических лиц ", id)
    entries <- xml2::xml_find_all(container, ".//ol[contains(concat(' ',normalize-space(@class),' '),' terrorist-list ')]/li")
    counts[[section]] <- length(entries)
    text <- stringi::stri_replace_all_regex(xml2::xml_text(entries, trim = TRUE), "\\s+", " ")
    parsed <- stringi::stri_match_first_regex(text, "^(\\d+)\\.\\s*([^,]+),(.*)$")
    if (anyNA(parsed[, 1])) stop("Изменился формат строки Росфинмониторинга")
    for (i in seq_along(text)) {
      raw_name <- parsed[i, 3]; marker <- if (grepl("*", raw_name, fixed = TRUE)) "*" else ""
      variants <- rw_name_variants(gsub("*", "", raw_name, fixed = TRUE))
      birthday <- stringi::stri_match_first_regex(parsed[i, 4], "(?<!\\d)(\\d{2}\\.\\d{2}\\.\\d{4})\\s*г\\.?\\s*р\\.?")[1, 2]
      born <- if (is.na(birthday)) "" else rw_date(birthday)
      key <- substr(rw_hash(rw_json_array(c(rw_norm(variants$name), born, section))), 1, 24)
      records[[length(records) + 1L]] <- rw_record("rosfinmonitoring", key, variants$name, variants$aliases,
        born, section = section, source_number = parsed[i, 2], source_marker = marker)
    }
  }
  if (counts$national < 2000L) stop("Слишком короткий перечень Росфинмониторинга: ", counts$national)
  if (anyDuplicated(vapply(records, `[[`, "", "key"))) stop("Неоднозначные повторяющиеся имя/дата в перечне Росфинмониторинга")
  list(records = records, metadata = list(document_as_of = "", persons = length(records), sections = counts,
    scope = "Физические лица, национальная и международная части"))
}

rw_downloader <- function(directory, timeout = 45) {
  temp <- tempfile("firsthand-r-ca-"); dir.create(temp)
  certificates <- file.path(rw_root, "certificates")
  pins <- rw_json_read(file.path(certificates, "sources.json"))
  files <- list("reestrs.minjust.gov.ru" = c("globalsign_root.pem", "globalsign_intermediate.pem"),
    "www.fedsfm.ru" = c("russian_root.pem", "russian_sub.pem", "russian_sub_2024.pem"),
    "fedsfm.ru" = c("russian_root.pem", "russian_sub.pem", "russian_sub_2024.pem"))
  ca <- c(Sys.getenv("CURL_CA_BUNDLE"), Sys.getenv("SSL_CERT_FILE"), "/etc/ssl/cert.pem",
          "/etc/ssl/certs/ca-certificates.crt", "/etc/pki/tls/certs/ca-bundle.crt")
  ca <- unique(ca[nzchar(ca) & file.exists(ca)])
  bundles <- list()
  for (host in names(files)) {
    for (filename in files[[host]]) {
      pin <- Filter(function(p) identical(p$file, filename), pins)
      if (length(pin) != 1L || rw_file_hash(file.path(certificates, filename)) != pin[[1]]$sha256)
        stop("Изменился локальный сертификат ", filename)
    }
    bundle <- file.path(temp, paste0(host, ".pem"))
    writeLines(unlist(lapply(c(head(ca, 1L), file.path(certificates, files[[host]])), readLines, warn = FALSE)), bundle)
    bundles[[host]] <- bundle
  }
  receipts <- new.env(parent = emptyenv()); receipts$value <- list()
  allowed <- c("minjust.gov.ru", "www.minjust.gov.ru", "reestrs.minjust.gov.ru", "fedsfm.ru", "www.fedsfm.ru")
  get <- function(url, filename) {
    requested <- url
    for (redirect in seq_len(6L)) {
      parts <- httr2::url_parse(url)
      if (parts$scheme != "https" || !parts$hostname %in% allowed || nzchar(rw_text(parts$username)))
        stop("Загрузка вне официальных HTTPS-источников запрещена: ", url)
      options <- list(timeout = timeout, connecttimeout = 8L, followlocation = FALSE,
                      ssl_verifypeer = TRUE, ssl_verifyhost = 2L,
                      useragent = "Firsthand/1.0 (public registry snapshots)")
      if (!is.null(bundles[[parts$hostname]])) options$cainfo <- bundles[[parts$hostname]]
      response <- NULL
      for (attempt in seq_len(3L)) {
        handle <- do.call(curl::new_handle, options)
        response <- tryCatch(curl::curl_fetch_memory(url, handle), error = identity)
        retry <- inherits(response, "error") || response$status_code %in% c(429L, 502L, 503L, 504L)
        if (!retry || attempt == 3L) break
        Sys.sleep(attempt)
      }
      if (inherits(response, "error")) stop(response)
      headers <- curl::parse_headers_list(response$headers)
      if (response$status_code %in% c(301L, 302L, 303L, 307L, 308L)) {
        location <- headers[[which(tolower(names(headers)) == "location")[1]]]
        if (is.null(location)) stop("Перенаправление без Location")
        url <- xml2::url_absolute(location, url)
        next
      }
      path <- file.path(directory, filename); writeBin(response$content, path)
      receipt <- list(requested_url = requested, url = url, file = filename, sha256 = rw_file_hash(path),
        bytes = length(response$content), http_status = response$status_code, headers = headers,
        downloaded_at = rw_utc(), tls_verified = TRUE, attempts = attempt)
      receipts$value[[length(receipts$value) + 1L]] <- receipt
      if (response$status_code < 200L || response$status_code >= 300L) stop("HTTP ", response$status_code, ": ", url)
      return(path)
    }
    stop("Слишком много перенаправлений")
  }
  list(get = get, receipts = receipts, close = function() unlink(temp, recursive = TRUE))
}

rw_download_minjust <- function(downloader) {
  page <- downloader$get(rw_pages[["minjust"]], "minjust_page.html")
  doc <- xml2::read_html(page, encoding = "UTF-8")
  scripts <- paste(xml2::xml_text(xml2::xml_find_all(doc, "//script")), collapse = "\n")
  base <- stringi::stri_match_first_regex(scripts, "ExternalApi\\.setBaseUrl\\(['\"]([^'\"]+)['\"]\\)")[1, 2]
  id <- stringi::stri_match_first_regex(scripts, "(?:let|const|var)\\s+id\\s*=\\s*['\"]([\\w-]+)['\"]")[1, 2]
  if (!is.na(base) && !is.na(id)) url <- paste0(sub("/$", "", base), "/rest/registry/", id, "/export")
  else {
    links <- xml2::xml_find_all(doc, "//a[@href]")
    href <- xml2::xml_attr(links, "href")
    found <- grepl("реестр", xml2::xml_text(links), ignore.case = TRUE) & grepl("\\.xlsx(?:\\?|$)", href, ignore.case = TRUE)
    if (sum(found) != 1L) stop("Не найдена однозначная официальная ссылка на выгрузку Минюста")
    url <- xml2::url_absolute(href[found], rw_pages[["minjust"]])
  }
  parsed <- rw_parse_minjust(downloader$get(url, "minjust.xlsx"))
  parsed$metadata$download_url <- url
  parsed
}

rw_match <- function(watches, records) {
  index <- new.env(hash = TRUE, parent = emptyenv())
  for (record in records) for (variant in c(record$name, unlist(record$aliases))) {
    parts <- strsplit(rw_norm(variant), " ", fixed = TRUE)[[1]]
    if (!length(parts)) next
    first <- parts[1]
    index[[first]] <- c(index[[first]], list(list(record = record, parts = parts, variant = variant)))
  }
  matches <- list()
  for (watch in watches) {
    found <- list()
    for (query in c(watch$name, unlist(watch$aliases))) {
      parts <- strsplit(rw_norm(query), " ", fixed = TRUE)[[1]]
      for (candidate in index[[parts[1]]]) {
        if (length(parts) > length(candidate$parts) || !identical(parts, head(candidate$parts, length(parts)))) next
        record <- candidate$record
        strength <- if (length(parts) == 1L) 1L else if (length(parts) != length(candidate$parts) || length(parts) < 3L) 2L else 3L
        dob <- "not_requested"
        if (nzchar(watch$birth_date)) {
          dob <- if (!nzchar(record$birth_date)) "unavailable" else if (watch$birth_date == record$birth_date) "match" else "conflict"
          if (dob == "match" && strength == 3L) strength <- 4L
        }
        level <- c("surname_candidate", "name_candidate", "full_name_match", "full_name_birth_date_match")[strength]
        row <- c(list(watch_key = watch$key, query = watch$name, query_birth_date = watch$birth_date,
                      match_level = level, birth_date_check = dob, matched_variant = candidate$variant), record)
        if (is.null(found[[record$key]]) || strength > found[[record$key]]$strength)
          found[[record$key]] <- list(strength = strength, row = row)
      }
    }
    if (length(found)) {
      order <- order(-vapply(found, `[[`, 0L, "strength"), vapply(found, function(v) v$row$name, ""), method = "radix")
      for (item in found[order]) {
        item$row$candidate_count <- length(found)
        matches[[length(matches) + 1L]] <- item$row
      }
    }
  }
  matches
}

rw_changes <- function(previous, watches, matches) {
  key <- function(row) paste(row$watch_key, row$key, sep = ":")
  old <- rw_or(previous$matches, list()); names(old) <- vapply(old, key, "")
  current <- matches; names(current) <- vapply(current, key, "")
  known <- unlist(previous$watch_keys); wanted <- vapply(watches, `[[`, "", "key")
  changes <- list()
  for (id in sort(union(names(old), names(current)), method = "radix")) {
    before <- old[[id]]; after <- current[[id]]; record <- rw_or(after, before)
    if (!record$watch_key %in% wanted) next
    event <- ""
    if (is.null(before)) event <- if (record$watch_key %in% known) "new_match" else "baseline_match"
    else if (is.null(after)) event <- "not_seen_in_current_snapshot"
    else if (before$status != after$status) event <- "status_changed"
    else if (any(vapply(c("name", "aliases", "birth_date", "included_on", "excluded_on", "birth_date_check", "source_marker"),
                         function(field) !identical(before[[field]], after[[field]]), logical(1)))) event <- "record_updated"
    if (nzchar(event)) changes[[length(changes) + 1L]] <- list(registry = record$registry, query = record$query,
      query_birth_date = record$query_birth_date, record_key = record$key, name = record$name, event = event,
      before_status = rw_text(before$status), after_status = rw_text(after$status), birth_date_check = record$birth_date_check)
  }
  changes
}

rw_summary <- function(run, watches, matches) {
  rows <- list()
  for (watch in watches) for (source in names(rw_pages)) {
    found <- Filter(function(m) m$watch_key == watch$key && m$registry == source, matches)
    compatible <- Filter(function(m) m$birth_date_check != "conflict", found)
    active <- Filter(function(m) m$status == "active", compatible)
    status <- if (run$sources[[source]]$status == "error") "source_error" else if (length(active)) "active_name_match"
      else if (length(compatible)) "excluded_name_match" else if (length(found)) "birth_date_conflict_only" else "not_found_in_snapshot"
    row <- list(query = watch$name, query_birth_date = watch$birth_date, registry = source, check_status = status,
      name_matches = length(found), active_compatible_matches = length(active),
      excluded_compatible_matches = sum(vapply(compatible, function(m) m$status == "excluded", logical(1))),
      birth_date_conflicts = sum(vapply(found, function(m) m$birth_date_check == "conflict", logical(1))))
    if (status == "source_error") for (field in rw_fields$checks[5:8]) row[field] <- list(NULL)
    rows[[length(rows) + 1L]] <- row
  }
  rows
}

rw_table <- function(rows, fields) {
  columns <- lapply(fields, function(field) {
    values <- lapply(rows, `[[`, field)
    if (length(values) && all(vapply(values, function(v) is.null(v) || is.numeric(v), logical(1))))
      return(vapply(values, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0))
    vapply(values, function(v) if (is.null(v)) "" else paste(unlist(v), collapse = " | "), "")
  })
  names(columns) <- fields
  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}

rw_csv <- function(path, rows, fields) {
  data <- rw_table(rows, fields)
  for (field in names(data)) if (is.character(data[[field]]))
    data[[field]] <- ifelse(grepl("^[=+@-]", data[[field]]), paste0("'", data[[field]]), data[[field]])
  con <- file(path, open = "wb"); on.exit(close(con))
  writeBin(as.raw(c(239, 187, 191)), con)
  write.table(data, con, sep = ",", row.names = FALSE, na = "", qmethod = "double")
}

rw_arguments <- function(args, defaults, flags = character()) {
  i <- 1L
  while (i <= length(args)) {
    key <- sub("^--", "", args[i]); key <- gsub("-", "_", key, fixed = TRUE)
    if (!startsWith(args[i], "--") || !key %in% names(defaults)) stop("Неизвестный аргумент: ", args[i])
    if (key %in% flags) defaults[[key]] <- TRUE else {
      i <- i + 1L
      if (i > length(args)) stop("Нет значения для ", key)
      defaults[key] <- list(args[i])
    }
    i <- i + 1L
  }
  defaults
}

rw_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if ("--help" %in% args) {
    cat("firsthand check [--watchlist FILE] [--output DIR]\n",
        "[--minjust-file XLSX] [--rosfin-file HTML] [--timeout SECONDS]\n")
    return(0L)
  }
  options <- rw_arguments(args, list(watchlist = file.path(rw_root, "watchlist.txt"), output = file.path(rw_root, "output"),
                                     minjust_file = NULL, rosfin_file = NULL, timeout = 45, cached = FALSE), flags = "cached")
  options$timeout <- as.numeric(options$timeout)
  if (!is.finite(options$timeout) || options$timeout <= 0) stop("timeout должен быть положительным")
  do.call(rw_check, options)$exit_code
}

source(file.path(rw_root, "storage.R"), local = environment())

if (sys.nframe() == 0L) {
  code <- tryCatch(rw_main(), error = function(error) { message("Ошибка: ", conditionMessage(error)); 1L })
  quit(save = "no", status = code)
}
