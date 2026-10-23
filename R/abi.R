# =============================================================================
# Abifunktsioonid: PXWeb ja Eurostati päringud, perioodikoodid, kontrollid,
# JSON-failide kirjutamine.
#
# Põhimõte: kui mõne seeria laadimine või kontroll ebaõnnestub, jäetakse
# alles eelmine (töötav) versioon ja kirjutatakse hoiatus faili hoiatused.txt.
# Leht ei jää kunagi tühjaks.
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

PX_API   <- "https://andmed.stat.ee/api/v1/et/stat/"
DATA_DIR <- "docs/data"
ALGUS    <- 2015   # aegridade algusaasta JSON-failides

# ---- Hoiatused ---------------------------------------------------------------
hoiatused <- character()
hoiata <- function(...) {
  m <- paste0(...)
  message("HOIATUS: ", m)
  hoiatused <<- c(hoiatused, m)
}

# ---- Statistikaameti PXWeb API ----------------------------------------------
# valik("Aasta", "*", "all"), valik("Näitaja", c("1", "2")), valik("Vaatlusperiood", "60", "top")
valik <- function(code, values, filter = if (identical(values, "*")) "all" else "item") {
  list(code = code, selection = list(filter = filter, values = as.list(values)))
}

# Tagastab data.frame'i: üks veerg iga dimensiooni koodiga + veerg value.
px_paring <- function(tabel, ..., katseid = 3) {
  keha <- list(query = list(...), response = list(format = "json"))
  vastus <- NULL
  for (k in seq_len(katseid)) {
    vastus <- tryCatch(
      POST(paste0(PX_API, tabel), body = toJSON(keha, auto_unbox = TRUE),
           content_type_json(), encode = "raw", timeout(90)),
      error = function(e) e
    )
    if (!inherits(vastus, "error") && status_code(vastus) == 200) break
    Sys.sleep(5 * k)
  }
  if (inherits(vastus, "error")) stop(tabel, ": ", conditionMessage(vastus))
  stop_for_status(vastus, task = paste("laadida tabelit", tabel))
  Sys.sleep(0.5)  # Statistikaameti API piirab päringute arvu

  js <- fromJSON(content(vastus, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  dims  <- Filter(function(x) x$type != "c", js$columns)
  nimed <- vapply(dims, function(x) x$code, character(1))
  if (!length(js$data)) stop(tabel, ": päring ei tagastanud ridu.")

  votmed <- do.call(rbind, lapply(js$data, function(r) unlist(r$key)))
  df <- as.data.frame(votmed, stringsAsFactors = FALSE)
  names(df) <- nimed
  # ".." ja "." tähistavad puuduvat väärtust
  df$value <- vapply(js$data, function(r) suppressWarnings(as.numeric(r$values[[1]])), numeric(1))
  df
}

# ---- Eurostat ---------------------------------------------------------------
eurostat_seeria <- function(id, filters) {
  df <- eurostat::get_eurostat(id, filters = filters, time_format = "raw", cache = FALSE)
  tcol <- intersect(c("time", "TIME_PERIOD"), names(df))[1]
  df <- df[!is.na(df$values), ]
  if (!nrow(df)) stop(id, ": päring ei tagastanud andmeid.")
  p <- norm_periood(as.character(df[[tcol]]))
  if (anyDuplicated(p)) stop(id, ": filtrid jätavad alles mitu seeriat.")
  seeria(p, df$values)
}

# ---- Perioodikoodid ----------------------------------------------------------
# Lehel kasutatavad koodid: "2019M01", "2019Q1", "2021S2", "2019"
RN <- c("I", "II", "III", "IV")
per_q <- function(aasta, kvartal) paste0(aasta, "Q", match(kvartal, RN))
per_m <- function(aasta, kuu) sprintf("%sM%02d", aasta, as.integer(kuu))

norm_periood <- function(x) {
  x <- trimws(as.character(x))
  out <- x
  m <- grepl("^\\d{4}-\\d{2}$", x);      out[m] <- sub("^(\\d{4})-(\\d{2})$", "\\1M\\2", x[m])
  m <- grepl("^\\d{4}-?Q\\d$", x);       out[m] <- sub("^(\\d{4})-?Q(\\d)$", "\\1Q\\2", x[m])
  m <- grepl("^\\d{4}-?S\\d$", x);       out[m] <- sub("^(\\d{4})-?S(\\d)$", "\\1S\\2", x[m])
  m <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x); out[m] <- format(as.Date(x[m]), "%YM%m")
  out
}

# Aegrida: perioodid kasvavas järjekorras, puuduvad väärtused välja jäetud
seeria <- function(periods, values, alates = ALGUS) {
  ok <- !is.na(values) & as.integer(substr(periods, 1, 4)) >= alates
  p <- periods[ok]; v <- values[ok]
  o <- order(p)
  list(periods = p[o], values = round(v[o], 6))
}

# ---- Kontroll ja varuvariant -------------------------------------------------
# uus:   funktsioon, mis tagastab seeria (list(periods, values))
# vana:  sama seeria eelmisest JSON-failist või NULL
# vahemik: lubatud väärtuste vahemik c(min, max)
turvaliselt <- function(nimi, uus, vana, vahemik = c(-Inf, Inf)) {
  s <- tryCatch(uus(), error = function(e) {
    hoiata(nimi, ": laadimine ebaõnnestus (", conditionMessage(e), "). Jäetakse alles eelmine versioon.")
    NULL
  })
  if (is.null(s)) return(list(seeria = vana, staatus = "vana"))

  probleem <- NULL
  if (!length(s$values)) {
    probleem <- "tühi seeria"
  } else if (any(s$values < vahemik[1] | s$values > vahemik[2])) {
    probleem <- sprintf("väärtused väljaspool vahemikku [%s, %s]", vahemik[1], vahemik[2])
  } else if (!is.null(vana) && length(vana$periods)) {
    if (tail(s$periods, 1) < tail(vana$periods, 1)) {
      probleem <- sprintf("viimane periood %s on varasem kui eelmisel korral (%s)", tail(s$periods, 1), tail(vana$periods, 1))
    } else if (length(s$periods) < 0.9 * length(vana$periods)) {
      probleem <- sprintf("perioode on %d, eelmisel korral %d", length(s$periods), length(vana$periods))
    }
  }
  if (!is.null(probleem)) {
    hoiata(nimi, ": ", probleem, ". Jäetakse alles eelmine versioon.")
    return(list(seeria = vana, staatus = "vana"))
  }
  list(seeria = s, staatus = "uuendatud")
}

# ---- JSON --------------------------------------------------------------------
loe_json <- function(fail) {
  tee <- file.path(DATA_DIR, fail)
  if (!file.exists(tee)) return(NULL)
  tryCatch(fromJSON(tee, simplifyVector = TRUE), error = function(e) NULL)
}

kirjuta_json <- function(x, fail) {
  dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
  tmp <- file.path(DATA_DIR, paste0(".", fail, ".tmp"))
  write_json(x, tmp, auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  file.rename(tmp, file.path(DATA_DIR, fail))
  message("Kirjutatud: ", file.path(DATA_DIR, fail))
}

# Seeria JSON-iks: perioodid ja väärtused jäävad alati massiivideks
seeria_json <- function(s) {
  if (is.null(s)) return(NULL)
  list(periods = I(as.character(s$periods)), values = I(as.numeric(s$values)))
}

aeg_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---- Majandusplaaniindeks -> JSON ---------------------------------------------
# Kutsutakse välja majandusplaaniindeks_sca.R lõpus.
kirjuta_majandusplaan <- function(kokku, naitajad, esi, base_year, std_algus, std_lopp, std_valja,
                                  fail = "majandusplaan.json") {
  cols <- c("time", "n_naitajaid", "Majandusplaaniindeks", "Koond_kv_muutus_%",
            "Koond_aasta_muutus_%", "Koond_z", naitajad, "ESI_hinnatud_kuid")
  puudu <- setdiff(cols, names(kokku))
  if (length(puudu)) stop("Majandusplaaniindeks: puuduvad veerud ", paste(puudu, collapse = ", "))

  df <- as.data.frame(kokku)[, cols]
  df$time <- format(as.Date(df$time), "%Y-%m-%d")

  # Kontroll: viimane arvutatud kvartal ei tohi olla varasem kui eelmisel korral
  vana <- loe_json(fail)
  uus_viimane <- max(df$time[!is.na(df$Majandusplaaniindeks)])
  if (!is.null(vana)) {
    vana_df <- as.data.frame(vana$rows, stringsAsFactors = FALSE)
    names(vana_df) <- vana$cols
    vana_viimane <- max(vana_df$time[!is.na(suppressWarnings(as.numeric(vana_df$Majandusplaaniindeks)))])
    if (uus_viimane < vana_viimane) {
      hoiata("Majandusplaaniindeks: viimane kvartal ", uus_viimane, " on varasem kui eelmisel korral (",
             vana_viimane, "). Jäetakse alles eelmine versioon.")
      return(invisible(FALSE))
    }
  }

  read <- lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = TRUE])))
  hinn <- esi$kuu[esi$kuu$allikas != "Eurostat", c("time", "Majandususaldus")]
  esi_hinnatud <- lapply(seq_len(nrow(hinn)), function(i) list(format(as.Date(hinn$time[i]), "%Y-%m-%d"), round(hinn$Majandususaldus[i], 6)))

  kirjuta_json(list(
    cols         = I(cols),
    rows         = read,
    naitajad     = I(naitajad),
    base_year    = base_year,
    std          = list(algus = format(std_algus), lopp = format(std_lopp), valja = I(as.integer(std_valja))),
    uuendatud    = as.numeric(Sys.time()),
    esi_hinnatud = esi_hinnatud
  ), fail)
  invisible(TRUE)
}
