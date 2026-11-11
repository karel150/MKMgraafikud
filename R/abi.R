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
PX_SQL   <- "https://andmed.stat.ee/api/v1/et/statsql/"   # Statistikaameti uus andmebaas (nt VKK10)
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
px_paring <- function(tabel, ..., katseid = 3, api = PX_API) {
  keha <- list(query = list(...), response = list(format = "json"))
  vastus <- NULL
  for (k in seq_len(katseid)) {
    vastus <- tryCatch(
      POST(paste0(api, tabel), body = toJSON(keha, auto_unbox = TRUE),
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

# ---- Maksu- ja Tolliamet: maksuvõla ülevaade ----------------------------------
# EMTA avaldab iga kuu 1. kuupäeva seisu (võlgnike arv ja võlasumma). Fail laaditakse
# EMTA lingilt; kui see ei õnnestu, kasutatakse repos olevat koopiat
# andmed/maksuvola_ulevaade.csv ja lisatakse hoiatus (koopia võib olla vananenud).
# Tagastab data.frame'i: per (eelmise kuu kood, nt 01.11.2026 -> "2026M10", sest
# 1. kuupäeva seis on eelmise kuu lõpu seis), kokku, fi, ji, summa (mln €).
EMTA_MAKSUVOLG_URL  <- "https://ncfailid.emta.ee/s/n8MYQDceSXrSBF8/download/maksuvola_ulevaade.csv"
EMTA_MAKSUVOLG_FAIL <- "andmed/maksuvola_ulevaade.csv"

emta_loe_maksuvolg <- function(tekst) {
  tekst <- sub("^\ufeff", "", tekst)
  df <- read.csv(text = tekst, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  veerg <- function(muster) {
    i <- grep(muster, names(df))
    if (length(i) != 1) stop("EMTA maksuvõlg: veergu '", muster, "' ei leitud (faili ülesehitus on muutunud).")
    df[[i]]
  }
  kp <- as.Date(trimws(veerg("^Kuup")), format = "%d.%m.%Y")
  out <- data.frame(
    kp    = kp,
    kokku = as.numeric(veerg("nike arv kokku$")),
    fi    = as.numeric(veerg("^F.+isikust v.lgnike arv$")),
    ji    = as.numeric(veerg("^Juriidilisest isikust v.lgnike arv$")),
    summa = as.numeric(veerg("^V.lasumma kokku$")) / 1e6
  )
  out <- out[!is.na(out$kp), ]
  if (!nrow(out)) stop("EMTA maksuvõlg: failis pole ridu.")
  lt <- as.POSIXlt(out$kp)
  out$per <- ifelse(lt$mon == 0, sprintf("%dM12", lt$year + 1899L), sprintf("%dM%02d", lt$year + 1900L, lt$mon))
  if (anyDuplicated(out$per)) stop("EMTA maksuvõlg: sama kuu kohta on mitu rida.")
  out[order(out$per), ]
}

emta_maksuvolg <- function(katseid = 3) {
  vastus <- NULL
  for (k in seq_len(katseid)) {
    vastus <- tryCatch(GET(EMTA_MAKSUVOLG_URL, timeout(60)), error = function(e) e)
    if (!inherits(vastus, "error") && status_code(vastus) == 200) break
    if (k < katseid) Sys.sleep(5 * k)
  }
  veebist <- tryCatch({
    if (inherits(vastus, "error")) stop(conditionMessage(vastus))
    stop_for_status(vastus, task = "laadida EMTA maksuvõla ülevaadet")
    emta_loe_maksuvolg(content(vastus, as = "text", encoding = "UTF-8"))
  }, error = function(e) e)
  if (!inherits(veebist, "error")) return(veebist)
  hoiata("EMTA maksuvõlg: faili ei õnnestunud laadida (", conditionMessage(veebist),
         "). Kasutatakse koopiat ", EMTA_MAKSUVOLG_FAIL, ", mis võib olla vananenud.")
  emta_loe_maksuvolg(paste(readLines(EMTA_MAKSUVOLG_FAIL, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
}

# ---- Töötleva tööstuse tootmisvõimsuse rakendatus ---------------------------
# Ajalooline rida: Eurostati tabel teibs070 (%, sesoonselt korrigeeritud). Kui see
# ei laadi, kasutatakse sama rida andmestikust ei_bsin_q_r2 (BS-ICU-PC, SA).
voimsus_eurostat_riigid <- function(geos) {
  tryCatch(eurostat_riigid("teibs070", list(), geos), error = function(e) {
    message("teibs070 ei laadinud (", conditionMessage(e), "), kasutan andmestikku ei_bsin_q_r2.")
    eurostat_riigid("ei_bsin_q_r2", list(indic = "BS-ICU-PC", s_adj = "SA", freq = "Q"), geos)
  })
}

# Eesti kvartalid, mida Eurostatis veel pole (pärast perioodi `viimane`, nt "2026Q2"):
# Statistikaameti tööstusbaromeeter BAR03, näitaja INDU_TOT_13_QP_Q. Küsimust
# küsitakse kvartali esimesel kuul.
#   icu: majandusplaaniindeksi skripti rida (time, values), kus BAR03 on juba
#        hooajaliselt korrigeeritud; siis näitavad leht ja indeks sama arvu.
#        Kui seda pole (indeksi arvutus ebaõnnestus), võetakse BAR03 otse,
#        hooajaliselt korrigeerimata.
voimsus_stat <- function(viimane, icu = NULL) {
  if (!length(viimane)) viimane <- ""
  if (!is.null(icu) && nrow(icu)) {
    lt <- as.POSIXlt(as.Date(icu$time))
    p <- sprintf("%dQ%d", lt$year + 1900L, lt$mon %/% 3L + 1L)
    v <- icu$values
  } else {
    d <- px_paring("BAR03", valik("N\u00e4itaja", "INDU_TOT_13_QP_Q"), valik("Vaatlusperiood", "*"))
    d <- d[!is.na(d$value), ]
    kv <- sprintf("%sQ%d", substr(d$Vaatlusperiood, 1, 4), (as.integer(substr(d$Vaatlusperiood, 6, 7)) - 1L) %/% 3L + 1L)
    v <- tapply(d$value, kv, mean)
    p <- names(v); v <- as.numeric(v)
  }
  ok <- !is.na(v) & p > viimane
  if (any(v[ok] < 30 | v[ok] > 100)) stop("BAR03: tootmisvõimsuse rakendatus väljaspool vahemikku [30, 100].")
  seeria(p[ok], v[ok])
}

# ---- Euroopa Keskpank (ECB Data Portal) -------------------------------------
# Üks päring mitme riigi kohta -> nimega list seeriatest (NULL, kui riigil seda rida pole).
#   voog: andmevoog, nt "MIR" (rahaloomeasutuste intressimäärad)
#   voti: seeriavõti ilma andmevoota, riigi koht tähisega {geo},
#         nt "M.{geo}.B.A2I.AM.R.A.2240.EUR.N"
# Sama päringu tulemus jäetakse käivituse ajaks meelde (naitajad.R ja vordlus.R
# kasutavad sama allalaadimist).
ECB_API <- "https://data-api.ecb.europa.eu/service/data/"
.ecb_malu <- new.env(parent = emptyenv())

ecb_riigid <- function(voog, voti, geos, katseid = 3) {
  url <- paste0(ECB_API, voog, "/", sub("{geo}", paste(geos, collapse = "+"), voti, fixed = TRUE))
  meeles <- .ecb_malu[[url]]
  if (inherits(meeles, "condition")) stop(meeles)
  if (!is.null(meeles)) return(meeles[geos])

  tulemus <- tryCatch({
    vastus <- NULL
    for (k in seq_len(katseid)) {
      vastus <- tryCatch(GET(url, query = list(format = "csvdata", startPeriod = paste0(ALGUS, "-01")), timeout(90)),
                         error = function(e) e)
      # 404 = sellist seeriat pole; korduskatse ei aita
      if (!inherits(vastus, "error") && status_code(vastus) %in% c(200, 404)) break
      if (k < katseid) Sys.sleep(5 * k)
    }
    if (inherits(vastus, "error")) stop("ECB ", voog, ": ", conditionMessage(vastus))
    stop_for_status(vastus, task = paste("laadida ECB andmevoogu", voog))
    ecb_loe_csv(content(vastus, as = "text", encoding = "UTF-8"), voog)
  }, error = function(e) e)
  assign(url, tulemus, envir = .ecb_malu)
  if (inherits(tulemus, "condition")) stop(tulemus)
  tulemus[geos]
}

# ECB csvdata-vastus -> nimega list seeriatest riigi koodi järgi
ecb_loe_csv <- function(tekst, voog = "ECB") {
  df <- read.csv(text = tekst, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  names(df) <- toupper(names(df))
  puudu <- setdiff(c("REF_AREA", "TIME_PERIOD", "OBS_VALUE"), names(df))
  if (length(puudu)) stop("ECB ", voog, ": vastuses puuduvad veerud ", paste(puudu, collapse = ", "), ".")
  df$v <- suppressWarnings(as.numeric(df$OBS_VALUE))
  df <- df[!is.na(df$v), ]
  if (!nrow(df)) stop("ECB ", voog, ": päring ei tagastanud andmeid.")
  lapply(split(df, df$REF_AREA), function(d) {
    p <- norm_periood(d$TIME_PERIOD)
    if (anyDuplicated(p)) stop("ECB ", voog, " ", d$REF_AREA[1], ": võti jätab alles mitu seeriat.")
    seeria(p, d$v)
  })
}

# Ettevõtete uute laenude intressimäär (ECB laenukulu koondnäitaja, % aastas): Eesti,
# võrdlusriigid ja euroala (U2). Riik, kelle kohta ECB seda rida ei avalda, jääb välja.
ECB_LAENUINTRESS <- "M.{geo}.B.A2I.AM.R.A.2240.EUR.N"
ecb_laenuintress <- function() {
  ecb_riigid("MIR", ECB_LAENUINTRESS, unique(c("EE", get0("VORDLUS_RIIGID", ifnotfound = character()), "U2")))
}

# ---- Eurostat ---------------------------------------------------------------
# get_eurostat: paketi eurostat samanimeline funktsioon koos korduskatsete ja
# vahemäluga. Kuna see on defineeritud töökeskkonnas, kasutavad seda ka
# metoodikaskriptid (majandusplaaniindeks_sca.R, esi_eesti.R), mis kutsuvad
# get_eurostat() ilma paketi nimeta; neid skripte pole vaja muuta ja eraldi
# käivitades töötavad nad endiselt paketi enda funktsiooniga.
#   * Ajutise vea korral proovitakse uuesti (vaikimisi 3 katset, paus 5 s, 10 s).
#   * Sama päringu (id + filtrid + vorming) tulemus jäetakse meelde, nii et üks
#     käivitus ei laadi sama asja mitu korda. Meelde jääb ka lõplik viga, et
#     sama ebaõnnestunud päringut ei korrataks iga sellest sõltuva seeria jaoks.
.eurostat_malu <- new.env(parent = emptyenv())

get_eurostat <- function(id, ..., katseid = 3, paus = 5, .lae = eurostat::get_eurostat) {
  voti <- paste(deparse(list(id, ...), width.cutoff = 500L), collapse = "")
  meeles <- .eurostat_malu[[voti]]
  if (inherits(meeles, "condition")) stop(meeles)
  if (!is.null(meeles)) return(meeles)

  viga <- NULL
  for (k in seq_len(katseid)) {
    df <- tryCatch(.lae(id, ...), error = function(e) e)
    if (is.data.frame(df)) {
      assign(voti, df, envir = .eurostat_malu)
      return(df)
    }
    # pakett eurostat võib võrguvea korral tagastada vea asemel NULL-i
    viga <- if (inherits(df, "condition")) conditionMessage(df) else "Eurostat ei tagastanud andmeid"
    if (k < katseid) {
      message("Eurostat ", id, ": katse ", k, " ebaõnnestus (", viga, "), proovin uuesti.")
      Sys.sleep(paus * k)
    }
  }
  viga <- simpleError(paste0(id, ": ", viga, " (", katseid, " katset)"))
  assign(voti, viga, envir = .eurostat_malu)
  stop(viga)
}

# Andmestikud, millest on vaja mitut seeriat (rahvusvaheline võrdlus + Eesti
# näitajad). Iga andmestik laaditakse ühe päringuga kõigi siin loetletud väärtuste
# kohta ja üksikud seeriad lõigatakse sellest välja (eurostat_tabel). Nii tehakse
# nt namq_10_gdp kohta seitsme päringu asemel üks.
# Kui mõne seeria filtrid siia ei mahu (teine dimensioon või väärtus, mida plaanis
# pole), tehakse selle seeria jaoks lihtsalt eraldi päring. Uue seeria lisamisel
# olemasolevast andmestikust lisa siia tema väärtused, muidu laaditakse ta eraldi.
eurostat_plaan <- function() {
  riigid <- unique(c("EE", get0("VORDLUS_RIIGID", ifnotfound = character())))
  list(
    ei_bssi_m_r2   = list(geo = riigid, indic = "BS-ESI-I", s_adj = "SA", freq = "M"),
    bop_iip6_q     = list(geo = riigid, currency = "MIO_EUR", bop_item = "FA__D__F", sector10 = "S1", sectpart = "S1",
                          stk_flow = "L_LE", partner = "WRL_REST", freq = "Q"),
    namq_10_gdp    = list(geo = riigid, na_item = c("B1GQ", "P6", "P61", "P62", "P51G"),
                          unit = c("CP_MEUR", "CLV_PCH_SM"), s_adj = c("NSA", "SCA"), freq = "Q"),
    nama_10_gdp    = list(geo = riigid, na_item = c("B1GQ", "P6"), unit = "CLV_PCH_PRE", freq = "A"),
    namq_10_lp_ulc = list(geo = riigid, na_item = c("RLPR_HW", "NULC_HW"), unit = "PCH_SM", s_adj = "SCA", freq = "Q"),
    nama_10_lp_ulc = list(geo = riigid, na_item = c("RLPR_HW", "NULC_HW"), unit = "PCH_PRE", freq = "A"),
    lfsi_emp_q     = list(geo = riigid, indic_em = c("EMP_LFS", "ACT"), age = c("Y20-64", "Y15-74"), sex = "T",
                          unit = "PC_POP", s_adj = "SA", freq = "Q"),
    nrg_pc_205     = list(geo = riigid, nrg_cons = "TOT_KWH", tax = "X_TAX", currency = "EUR", unit = "KWH"),
    nasa_10_ki     = list(geo = riigid, na_item = "SRG_S14_S15", unit = "PC", freq = "A"),
    teibs070       = list(geo = riigid),
    isoc_eb_ai     = list(geo = c(riigid, "EU27_2020"), indic_is = "E_AI_TANY", size_emp = "GE10", unit = "PC_ENT", freq = "A")
  )
}

# Ühe seeria(rühma) toorandmed. Kui filtrid mahuvad andmestiku plaani, kasutatakse
# ühist (meelde jäetud) päringut ja valik tehakse kohapeal; muidu tavaline päring.
eurostat_tabel <- function(id, filters) {
  plaan <- eurostat_plaan()[[id]]
  mahub <- !is.null(plaan) && setequal(names(plaan), names(filters)) &&
    all(vapply(names(plaan), function(n) all(filters[[n]] %in% plaan[[n]]), logical(1)))
  if (!mahub) return(get_eurostat(id, filters = filters, time_format = "raw", cache = FALSE))

  df <- get_eurostat(id, filters = plaan, time_format = "raw", cache = FALSE)
  for (n in names(plaan)) {
    if (n %in% names(df)) {
      df <- df[df[[n]] %in% filters[[n]], , drop = FALSE]
    } else if (length(plaan[[n]]) > 1) {
      # ilma selle veeruta ei saa seeriaid eristada
      stop(id, ": vastuses puudub veerg ", n, ".")
    }
  }
  df
}

# vali: lisavalik pärast allalaadimist, nt list(nace_r2 = "C10-S951_X_K"), kui
# dimensiooni ei saa päringusse panna või selle nimi võib erineda.
eurostat_seeria <- function(id, filters, vali = NULL) {
  df <- eurostat_tabel(id, filters)
  for (v in names(vali)) if (v %in% names(df)) df <- df[df[[v]] %in% vali[[v]], ]
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

# Fail kirjutatakse ainult siis, kui sisu muutus. Välja "uuendatud" (ajatempel)
# võrdlusel ei arvestata: muidu muutuks iga fail igal käivitusel ja GitHubi tekiks
# iga päev commit ka siis, kui ühtki uut arvu ei lisandunud. "uuendatud" tähendab
# seega "andmed viimati muutusid"; viimase kontrolli aeg on failis seis.json.
# Tagastab (nähtamatult) TRUE, kui fail kirjutati.
kirjuta_json <- function(x, fail) {
  dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
  tee <- file.path(DATA_DIR, fail)
  uus <- as.character(toJSON(x, auto_unbox = TRUE, digits = NA, na = "null", null = "null"))
  if (file.exists(tee)) {
    ilma_ajata <- function(s) charToRaw(enc2utf8(sub('"uuendatud":("[^"]*"|[-+0-9.eE]+)', '"uuendatud":0', s)))
    vana <- tryCatch(paste(readLines(tee, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) NULL)
    if (!is.null(vana) && identical(ilma_ajata(vana), ilma_ajata(uus))) {
      message("Muutusteta: ", tee)
      return(invisible(FALSE))
    }
  }
  tmp <- file.path(DATA_DIR, paste0(".", fail, ".tmp"))
  writeLines(uus, tmp, useBytes = TRUE)
  file.rename(tmp, tee)
  message("Kirjutatud: ", tee)
  invisible(TRUE)
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
                                  icu_bar03 = NULL, fail = "majandusplaan.json") {
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
    esi_hinnatud = esi_hinnatud,
    # Kvartalid, mille tootmisvõimsuste tase on Statistikaameti tabelist BAR03
    icu_bar03    = I(if (is.null(icu_bar03)) character() else format(as.Date(icu_bar03$time), "%Y-%m-%d"))
  ), fail)
  invisible(TRUE)
}
