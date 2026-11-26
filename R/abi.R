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

# Ajapiirang andmete laadimisele. GitHub katkestab töö 45 minuti pärast ja siis ei
# salvestata midagi. Kui mõni allikas on aeglane (24.11.2026 vastas Eurostat iga päringu
# peale pool minutit hiljem ja R-i pakettide paigaldus venis), jäetakse pärast tähtaega
# ülejäänud päringud vahele: nende seeriate eelmised väärtused jäävad alles ja jõutakse
# salvestada. Tähtaeg on UUENDUSE_TAHTAEG_MIN minutit GitHubi töö algusest (TOO_ALGUS,
# sekundid), käsitsi käivitamisel R-i käivitumisest. Tavaline jooks võtab ~5 minutit.
UUENDUSE_ALGUS <- local({
  t <- suppressWarnings(as.numeric(Sys.getenv("TOO_ALGUS")))
  if (is.na(t)) Sys.time() else as.POSIXct(t, origin = "1970-01-01")
})
UUENDUSE_TAHTAEG_MIN <- as.numeric(Sys.getenv("UUENDUSE_TAHTAEG_MIN", "20"))
.aeg_teatatud <- FALSE
aja_kontroll <- function(mis) {
  kulunud <- as.numeric(difftime(Sys.time(), UUENDUSE_ALGUS, units = "mins"))
  if (kulunud < UUENDUSE_TAHTAEG_MIN) return(invisible())
  if (!.aeg_teatatud) {
    .aeg_teatatud <<- TRUE
    hoiata("Uuendus on kestnud üle ", UUENDUSE_TAHTAEG_MIN, " minuti (mõni allikas vastab aeglaselt): ",
           "ülejäänud päringud jäetakse vahele, nende näitajate eelmised väärtused jäävad alles.")
  }
  stop(mis, ": ajapiirang täis, päring jäeti vahele")
}

# Kui Statistikaamet ei vasta, kulub iga päringu kolmele katsele ~5 minutit ja kümnete
# seeriate puhul jõuab töö ajapiiranguni enne, kui ülejäänud andmed (Eurostat, IMF jm)
# kirja saavad. Seepärast loetakse järjestikuseid ebaõnnestunud päringuid: pärast
# STATEE_PIIR-i jäetakse ülejäänud Statistikaameti päringud vahele (nende seeriate
# eelmised väärtused jäävad alles) ja ühe ebaõnnestumise järel proovitakse vaid korra.
# Õnnestunud päring nullib loenduri. HTTP 4xx (vigane päring) ei loe – server vastas.
STATEE_PIIR <- 3
statee_vigu <- 0
statee_kontroll <- function(tabel) {
  if (statee_vigu >= STATEE_PIIR)
    stop(tabel, ": Statistikaamet ei vastanud ", STATEE_PIIR, " järjestikusele päringule, päring jäeti vahele")
}
statee_tulemus <- function(ok) {
  if (ok) { statee_vigu <<- 0; return(invisible()) }
  statee_vigu <<- statee_vigu + 1
  if (statee_vigu == STATEE_PIIR)
    hoiata("Statistikaamet ei vasta: ülejäänud Statistikaameti päringud jäetakse selles jooksus vahele, ",
           "nende näitajate eelmised väärtused jäävad alles.")
}

# Tagastab data.frame'i: üks veerg iga dimensiooni koodiga + veerg value.
px_paring <- function(tabel, ..., katseid = 3, api = PX_API) {
  aja_kontroll(tabel)
  statee_kontroll(tabel)
  if (statee_vigu > 0) katseid <- 1
  keha <- list(query = list(...), response = list(format = "json"))
  vastus <- NULL
  for (k in seq_len(katseid)) {
    vastus <- tryCatch(
      POST(paste0(api, tabel), body = toJSON(keha, auto_unbox = TRUE),
           content_type_json(), encode = "raw", timeout(90)),
      error = function(e) e
    )
    if (!inherits(vastus, "error") && status_code(vastus) == 200) break
    if (k < katseid) Sys.sleep(5 * k)
  }
  statee_tulemus(!inherits(vastus, "error") && status_code(vastus) < 500)
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

# ---- Maailmapank (Data360 API): WGI ja B-READY kohad ---------------------------
# Kummalgi koondkohta ei ole, seepärast näidatakse kaardil Eesti kohta iga mõõtme
# (WGI) või samba (B-READY) järgi. Maailmapank avaldab skoorid; koht arvutatakse
# siin kõigi riikide skooride järjestusest (1 = kõrgeim skoor).
#
# WGI: andmebaas WB_WGI, näitajad GOV_WGI_VA, _PV, _GE, _RQ, _RL, _CC; mõõdik on väljal
#   COMP_BREAKDOWN_1 (WGI_SC = 0–100 skoor; lisaks hinnang, standardviga, allikate arv
#   ja usalduspiirid, mida ei kasutata).
# B-READY: andmebaas WB_WDI, näitajad WB_WDI_IC_BRE_P1_RF, _P2_PS, _P3_OE (kolm sammast,
#   skoor 0–100); TIME_PERIOD on väljaande aasta.
WGI_MOOTMED <- c(VA = "Kodanike hääl ja avalik vastutus", PV = "Poliitiline stabiilsus",
                 GE = "Valitsuse tulemuslikkus", RQ = "Regulatsioonide kvaliteet",
                 RL = "Õigusriik", CC = "Korruptsiooni ohjamine")
BREADY_SAMBAD <- c(WB_WDI_IC_BRE_P1_RF = "Õigusraamistik", WB_WDI_IC_BRE_P2_PS = "Avalikud teenused",
                   WB_WDI_IC_BRE_P3_OE = "Tegevuse tõhusus")

http_json <- function(url, query = list(), katseid = 3) {
  aja_kontroll(sub("\\?.*$", "", url))
  vastus <- NULL
  for (k in seq_len(katseid)) {
    vastus <- tryCatch(GET(url, query = query, timeout(60)), error = function(e) e)
    if (!inherits(vastus, "error") && status_code(vastus) == 200) break
    if (k < katseid) Sys.sleep(5 * k)
  }
  if (inherits(vastus, "error")) stop(conditionMessage(vastus))
  stop_for_status(vastus, task = paste("laadida", url))
  fromJSON(content(vastus, as = "text", encoding = "UTF-8"))
}

# Data360 päring (API annab kuni 1000 rida korraga) -> data.frame(naitaja, riik, moot, aasta, v)
wb_data360 <- function(...) {
  osad <- list(); skip <- 0
  repeat {
    v <- http_json("https://data360api.worldbank.org/data360/data", c(list(...), list(skip = skip)))
    d <- v$value
    if (!is.data.frame(d) || !nrow(d)) break
    names(d) <- toupper(names(d))
    puudu <- setdiff(c("INDICATOR", "REF_AREA", "TIME_PERIOD", "OBS_VALUE"), names(d))
    if (length(puudu)) stop("Data360 vastuses puuduvad väljad ", paste(puudu, collapse = ", "), ".")
    osad[[length(osad) + 1]] <- data.frame(
      naitaja = as.character(d$INDICATOR), riik = as.character(d$REF_AREA),
      moot = if ("COMP_BREAKDOWN_1" %in% names(d)) as.character(d$COMP_BREAKDOWN_1) else NA_character_,
      aasta = suppressWarnings(as.integer(substr(d$TIME_PERIOD, 1, 4))),
      v = suppressWarnings(as.numeric(d$OBS_VALUE)), stringsAsFactors = FALSE)
    skip <- skip + nrow(d)
    if (is.null(v$count) || skip >= v$count || length(osad) >= 30) break
  }
  if (!length(osad)) stop("Data360 ei tagastanud andmeid (", paste(names(list(...)), unlist(list(...)), sep = "=", collapse = ", "), ").")
  d <- do.call(rbind, osad)
  d[!is.na(d$v) & !is.na(d$aasta), ]
}

# Eesti koht riikide seas ühe näitaja ja aasta järgi: list(koht, riike) või NULL, kui Eestit pole
wb_koht <- function(d, riik = "EST") {
  d <- d[!duplicated(d$riik), ]
  oma <- d$v[d$riik == riik]
  if (!length(oma)) return(NULL)
  list(koht = sum(d$v > oma) + 1L, riike = nrow(d))
}

wgi_eesti <- function() {
  ee <- wb_data360(DATABASE_ID = "WB_WGI", REF_AREA = "EST", COMP_BREAKDOWN_1 = "WGI_SC",
                   timePeriodFrom = as.integer(format(Sys.Date(), "%Y")) - 6L)
  ee <- ee[ee$moot %in% "WGI_SC", ]
  koodid <- paste0("GOV_WGI_", names(WGI_MOOTMED))
  # viimane aasta, mille kohta on Eestil kõik kuus mõõdet
  aasta <- Find(function(a) all(koodid %in% ee$naitaja[ee$aasta == a]), sort(unique(ee$aasta), decreasing = TRUE))
  if (is.null(aasta)) stop("WGI: Eesti kohta ei leitud aastat, kus on kõik kuus mõõdet.")
  koik <- wb_data360(DATABASE_ID = "WB_WGI", COMP_BREAKDOWN_1 = "WGI_SC", timePeriodFrom = aasta - 1L, timePeriodTo = aasta)
  koik <- koik[koik$moot %in% "WGI_SC", ]
  koht <- function(kood, a) wb_koht(koik[koik$naitaja == kood & koik$aasta == a, ])
  praegu <- lapply(koodid, koht, a = aasta)
  if (any(vapply(praegu, is.null, logical(1))) || min(vapply(praegu, function(x) x$riike, numeric(1))) < 100)
    stop("WGI: kõigi riikide skoore ei õnnestunud laadida (aasta ", aasta, ").")
  enne <- lapply(koodid, koht, a = aasta - 1L)
  on_eelmine <- !any(vapply(enne, is.null, logical(1)))
  w <- list(aasta = aasta, eelmine_aasta = if (on_eelmine) aasta - 1L else NULL,
            riike = max(vapply(praegu, function(x) x$riike, numeric(1))),
            sambad = lapply(seq_along(koodid), function(i) list(WGI_MOOTMED[[i]], praegu[[i]]$koht, if (on_eelmine) enne[[i]]$koht else NULL)))
  message("WGI: aasta ", w$aasta, ", ", w$riike, " riiki: ",
          paste(vapply(w$sambad, function(x) paste0(x[[1]], " ", x[[2]], ".", if (!is.null(x[[3]])) paste0(" (", w$eelmine_aasta, ": ", x[[3]], ".)") else ""), character(1)), collapse = "; "))
  w
}

bready_eesti <- function() {
  read <- lapply(names(BREADY_SAMBAD), function(kood) wb_data360(DATABASE_ID = "WB_WDI", INDICATOR = kood))
  # viimane väljaanne, kus Eesti on kõigis kolmes sambas
  aastad <- Reduce(intersect, lapply(read, function(d) d$aasta[d$riik == "EST"]))
  if (!length(aastad)) stop("B-READY: Eesti andmeid ei leitud.")
  aasta <- max(aastad)
  kohad <- lapply(read, function(d) wb_koht(d[d$aasta == aasta, ]))
  if (min(vapply(kohad, function(x) x$riike, numeric(1))) < 30) stop("B-READY: kõigi riikide skoore ei õnnestunud laadida (aasta ", aasta, ").")
  w <- list(aasta = aasta, eelmine_aasta = NULL, riike = max(vapply(kohad, function(x) x$riike, numeric(1))),
            sambad = lapply(seq_along(kohad), function(i) list(BREADY_SAMBAD[[i]], kohad[[i]]$koht, NULL)))
  message("B-READY: väljaanne ", w$aasta, ", ", w$riike, " riiki: ",
          paste(vapply(w$sambad, function(x) paste0(x[[1]], " ", x[[2]], "."), character(1)), collapse = "; "))
  w
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

# ---- Elering: elektri börsihind ----------------------------------------------
# Nord Pooli järgmise päeva turu hinnad (€/MWh) Eleringi avaliku API kaudu: Eesti,
# Läti, Leedu ja Soome hinnapiirkond. Rootsi hindu Elering ei avalda.
# Alates 1.10.2025 on hinnad veerandtunni kaupa, varem tunni kaupa: mõlemad
# keskmistatakse kõigepealt tunniks, et kuu keskmine oleks ajaga kaalutud.
# Päevad ja kuud on Eesti aja järgi. Laaditakse aasta kaupa; tulemus jäetakse
# meelde, sest seda kasutavad nii naitajad.json kui ka vordlus.json.
ELERING_HIND <- "https://dashboard.elering.ee/api/nps/price"
ELERING_ALAD <- c(ee = "EE", lv = "LV", lt = "LT", fi = "FI")
.elering_malu <- new.env(parent = emptyenv())

elering_tunnid <- function() {
  if (!is.null(.elering_malu$tunnid)) return(.elering_malu$tunnid)
  tz <- "Europe/Tallinn"
  utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")
  aastad <- ALGUS:as.integer(format(Sys.Date(), "%Y"))
  osad <- lapply(aastad, function(a) {
    algus <- as.POSIXct(sprintf("%d-01-01 00:00:00", a), tz = tz)
    lopp <- min(as.POSIXct(sprintf("%d-01-01 00:00:00", a + 1), tz = tz), Sys.time() + 3 * 86400) - 1
    v <- http_json(ELERING_HIND, list(start = utc(algus), end = utc(lopp)))
    if (!isTRUE(v$success)) stop("Elering ei tagastanud andmeid aastaks ", a)
    do.call(rbind, lapply(names(ELERING_ALAD), function(k) {
      d <- v$data[[k]]
      if (!is.data.frame(d) || !nrow(d)) return(NULL)
      data.frame(ala = ELERING_ALAD[[k]], ts = as.numeric(d$timestamp), hind = as.numeric(d$price))
    }))
  })
  d <- do.call(rbind, osad)
  if (is.null(d) || !nrow(d)) stop("Eleringi hinnaandmed puuduvad")
  d <- d[!is.na(d$hind) & !duplicated(d[, c("ala", "ts")]), ]
  # tunni keskmine (UTC tund, et suveaja üleminek ei liidaks kaht tundi kokku)
  d$tund <- floor(d$ts / 3600) * 3600
  t <- stats::aggregate(hind ~ ala + tund, data = d, FUN = mean)
  aeg <- as.POSIXct(t$tund, origin = "1970-01-01", tz = "UTC")
  t$paev <- format(aeg, "%Y-%m-%d", tz = tz)
  t$kuu <- format(aeg, "%YM%m", tz = tz)
  message("Elering: ", nrow(d), " hinda, ", nrow(t), " tundi, ", min(t$paev), " … ", max(t$paev))
  .elering_malu$tunnid <- t
  t
}

# Täiskuude keskmised hinnad riigiti: list(EE = seeria, LV = …). Jooksev kuu jääb välja.
elering_kuud <- function() {
  t <- elering_tunnid()
  jooksev <- format(Sys.time(), "%YM%m", tz = "Europe/Tallinn")
  t <- t[t$kuu < jooksev, ]
  k <- stats::aggregate(hind ~ ala + kuu, data = t, FUN = mean)
  n <- stats::aggregate(hind ~ ala + kuu, data = t, FUN = length)
  k <- k[n$hind >= 24 * 27, ]   # kuu peab olema peaaegu täielik
  viim <- k[k$kuu == max(k$kuu), ]
  message("Elering, kuu keskmine ", viim$kuu[1], ": ", paste(sprintf("%s %.1f", viim$ala, viim$hind), collapse = ", "), " €/MWh")
  stats::setNames(lapply(unname(ELERING_ALAD), function(g) {
    x <- k[k$ala == g, ]
    if (!nrow(x)) NULL else seeria(x$kuu, round(x$hind, 2))
  }), unname(ELERING_ALAD))
}

# Eesti viimaste päevade keskmine hind (koos homsega, kui see on juba teada). Avalehe
# mõõdik võrdleb tänast hinda viimase 365 päevaga, seega hoitakse veidi üle aasta.
elering_paevad <- function(paevi = 400) {
  t <- elering_tunnid()
  t <- t[t$ala == "EE", ]
  p <- stats::aggregate(hind ~ paev, data = t, FUN = mean)
  n <- stats::aggregate(hind ~ paev, data = t, FUN = length)
  p <- p[n$hind >= 23, ]
  p <- utils::tail(p[order(p$paev), ], paevi)
  message("Elering, Eesti päevahinnad: ", paste(sprintf("%s %.1f", utils::tail(p$paev, 3), utils::tail(p$hind, 3)), collapse = ", "))
  list(paevad = p$paev, ee = round(p$hind, 2))
}

# ---- IMF PortWatch: sadamate laevakülastused ja hinnanguline kaubamaht --------
# Laevade satelliitpositsioonidel (AIS) põhinevad päevased hinnangud iga sadama kohta.
# Kaubamaht (import, eksport) on tonnides ja esialgne. Allikas palub viidata:
# "UN Global Platform; IMF PortWatch (portwatch.imf.org)". Päringud tehakse serveris
# koondatuna (outStatistics), nii et ridu tuleb sadu, mitte sadu tuhandeid.
PW_URL <- "https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services/Daily_Ports_Data/FeatureServer/0"
PW_RIIGID <- c(EE = "EST", LV = "LVA", LT = "LTU", FI = "FIN", SE = "SWE")
PW_ALGUS <- 2019   # PortWatchi andmed algavad 2019. aastast
.pw_malu <- new.env(parent = emptyenv())
# PortWatchi päringutele on oma ajaeelarve, et aeglane või hätta jäänud ArcGIS ei sööks ära
# kogu uuenduse aega (siis jääksid vahele ka Eurostati jm päringud).
PW_AJAEELARVE_MIN <- as.numeric(Sys.getenv("PORTWATCH_AJAEELARVE_MIN", "6"))
pw_aja_kontroll <- function() {
  if (is.null(.pw_malu$algus)) .pw_malu$algus <- Sys.time()
  if (as.numeric(difftime(Sys.time(), .pw_malu$algus, units = "mins")) > PW_AJAEELARVE_MIN)
    stop("PortWatchi ajaeelarve (", PW_AJAEELARVE_MIN, " min) täis, ülejäänud päringud jäeti vahele")
}

# PortWatch uueneb kord nädalas (teisipäeval, hoolduse ajal on teenus maas), seega päritakse
# seda ainult kolmapäeviti või siis, kui viimasest õnnestunud uuendusest on möödas vähemalt
# 7 päeva (nt kolmapäevane päring ebaõnnestus). Muul ajal jääb alles eelmine versioon.
# Keskkonnamuutuja PORTWATCH_ALATI=1 sunnib uuendama.
PW_UUENDUSPAEV <- 3L   # ISO nädalapäev: 3 = kolmapäev
pw_uuenda <- function() {
  if (!is.null(.pw_malu$uuenda)) return(.pw_malu$uuenda)
  eelmine <- tryCatch(as.Date(loe_json("naitajad.json")$sadamad_uuendatud), error = function(e) NA)
  tana <- Sys.Date()
  otsus <- identical(Sys.getenv("PORTWATCH_ALATI"), "1") || length(eelmine) != 1 || is.na(eelmine) ||
    as.integer(format(tana, "%u")) == PW_UUENDUSPAEV || as.numeric(tana - eelmine) >= 7
  message("PortWatch: ", if (otsus) "uuendatakse" else paste0("jäetakse vahele (viimati uuendatud ", eelmine, ")"))
  .pw_malu$uuenda <- otsus
  otsus
}
# Kas tänane kuuandmete päring õnnestus (siis kirjutatakse uus uuenduskuupäev)
pw_onnestus <- function() !is.null(.pw_malu$kuud) && !inherits(.pw_malu$kuud, "error")

pw_koond <- function(where, grupp) {
  stat <- lapply(c(laevu = "portcalls", eksport = "export", import = "import"), function(v) v)
  out <- jsonlite::toJSON(lapply(names(stat), function(n)
    list(statisticType = "sum", onStatisticField = stat[[n]], outStatisticFieldName = n)), auto_unbox = TRUE)
  # ArcGIS annab ülekoormuse korral HTTP 200 koos veateatega, seega korratakse ka siin
  for (k in 1:2) {
    pw_aja_kontroll()
    v <- http_json(paste0(PW_URL, "/query"), list(where = where, outStatistics = out,
      groupByFieldsForStatistics = paste(grupp, collapse = ","), f = "json"), katseid = 2)
    if (is.null(v$error)) break
    if (k < 2) Sys.sleep(5)
  }
  if (!is.null(v$error)) {
    # Kihi kirjeldusest väljade nimed logisse, et skeemi muutumist oleks lihtne näha
    m <- tryCatch(http_json(PW_URL, list(f = "json"), katseid = 1), error = function(e) NULL)
    if (!is.null(m$fields)) message("PortWatchi väljad: ", paste0(m$fields$name, " (", sub("esriFieldType", "", m$fields$type), ")", collapse = ", "))
    stop("PortWatch: ", v$error$message, " ", paste(unlist(v$error$details), collapse = " "), " (", where, ")")
  }
  d <- v$features$attributes
  if (!is.data.frame(d) || !nrow(d)) stop("PortWatch ei tagastanud ridu (", where, ")")
  names(d) <- tolower(names(d))
  if (isTRUE(v$exceededTransferLimit)) stop("PortWatch: vastus jäi poolikuks (", where, ")")
  d
}

# Eesti päevad viimase ~400 päeva kohta + kõige uuem päev andmetes
pw_eesti_paevad <- function() {
  if (!is.null(.pw_malu$paevad)) return(.pw_malu$paevad)
  y <- as.integer(format(Sys.Date(), "%Y")) - 1
  d <- pw_koond(sprintf("ISO3 = 'EST' AND year >= %d", y), c("year", "month", "day"))
  d$kp <- as.Date(sprintf("%04d-%02d-%02d", as.integer(d$year), as.integer(d$month), as.integer(d$day)))
  d <- d[order(d$kp), ]
  message("PortWatch: Eesti päevi ", nrow(d), ", viimane ", max(d$kp))
  .pw_malu$paevad <- d
  d
}

# Kuusummad riigiti: list(EE = list(laevu = seeria, tonnid = seeria, eksport = …, import = …), …).
# Jooksev kuu jääb välja, kuni andmed katavad selle viimase päeva.
pw_kuud <- function() {
  if (inherits(.pw_malu$kuud, "error")) stop(.pw_malu$kuud)   # sama viga ei korrata iga seeria jaoks
  if (!is.null(.pw_malu$kuud)) return(.pw_malu$kuud)
  tryCatch(pw_kuud_lae(), error = function(e) { .pw_malu$kuud <- e; stop(e) })
}
# Eelmise uuenduse kuuseeriad riigiti (vordlus.json: laevad ja tonnid; naitajad.json: Eesti eksport ja import)
pw_vanad <- function() {
  n <- loe_json("naitajad.json")$seeriad
  v <- loe_json("vordlus.json")$seeriad
  lapply(setNames(names(PW_RIIGID), names(PW_RIIGID)), function(g) list(
    laevu = v[[paste0("sadam_laevu_m_", g)]], tonnid = v[[paste0("sadam_t_m_", g)]],
    eksport = if (g == "EE") n$sadam_eksport_m, import = if (g == "EE") n$sadam_import_m))
}
pw_kuud_lae <- function() {
  viimane <- max(pw_eesti_paevad()$kp)
  # Üks päring riigi ja aasta kohta: kõiki riike ja aastaid korraga koondades jääb server hätta.
  # Varasemad aastad võetakse eelmisest failist; uuesti päritakse ainult eelmine ja jooksev
  # aasta (PortWatch võib viimaseid kuid täpsustada). Kui eelmist seeriat pole, päritakse kõik.
  y <- as.integer(format(viimane, "%Y"))
  vana <- pw_vanad()
  aastad_riigil <- function(g) {
    v <- vana[[g]]
    vaja <- c("laevu", "tonnid", if (g == "EE") c("eksport", "import"))
    taielik <- all(vapply(vaja, function(k) length(v[[k]]$periods) >= 24, logical(1)))
    if (taielik) (y - 1):y else PW_ALGUS:y
  }
  # Päevade kaupa (nagu Eesti päevapäring), kuud liidetakse siin. Kui aasta korraga ei
  # õnnestu (suurte riikide puhul jääb server mõnikord hätta), küsitakse kuu kaupa.
  pw_aasta <- function(iso, a) {
    paevad <- function(w) pw_koond(w, c("year", "month", "day"))
    x <- tryCatch(paevad(sprintf("ISO3 = '%s' AND year = %d", iso, a)), error = function(e) NULL)
    if (is.null(x)) x <- do.call(rbind, lapply(1:12, function(k) tryCatch(
      paevad(sprintf("ISO3 = '%s' AND year = %d AND month = %d", iso, a, k)),
      error = function(e) if (grepl("ei tagastanud ridu", conditionMessage(e))) NULL else stop(e))))
    x <- stats::aggregate(cbind(laevu, eksport, import) ~ year + month, data = x, FUN = sum)
    x$iso3 <- iso
    x
  }
  # Ühe riigi rike ei too kaasa teiste (ega Eesti) andmete kadu
  # Viimased aastad ühe päringuga (year >= …): see kuju töötab ka siis, kui aasta kaupa
  # (year = …) päringud ArcGIS-is ebaõnnestuvad. Eesti päevad on juba olemas.
  pw_viimased <- function(iso, a0) {
    x <- if (iso == "EST") pw_eesti_paevad() else
      pw_koond(sprintf("ISO3 = '%s' AND year >= %d", iso, a0), c("year", "month", "day"))
    x <- x[as.integer(x$year) >= a0, ]
    x <- stats::aggregate(cbind(laevu, eksport, import) ~ year + month, data = x, FUN = sum)
    x$iso3 <- iso
    x
  }
  d <- do.call(rbind, lapply(names(PW_RIIGID), function(g) tryCatch({
    a <- aastad_riigil(g)
    if (length(a) == 2) pw_viimased(PW_RIIGID[[g]], a[1]) else
      do.call(rbind, lapply(a, function(k) pw_aasta(PW_RIIGID[[g]], k)))
  },
    error = function(e) { hoiata("PortWatch ", PW_RIIGID[[g]], ": ", conditionMessage(e)); NULL })))
  if (is.null(d) || !nrow(d)) stop("PortWatchist ei saanud ühegi riigi kuuandmeid")
  kuu_lopp <- as.Date(sprintf("%04d-%02d-01", as.integer(d$year), as.integer(d$month))) + 31
  kuu_lopp <- as.Date(format(kuu_lopp, "%Y-%m-01")) - 1
  d <- d[kuu_lopp <= viimane, ]
  d$per <- sprintf("%04dM%02d", as.integer(d$year), as.integer(d$month))
  d <- d[order(d$per), ]
  # Uued kuud + eelmise faili kuud enne esimest uuesti päritud kuud
  liida <- function(v, u) {
    if (is.null(v) || !length(v$periods) || !length(u$periods)) return(u)
    hoia <- v$periods < u$periods[1]
    seeria(c(v$periods[hoia], u$periods), c(v$values[hoia], u$values))
  }
  tul <- lapply(setNames(names(PW_RIIGID), names(PW_RIIGID)), function(g) {
    x <- d[toupper(d$iso3) == PW_RIIGID[[g]], ]
    if (!nrow(x)) return(NULL)
    u <- list(laevu = seeria(x$per, x$laevu),
              eksport = seeria(x$per, round(x$eksport / 1000, 1)),
              import = seeria(x$per, round(x$import / 1000, 1)),
              tonnid = seeria(x$per, round((x$eksport + x$import) / 1000, 1)))
    lapply(setNames(names(u), names(u)), function(k) liida(vana[[g]][[k]], u[[k]]))
  })
  ee <- tul$EE
  if (!is.null(ee)) message("PortWatch, Eesti ", utils::tail(ee$laevu$periods, 1), ": laevu ",
    utils::tail(ee$laevu$values, 1), ", eksport ", utils::tail(ee$eksport$values, 1), " tuh t, import ",
    utils::tail(ee$import$values, 1), " tuh t")
  .pw_malu$kuud <- tul
  tul
}

# Avalehe kiire näitaja: viimase 28 päeva summa vs sama periood 364 päeva varem (samad nädalapäevad)
pw_kiire <- function(paevi = 28) {
  d <- pw_eesti_paevad()
  lopp <- max(d$kp)
  aken <- function(l) { x <- d[d$kp > l - paevi & d$kp <= l, ]; if (nrow(x) < paevi - 2) NULL else x }
  n <- aken(lopp); v <- aken(lopp - 364)
  if (is.null(n) || is.null(v)) stop("PortWatch: kiire näitaja jaoks pole piisavalt päevi")
  s <- function(x, k) round(sum(x[[k]]) / 1000, 1)
  list(algus = format(lopp - paevi + 1), lopp = format(lopp), paevi = paevi,
       eksport = s(n, "eksport"), eksport_varem = s(v, "eksport"),
       import = s(n, "import"), import_varem = s(v, "import"),
       laevu = sum(n$laevu), laevu_varem = sum(v$laevu))
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

# Eurostati katkesti, sama põhimõttega nagu Statistikaameti oma (vt statee_kontroll):
# kui Eurostat ei vasta, kulub iga andmestiku kolmele katsele mitu minutit ja töö jõuab
# ajapiiranguni enne, kui midagi salvestatakse (nii juhtus 24.11.2026). Pärast esimest
# ebaõnnestunud andmestikku proovitakse iga järgmist vaid korra ja pärast EUROSTAT_PIIR
# järjestikust ebaõnnestumist jäetakse ülejäänud Eurostati päringud vahele (nende seeriate
# eelmised väärtused jäävad alles). Õnnestunud päring nullib loenduri.
EUROSTAT_PIIR <- 4
eurostat_vigu <- 0
eurostat_kontroll <- function(id) {
  if (eurostat_vigu >= EUROSTAT_PIIR)
    stop(id, ": Eurostat ei vastanud ", EUROSTAT_PIIR, " järjestikusele päringule, päring jäeti vahele")
}
eurostat_tulemus <- function(ok) {
  if (ok) { eurostat_vigu <<- 0; return(invisible()) }
  eurostat_vigu <<- eurostat_vigu + 1
  if (eurostat_vigu == EUROSTAT_PIIR)
    hoiata("Eurostat ei vasta: ülejäänud Eurostati päringud jäetakse selles jooksus vahele, ",
           "nende näitajate eelmised väärtused jäävad alles.")
}

get_eurostat <- function(id, ..., katseid = 3, paus = 5, .lae = eurostat::get_eurostat) {
  voti <- paste(deparse(list(id, ...), width.cutoff = 500L), collapse = "")
  meeles <- .eurostat_malu[[voti]]
  if (inherits(meeles, "condition")) stop(meeles)
  if (!is.null(meeles)) return(meeles)
  aja_kontroll(id)
  eurostat_kontroll(id)
  if (eurostat_vigu > 0) katseid <- 1

  viga <- NULL
  for (k in seq_len(katseid)) {
    df <- tryCatch(.lae(id, ...), error = function(e) e)
    if (is.data.frame(df)) {
      eurostat_tulemus(TRUE)
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
  eurostat_tulemus(FALSE)
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

# ---- Eurostati Comext (väliskaubandus kaubakoodide kaupa) ---------------------
COMEXT_API <- "https://ec.europa.eu/eurostat/api/comext/dissemination/statistics/1.0/data/"
# JSON-stat 2.0 vastus -> data.frame (iga dimensiooni kood + value)
jsonstat_df <- function(js) {
  ids <- unlist(js$id); suurus <- unlist(js$size)
  koodid <- lapply(ids, function(d) { ix <- unlist(js$dimension[[d]]$category$index); names(sort(ix)) })
  names(koodid) <- ids
  v <- js$value
  if (is.null(v) || !length(v)) return(NULL)
  if (is.list(v) && !is.null(names(v))) { pos <- as.integer(names(v)); x <- as.numeric(unlist(v)) }
  else { x <- as.numeric(unlist(v)); pos <- seq_along(x) - 1L; hoia <- !is.na(x); pos <- pos[hoia]; x <- x[hoia] }
  # lame indeks -> dimensioonide indeksid (viimane dimensioon muutub kõige kiiremini)
  df <- data.frame(value = x)
  jaak <- pos
  for (i in rev(seq_along(ids))) {
    df[[ids[i]]] <- koodid[[i]][jaak %% suurus[i] + 1L]
    jaak <- jaak %/% suurus[i]
  }
  df
}
# Comexti päring: väärtused eurodes kuude kaupa, kaubakood x kaubavoog (1 = import, 2 = eksport)
comext_kuud <- function(tooted, reporter = "EE", partner = "WORLD", andmestik = "DS-045409") {
  aja_kontroll(andmestik)
  q <- c(list(format = "JSON", lang = "EN", freq = "M", partner = partner, indicators = "VALUE_IN_EUROS"),
         setNames(as.list(reporter), rep("reporter", length(reporter))),
         setNames(as.list(tooted), rep("product", length(tooted))),
         list(flow = "1", flow = "2"))
  vastus <- NULL
  for (k in 1:3) {
    vastus <- tryCatch(GET(paste0(COMEXT_API, andmestik), query = q, timeout(120)), error = function(e) e)
    if (!inherits(vastus, "error") && status_code(vastus) == 200) break
    if (k < 3) Sys.sleep(10 * k)
  }
  if (inherits(vastus, "error")) stop(andmestik, ": ", conditionMessage(vastus))
  if (status_code(vastus) != 200) stop(andmestik, ": HTTP ", status_code(vastus), " ",
    substr(content(vastus, as = "text", encoding = "UTF-8"), 1, 300))
  df <- jsonstat_df(fromJSON(content(vastus, as = "text", encoding = "UTF-8"), simplifyVector = FALSE))
  if (is.null(df) || !nrow(df)) stop(andmestik, ": päring ei tagastanud andmeid.")
  df$per <- norm_periood(df$time)
  df
}

# Automatiseerimise indikaator: automatiseerimisseadmete import ja eksport (Acemoglu–Restrepo
# põhiloend, HS 2012). Vahemikud, mis katavad terve rubriigi, päritakse HS4 tasemel.
# 3D-printerid on HS 2022-s rubriigis 8485 (enne 2022 oli 8485 muude masinaosade rubriik,
# seepärast arvestatakse 8485 ainult alates 2022). Tulemus: kuude seeriad, mln eurot.
AUTOMATISEERIMINE_HS <- c(
  "847950",                                         # tööstusrobotid
  "847989",                                         # spetsiaalmasinad
  sprintf("%d", 8456:8466), "846820", "846880", "846890", "851511", "851519",  # tööpingid
  "851521", "851531", "851580", "851590",           # automaatsed keevitusmasinad
  "8446", "8447",                                   # kudumis- ja kootmismasinad
  "8444", "8445",                                   # muud tekstiilitööstuse spetsiaalmasinad
  "842831", "842832", "842833", "842839",           # automaatsed konveierid
  "9032",                                           # automaatsed reguleerimisseadmed
  "847780", "848520", "848530", "848580", "848590"  # 3D-printerid (HS 2012 ja HS 2022)
)
# Riigid, mille kohta Comextist päritakse (sama mis võrdluslehel; vordlus.R laaditakse hiljem)
AUTOMATISEERIMINE_RIIGID <- c("EE", "LV", "LT", "FI", "SE")
# Kategooriad lehe jaotusgraafiku jaoks (kaubakoodi järgi)
AUTOMATISEERIMINE_GRUPID <- c(robotid = "Tööstusrobotid", toopingid = "Tööpingid ja keevitusmasinad",
  reguleerimine = "Reguleerimisseadmed", konveierid = "Konveierid", tekstiil = "Tekstiilimasinad",
  muud = "Spetsiaalmasinad ja 3D-printerid")
automatiseerimise_grupp <- function(kood) {
  h4 <- substr(kood, 1, 4)
  ifelse(kood == "847950", "robotid",
  ifelse(h4 %in% c(sprintf("%d", 8456:8466), "8468", "8515"), "toopingid",
  ifelse(h4 == "9032", "reguleerimine",
  ifelse(h4 == "8428", "konveierid",
  ifelse(h4 %in% c("8444", "8445", "8446", "8447"), "tekstiil", "muud")))))
}
# Tulemus: list(import, eksport = Eesti kuud; grupid = Eesti netoimport kategooriate kaupa;
# neto = netoimport riigiti). Kõik mln eurot kuus.
automatiseerimise_kaubandus <- local({
  malu <- NULL
  function() {
    if (!is.null(malu)) { if (inherits(malu, "error")) stop(malu); return(malu) }
    malu <<- tryCatch({
      df <- comext_kuud(AUTOMATISEERIMINE_HS, reporter = AUTOMATISEERIMINE_RIIGID)
      df <- df[!(substr(df$product, 1, 4) == "8485" & as.integer(substr(df$per, 1, 4)) < 2022), ]
      puudu <- setdiff(AUTOMATISEERIMINE_HS, unique(df$product[df$reporter == "EE"]))
      if (length(puudu)) message("Comext: Eesti andmeteta kaubakoodid: ", paste(puudu, collapse = ", "))
      # kuu x voog summa; puuduv voog kuul, mil teine on olemas, on 0
      summad <- function(x) {
        if (!nrow(x)) return(NULL)
        t <- xtabs(value ~ per + flow, data = x)
        v <- function(f) if (f %in% colnames(t)) as.numeric(t[, f]) else rep(0, nrow(t))
        list(per = rownames(t), import = v("1") / 1e6, eksport = v("2") / 1e6)
      }
      ee <- summad(df[df$reporter == "EE", ])
      if (is.null(ee)) stop("Comext: Eesti andmeid pole.")
      df$grupp <- automatiseerimise_grupp(df$product)
      grupid <- lapply(setNames(names(AUTOMATISEERIMINE_GRUPID), names(AUTOMATISEERIMINE_GRUPID)), function(g) {
        x <- summad(df[df$reporter == "EE" & df$grupp == g, ])
        # kõik Eesti kuud, ka need, kus selle kategooria kaubavahetust polnud (0)
        n <- if (is.null(x)) rep(NA_real_, length(ee$per)) else
          unname(setNames(round(x$import - x$eksport, 2), x$per)[ee$per])
        seeria(ee$per, ifelse(is.na(n), 0, n))
      })
      neto <- lapply(setNames(AUTOMATISEERIMINE_RIIGID, AUTOMATISEERIMINE_RIIGID), function(r) {
        x <- summad(df[df$reporter == r, ])
        if (is.null(x)) NULL else seeria(x$per, round(x$import - x$eksport, 2))
      })
      tul <- list(import = seeria(ee$per, round(ee$import, 2)), eksport = seeria(ee$per, round(ee$eksport, 2)),
                  grupid = grupid, neto = neto)
      message("Automatiseerimisseadmed ", utils::tail(tul$import$periods, 1), ": import ",
              utils::tail(tul$import$values, 1), " mln €, eksport ", utils::tail(tul$eksport$values, 1), " mln €; riigid: ",
              paste(names(Filter(Negate(is.null), neto)), collapse = ", "))
      tul
    }, error = function(e) e)
    if (inherits(malu, "error")) stop(malu)
    malu
  }
})

# ---- Tootlikkus tegevusalati (Eurostat) --------------------------------------
# Lisandväärtus töötatud tunni kohta, indeks baas = 100. Mahu muutus tuleb aheldatud
# mahu aastamuutusest (nama_10_a10, CLV_PCH_PRE), mis ei sõltu Eurostati baasaastast;
# tunnid andmestikust nama_10_a10_e (THS_HW). Tulemus: nimega list seeriatest NACE
# koodi järgi (NULL, kui tegevusalal andmeid pole). Vahemälus koos veaga.
tootlikkus_tegevusalati <- local({
  vahemalu <- list()
  function(alad, geo = "EE", baas = "2015") {
    voti <- paste(geo, baas, paste(alad, collapse = ","))
    if (!is.null(vahemalu[[voti]])) {
      if (inherits(vahemalu[[voti]], "error")) stop(vahemalu[[voti]])
      return(vahemalu[[voti]])
    }
    tulemus <- tryCatch({
      lv <- eurostat_tabel("nama_10_a10", list(geo = geo, na_item = "B1G", unit = "CLV_PCH_PRE", nace_r2 = alad, freq = "A"))
      tu <- eurostat_tabel("nama_10_a10_e", list(geo = geo, na_item = "EMP_DC", unit = "THS_HW", nace_r2 = alad, freq = "A"))
      aeg <- function(df) norm_periood(as.character(df[[intersect(c("time", "TIME_PERIOD"), names(df))[1]]]))
      lv$per <- aeg(lv); tu$per <- aeg(tu)
      lv <- lv[!is.na(lv$values), ]; tu <- tu[!is.na(tu$values) & tu$values > 0, ]
      if (!nrow(lv) || !nrow(tu)) stop("nama_10_a10: päring ei tagastanud andmeid.")
      out <- lapply(alad, function(a) {
        g <- setNames(lv$values[lv$nace_r2 == a], lv$per[lv$nace_r2 == a])
        h <- setNames(tu$values[tu$nace_r2 == a], tu$per[tu$nace_r2 == a])
        if (length(h) < 3 || !length(g)) return(NULL)
        # järjestikused aastad esimesest tundide aastast, mille kohta on nii mahu muutus kui ka tunnid
        a0 <- min(as.integer(names(h))); a1 <- max(as.integer(names(h)))
        maht <- c(); eelmine <- NULL
        for (y in as.character(a0:a1)) {
          if (is.null(eelmine)) { maht[y] <- 100; eelmine <- 100; next }
          if (is.na(g[y]) || is.na(h[y])) break
          maht[y] <- eelmine * (1 + g[[y]] / 100); eelmine <- maht[[y]]
        }
        if (!(baas %in% names(maht)) || length(maht) < 3) return(NULL)
        tl <- maht / h[names(maht)]
        seeria(names(tl), unname(round(100 * tl / tl[[baas]], 1)))
      })
      names(out) <- alad
      out
    }, error = function(e) e)
    vahemalu[[voti]] <<- tulemus
    if (inherits(tulemus, "error")) stop(tulemus)
    tulemus
  }
})

# ---- AMECO (Euroopa Komisjon) DBnomicsi kaudu ---------------------------------
# AMECO riigikoodid. Komisjoni prognoosiaastaid ei võeta: ainult aastad enne jooksvat.
AMECO_RIIGID <- c(EE = "EST", LV = "LVA", LT = "LTU", FI = "FIN", SE = "SWE", EU27_2020 = "EU27")
ameco_seeriad <- local({
  vahemalu <- list()
  function(muutuja) {
    if (!is.null(vahemalu[[muutuja]])) {
      if (inherits(vahemalu[[muutuja]], "error")) stop(vahemalu[[muutuja]])
      return(vahemalu[[muutuja]])
    }
    tulemus <- tryCatch({
      v <- http_json(paste0("https://api.db.nomics.world/v22/series/AMECO/", muutuja),
                     list(observations = 1, limit = 100))
      d <- v$series$docs
      if (!is.data.frame(d) || !nrow(d)) stop("AMECO ", muutuja, ": DBnomics ei tagastanud seeriaid.")
      kood <- toupper(sub("\\..*$", "", d$series_code))
      viimane <- as.integer(format(Sys.Date(), "%Y")) - 1
      out <- lapply(AMECO_RIIGID, function(k) {
        i <- which(kood == k)[1]
        if (is.na(i)) return(NULL)
        p <- as.character(unlist(d$period[[i]])); x <- suppressWarnings(as.numeric(unlist(d$value[[i]])))
        hoia <- !is.na(x) & suppressWarnings(as.integer(substr(p, 1, 4))) <= viimane
        if (!any(hoia)) return(NULL)
        seeria(p[hoia], round(x[hoia], 1))
      })
      names(out) <- names(AMECO_RIIGID)
      if (is.null(out$EE)) stop("AMECO ", muutuja, ": Eesti seeriat ei leitud (koodid: ", paste(head(kood, 10), collapse = ", "), ").")
      out
    }, error = function(e) e)
    vahemalu[[muutuja]] <<- tulemus
    if (inherits(tulemus, "error")) stop(tulemus)
    tulemus
  }
})

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

# ---- Muudatuste logi ---------------------------------------------------------
# Võrdleb värskelt arvutatud näitajaid eelmise failiga ja kirjutab muudatused
# faili uuendused.json: mis seeria sai uue perioodi ja mille varasem väärtus
# parandati. Veebileht teeb neist kirjetest kasti "Viimati uuenenud".
# Seeriate nimesid siin ei teata – leht seob seeria võtme näitajaga ise.
MUUD_PAEVI <- 21    # kui vanu kirjeid alles hoitakse
MUUD_MAX   <- 300   # ülempiir, et fail ei kasvaks piiramatult
MUUD_VAATA <- 6     # mitut viimast perioodi paranduste osas vaadatakse
MUUD_UUSI  <- 15    # rohkem uusi seeriaid korraga = töölaua muudatus, mitte andmeuudis

# Kas erinevus on päris muudatus või ümardamismüra?
oluline_vahe <- function(uus, vana) {
  if (is.na(uus) || is.na(vana)) return(xor(is.na(uus), is.na(vana)))
  abs(uus - vana) > max(0.05, abs(vana) * 0.001)
}

logi_muudatused <- function(uued, vana_fail = "naitajad.json", logi_fail = "uuendused.json") {
  vana <- loe_json(vana_fail)
  if (is.null(vana) || is.null(vana$seeriad)) return(invisible(FALSE))  # esimene käivitus: võrrelda pole millegagi
  paev <- format(Sys.Date())
  kirjed <- list(); uued_seeriad <- list()
  for (k in names(uued$seeriad)) {
    u <- uued$seeriad[[k]]; v <- vana$seeriad[[k]]
    if (is.null(u) || !length(u$periods)) next
    up <- as.character(u$periods); uv <- as.numeric(u$values)
    # Failis uus seeria (uus näitaja või ümber nimetatud rida): kogu ajalugu ei logita,
    # ainult viimane periood, sest sealt tuleb tegelik uudis.
    if (is.null(v) || !length(v$periods)) {
      i <- utils::tail(which(!is.na(uv)), 1)
      if (length(i)) uued_seeriad[[length(uued_seeriad) + 1]] <-
        list(paev = paev, seeria = k, liik = "uus", periood = up[i], v = uv[i])
      next
    }
    vp <- as.character(v$periods); vv <- as.numeric(v$values)
    # uued perioodid: kuni kaks kõige värskemat
    for (p in utils::tail(sort(setdiff(up, vp)), 2)) {
      i <- match(p, up)
      if (!is.na(uv[i])) kirjed[[length(kirjed) + 1]] <-
        list(paev = paev, seeria = k, liik = "uus", periood = p, v = uv[i])
    }
    # parandused: ainult viimastes perioodides ja seeria kohta kõige värskem
    for (p in rev(intersect(utils::tail(vp, MUUD_VAATA), up))) {
      iu <- match(p, up); iv <- match(p, vp)
      if (!is.na(uv[iu]) && oluline_vahe(uv[iu], vv[iv])) {
        kirjed[[length(kirjed) + 1]] <- list(paev = paev, seeria = k, liik = "parandus",
                                             periood = p, v = uv[iu], vana = vv[iv])
        break
      }
    }
  }
  # Korraga palju uusi seeriaid tähendab töölaua enda muudatust, mitte uusi andmeid
  if (length(uued_seeriad) > MUUD_UUSI) {
    message("Uusi seeriaid ", length(uued_seeriad), " – logisse ei lisata")
  } else {
    kirjed <- c(kirjed, uued_seeriad)
  }
  tee <- file.path(DATA_DIR, logi_fail)
  olemas <- if (file.exists(tee)) tryCatch(fromJSON(tee, simplifyVector = FALSE)$kirjed, error = function(e) NULL) else NULL
  koik <- c(if (is.null(olemas)) list() else olemas, kirjed)
  # vanad kirjed välja, uuemad lõppu, ülempiir peale
  alates <- format(Sys.Date() - MUUD_PAEVI)
  koik <- Filter(function(r) !is.null(r$paev) && r$paev >= alates, koik)
  if (length(koik) > MUUD_MAX) koik <- utils::tail(koik, MUUD_MAX)
  if (length(kirjed)) message("Muudatusi: ", length(kirjed))
  kirjuta_json(list(uuendatud = aeg_utc(), kirjed = koik), logi_fail)
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
