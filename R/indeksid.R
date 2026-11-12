# =============================================================================
# Rahvusvahelised edetabelid: andmed/indeksid.csv + andmed/indeksid_vordlus.csv
# -> docs/data/indeksid.json
#
# Edetabelitel API-t ei ole. Uuenda CSV-faile käsitsi, kui uus väljaanne
# ilmub (vt veergu rutm).
# Erand: WGI kuue mõõtme ja B-READY kolme samba kohad (ning aasta ja silt) arvutatakse
# igal käivitusel Maailmapanga API andmetest; CSV-s on nende kohta tekstid ja lingid. Indeksite valik ja kirjeldused pärinevad MAPO
# to-do faili lehelt "2.1 Indeksid".
#
# indeksid.csv veerud:
#   id, prioriteet (1 = põhiindeks, 2 = täiendav), pealkiri, nimi_en, aasta,
#   koht, riike            Eesti koht ja riikide arv (tühi, kui edetabelit pole)
#   skoor, skaala          kui indeks avaldab ainult skoori (nt 98,5 / 100)
#   ruhm, ruhmad           Eesti tulemusrühm ja kõik rühmad (eraldaja |)
#   silt                   lühike lisainfo kaardil
#   eelmine_aasta, eelmine_koht, eelmine_skoor, eelmine_silt   eelmise väljaande tulemus
#   sambad                 alaskoorid "nimi:praegu:eelmine|..." (nt B-READY kolm sammast)
#   kirjeldus, koosseis, pikk, piirang, asutus, rutm   tekstid kaardi lisainfos
#   allikas, url, url2     allika nimi ja lingid
#   vordlus_aasta, vordlus_riike   mis väljaandest võrdlusriikide kohad on
# indeksid_vordlus.csv veerud: id, riik, koht, skoor
# =============================================================================

koosta_indeksid <- function() {
  loe <- function(f) read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE,
                              na.strings = "", check.names = FALSE, colClasses = "character")
  ix <- loe("andmed/indeksid.csv")
  vr <- loe("andmed/indeksid_vordlus.csv")

  vajalik <- c("id", "prioriteet", "pealkiri", "aasta", "koht", "riike", "skoor", "skaala",
               "ruhm", "ruhmad", "silt", "kirjeldus", "koosseis", "pikk", "piirang",
               "asutus", "rutm", "allikas", "url", "url2", "vordlus_aasta", "vordlus_riike")
  puudu <- setdiff(vajalik, names(ix))
  if (length(puudu)) stop("indeksid.csv: puuduvad veerud ", paste(puudu, collapse = ", "))
  if (anyDuplicated(ix$id)) stop("indeksid.csv: id peab olema unikaalne.")
  if (!all(c("id", "riik", "koht") %in% names(vr))) stop("indeksid_vordlus.csv: puuduvad veerud id, riik, koht")
  if (!"skoor" %in% names(vr)) vr$skoor <- NA

  nr  <- function(x) if (is.na(x) || !nzchar(x)) NULL else as.integer(x)
  num <- function(x) if (is.na(x) || !nzchar(x)) NULL else as.numeric(sub(",", ".", x))
  tx  <- function(x) if (is.na(x)) "" else trimws(x)

  indeksid <- lapply(seq_len(nrow(ix)), function(i) {
    r <- ix[i, ]
    v <- vr[vr$id == r$id, ]
    peers <- if (nrow(v)) lapply(seq_len(nrow(v)), function(j) list(v$riik[j], nr(v$koht[j]), num(v$skoor[j]))) else NULL
    list(
      id        = r$id,
      prio      = nr(r$prioriteet) %||% 1L,
      title     = tx(r$pealkiri),
      year      = nr(r$aasta),
      rank      = nr(r$koht),
      of        = nr(r$riike),
      score     = num(r$skoor),
      scale     = num(r$skaala),
      group     = tx(r$ruhm),
      groups    = if (nzchar(tx(r$ruhmad))) strsplit(tx(r$ruhmad), "|", fixed = TRUE)[[1]] else NULL,
      badge     = tx(r$silt),
      prevYear  = if ("eelmine_aasta" %in% names(r)) nr(r$eelmine_aasta) else NULL,
      prevRank  = if ("eelmine_koht" %in% names(r)) nr(r$eelmine_koht) else NULL,
      prevScore = if ("eelmine_skoor" %in% names(r)) num(r$eelmine_skoor) else NULL,
      prevBadge = if ("eelmine_silt" %in% names(r)) tx(r$eelmine_silt) else "",
      pillars   = if ("sambad" %in% names(r) && nzchar(tx(r$sambad)))
                    lapply(strsplit(tx(r$sambad), "|", fixed = TRUE)[[1]], function(x) {
                      o <- strsplit(x, ":", fixed = TRUE)[[1]]
                      list(o[1], num(o[2]), if (length(o) > 2) num(o[3]) else NULL)
                    }) else NULL,
      desc      = tx(r$kirjeldus),
      parts     = tx(r$koosseis),
      long      = tx(r$pikk),
      limits    = tx(r$piirang),
      org       = tx(r$asutus),
      rhythm    = tx(r$rutm),
      peersYear = nr(r$vordlus_aasta),
      peersOf   = nr(r$vordlus_riike),
      peers     = peers,
      src       = tx(r$allikas),
      url       = tx(r$url),
      url2      = tx(r$url2)
    )
  })
  # WGI ja B-READY: koondkohta ei ole, seepärast näidatakse Eesti kohta iga mõõtme / samba
  # järgi. Kohad arvutatakse Maailmapanga API andmetest (vt wgi_eesti ja bready_eesti failis
  # abi.R). Kui laadimine ei õnnestu, jääb alles eelmisel korral salvestatu ja tekib hoiatus.
  vana <- tryCatch(fromJSON(file.path(DATA_DIR, "indeksid.json"), simplifyVector = FALSE)$indeksid, error = function(e) NULL)
  kohad <- function(id, nimi, laadija, silt) {
    i <- which(vapply(indeksid, function(x) identical(x$id, id), logical(1)))
    if (length(i) != 1) return(invisible())
    w <- tryCatch(laadija(), error = function(e) { hoiata(nimi, " (Maailmapank): ", conditionMessage(e), " Jäetakse alles eelmine versioon."); NULL })
    if (!is.null(w)) {
      indeksid[[i]]$pillars     <<- w$sambad
      indeksid[[i]]$pillarsKind <<- "koht"
      indeksid[[i]]$pillarsOf   <<- w$riike
      indeksid[[i]]$year        <<- w$aasta
      indeksid[[i]]$prevYear    <<- w$eelmine_aasta
      indeksid[[i]]$badge       <<- silt(w)
    } else {
      v <- Filter(function(x) identical(x$id, id), vana)
      if (length(v) && length(v[[1]]$pillars)) for (n in c("pillars", "pillarsKind", "pillarsOf", "year", "prevYear", "badge")) indeksid[[i]][[n]] <<- v[[1]][[n]]
    }
  }
  kohad("wgi", "WGI", wgi_eesti, function(w) "Kuus mõõdet, koondkohta ei ole")
  kohad("bready", "B-READY", bready_eesti, function(w) "Kolm sammast, koondkohta ei avaldata")
  list(uuendatud = aeg_utc(), indeksid = indeksid)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
