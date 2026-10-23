# =============================================================================
# Rahvusvahelised edetabelid: andmed/indeksid.csv + andmed/indeksid_vordlus.csv
# -> docs/data/indeksid.json
#
# Edetabelitel API-t ei ole. Uuenda CSV-faile käsitsi, kui uus väljaanne
# ilmub (IMD juunis, GII septembris, CPI veebruaris, EIS juulis jne).
# =============================================================================

koosta_indeksid <- function() {
  loe <- function(f) read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE,
                              na.strings = "", check.names = FALSE)
  ix <- loe("andmed/indeksid.csv")
  vr <- loe("andmed/indeksid_vordlus.csv")

  vajalik <- c("id", "pealkiri", "aasta", "koht", "riike", "silt", "kirjeldus",
               "allikas", "url", "vordlus_aasta", "vordlus_riike")
  puudu <- setdiff(vajalik, names(ix))
  if (length(puudu)) stop("indeksid.csv: puuduvad veerud ", paste(puudu, collapse = ", "))
  if (any(is.na(ix$koht))) stop("indeksid.csv: igal real peab olema koht.")

  nr <- function(x) if (is.na(x)) NULL else as.integer(x)
  tx <- function(x) if (is.na(x)) "" else x

  indeksid <- lapply(seq_len(nrow(ix)), function(i) {
    r <- ix[i, ]
    v <- vr[vr$id == r$id, ]
    list(
      title     = r$pealkiri,
      year      = nr(r$aasta),
      rank      = nr(r$koht),
      of        = nr(r$riike),
      badge     = tx(r$silt),
      desc      = tx(r$kirjeldus),
      peersYear = nr(r$vordlus_aasta),
      peersOf   = nr(r$vordlus_riike),
      peers     = if (nrow(v)) lapply(seq_len(nrow(v)), function(j) list(v$riik[j], as.integer(v$koht[j]))) else NULL,
      src       = tx(r$allikas),
      url       = tx(r$url)
    )
  })
  list(uuendatud = aeg_utc(), indeksid = indeksid)
}
