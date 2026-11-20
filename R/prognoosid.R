# =============================================================================
# Majandusprognooside võrdlus -> docs/data/prognoosid.json
#
# Asutuste viimased prognoosid Eesti majanduskasvu, inflatsiooni ja töötuse
# määra kohta. Kõigi asutuste arvud, ka IMF-i omad, on failis andmed/prognoosid.csv
# ja neid uuendatakse vormiga „Uuenda prognoosi“ (tools/prognoosi_vorm.py).
# IMF-i DataMapperi API keelab GitHub Actionsi päringud (HTTP 403), seetõttu
# automaatset laadimist ei ole.
# =============================================================================

PROGN_CSV <- "andmed/prognoosid.csv"

PROGN_NAITAJAD <- list(
  list(id = "skp", nimi = "Majanduskasv", kirjeldus = "SKP reaalkasv, %"),
  list(id = "inflatsioon", nimi = "Inflatsioon", kirjeldus = "Tarbijahindade aastane muutus, %"),
  list(id = "tootus", nimi = "Töötuse määr", kirjeldus = "Töötute osakaal tööjõus, %")
)
PROGN_VAHEMIK <- list(skp = c(-15, 15), inflatsioon = c(-5, 30), tootus = c(1, 25))

koosta_prognoosid <- function() {
  d <- utils::read.csv(PROGN_CSV, encoding = "UTF-8", stringsAsFactors = FALSE,
                       colClasses = c(aasta = "integer", vaartus = "numeric"))
  d$markus[is.na(d$markus)] <- ""
  vigased <- !(d$naitaja %in% names(PROGN_VAHEMIK))
  if (any(vigased)) stop("tundmatu näitaja: ", paste(unique(d$naitaja[vigased]), collapse = ", "))

  # Näidatakse ainult jooksva ja tulevaste aastate prognoose
  see_aasta <- as.integer(format(Sys.Date(), "%Y"))
  d <- d[d$aasta >= see_aasta, ]
  csv_jarjekord <- unique(d$asutus)
  aastad <- see_aasta + 0:2

  jarjekord <- c(intersect(csv_jarjekord, d$asutus), setdiff(unique(d$asutus), csv_jarjekord))
  asutused <- lapply(jarjekord, function(a) {
    r <- d[d$asutus == a, ]
    vaartused <- stats::setNames(lapply(names(PROGN_VAHEMIK), function(n) {
      x <- r[r$naitaja == n, ]
      stats::setNames(as.list(x$vaartus), as.character(x$aasta))
    }), names(PROGN_VAHEMIK))
    vaartused <- lapply(vaartused, function(v) if (length(v)) v else structure(list(), names = character()))
    m <- unique(r$markus[nzchar(r$markus)])
    list(asutus = a, prognoos = r$prognoos[1], avaldatud = r$avaldatud[1], url = r$url[1],
         markus = if (length(m)) paste(m, collapse = " ") else NULL,
         vaartused = vaartused)
  })
  if (length(asutused) < 2) stop("prognoose on liiga vähe")

  list(uuendatud = aeg_utc(), aastad = aastad, naitajad = PROGN_NAITAJAD, asutused = asutused)
}
