# =============================================================================
# Majandusprognooside võrdlus -> docs/data/prognoosid.json
#
# Asutuste viimased prognoosid Eesti majanduskasvu, inflatsiooni ja töötuse
# määra kohta. Enamikul asutustel masinloetavat allikat ei ole, nii et nende
# arvud on käsitsi failis andmed/prognoosid.csv (uue prognoosi ilmudes
# asendatakse selle asutuse read). IMF-i arvud laetakse DataMapperi API-st;
# kui see ei õnnestu, jäävad kehtima CSV varuread.
# =============================================================================

PROGN_CSV <- "andmed/prognoosid.csv"

PROGN_NAITAJAD <- list(
  list(id = "skp", nimi = "Majanduskasv", kirjeldus = "SKP reaalkasv, %"),
  list(id = "inflatsioon", nimi = "Inflatsioon", kirjeldus = "Tarbijahindade aastane muutus, %"),
  list(id = "tootus", nimi = "Töötuse määr", kirjeldus = "Töötute osakaal tööjõus, %")
)
PROGN_IMF <- c(skp = "NGDP_RPCH", inflatsioon = "PCPIPCH", tootus = "LUR")
PROGN_VAHEMIK <- list(skp = c(-15, 15), inflatsioon = c(-5, 30), tootus = c(1, 25))

# IMF-i World Economic Outlooki arvud Eesti kohta: data.frame(aasta, naitaja, vaartus)
imf_prognoos <- function(aastad) {
  osad <- lapply(names(PROGN_IMF), function(n) {
    v <- http_json(paste0("https://www.imf.org/external/datamapper/api/v1/", PROGN_IMF[[n]], "/EST"),
                   list(periods = paste(aastad, collapse = ",")))
    x <- v$values[[PROGN_IMF[[n]]]]$EST
    if (is.null(x) || !length(x)) stop("IMF-i vastuses puudub ", PROGN_IMF[[n]])
    d <- data.frame(aasta = as.integer(names(x)), naitaja = n, vaartus = as.numeric(unlist(x)))
    d <- d[d$aasta %in% aastad & !is.na(d$vaartus), ]
    piir <- PROGN_VAHEMIK[[n]]
    if (any(d$vaartus < piir[1] | d$vaartus > piir[2])) stop("IMF-i ", n, " väärtus väljaspool mõistlikku vahemikku")
    d
  })
  d <- do.call(rbind, osad)
  if (!nrow(d) || !any(d$naitaja == "skp")) stop("IMF-i vastuses pole majanduskasvu prognoosi")
  d
}

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

  # IMF automaatselt. Avaldamise kuupäeva API ei anna: kui arvud muutuvad,
  # loetakse uueks avaldamiseks muutuse päev, muidu jääb eelmise faili kuupäev.
  vana <- loe_json("prognoosid.json")
  imf <- tryCatch(imf_prognoos(aastad), error = function(e) {
    hoiata("IMF-i prognoos: ", conditionMessage(e), ". Kasutatakse CSV varuridu."); NULL })
  if (!is.null(imf)) {
    varu <- d[d$asutus == "IMF", ][1, ]
    message("IMF: ", paste(sprintf("%s %d = %.1f", imf$naitaja, imf$aasta, imf$vaartus), collapse = "; "))
    # Võrreldakse ainult eelmise automaatse laadimisega; CSV varurea järel kehtib CSV kuupäev
    vanaImf <- if (is.data.frame(vana$asutused) && isTRUE("automaatne" %in% names(vana$asutused)))
      which(vana$asutused$asutus == "IMF" & vana$asutused$automaatne %in% TRUE) else integer()
    kuup <- varu$avaldatud
    if (length(vanaImf)) {
      vv <- vana$asutused$vaartused[vanaImf[1], , drop = FALSE]
      sama <- all(vapply(seq_len(nrow(imf)), function(i) {
        x <- tryCatch(vv[[imf$naitaja[i]]][[as.character(imf$aasta[i])]], error = function(e) NULL)
        !is.null(x) && length(x) == 1 && !is.na(x) && abs(x - imf$vaartus[i]) < 0.05
      }, logical(1)))
      kuup <- if (sama) vana$asutused$avaldatud[vanaImf[1]] else format(Sys.Date(), "%Y-%m-%d")
    }
    d <- rbind(d[d$asutus != "IMF", ],
               data.frame(asutus = "IMF", prognoos = varu$prognoos, avaldatud = kuup, aasta = imf$aasta,
                          naitaja = imf$naitaja, vaartus = round(imf$vaartus, 1), url = varu$url,
                          markus = "Laetakse automaatselt IMF DataMapperist."))
  }

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
         automaatne = a == "IMF" && !is.null(imf),
         markus = if (length(m)) paste(m, collapse = " ") else NULL,
         vaartused = vaartused)
  })
  if (length(asutused) < 2) stop("prognoose on liiga vähe")

  list(uuendatud = aeg_utc(), aastad = aastad, naitajad = PROGN_NAITAJAD, asutused = asutused)
}
