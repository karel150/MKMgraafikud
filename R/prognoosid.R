# =============================================================================
# Majandusprognooside võrdlus -> docs/data/prognoosid.json
#
# Asutuste viimased prognoosid Eesti majanduskasvu, inflatsiooni ja töötuse
# määra kohta. Enamikul asutustel masinloetavat allikat ei ole, nii et nende
# arvud on käsitsi failis andmed/prognoosid.csv (uue prognoosi ilmudes
# asendatakse selle asutuse read). IMF-i arvud laetakse DataMapperi API-st
# kord nädalas (vt PROGN_IMF_SAGEDUS). Kui laadimine ei õnnestu, jäävad eelmise
# laadimise arvud ja kui neid pole, siis CSV varuread.
# =============================================================================

PROGN_CSV <- "andmed/prognoosid.csv"

PROGN_NAITAJAD <- list(
  list(id = "skp", nimi = "Majanduskasv", kirjeldus = "SKP reaalkasv, %"),
  list(id = "inflatsioon", nimi = "Inflatsioon", kirjeldus = "Tarbijahindade aastane muutus, %"),
  list(id = "tootus", nimi = "Töötuse määr", kirjeldus = "Töötute osakaal tööjõus, %")
)
PROGN_IMF <- c(skp = "NGDP_RPCH", inflatsioon = "PCPIPCH", tootus = "LUR")
PROGN_IMF_SAGEDUS <- 7   # päeva: nii tihti küsitakse IMF-ilt uusi arve
PROGN_IMF_HOIATUS <- 14  # päeva: nii kaua võib laadimine ebaõnnestuda ilma hoiatuseta
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

# Eelmise faili automaatselt laaditud IMF-i arvud samade aastate kohta:
# list(d = data.frame(aasta, naitaja, vaartus), avaldatud, laaditud) või NULL
vana_imf <- function(vana, aastad) {
  a <- vana$asutused
  if (!is.data.frame(a) || !("automaatne" %in% names(a)) || !identical(as.integer(vana$aastad), as.integer(aastad)))
    return(NULL)
  i <- which(a$asutus == "IMF" & a$automaatne %in% TRUE)
  if (!length(i)) return(NULL)
  i <- i[1]
  d <- do.call(rbind, lapply(names(PROGN_IMF), function(n) {
    vv <- a$vaartused[[n]]
    if (!is.data.frame(vv)) return(NULL)
    do.call(rbind, lapply(intersect(as.character(aastad), names(vv)), function(y) {
      x <- vv[[y]][i]
      if (is.null(x) || is.na(x)) NULL else data.frame(aasta = as.integer(y), naitaja = n, vaartus = as.numeric(x))
    }))
  }))
  if (is.null(d) || !any(d$naitaja == "skp")) return(NULL)
  laaditud <- if ("laaditud" %in% names(a)) as.Date(a$laaditud[i], optional = TRUE) else as.Date(NA)
  if (!length(laaditud)) laaditud <- as.Date(NA)
  list(d = d, avaldatud = a$avaldatud[i], laaditud = laaditud)
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
  # WEO arvud muutuvad paar korda aastas, seetõttu küsitakse IMF-ilt uuesti alles
  # siis, kui viimane õnnestunud laadimine on vähemalt PROGN_IMF_SAGEDUS päeva vana.
  # Vahepeal ja ühekordse vea korral jäävad eelmise laadimise arvud; hoiatus tuleb
  # alles siis, kui laadimine pole õnnestunud PROGN_IMF_HOIATUS päeva jooksul.
  vana <- vana_imf(loe_json("prognoosid.json"), aastad)
  vanus <- if (is.null(vana) || is.na(vana$laaditud)) Inf else as.numeric(Sys.Date() - vana$laaditud)
  imf <- NULL; laaditud <- NA
  if (vanus < PROGN_IMF_SAGEDUS) {
    message("IMF: kasutatakse ", format(vana$laaditud), " laaditud arve (uus päring alates ",
            format(vana$laaditud + PROGN_IMF_SAGEDUS), ").")
    imf <- vana$d; laaditud <- vana$laaditud
  } else {
    imf <- tryCatch(imf_prognoos(aastad), error = function(e) e)
    if (inherits(imf, "error")) {
      viga <- conditionMessage(imf); imf <- NULL
      if (is.null(vana)) {
        hoiata("IMF-i prognoos: ", viga, ". Kasutatakse CSV varuridu.")
      } else {
        teade <- paste0("IMF-i prognoos: ", viga, ". Jäävad ",
                        if (is.na(vana$laaditud)) "eelmise" else format(vana$laaditud), " laaditud arvud.")
        if (vanus < PROGN_IMF_HOIATUS) message(teade) else hoiata(teade)
        imf <- vana$d; laaditud <- vana$laaditud
      }
    } else {
      laaditud <- Sys.Date()
    }
  }
  if (!is.null(imf)) {
    varu <- d[d$asutus == "IMF", ][1, ]
    message("IMF: ", paste(sprintf("%s %d = %.1f", imf$naitaja, imf$aasta, imf$vaartus), collapse = "; "))
    # Võrreldakse ainult eelmise automaatse laadimisega; CSV varurea järel kehtib CSV kuupäev
    kuup <- varu$avaldatud
    if (!is.null(vana)) {
      sama <- nrow(imf) == nrow(vana$d) && all(vapply(seq_len(nrow(imf)), function(i) {
        x <- vana$d$vaartus[vana$d$naitaja == imf$naitaja[i] & vana$d$aasta == imf$aasta[i]]
        length(x) == 1 && abs(x - imf$vaartus[i]) < 0.05
      }, logical(1)))
      kuup <- if (sama) vana$avaldatud else format(Sys.Date(), "%Y-%m-%d")
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
  # Viimase õnnestunud IMF-i päringu kuupäev, et järgmine jooks teaks, kas küsida uuesti
  i <- which(jarjekord == "IMF")
  if (length(i) && !is.null(imf) && !is.na(laaditud)) asutused[[i]]$laaditud <- format(laaditud)
  if (length(asutused) < 2) stop("prognoose on liiga vähe")

  list(uuendatud = aeg_utc(), aastad = aastad, naitajad = PROGN_NAITAJAD, asutused = asutused)
}
