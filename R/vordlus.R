# =============================================================================
# Rahvusvaheline võrdlus -> docs/data/vordlus.json
#
# Eurostati näitajad, mis on lehel "Eesti majanduse graafikud", Eesti, Läti,
# Leedu, Soome ja Rootsi kohta. Iga andmestik laaditakse ühe päringuga kõigi
# riikide kohta ja jagatakse riigiti seeriateks nimega <näitaja>_<riik>.
# Seeriate teisendused (nt otseinvesteeringud % SKP-st) teeb leht.
# =============================================================================

VORDLUS_RIIGID <- c("EE", "LV", "LT", "FI", "SE")

# Üks Eurostati päring mitme riigi kohta -> nimega list seeriatest (NULL, kui riigil andmeid pole)
eurostat_riigid <- function(id, filters, geos = VORDLUS_RIIGID, vali = NULL) {
  df <- eurostat::get_eurostat(id, filters = c(filters, list(geo = geos)), time_format = "raw", cache = FALSE)
  for (v in names(vali)) if (v %in% names(df)) df <- df[df[[v]] %in% vali[[v]], ]
  tcol <- intersect(c("time", "TIME_PERIOD"), names(df))[1]
  df <- df[!is.na(df$values), ]
  if (!nrow(df)) stop(id, ": päring ei tagastanud andmeid.")
  out <- lapply(geos, function(g) {
    d <- df[df$geo == g, ]
    if (!nrow(d)) return(NULL)
    p <- norm_periood(as.character(d[[tcol]]))
    if (anyDuplicated(p)) stop(id, " ", g, ": filtrid jätavad alles mitu seeriat.")
    seeria(p, d$values)
  })
  names(out) <- geos
  out
}

koosta_vordlus <- function() {
  vana <- loe_json("vordlus.json")$seeriad
  tul  <- list()

  # Lisab andmestiku kõigi riikide seeriad. Kui riigil pole andmeid kunagi olnud,
  # jäetakse ta vaikselt välja (nt Eurostat ei avalda seda rida); kui varem oli,
  # kehtib tavaline kontroll ja hoiatus.
  andmestik <- function(nimi, id, filters, vahemik, geos = VORDLUS_RIIGID, vali = NULL) {
    cache <- NULL; viga <- NULL
    lae <- function() {
      if (is.null(cache) && is.null(viga)) cache <<- tryCatch(eurostat_riigid(id, filters, geos, vali), error = function(e) { viga <<- e; NULL })
      if (!is.null(viga)) stop(viga)
      cache
    }
    for (g in geos) local({
      g <- g; k <- paste0(nimi, "_", g)
      if (is.null(vana[[k]])) {
        olemas <- tryCatch(!is.null(lae()[[g]]), error = function(e) TRUE)
        if (!olemas) { message(k, ": Eurostatis andmeid pole, jäetakse välja."); return(invisible()) }
      }
      tul[[k]] <<- turvaliselt(k, function() {
        s <- lae()[[g]]
        if (is.null(s)) stop("Eurostatis pole selle riigi andmeid.")
        s
      }, vana[[k]], vahemik)
    })
  }

  # 13 Majandususaldus (ESI), SA
  andmestik("esi_m", "ei_bssi_m_r2", list(indic = "BS-ESI-I", s_adj = "SA", freq = "M"), c(30, 170))
  # 6 Otseinvesteeringute kohustiste positsioon, mln €, ja nominaalne SKP (suhte jaoks)
  andmestik("fdi_q", "bop_iip6_q", list(currency = "MIO_EUR", bop_item = "FA__D__F", sector10 = "S1", sectpart = "S1",
                                        stk_flow = "L_LE", partner = "WRL_REST", freq = "Q"), c(1, 1e7))
  andmestik("skp_q", "namq_10_gdp", list(na_item = "B1GQ", unit = "CP_MEUR", s_adj = "NSA", freq = "Q"), c(1, 1e7))
  # 10 Elektri hind mittekodutarbijatele, ilma maksudeta, €/kWh
  andmestik("elekter_h", "nrg_pc_205", list(nrg_cons = "TOT_KWH", tax = "X_TAX", currency = "EUR", unit = "KWH"), c(0, 2))
  # 26 Kodumajapidamiste brutosäästu määr, %
  andmestik("saast_q", "nasq_10_ki", list(na_item = "SRG_S14_S15", s_adj = "NSA", unit = "PC", freq = "Q"), c(-50, 60))
  andmestik("saast_a", "nasa_10_ki", list(na_item = "SRG_S14_S15", unit = "PC", freq = "A"), c(-50, 60))
  # 19 Tehisintellekti kasutavad ettevõtted (>= 10 hõivatut), %; lisaks EL-27
  andmestik("ai_a", "isoc_eb_ai", list(indic_is = "E_AI_TANY", size_emp = "GE10", unit = "PC_ENT", freq = "A"), c(0, 100),
            geos = c(VORDLUS_RIIGID, "EU27_2020"), vali = list(nace_r2 = "C10-S951_X_K"))

  list(
    uuendatud = aeg_utc(),
    seeriad   = lapply(tul, function(r) seeria_json(r$seeria)),
    staatus   = lapply(tul, function(r) list(
      staatus = r$staatus,
      viimane = if (length(r$seeria$periods)) tail(r$seeria$periods, 1) else NULL))
  )
}
