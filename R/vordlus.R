# =============================================================================
# Rahvusvaheline võrdlus -> docs/data/vordlus.json
#
# Lehe "Eesti majanduse graafikud" näitajad, millel on Eurostatis vaste
# (sh Statistikaameti näitajate Eurostati vasted), Eesti, Läti, Leedu, Soome ja
# Rootsi kohta. Iga andmestik laaditakse ühe päringuga kõigi
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

# Vabade ametikohtade määr (B-S, kõik suurusklassid, NSA). Eurostat vahetas 2026. aastal
# andmestiku (NACE Rev. 2 jvs_q_nace2 -> NACE Rev. 2.1 jvs_q_r21). Proovitakse uut, siis
# teisi leitud jvs_q_* andmestikke; uuem on eelistatud, puuduvad kvartalid tulevad vanast.
jvs_riigid <- function(geos = VORDLUS_RIIGID) {
  ids <- tryCatch({
    s <- eurostat::search_eurostat("job vacancy", type = "dataset")
    unique(s$code[grepl("^jvs_q_(nace|r2)", s$code)])
  }, error = function(e) character())
  # jvs_q_r21 = NACE Rev. 2.1 (andmed alates 2001), jvs_q_nace2 = vana NACE Rev. 2 tabel
  ids <- unique(c("jvs_q_r21", ids, "jvs_q_nace2"))
  eelista <- c("B-S", "B-T", "B-U", "A-S", "A-T", "A-U", "TOTAL")
  tul <- setNames(vector("list", length(geos)), geos)
  logi <- character()
  for (id in ids) {
    # Kõigepealt kitsa päringuga; kui dimensiooni nimi on muutunud, siis ainult riigi ja sagedusega
    df <- tryCatch(eurostat::get_eurostat(id, filters = list(geo = geos, indic_em = c("JOBRATE", "JVR"), freq = "Q"),
                                          time_format = "raw", cache = FALSE), error = function(e) NULL)
    if (is.null(df) || !nrow(df))
      df <- tryCatch(eurostat::get_eurostat(id, filters = list(geo = geos, freq = "Q"), time_format = "raw", cache = FALSE),
                     error = function(e) { logi <<- c(logi, paste0(id, ": ", conditionMessage(e))); NULL })
    if (is.null(df) || !nrow(df)) { logi <- c(logi, paste0(id, ": andmeid pole")); next }
    df <- df[!is.na(df$values), ]
    # Vabade ametikohtade määra kood: varem JOBRATE, uutes tabelites JVR
    for (v in grep("^indic", names(df), value = TRUE)) {
      k <- intersect(c("JOBRATE", "JVR"), unique(df[[v]]))[1]
      if (is.na(k)) { logi <- c(logi, paste0(id, ": määra koodi pole (", v, ": ", paste(head(unique(df[[v]]), 10), collapse = ", "), ")")); df <- df[0, ]; break }
      df <- df[df[[v]] == k, ]
    }
    if (!nrow(df)) next
    if ("s_adj" %in% names(df))    df <- df[df$s_adj == "NSA", ]
    if ("sizeclas" %in% names(df)) df <- df[df$sizeclas == "TOTAL", ]
    if ("unit" %in% names(df) && "PC" %in% df$unit) df <- df[df$unit == "PC", ]
    nc <- grep("^nace", names(df), value = TRUE)[1]
    kood <- NA
    if (!is.na(nc)) {
      koodid <- unique(df[[nc]][df$geo == "EE"])
      kood <- c(intersect(eelista, koodid), sort(grep("^B-", koodid, value = TRUE), decreasing = TRUE),
                sort(grep("^A-", koodid, value = TRUE), decreasing = TRUE))[1]
      if (is.na(kood)) { logi <- c(logi, paste0(id, ": sobivat tegevusala pole (", paste(head(koodid, 15), collapse = ", "), ")")); next }
      df <- df[df[[nc]] == kood, ]
    }
    if (!nrow(df)) { logi <- c(logi, paste0(id, ": filtrite järel andmeid pole")); next }
    message("Vabad ametikohad: ", id, if (!is.na(nc)) paste0(" (", nc, " = ", kood, ")"), ", kuni ", max(as.character(df[[intersect(c("time", "TIME_PERIOD"), names(df))[1]]])))
    tcol <- intersect(c("time", "TIME_PERIOD"), names(df))[1]
    for (g in geos) {
      d <- df[df$geo == g, ]
      if (!nrow(d)) next
      p <- norm_periood(as.character(d[[tcol]]))
      if (anyDuplicated(p)) {
        mitu <- names(d)[vapply(d, function(x) length(unique(x)) > 1, logical(1))]
        logi <- c(logi, paste0(id, " ", g, ": mitu rida perioodi kohta (erinevad: ", paste(setdiff(mitu, c(tcol, "values")), collapse = ", "), ")"))
        next
      }
      uus <- data.frame(p = p, v = d$values)
      vana <- tul[[g]]
      tul[[g]] <- if (is.null(vana)) uus else rbind(vana, uus[!uus$p %in% vana$p, ])
    }
  }
  if (all(vapply(tul, is.null, logical(1)))) stop("vabade ametikohtade andmeid ei leitud: ", paste(substr(logi, 1, 200), collapse = " | "))
  lapply(tul, function(d) if (is.null(d)) NULL else { d <- d[order(d$p), ]; seeria(d$p, d$v) })
}

koosta_vordlus <- function() {
  vana <- loe_json("vordlus.json")$seeriad
  tul  <- list()

  # Lisab andmestiku kõigi riikide seeriad. Kui riigil pole andmeid kunagi olnud,
  # jäetakse ta vaikselt välja (nt Eurostat ei avalda seda rida); kui varem oli,
  # kehtib tavaline kontroll ja hoiatus.
  # varu: list(list(id =, filters =), ...) proovitakse järjest, kui põhipäring ebaõnnestub
  # (nt Eurostat muutis andmestiku koodi või klassifikaatorit).
  andmestik <- function(nimi, id, filters, vahemik, geos = VORDLUS_RIIGID, vali = NULL, varu = list(), fn = NULL) {
    if (!is.null(fn)) varu <- c(list(list(fn = fn)), varu)
    cache <- NULL; viga <- NULL
    lae <- function() {
      if (is.null(cache) && is.null(viga)) {
        for (a in if (is.null(fn)) c(list(list(id = id, filters = filters)), varu) else varu) {
          cache <<- tryCatch(if (!is.null(a$fn)) a$fn(geos) else eurostat_riigid(a$id, a$filters, geos, vali), error = function(e) { viga <<- e; NULL })
          if (!is.null(cache)) { viga <<- NULL; break }
        }
      }
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

  YOY <- c(-80, 150)
  IDX <- c(1, 1000)
  Q_  <- function(...) c(list(...), list(freq = "Q"))
  A_  <- function(...) c(list(...), list(freq = "A"))
  M_  <- function(...) c(list(...), list(freq = "M"))

  # ---- Statistikaameti näitajate Eurostati vasted ------------------------------
  # 1 SKP ja 3 eksport: aheldatud mahu muutus
  andmestik("skp_kasv_q",  "namq_10_gdp", Q_(na_item = "B1GQ", unit = "CLV_PCH_SM", s_adj = "SCA"), YOY)
  andmestik("skp_kasv_a",  "nama_10_gdp", A_(na_item = "B1GQ", unit = "CLV_PCH_PRE"), YOY)
  andmestik("eksp_kasv_q", "namq_10_gdp", Q_(na_item = "P6", unit = "CLV_PCH_SM", s_adj = "SCA"), YOY)
  andmestik("eksp_kasv_a", "nama_10_gdp", A_(na_item = "P6", unit = "CLV_PCH_PRE"), YOY)
  # 4, 5 kaupade ja teenuste eksport, 18 põhivarainvesteeringud: jooksevhindades, mln €
  andmestik("ekspK_q", "namq_10_gdp", Q_(na_item = "P61",  unit = "CP_MEUR", s_adj = "NSA"), c(1, 1e7))
  andmestik("ekspT_q", "namq_10_gdp", Q_(na_item = "P62",  unit = "CP_MEUR", s_adj = "NSA"), c(1, 1e7))
  andmestik("inv_q",   "namq_10_gdp", Q_(na_item = "P51G", unit = "CP_MEUR", s_adj = "NSA"), c(1, 1e7))
  # 2 tööstustoodang (B-D), 20 jaekaubandus (G47), 23 tootjahinnad: indeksid 2021 = 100
  andmestik("too_m", "sts_inpr_m", M_(indic_bt = "PRD", nace_r2 = "B-D", s_adj = "CA", unit = "I21"), IDX)
  andmestik("jae_m", "sts_trtu_m", M_(indic_bt = "VOL_SLS", nace_r2 = "G47", s_adj = "CA", unit = "I21"), IDX,
            varu = list(list(id = "sts_trtu_m", filters = M_(indic_bt = "TOVV", nace_r2 = "G47", s_adj = "CA", unit = "I21"))))
  andmestik("ppi_m", "sts_inpp_m", M_(indic_bt = "PRC_PRR", nace_r2 = "B-E36", s_adj = "NSA", unit = "I21"), IDX)
  # 9 tarbijahinnad: ühtlustatud THI (alates 2026 ECOICOP 2, 2025 = 100)
  andmestik("hicp_m", "prc_hicp_minr", M_(coicop18 = "TOTAL", unit = "I25"), IDX,
            varu = list(list(id = "prc_hicp_midx", filters = M_(coicop = "CP00", unit = "I15"))))
  # 8 tootlikkus töötatud tunni kohta, 17 nominaalne tööjõu ühikukulu (tunnipõhine)
  andmestik("tootl_q", "namq_10_lp_ulc", Q_(na_item = "RLPR_HW", unit = "PCH_SM", s_adj = "SCA"), YOY)
  andmestik("tootl_a", "nama_10_lp_ulc", A_(na_item = "RLPR_HW", unit = "PCH_PRE"), YOY)
  andmestik("ulc_q",   "namq_10_lp_ulc", Q_(na_item = "NULC_HW", unit = "PCH_SM", s_adj = "SCA"), YOY)
  andmestik("ulc_a",   "nama_10_lp_ulc", A_(na_item = "NULC_HW", unit = "PCH_PRE"), YOY)
  # 7 ettevõtlussektori T&A, % SKP-st
  andmestik("berd_a", "rd_e_gerdtot", A_(sectperf = "BES", unit = "PC_GDP"), c(0, 10))
  # 11 palgad: tööjõukulu indeksi palgaosa (D11), aastamuutus
  andmestik("lci_q", "lc_lci_r2_q", Q_(lcstruct = "D11", nace_r2 = "B-S", s_adj = "SCA", unit = "PCH_SM"), YOY)
  # 14 töötuse määr 15-74 (kuu: age TOTAL = 15-74)
  andmestik("tootus_m", "une_rt_m", M_(age = "TOTAL", sex = "T", unit = "PC_ACT", s_adj = "SA"), c(0, 40))
  andmestik("tootus_a", "une_rt_a", A_(age = "Y15-74", sex = "T", unit = "PC_ACT"), c(0, 40))
  # 15 valitsemissektori võlg, % SKP-st
  andmestik("volg_a", "gov_10dd_edpt1", A_(na_item = "GD", sector = "S13", unit = "PC_GDP"), c(0, 250))
  # 16 vabade ametikohtade määr (B-S)
  # Alates 2026 I kv avaldab Eurostat vabade ametikohtade andmed NACE Rev. 2.1 järgi uues andmestikus;
  # jvs_riigid otsib andmestiku ise ja liidab vana (NACE Rev. 2) rea uue ette.
  andmestik("vab_q", "jvs", list(), c(0, 20), varu = list(), fn = jvs_riigid)
  # 21 tööhõive määr 20-64, 22 aktiivsuse määr 15-74 (mitteaktiivsus = 100 - aktiivsus)
  andmestik("hoive_q", "lfsi_emp_q", Q_(indic_em = "EMP_LFS", age = "Y20-64", sex = "T", unit = "PC_POP", s_adj = "SA"), c(0, 100))
  andmestik("akt_q",   "lfsi_emp_q", Q_(indic_em = "ACT",     age = "Y15-74", sex = "T", unit = "PC_POP", s_adj = "SA"), c(0, 100))
  # 29 eluasemehinnaindeksi aastamuutus
  andmestik("hpi_q", "prc_hpi_q", Q_(purchase = "TOTAL", unit = "RCH_A"), YOY)
  andmestik("hpi_a", "prc_hpi_a", A_(purchase = "TOTAL", unit = "RCH_A_AVG"), YOY)

  list(
    uuendatud = aeg_utc(),
    seeriad   = lapply(tul, function(r) seeria_json(r$seeria)),
    staatus   = lapply(tul, function(r) list(
      staatus = r$staatus,
      viimane = if (length(r$seeria$periods)) tail(r$seeria$periods, 1) else NULL))
  )
}
