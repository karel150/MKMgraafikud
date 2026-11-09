# =============================================================================
# Eesti majanduse näitajad -> docs/data/naitajad.json
#
# Iga seeria laaditakse toorel kujul (perioodikood + väärtus). Kvartali- ja
# aastavaated, aastamuutused ja koondamised arvutab leht ise (docs/index.html,
# funktsioon koostaGrupid), et loogika oleks ühes kohas.
#
# Seeriate nimed peavad klappima lehel kasutatavatega.
# =============================================================================

NAITAJA <- "Näitaja"

koosta_naitajad <- function(esi_kuu = NULL) {
  vana <- loe_json("naitajad.json")$seeriad
  tul  <- list()
  lisa <- function(nimi, f, vahemik = c(-Inf, Inf)) {
    r <- turvaliselt(nimi, f, vana[[nimi]], vahemik)
    tul[[nimi]] <<- r
  }
  # Mitu seeriat ühest päringust: päring tehakse üks kord, jagatakse osadeks
  mitu <- function(paring) { cache <- NULL; function() { if (is.null(cache)) cache <<- paring(); cache } }

  YOY <- c(-80, 150)   # aastamuutuste mõistlik vahemik, %

  # ---- SKP ja kogurahvatulu (RAA0012) -------------------------------------------
  # 5 = SKP reaalmuutus aasta varem, %; 1 = SKP jooksevhindades, mln €; 6 = KRT jooksevhindades, mln €
  skp <- mitu(function() px_paring("RAA0012",
    valik("Aasta", "*"), valik("Kvartal", c("1", RN)),
    valik("Sesoonne korrigeerimine", "1"), valik(NAITAJA, c("1", "5", "6"))))
  kv <- function(d) d[d$Kvartal != "1", ]
  aa <- function(d) d[d$Kvartal == "1", ]
  osa <- function(d, kood) d[d[[NAITAJA]] == kood, ]
  lisa("skp_q", function() { d <- kv(osa(skp(), "5")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("skp_a", function() { d <- aa(osa(skp(), "5")); seeria(d$Aasta, d$value) }, YOY)
  lisa("skp_q_cur", function() { d <- kv(osa(skp(), "1")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(100, 1e6))
  lisa("skp_a_cur", function() { d <- aa(osa(skp(), "1")); seeria(d$Aasta, d$value) }, c(100, 1e6))
  lisa("krt_q_cur", function() { d <- kv(osa(skp(), "6")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(100, 1e6))
  lisa("krt_a_cur", function() { d <- aa(osa(skp(), "6")); seeria(d$Aasta, d$value) }, c(100, 1e6))

  # ---- RAA0061: eksport ja kapitali kogumahutus põhivarasse -----------------
  # Komponent: 7 = eksport kokku, 8 = kaupade eksport, 9 = teenuste eksport, 4 = põhivara
  # Näitaja: 4 = reaalmuutus aasta varem, %; 1 = jooksevhindades, mln €; 7 = osatähtsus SKP-s, %
  eks <- mitu(function() px_paring("RAA0061",
    valik("Komponent", c("4", "7", "8", "9")), valik("Aasta", "*"),
    valik("Kvartal", c("1", RN)), valik(NAITAJA, c("1", "4", "7"))))
  komp <- function(kood, n) { d <- eks(); d[d$Komponent == kood & d[[NAITAJA]] == n, ] }
  for (k in list(c("7", "eksp"), c("8", "ekspK"), c("9", "ekspT"))) local({
    kood <- k[1]; nimi <- k[2]
    lisa(paste0(nimi, "_q"), function() { d <- kv(komp(kood, "4")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
    lisa(paste0(nimi, "_a"), function() { d <- aa(komp(kood, "4")); seeria(d$Aasta, d$value) }, YOY)
    if (kood != "7") {
      lisa(paste0(nimi, "_q_cur"), function() { d <- kv(komp(kood, "1")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(1, 1e6))
      lisa(paste0(nimi, "_a_cur"), function() { d <- aa(komp(kood, "1")); seeria(d$Aasta, d$value) }, c(1, 1e6))
    }
  })
  lisa("kapital_q_osa", function() { d <- kv(komp("4", "7")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(0, 100))
  lisa("kapital_a_osa", function() { d <- aa(komp("4", "7")); seeria(d$Aasta, d$value) }, c(0, 100))
  lisa("kapital_q_yoy", function() { d <- kv(komp("4", "4")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("kapital_a_yoy", function() { d <- aa(komp("4", "4")); seeria(d$Aasta, d$value) }, YOY)

  # ---- Jaemüük kuude kaupa (KM020): jaekaubandus v.a mootorsõidukid (G47) -----
  # TOVV = müügitulu mahuindeks (2021 = 100), TOVT = müügitulu, mln €
  km20 <- mitu(function() px_paring("KM020",
    valik(NAITAJA, c("TOVV", "TOVT")), valik("Tegevusala", "G47"), valik("Vaatlusperiood", "*")))
  lisa("jaem_m_idx", function() { d <- km20(); d <- d[d[[NAITAJA]] == "TOVV", ]; seeria(d$Vaatlusperiood, d$value) }, c(1, 1000))
  lisa("jaem_m_eur", function() { d <- km20(); d <- d[d[[NAITAJA]] == "TOVT", ]; seeria(d$Vaatlusperiood, d$value) }, c(10, 1e5))

  # ---- Töötukassa avaandmed (andmed.eesti.ee) ----------------------------------
  # Faili link leitakse andmestiku kirjeldusest pealkirja järgi, nii et Töötukassa
  # faili uuendamisel muutuv aadress päringut ei murra.
  tk_fail <- function(ds_id, muster) {
    ds <- fromJSON(content(stop_for_status(GET(paste0("https://andmed.eesti.ee/api/datasets/", ds_id), timeout(60))),
                           as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    jaot <- Filter(function(x) grepl(muster, x$titleEt, ignore.case = TRUE), ds$distributions)
    if (!length(jaot) || !length(jaot[[1]]$accessUrls)) stop("Töötukassa failist puudub allalaadimise link (", muster, ").")
    tmp <- tempfile(fileext = ".xlsx")
    stop_for_status(GET(jaot[[1]]$accessUrls[[1]], write_disk(tmp, overwrite = TRUE), timeout(180)))
    tmp
  }
  # Loeb kõik lehed, kus esimene veerg on kuupäev; tagastab p (YYYYMmm) + valitud veerud
  tk_lehed <- function(tmp) {
    osad <- lapply(readxl::excel_sheets(tmp), function(leht) {
      y <- readxl::read_excel(tmp, sheet = leht)
      if (!nrow(y) || !inherits(y[[1]], c("POSIXct", "Date"))) return(NULL)
      y$p <- format(as.Date(y[[1]]), "%YM%m")
      y
    })
    x <- dplyr::bind_rows(osad)
    if (!nrow(x)) stop("Töötukassa failist ei leitud andmeid.")
    x
  }
  kuusumma <- function(p, v) { a <- aggregate(v ~ p, data.frame(p = p, v = as.numeric(v)), sum, na.rm = TRUE); seeria(a$p, a$v) }

  # Registreeritud töötud, kuu lõpu seis (fail "... viimase töökogemuse tegevusala järgi")
  lisa("regtootud_m", function() {
    x <- tk_lehed(tk_fail("e7ce24f2-7c77-403a-8559-12e9fd75b249", "tegevusala"))
    v <- grep("arv", names(x), value = TRUE, ignore.case = TRUE)[1]
    kuusumma(x$p, x[[v]])
  }, c(5000, 200000))

  # Töötukassa vahendatud vabad töökohad Eestis, kuu lõpu seis: kokku ja töötlev tööstus (C).
  # Välja jäetakse maakond "Välismaa" (EURES-e kaudu vahendatavad välismaa töökohad).
  vk <- mitu(function() {
    x <- tk_lehed(tk_fail("e5707aaa-a6cc-4cd9-a3e6-fb9df309ac76", "tegevusala"))
    mk <- grep("^MAAKOND$", names(x), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(mk)) stop("Töötukassa vabade töökohtade failis puudub veerg MAAKOND.")
    x[!grepl("^V\u00e4lismaa", x[[mk]], ignore.case = TRUE), ]
  })
  vk_veerg <- function(x) grep("kuu l\u00f5pu seisuga", names(x), value = TRUE, ignore.case = TRUE)[1]
  lisa("tk_vabad_m", function() { x <- vk(); kuusumma(x$p, x[[vk_veerg(x)]]) }, c(100, 100000))
  lisa("tk_vabad_c_m", function() {
    x <- vk(); tt <- x[grepl("^T\u00f6\u00f6tlev t\u00f6\u00f6stus", x[[3]]), ]
    if (!nrow(tt)) stop("Töötleva tööstuse rida puudub.")
    kuusumma(tt$p, tt[[vk_veerg(tt)]])
  }, c(10, 50000))

  # ---- Kaupade eksport kuude kaupa (VKK10, uus andmebaas statsql) ------------
  # EXP = kaupade eksport kokku, EXP_EE = Eesti päritolu kaupade eksport; eurodes -> mln €
  vkk <- mitu(function() px_paring("VKK10",
    valik("FLOW", c("EXP", "EXP_EE")), valik("CN_PRODUCT", "TOTAL"), valik("PART_COUNTRY", "TOTAL"),
    valik("ContentsCode", "TRD_VAL"), valik("TIME", "*"), api = PX_SQL))
  lisa("kaup_m",   function() { d <- vkk(); d <- d[d$FLOW == "EXP", ];    seeria(d$TIME, d$value / 1e6) }, c(10, 1e5))
  lisa("kaupEE_m", function() { d <- vkk(); d <- d[d$FLOW == "EXP_EE", ]; seeria(d$TIME, d$value / 1e6) }, c(10, 1e5))

  # ---- Tootlikkus ja tööjõu ühikukulu (RAL0013) ------------------------------
  # Suhtarv: 2 = tootlikkus töötatud tunni kohta, 3 = tööjõu ühikukulu
  # Näitaja: 2 = nominaal, 6 = reaal (aasta varem); 4 = nominaal SA, 8 = reaal SA
  ral <- mitu(function() px_paring("RAL0013",
    valik("Suhtarv", c("2", "3")), valik("Aasta", "*"), valik("Kvartal", c("1", RN)), valik(NAITAJA, c("2", "4", "6", "8"))))
  rl <- function(suhe, n) { d <- ral(); d[d$Suhtarv == suhe & d[[NAITAJA]] == n, ] }
  lisa("tootl_q", function() { d <- kv(rl("2", "8")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("tootl_a", function() { d <- aa(rl("2", "6")); seeria(d$Aasta, d$value) }, YOY)
  lisa("ulc_nom_q",  function() { d <- kv(rl("3", "4")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("ulc_real_q", function() { d <- kv(rl("3", "8")); seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("ulc_nom_a",  function() { d <- aa(rl("3", "2")); seeria(d$Aasta, d$value) }, YOY)
  lisa("ulc_real_a", function() { d <- aa(rl("3", "6")); seeria(d$Aasta, d$value) }, YOY)

  # ---- Tööstustoodang (TO0053): B-D, kalendaarselt korrigeeritud -----------
  to <- mitu(function() px_paring("TO0053",
    valik(NAITAJA, c("PROD", "PROD_SM")), valik("Tegevusala", "BTD"),
    valik("Korrigeerimine", "W"), valik("Vaatlusperiood", "*")))
  lisa("too_m_yoy", function() { d <- to(); d <- d[d[[NAITAJA]] == "PROD_SM", ]; seeria(d$Vaatlusperiood, d$value) }, YOY)
  lisa("too_m_lvl", function() { d <- to(); d <- d[d[[NAITAJA]] == "PROD", ]; seeria(d$Vaatlusperiood, d$value) }, c(1, 1000))

  # ---- Tarbijahinnaindeks (IA021 kuu, IA001 aasta) --------------------------
  lisa("thi_m", function() { d <- px_paring("IA021", valik(NAITAJA, "1"), valik("Aasta", "*"), valik("Kuu", "*")); seeria(per_m(d$Aasta, d$Kuu), d$value) }, c(-20, 50))
  lisa("thi_a", function() { d <- px_paring("IA001", valik("Aasta", "*"), valik("Kaubagrupp", "1")); seeria(d$Aasta, d$value) }, c(-20, 50))

  # ---- Palgad (PA115) ---------------------------------------------------------
  pa <- mitu(function() px_paring("PA115",
    valik(NAITAJA, c("GR_W_AVG", "GR_W_AVG_SM", "GR_W_D5")), valik("Omaniku liik", "TOTAL"), valik("Vaatlusperiood", "*")))
  lisa("palk_q_lvl", function() { d <- pa(); d <- d[d[[NAITAJA]] == "GR_W_AVG", ];    seeria(d$Vaatlusperiood, d$value) }, c(100, 20000))
  lisa("palk_q_yoy", function() { d <- pa(); d <- d[d[[NAITAJA]] == "GR_W_AVG_SM", ]; seeria(d$Vaatlusperiood, d$value) }, YOY)
  lisa("mediaan_q",  function() { d <- pa(); d <- d[d[[NAITAJA]] == "GR_W_D5", ];     seeria(d$Vaatlusperiood, d$value) }, c(100, 20000))

  # ---- Tööturg (TT0160): töötuse, tööhõive ja mitteaktiivsuse määr ---------
  tt <- mitu(function() px_paring("TT0160",
    valik(NAITAJA, c("UNEMP_RATE", "EMPRATE", "INACT_NR", "LABOUR_INACT")), valik("Sugu", "T"),
    valik("Vanuser\u00fchm", c("Y15-74", "Y20-64")), valik("Vaatlusperiood", "*")))
  vr <- "Vanuser\u00fchm"
  ttosa <- function(n, vanus) { d <- tt(); d[d[[NAITAJA]] == n & d[[vr]] == vanus, ] }
  lisa("tootus_q",    function() { d <- ttosa("UNEMP_RATE", "Y15-74"); seeria(d$Vaatlusperiood, d$value) }, c(0, 40))
  lisa("hoive_q_1574", function() { d <- ttosa("EMPRATE", "Y15-74"); seeria(d$Vaatlusperiood, d$value) }, c(20, 100))
  lisa("hoive_q_2064", function() { d <- ttosa("EMPRATE", "Y20-64"); seeria(d$Vaatlusperiood, d$value) }, c(20, 100))
  # Mitteaktiivsuse määr = mitteaktiivsed / (tööjõud + mitteaktiivsed), 15-74
  lisa("mitteakt_q", function() {
    a <- ttosa("INACT_NR", "Y15-74"); b <- ttosa("LABOUR_INACT", "Y15-74")
    m <- merge(a[, c("Vaatlusperiood", "value")], b[, c("Vaatlusperiood", "value")], by = "Vaatlusperiood")
    seeria(m$Vaatlusperiood, round(100 * m$value.x / m$value.y, 2))
  }, c(5, 60))

  # ---- Vabad ametikohad (PAV011) -------------------------------------------
  pav <- mitu(function() px_paring("PAV011", valik(NAITAJA, c("VAC_RATE", "VAC")), valik("Tegevusala", "_T"), valik("Vaatlusperiood", "*")))
  lisa("vab_maar_q", function() { d <- pav(); d <- d[d[[NAITAJA]] == "VAC_RATE", ]; seeria(d$Vaatlusperiood, d$value) }, c(0, 20))
  lisa("vab_arv_q",  function() { d <- pav(); d <- d[d[[NAITAJA]] == "VAC", ];      seeria(d$Vaatlusperiood, d$value) }, c(0, 1e6))

  # ---- Palkade aastastatistika (PA101) ----------------------------------------
  pa1 <- mitu(function() px_paring("PA101", valik(NAITAJA, c("GR_W_AVG", "GR_W_D5")), valik("Tegevusala", "TOTAL"), valik("Vaatlusperiood", "*")))
  lisa("palk_a_lvl", function() { d <- pa1(); d <- d[d[[NAITAJA]] == "GR_W_AVG", ]; seeria(d$Vaatlusperiood, d$value) }, c(100, 20000))
  lisa("mediaan_a",  function() { d <- pa1(); d <- d[d[[NAITAJA]] == "GR_W_D5", ];  seeria(d$Vaatlusperiood, d$value) }, c(100, 20000))

  # ---- Jaekaubanduse mahuindeks (KM0102) ---------------------------------------
  km <- mitu(function() px_paring("KM0102", valik(NAITAJA, "RETAIL_SM"), valik("Tegevusala", c("G45_47", "G45", "G47")), valik("Vaatlusperiood", "*")))
  for (g in c("G45_47", "G45", "G47")) local({
    gg <- g
    lisa(paste0("jae_q_", gg), function() { d <- km(); d <- d[d$Tegevusala == gg, ]; seeria(d$Vaatlusperiood, d$value) }, YOY)
  })

  # ---- Tööstustoodangu tootjahinnaindeks (IA039 kuu, IA029 aasta) -----------
  # Tegevusala: 1 = kokku, 5 = töötlev tööstus
  TA <- "Tegevusala (EMTAK 2008)"
  ppi <- mitu(function() px_paring("IA039", valik(TA, c("1", "5")), valik("Aasta", "*"), valik("Kuu", "*")))
  ppa <- mitu(function() px_paring("IA029", valik("Aasta", "*"), valik(TA, c("1", "5"))))
  for (k in list(c("1", "kokku"), c("5", "c"))) local({
    kood <- k[1]; nimi <- k[2]
    lisa(paste0("ppi_m_", nimi), function() { d <- ppi(); d <- d[d[[TA]] == kood, ]; seeria(per_m(d$Aasta, d$Kuu), d$value, alates = ALGUS - 1) }, c(10, 1000))
    lisa(paste0("ppi_a_", nimi), function() { d <- ppa(); d <- d[d[[TA]] == kood, ]; seeria(d$Aasta, d$value) }, c(-50, 100))
  })

  # ---- Tegevusalade panus SKP kasvu (RAA0042), protsendipunkti ----------------
  # Kõik tegevusalad (v.a alamread 2 ja 3) + neto-tootemaksud (23). Rühmitab leht.
  PANUS <- c(as.character(c(1, 4:21, 23, 25)))
  TK <- "Tegevusala (EMTAK 2008) /komponent"
  pan <- mitu(function() px_paring("RAA0042", valik(TK, PANUS), valik("Aasta", "*"), valik("Kvartal", c("1", RN)), valik(NAITAJA, "6")))
  for (k in PANUS) local({
    kood <- k
    lisa(paste0("panus_q_", kood), function() { d <- kv(pan()); d <- d[d[[TK]] == kood, ]; seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(-30, 30))
    lisa(paste0("panus_a_", kood), function() { d <- aa(pan()); d <- d[d[[TK]] == kood, ]; seeria(d$Aasta, d$value) }, c(-30, 30))
  })

  # ---- Ehitusloa saanud eluruumid (EH04) ---------------------------------------
  lisa("ehitusload_q", function() {
    d <- px_paring("EH04", valik(NAITAJA, "PNUM"), valik("Loa/teatise liik", "BPERMIT"), valik("Vaatlusperiood", "*"))
    seeria(d$Vaatlusperiood, d$value)
  }, c(0, 1e5))

  # ---- Maanteekaubavedu (TS54): veosed, tuhat tonni ---------------------------
  ts <- mitu(function() px_paring("TS54", valik("Aasta", "*"), valik(NAITAJA, "1"), valik("Kvartal", c("1", RN)), valik("Veo liik", c("2", "3"))))
  for (k in list(c("2", "riigisis"), c("3", "rahvusv"))) local({
    kood <- k[1]; nimi <- k[2]
    lisa(paste0("vedu_q_", nimi), function() { d <- kv(ts()); d <- d[d[["Veo liik"]] == kood, ]; seeria(per_q(d$Aasta, d$Kvartal), d$value) }, c(0, 1e6))
    lisa(paste0("vedu_a_", nimi), function() { d <- aa(ts()); d <- d[d[["Veo liik"]] == kood, ]; seeria(d$Aasta, d$value) }, c(0, 1e7))
  })

  # ---- Eluaseme hinnaindeks (IA028), 2010 = 100 --------------------------------
  lisa("eluase_q", function() {
    d <- px_paring("IA028", valik("Aasta", "*"), valik("Kvartal", RN), valik("Eluaseme liik", "1"))
    seeria(per_q(d$Aasta, d$Kvartal), d$value, alates = ALGUS - 1)
  }, c(10, 1000))

  # ---- Esmaselt registreeritud uued sõiduautod (TS322) --------------------------
  lisa("autod_m", function() {
    d <- px_paring("TS322", valik("S\u00f5iduki t\u00fc\u00fcp", "1"), valik("Aasta", "*"), valik(NAITAJA, "2"), valik("Kuu", as.character(1:12)))
    seeria(per_m(d$Aasta, d$Kuu), d$value, alates = ALGUS - 1)
  }, c(0, 1e5))

  # ---- Valitsemissektori võlg (RR061) ja ettevõtete T&A (TD052) -------------
  lisa("volg_a", function() { d <- px_paring("RR061", valik(NAITAJA, "2"), valik("Aasta", "*")); seeria(d$Aasta, d$value, alates = 2010) }, c(0, 200))
  lisa("berd_a", function() { d <- px_paring("TD052", valik(NAITAJA, "BES_GDP_RD"), valik("Vaatlusperiood", "*")); seeria(d$Vaatlusperiood, d$value, alates = 2010) }, c(0, 10))

  # ---- Eurostat -------------------------------------------------------------
  lisa("fdi_q", function() eurostat_seeria("bop_iip6_q", list(
    geo = "EE", currency = "MIO_EUR", bop_item = "FA__D__F", sector10 = "S1", sectpart = "S1",
    stk_flow = "L_LE", partner = "WRL_REST", freq = "Q")), c(1, 1e6))
  lisa("elekter_h", function() eurostat_seeria("nrg_pc_205", list(
    geo = "EE", nrg_cons = "TOT_KWH", tax = "X_TAX", currency = "EUR", unit = "KWH")), c(0, 2))
  # Kodumajapidamiste säästumäär (brutosäästu määr, %): ainult aasta, Eurostat ei avalda Eesti kohta kvartalirida
  lisa("saast_a", function() eurostat_seeria("nasa_10_ki", list(geo = "EE", na_item = "SRG_S14_S15", unit = "PC", freq = "A")), c(-50, 60))
  # Tehisintellekti kasutavad ettevõtted (>= 10 hõivatut), % ettevõtetest
  for (k in list(c("EE", "ee"), c("EU27_2020", "eu"))) local({
    geo <- k[1]; nimi <- k[2]
    lisa(paste0("ai_a_", nimi), function() eurostat_seeria("isoc_eb_ai",
      list(geo = geo, indic_is = "E_AI_TANY", size_emp = "GE10", unit = "PC_ENT", freq = "A"),
      vali = list(nace_r2 = "C10-S951_X_K")), c(0, 100))
  })

  # ---- Majandususaldus ---------------------------------------------------------
  # Kui majandusplaaniindeksi arvutus õnnestus, kasutatakse sealset ESI-t
  # (Eurostat + hinnang Statistikaameti baromeetritest). Muidu ainult Eurostat.
  if (!is.null(esi_kuu) && nrow(esi_kuu)) {
    ametlik <- esi_kuu[esi_kuu$allikas == "Eurostat", ]
    hinnang <- esi_kuu[esi_kuu$allikas != "Eurostat", ]
    lisa("esi_m", function() seeria(norm_periood(ametlik$time), ametlik$Majandususaldus), c(30, 170))
    tul$esi_m_hinnang <- list(
      seeria = if (nrow(hinnang)) seeria(norm_periood(hinnang$time), hinnang$Majandususaldus) else list(periods = character(), values = numeric()),
      staatus = "uuendatud")
  } else {
    lisa("esi_m", function() eurostat_seeria("ei_bssi_m_r2", list(geo = "EE", indic = "BS-ESI-I", s_adj = "SA", freq = "M")), c(30, 170))
    tul$esi_m_hinnang <- list(seeria = vana$esi_m_hinnang, staatus = "vana")
  }

  list(
    uuendatud = aeg_utc(),
    seeriad   = lapply(tul, function(r) seeria_json(r$seeria)),
    staatus   = lapply(tul, function(r) list(
      staatus = r$staatus,
      viimane = if (length(r$seeria$periods)) tail(r$seeria$periods, 1) else NULL))
  )
}
