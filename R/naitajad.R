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

  # ---- SKP (RAA0012) -------------------------------------------------------
  skp <- mitu(function() px_paring("RAA0012",
    valik("Aasta", "*"), valik("Kvartal", c("1", RN)),
    valik("Sesoonne korrigeerimine", "1"), valik(NAITAJA, "5")))
  lisa("skp_q", function() { d <- skp(); d <- d[d$Kvartal != "1", ]; seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("skp_a", function() { d <- skp(); d <- d[d$Kvartal == "1", ]; seeria(d$Aasta, d$value) }, YOY)

  # ---- Eksport (RAA0061): 7 = kokku, 8 = kaubad, 9 = teenused ---------------
  eks <- mitu(function() px_paring("RAA0061",
    valik("Komponent", c("7", "8", "9")), valik("Aasta", "*"),
    valik("Kvartal", c("1", RN)), valik(NAITAJA, "4")))
  for (k in list(c("7", "eksp"), c("8", "ekspK"), c("9", "ekspT"))) local({
    kood <- k[1]; nimi <- k[2]
    lisa(paste0(nimi, "_q"), function() { d <- eks(); d <- d[d$Komponent == kood & d$Kvartal != "1", ]; seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
    lisa(paste0(nimi, "_a"), function() { d <- eks(); d <- d[d$Komponent == kood & d$Kvartal == "1", ]; seeria(d$Aasta, d$value) }, YOY)
  })

  # ---- Tootlikkus töötatud tunni kohta (RAL0013) ----------------------------
  # Kvartal: reaalmuutus aasta varem, SA (8). Aasta: reaalmuutus (6).
  ral <- mitu(function() px_paring("RAL0013",
    valik("Suhtarv", "2"), valik("Aasta", "*"), valik("Kvartal", c("1", RN)), valik(NAITAJA, c("6", "8"))))
  lisa("tootl_q", function() { d <- ral(); d <- d[d$Kvartal != "1" & d[[NAITAJA]] == "8", ]; seeria(per_q(d$Aasta, d$Kvartal), d$value) }, YOY)
  lisa("tootl_a", function() { d <- ral(); d <- d[d$Kvartal == "1" & d[[NAITAJA]] == "6", ]; seeria(d$Aasta, d$value) }, YOY)

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

  # ---- Töötuse määr (TT0160) -------------------------------------------------
  lisa("tootus_q", function() {
    d <- px_paring("TT0160", valik(NAITAJA, "UNEMP_RATE"), valik("Sugu", "T"),
                   valik("Vanuser\u00fchm", "Y15-74"), valik("Vaatlusperiood", "*"))
    seeria(d$Vaatlusperiood, d$value)
  }, c(0, 40))

  # ---- Valitsemissektori võlg (RR061) ja ettevõtete T&A (TD052) -------------
  lisa("volg_a", function() { d <- px_paring("RR061", valik(NAITAJA, "2"), valik("Aasta", "*")); seeria(d$Aasta, d$value, alates = 2010) }, c(0, 200))
  lisa("berd_a", function() { d <- px_paring("TD052", valik(NAITAJA, "BES_GDP_RD"), valik("Vaatlusperiood", "*")); seeria(d$Vaatlusperiood, d$value, alates = 2010) }, c(0, 10))

  # ---- Eurostat -------------------------------------------------------------
  lisa("fdi_q", function() eurostat_seeria("bop_iip6_q", list(
    geo = "EE", currency = "MIO_EUR", bop_item = "FA__D__F", sector10 = "S1", sectpart = "S1",
    stk_flow = "L_LE", partner = "WRL_REST", freq = "Q")), c(1, 1e6))
  lisa("elekter_h", function() eurostat_seeria("nrg_pc_205", list(
    geo = "EE", nrg_cons = "TOT_KWH", tax = "X_TAX", currency = "EUR", unit = "KWH")), c(0, 2))

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
