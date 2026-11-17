# =============================================================================
# TAIE arengukava mõõdikud -> docs/data/taie.json
#
# Mõõdikud pärinevad "Eesti teadus- ja arendustegevuse, innovatsiooni ning
# ettevõtluse arengukava 2021-2035" lisast 3. Mõõdikute nimed, sihttasemed ja
# kirjeldused on failis andmed/taie.csv; see moodul laeb nende kõrvale aegread.
#
# Osal mõõdikutel masinloetavat allikat ei ole (nt lepingulise TA maht tuleb
# baasfinantseerimise alusandmetest, Doing Business lõpetati 2021). Nende
# viimane teada olev tase on samas CSV-s ja leht näitab neid ilma graafikuta.
# =============================================================================

TAIE_CSV <- "andmed/taie.csv"

koosta_taie <- function(naitajad = NULL, indeksid = NULL) {
  # Eelmise faili seeriad: jsonlite teeb neist pesastatud loendiveerud, mis tuleb lahti pakkida
  vana <- loe_json("taie.json")
  vanaS <- NULL
  if (is.data.frame(vana$moodikud) && "seeria" %in% names(vana$moodikud)) {
    s <- vana$moodikud$seeria
    vanaS <- stats::setNames(lapply(seq_len(nrow(vana$moodikud)), function(i) {
      p <- s$periods[[i]]; v <- s$values[[i]]
      if (is.null(p) || !length(p)) NULL else list(periods = as.character(p), values = as.numeric(v))
    }), vana$moodikud$id)
  }

  def <- utils::read.csv(TAIE_CSV, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  tul <- list()
  lisa <- function(id, f, vahemik = c(-Inf, Inf)) {
    r <- turvaliselt(paste0("TAIE ", id), f, vanaS[[id]], vahemik)
    tul[[id]] <<- r$seeria
  }
  # Juba arvutatud seeria majandusnäitajate failist
  olemas <- function(nimi) {
    s <- if (is.null(naitajad)) NULL else naitajad$seeriad[[nimi]]
    if (is.null(s) || !length(s$periods)) NULL else list(periods = as.character(s$periods), values = as.numeric(s$values))
  }

  # ---- Üldeesmärk -------------------------------------------------------------
  # Riigieelarveline TA rahastus (GBARD) osakaaluna SKP-st
  lisa("yld1", function() eurostat_seeria("gba_nabsfin07",
    list(geo = "EE", unit = "PC_GDP", nabs07 = "TOTAL", freq = "A")), c(0, 5))

  # Erasektori TA kulutused: sama rida, mida näitab majandusnäitajate leht
  lisa("yld2", function() {
    s <- olemas("berd_a")
    if (is.null(s)) stop("berd_a puudub majandusnäitajate failis")
    s
  }, c(0, 10))

  # Nominaalne tööjõu tootlikkus hõivatu kohta, % EL-27 keskmisest
  lisa("yld3", function() eurostat_seeria("nama_10_lp_ulc",
    list(geo = "EE", na_item = "NLPR_PER", unit = "PC_EU27_2020_MPPS_CP", freq = "A")), c(20, 200))

  # Koht Euroopa innovatsiooni tulemustabelis: sama allikas, mida hoitakse indeksite lehel
  # Indeksite failis on väljad ingliskeelsed: year, rank, of, badge
  tul$yld4 <- local({
    ix <- if (is.null(indeksid)) loe_json("indeksid.json")$indeksid else indeksid
    r <- if (is.null(ix) || !is.data.frame(ix) || !"id" %in% names(ix)) NULL else ix[!is.na(ix$id) & ix$id == "eis", , drop = FALSE]
    if (is.null(r) || nrow(r) != 1 || is.na(r$rank[1]) || is.na(r$year[1])) {
      hoiata("TAIE yld4: Euroopa innovatsiooni tulemustabeli kohta ei leitud indeksite failist.")
      NULL
    } else {
      list(periods = as.character(r$year[1]), values = as.numeric(r$rank[1]),
           riike = if (is.na(r$of[1])) NULL else as.numeric(r$of[1]),
           silt = if (is.na(r$badge[1])) NULL else as.character(r$badge[1]))
    }
  })

  # ---- Teadmussiire -----------------------------------------------------------
  # Investeeringud intellektuaalomandiõigustega seotud toodetesse, % SKP-st
  lisa("t1", function() {
    d <- px_paring("RAA0062", valik("Aasta", "*"), valik("Vara liik", "9"),
                   valik("Tegevusala (EMTAK 2008)", "1"), valik("Näitaja", "6"))
    seeria(d$Aasta, d$value)
  }, c(0, 20))

  # Teadlased ja insenerid ettevõtlussektoris ja kasumitaotluseta erasektoris,
  # täistööajale taandatud, tuhande elaniku kohta
  lisa("t2", function() {
    d <- px_paring("TD01", valik("Näitaja", "EMPLOYEES_FTE"),
                   valik("Institutsionaalne sektor", c("BES", "PNP")),
                   valik("Ametikoha liik", "RSE"), valik("Sugu", "_T"),
                   valik("Vaatlusperiood", "*"))
    d <- d[!is.na(d$value), ]
    arv <- stats::aggregate(value ~ Vaatlusperiood, data = d, FUN = sum)
    # mõlema sektori andmed peavad olemas olema, muidu jääks aasta poolikuks
    taielik <- stats::aggregate(value ~ Vaatlusperiood, data = d, FUN = length)
    arv <- arv[arv$Vaatlusperiood %in% taielik$Vaatlusperiood[taielik$value == 2], ]

    r <- px_paring("RV0291U", valik("Elukoht", "1"), valik("Aasta", "*"), valik("Näitaja", "1"))
    rahvas <- stats::setNames(r$value, as.character(r$Aasta))
    aastad <- intersect(as.character(arv$Vaatlusperiood), names(rahvas))
    seeria(aastad, arv$value[match(aastad, as.character(arv$Vaatlusperiood))] / rahvas[aastad] * 1000)
  }, c(0, 20))

  # ---- Ettevõtluskeskkond -----------------------------------------------------
  # Kaupade ja teenuste eksport jooksevhindades, mld eurot
  lisa("e3", function() {
    k <- olemas("ekspK_a_cur"); t <- olemas("ekspT_a_cur")
    if (is.null(k) || is.null(t)) stop("ekspordi read puuduvad majandusnäitajate failis")
    aastad <- intersect(k$periods, t$periods)
    seeria(aastad, (k$values[match(aastad, k$periods)] + t$values[match(aastad, t$periods)]) / 1000)
  }, c(0, 200))

  # ---- Kokkupanek -------------------------------------------------------------
  # Viimased väärtused logisse: nii on käivituse logist näha, kas numbrid on mõistlikud
  for (id in names(tul)) {
    s <- tul[[id]]
    if (!is.null(s) && length(s$periods))
      message(sprintf("  %s: %s perioodi, viimane %s = %s", id, length(s$periods),
                      utils::tail(s$periods, 1), signif(utils::tail(s$values, 1), 4)))
  }
  arv <- function(x) if (is.na(x) || !nzchar(x)) NULL else as.numeric(x)
  tekst <- function(x) if (is.na(x) || !nzchar(x)) NULL else x
  moodikud <- lapply(seq_len(nrow(def)), function(i) {
    d <- as.list(def[i, ])
    s <- tul[[d$id]]
    list(id = d$id, eesmark = d$eesmark, nimi = d$nimi, yhik = d$yhik, skaala = tekst(d$skaala),
         siht = arv(d$siht), siht_tekst = d$siht_tekst, suund = as.numeric(d$suund),
         allikas = d$allikas, url = tekst(d$url), kirjeldus = tekst(d$kirjeldus), markus = tekst(d$markus),
         seeria = if (is.null(s) || is.null(s$periods)) NULL else seeria_json(s),
         riike = if (is.null(s)) NULL else s$riike, silt = if (is.null(s)) NULL else s$silt,
         # käsitsi hoitav viimane teada olev tase (kui aegrida puudub)
         kasitsi = if (is.null(arv(d$aasta))) NULL else
           list(aasta = arv(d$aasta), v = arv(d$vaartus), tekst = tekst(d$vaartus_tekst)))
  })
  puudu <- vapply(moodikud, function(m) is.null(m$seeria) && is.null(m$kasitsi), logical(1))
  if (any(puudu)) hoiata("TAIE: andmeteta mõõdikud: ", paste(def$id[puudu], collapse = ", "))

  list(uuendatud = aeg_utc(),
       eesmargid = list(
         list(id = "yld", nimi = "Üldeesmärk",
              tekst = "Eesti teadus, arendustegevus, innovatsioon ja ettevõtlus suurendavad koostoimes Eesti ühiskonna heaolu ja majanduse tootlikkust, pakkudes konkurentsivõimelisi ja kestlikke lahendusi Eesti ja maailma arenguvajadustele."),
         list(id = "teadmus", nimi = "Teadmussiire",
              tekst = "Eesti areng tugineb teadmuspõhistele ja innovaatilistele lahendustele."),
         list(id = "teadus", nimi = "Teadussüsteem",
              tekst = "Eesti teadus on kõrgetasemeline, mõjus ja mitmekesine."),
         list(id = "ettevotlus", nimi = "Ettevõtluskeskkond",
              tekst = "Eesti ettevõtluskeskkond soodustab ettevõtlikkust ning teadmusmahuka ettevõtluse teket ja kasvu, kõrgema lisandväärtusega toodete ja teenuste loomist ja eksporti ning investeeringuid kõigis Eesti piirkondades.")),
       moodikud = moodikud)
}
