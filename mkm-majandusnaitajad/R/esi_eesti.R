# =============================================================================
# Eesti majandususalduse indikaator (ESI) — Euroopa Komisjoni metoodika
#
# Andmed:
#   * Eurostat: ametlik ESI ja 15 komponendi saldod (SA ja NSA) kuni viimase
#     avaldatud kuuni (operaatori vahetuse tõttu praegu 2026M04).
#   * Statistikaamet (BAR01–BAR05): samad küsimused alates 2026M05, hooajaliselt
#     korrigeerimata saldod.
#
# Metoodika (EK, Joint Harmonised EU Programme of Business and Consumer Surveys):
#   1. 15 komponendi SA-saldod; varude seeriad (tööstus, jaekaubandus) märgiga -.
#   2. Iga komponent standardiseeritakse: z = (x - keskmine) / st.hälve.
#   3. Kaalutud keskmine; sektorite kaalud tööstus 40, teenindus 30, tarbijad 20,
#      jaekaubandus 5, ehitus 5, jagatud võrdselt sektori komponentide vahel.
#      Puuduvate komponentide korral normeeritakse kaalud olemasolevatele.
#   4. Skaleerimine: keskmine 100, st.hälve 10.
#
# Statistikaameti NSA-saldod korrigeeritakse hooajaliselt Eurostati andmetest
# hinnatud hooajateguritega (NSA - SA sama kalendrikuu keskmine viimastel
# aastatel). Lõpuks kalibreeritakse rekonstrueeritud ESI ametliku ESI vastu
# (lineaarne regressioon kattuval perioodil). See peab andma a ≈ 0, b ≈ 1 ja
# väikese vea; diagnostika trükitakse konsooli.
# =============================================================================

library(tidyverse)
library(eurostat)
library(httr)
library(jsonlite)

# ---- Seaded ------------------------------------------------------------------
esi_seaded <- list(
  statee_api   = "https://andmed.stat.ee/api/v1/et/stat/",
  statee_on_sa = FALSE,  # TRUE, kui statistikaameti andmed on juba SA
  sf_aastad    = 5,      # mitme viimase täisaasta põhjal hooajategurid
  min_kaal     = 0.8,    # min. kaetud kaalu osa, et kuu ESI arvutada
  kal_algus    = as.Date("2010-01-01")  # kalibreerimise algus
)

# ---- Komponendid -------------------------------------------------------------
esi_komponendid <- tribble(
  ~sektor,        ~eurostat_id,   ~indic,        ~statee_tabel, ~statee_kood,      ~suund,
  "T\u00f6\u00f6stus",      "ei_bsin_m_r2", "BS-IOB",      "BAR03",       "INDU_TOT_2_B_M",   1,  # tellimused praegu
  "T\u00f6\u00f6stus",      "ei_bsin_m_r2", "BS-ISFP",     "BAR03",       "INDU_TOT_4_B_M",  -1,  # valmistoodete varud
  "T\u00f6\u00f6stus",      "ei_bsin_m_r2", "BS-IPE",      "BAR03",       "INDU_TOT_5_B_M",   1,  # toodang järgm. 3 kuud
  "Teenindus",    "ei_bsse_m_r2", "BS-SABC",     "BAR05",       "SERV_TOT_1_B_M",   1,  # äriolukord viim. 3 kuud
  "Teenindus",    "ei_bsse_m_r2", "BS-SARM",     "BAR05",       "SERV_TOT_2_B_M",   1,  # nõudlus viim. 3 kuud
  "Teenindus",    "ei_bsse_m_r2", "BS-SAEM",     "BAR05",       "SERV_TOT_3_B_M",   1,  # nõudlus järgm. 3 kuud
  "Tarbijad",     "ei_bsco_m",    "BS-FS-LY",    "BAR01",       "CONS_TOT_1_B_M",   1,  # leibkonna olukord viim. 12 k
  "Tarbijad",     "ei_bsco_m",    "BS-FS-NY",    "BAR01",       "CONS_TOT_2_B_M",   1,  # leibkonna olukord järgm. 12 k
  "Tarbijad",     "ei_bsco_m",    "BS-GES-NY",   "BAR01",       "CONS_TOT_4_B_M",   1,  # riigi olukord järgm. 12 k
  "Tarbijad",     "ei_bsco_m",    "BS-MP-NY",    "BAR01",       "CONS_TOT_9_B_M",   1,  # püsikaupade ostud järgm. 12 k
  "Jaekaubandus", "ei_bsrt_m_r2", "BS-RPBS",     "BAR04",       "RETA_TOT_1_B_M",   1,  # müük viim. 3 kuud
  "Jaekaubandus", "ei_bsrt_m_r2", "BS-RAS",      "BAR04",       "RETA_TOT_2_B_M",  -1,  # kaubavarud
  "Jaekaubandus", "ei_bsrt_m_r2", "BS-REBS",     "BAR04",       "RETA_TOT_4_B_M",   1,  # müük järgm. 3 kuud
  "Ehitus",       "ei_bsbu_m_r2", "BS-COB-BAL",  "BAR02",       "BUIL_TOT_3_B_M",   1,  # tellimuste portfell
  "Ehitus",       "ei_bsbu_m_r2", "BS-CEME-BAL", "BAR02",       "BUIL_TOT_4_B_M",   1   # töötajate arv järgm. 3 k
)

esi_sektori_kaal <- setNames(c(40, 30, 20, 5, 5),
                             c("T\u00f6\u00f6stus", "Teenindus", "Tarbijad", "Jaekaubandus", "Ehitus"))

esi_komponendid <- esi_komponendid |>
  add_count(sektor, name = "n_sektoris") |>
  mutate(kaal = esi_sektori_kaal[sektor] / n_sektoris) |>
  select(-n_sektoris)

# ---- Andmete laadimine -------------------------------------------------------

# Eurostati komponendid: SA ja NSA, pikk tabel (indic, time, SA, NSA)
esi_lae_eurostat <- function(komp = esi_komponendid) {
  komp |>
    distinct(eurostat_id) |>
    pull() |>
    map(\(id) {
      get_eurostat(id, time_format = "date", cache = FALSE,
                   filters = list(geo = "EE", freq = "M", s_adj = c("SA", "NSA"),
                                  indic = komp$indic[komp$eurostat_id == id]))
    }) |>
    bind_rows() |>
    filter(!is.na(values)) |>
    select(indic, s_adj, time, values) |>
    pivot_wider(names_from = s_adj, values_from = values, values_fn = \(x) {
      if (length(x) > 1) stop("Eurostat: mitu väärtust ühe indic/s_adj/time kohta.")
      x
    })
}

esi_lae_eurostat_ametlik <- function() {
  get_eurostat("ei_bssi_m_r2", time_format = "date", cache = FALSE,
               filters = list(geo = "EE", freq = "M", s_adj = "SA", indic = "BS-ESI-I")) |>
    filter(!is.na(values)) |>
    select(time, esi_ametlik = values) |>
    arrange(time)
}

# JSON-stat2 -> pikk tabel (üks veerg dimensiooni kohta + value)
parse_jsonstat2 <- function(js) {
  dims  <- unlist(js$id)
  koodid <- map(dims, \(d) {
    idx <- unlist(js$dimension[[d]]$category$index)
    if (is.null(idx)) return(names(unlist(js$dimension[[d]]$category$label)))
    if (is.null(names(idx))) return(as.character(idx))   # massiiv: ["k1","k2",...]
    names(sort(idx))                                     # objekt: {"k1":0,"k2":1}
  }) |> set_names(dims)
  # JSON-stat väärtused on reajärjestuses: viimane dimensioon muutub kiiremini
  grid <- expand.grid(rev(koodid), stringsAsFactors = FALSE)[, dims, drop = FALSE]
  vals <- js$value
  if (is.list(vals)) vals <- map_dbl(vals, \(v) if (is.null(v)) NA_real_ else as.numeric(v))
  if (length(vals) != nrow(grid)) stop("JSON-stat: väärtuste arv ei klapi dimensioonidega.")
  as_tibble(grid) |> mutate(value = as.numeric(vals))
}

# Üks statistikaameti PxWeb tabel -> (statee_kood, time, NSA)
esi_lae_statee_tabel <- function(tabel, koodid, api = esi_seaded$statee_api) {
  paring <- list(
    query = list(
      list(code = "Näitaja",        selection = list(filter = "item", values = as.list(koodid))),
      list(code = "Vaatlusperiood", selection = list(filter = "all",  values = list("*")))
    ),
    response = list(format = "json-stat2")
  )
  vastus <- POST(paste0(api, tabel), body = toJSON(paring, auto_unbox = TRUE),
                 content_type_json(), encode = "raw")
  stop_for_status(vastus, task = paste("laadida tabelit", tabel))
  js <- fromJSON(content(vastus, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

  df <- parse_jsonstat2(js)
  aeg_veerg <- names(df)[map_lgl(df, \(v) is.character(v) && all(grepl("^\\d{4}M\\d{2}$", v)))]
  kood_veerg <- setdiff(names(df), c(aeg_veerg, "value"))
  stopifnot(length(aeg_veerg) == 1, length(kood_veerg) == 1)

  df |>
    transmute(
      statee_kood = .data[[kood_veerg]],
      time        = as.Date(paste0(substr(.data[[aeg_veerg]], 1, 4), "-",
                                   substr(.data[[aeg_veerg]], 6, 7), "-01")),
      NSA         = value
    ) |>
    filter(!is.na(NSA))
}

esi_lae_statee <- function(komp = esi_komponendid) {
  komp |>
    group_by(statee_tabel) |>
    group_map(\(g, k) esi_lae_statee_tabel(k$statee_tabel, g$statee_kood)) |>
    bind_rows()
}

# ---- Arvutus -----------------------------------------------------------------

# Hooajategurid: NSA - SA, sama kalendrikuu keskmine viimase n täisaasta jooksul
esi_hooajategurid <- function(eu, n_aastat = esi_seaded$sf_aastad) {
  eu |>
    filter(!is.na(SA), !is.na(NSA)) |>
    group_by(indic) |>
    mutate(viimane_taisaasta = max(year(time[month(time) == 12]))) |>
    filter(year(time) > viimane_taisaasta - n_aastat, year(time) <= viimane_taisaasta) |>
    group_by(indic, kuu = month(time)) |>
    summarise(sf = mean(NSA - SA), n = n(), .groups = "drop")
}

# Komponentide paneel: Eurostati SA, sellele järgnevad kuud statistikaametist
esi_paneel <- function(eu, statee, komp = esi_komponendid, seaded = esi_seaded) {
  sf <- esi_hooajategurid(eu, seaded$sf_aastad)

  st <- statee |>
    inner_join(select(komp, indic, statee_kood), by = "statee_kood") |>
    mutate(kuu = month(time)) |>
    left_join(sf, by = c("indic", "kuu")) |>
    mutate(SA = if (seaded$statee_on_sa) NSA else NSA - sf)

  # Kontroll: kui perioodid kattuvad (EK on andmed tagantjärele avaldanud),
  # näita, kui hästi statistikaameti NSA klapib Eurostati NSA-ga.
  kattuv <- inner_join(st, eu, by = c("indic", "time"), suffix = c("_ee", "_eu"))
  if (nrow(kattuv) > 0) {
    message(sprintf("Kattuvaid vaatlusi: %d; NSA keskmine abs. erinevus %.2f pp",
                    nrow(kattuv), mean(abs(kattuv$NSA_ee - kattuv$NSA_eu), na.rm = TRUE)))
  }

  eu_viimane <- eu |> filter(!is.na(SA)) |> group_by(indic) |> summarise(viimane = max(time))

  bind_rows(
    eu |> filter(!is.na(SA)) |> transmute(indic, time, SA, allikas = "Eurostat"),
    st |>
      left_join(eu_viimane, by = "indic") |>
      filter(is.na(viimane) | time > viimane) |>
      transmute(indic, time, SA, allikas = "Statistikaamet")
  ) |>
    filter(!is.na(SA)) |>
    arrange(indic, time)
}

# ESI EK metoodikaga. std_lopp: viimane kuu, mis läheb standardiseerimisse
# (ametliku ESI viimane kuu, et hinnangukuud ei muudaks parameetreid).
esi_arvuta <- function(paneel, std_lopp, komp = esi_komponendid, min_kaal = esi_seaded$min_kaal) {
  lai <- paneel |>
    inner_join(select(komp, indic, suund), by = "indic") |>
    mutate(x = suund * SA) |>
    select(time, indic, x) |>
    pivot_wider(names_from = indic, values_from = x) |>
    arrange(time)

  X <- as.matrix(lai[komp$indic])
  std <- lai$time <= std_lopp
  mu <- colMeans(X[std, , drop = FALSE], na.rm = TRUE)
  s  <- apply(X[std, , drop = FALSE], 2, sd, na.rm = TRUE)
  Z  <- sweep(sweep(X, 2, mu, `-`), 2, s, `/`)

  w <- set_names(komp$kaal, komp$indic)
  W <- sweep(!is.na(Z), 2, w, `*`)
  kaetud <- rowSums(W) / sum(w)
  raw <- rowSums(sweep(Z, 2, w, `*`), na.rm = TRUE) / rowSums(W)
  raw[kaetud < min_kaal] <- NA

  esi <- 100 + 10 * (raw - mean(raw[std], na.rm = TRUE)) / sd(raw[std], na.rm = TRUE)
  tibble(time = lai$time, esi_rekonstrueeritud = esi, kaetud_kaal = kaetud)
}

# ---- Põhifunktsioon ----------------------------------------------------------
esi_eesti <- function(seaded = esi_seaded, komp = esi_komponendid) {
  eu      <- esi_lae_eurostat(komp)
  ametlik <- esi_lae_eurostat_ametlik()
  statee  <- esi_lae_statee(komp)

  puudu <- setdiff(komp$indic, unique(eu$indic))
  if (length(puudu)) stop("Eurostatist puuduvad komponendid: ", paste(puudu, collapse = ", "))

  paneel  <- esi_paneel(eu, statee, komp, seaded)
  std_lopp <- max(ametlik$time)
  rek     <- esi_arvuta(paneel, std_lopp, komp, seaded$min_kaal)

  # Kalibreerimine ametliku ESI vastu
  kal <- inner_join(ametlik, rek, by = "time") |>
    filter(time >= seaded$kal_algus, !is.na(esi_rekonstrueeritud))
  mudel <- lm(esi_ametlik ~ esi_rekonstrueeritud, data = kal)
  jaak  <- residuals(mudel)
  diag  <- tibble(
    vaatlusi = nrow(kal), a = coef(mudel)[[1]], b = coef(mudel)[[2]],
    R2 = summary(mudel)$r.squared, RMSE = sqrt(mean(jaak^2)), max_abs_viga = max(abs(jaak)),
    viimane_ametlik = std_lopp
  )
  message(sprintf(
    "ESI rekonstrueerimine: R² = %.4f, RMSE = %.2f, max viga = %.2f (a = %.2f, b = %.3f, n = %d)",
    diag$R2, diag$RMSE, diag$max_abs_viga, diag$a, diag$b, diag$vaatlusi))
  if (diag$R2 < 0.98) warning("Rekonstrueeritud ESI klapib ametlikuga halvasti — kontrolli komponente.")

  kuu <- rek |>
    full_join(ametlik, by = "time") |>
    arrange(time) |>
    mutate(
      esi_hinnang     = predict(mudel, newdata = pick(esi_rekonstrueeritud)),
      Majandususaldus = coalesce(esi_ametlik, esi_hinnang),
      allikas         = case_when(!is.na(esi_ametlik)      ~ "Eurostat",
                                  !is.na(Majandususaldus)  ~ "Hinnang (Statistikaamet)",
                                  TRUE                     ~ NA_character_)
    ) |>
    filter(!is.na(Majandususaldus)) |>
    select(time, Majandususaldus, allikas, esi_ametlik, esi_hinnang, esi_rekonstrueeritud, kaetud_kaal)

  hinnangukuud <- filter(kuu, allikas != "Eurostat")
  if (nrow(hinnangukuud)) {
    message("Hinnatud kuud: ", paste(format(hinnangukuud$time, "%Y-%m"), collapse = ", "))
    if (any(hinnangukuud$kaetud_kaal < 1)) {
      message("  osaliste komponentidega: ",
              paste(format(hinnangukuud$time[hinnangukuud$kaetud_kaal < 1], "%Y-%m"), collapse = ", "))
    }
  }

  list(
    kuu         = kuu,
    diagnostika = diag,
    komponendid = paneel |>
      left_join(select(komp, sektor, indic, statee_kood, suund, kaal), by = "indic") |>
      relocate(sektor, indic, statee_kood)
  )
}
