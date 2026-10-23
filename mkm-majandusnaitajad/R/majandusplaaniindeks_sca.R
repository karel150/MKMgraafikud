library(tidyverse)   # sisaldab lubridate'i (year, floor_date)
library(eurostat)
source("R/esi_eesti.R", encoding = "UTF-8")   # majandususalduse indikaator (ESI)

# =============================================================================
# Majandusplaaniindeks — hooajaliselt korrigeeritud tasemete põhjal
# (repo versioon: käivitatakse R/uuenda.R kaudu, tulemus docs/data/majandusplaan.json)
#
# 1. Iga näitaja: hooajaliselt (ja kalendri)korrigeeritud tase.
# 2. Kvartalimuutus: 100 * Δlog(tase).
# 3. Standardiseerimine: z = (muutus - keskmine) / standardhälve, kus keskmine
#    ja hälve hinnatakse ühisel perioodil, COVID-kvartalid välja jäetud.
# 4. Koondid:
#    - Koond_z: z-skooride keskmine (0 = keskmine kasvutempo, ühik = st.hälve)
#    - Majandusplaaniindeks: tasemeindeks, mille kvartalimuutus on näitajate
#      muutuste kaalutud keskmine, kaal ~ 1/st.hälve (Conference Boardi
#      meetod). Trend säilib, kuid volatiilsed näitajad ei domineeri.
#      Normeeritud: baasaasta keskmine = 100.
# =============================================================================

# ---- Seaded ------------------------------------------------------------------
base_year     <- 2023
base_index    <- 100
min_naitajaid <- 8      # mitu näitajat peab kvartalis olemas olema (8 = kõik)
algus_visual  <- as.Date("2022-01-01")

# Standardiseerimisparameetrite (keskmine, st.hälve) hindamise periood
std_algus     <- as.Date("2010-01-01")
std_lopp      <- as.Date(paste0(base_year, "-12-31"))
std_valja     <- 2020:2021   # COVID-aastad: ei mõjuta keskmist ega hälvet

# ---- Abifunktsioonid ---------------------------------------------------------

# Laeb ühe aegrea Eurostatist. Kontrollib, et tulemus poleks tühi ja et filtrid
# jätaksid alles täpselt ühe seeria (korduvad perioodid = mitu seeriat koos).
fetch_series <- function(id, filters) {
  df <- get_eurostat(id, filters = filters, time_format = "date", cache = FALSE) |>
    filter(!is.na(values)) |>
    select(time, values) |>
    arrange(time)
  if (nrow(df) == 0)           stop(id, ": päring ei tagastanud andmeid.")
  if (anyDuplicated(df$time))  stop(id, ": filtrid jätavad alles mitu seeriat.")
  df
}

# Kuuandmed -> kvartalikeskmine; poolikud kvartalid (< 3 kuud) jäetakse välja.
to_quarterly <- function(df) {
  df |>
    mutate(time = floor_date(time, "quarter")) |>
    group_by(time) |>
    summarise(kuid = n(), values = mean(values), .groups = "drop") |>
    filter(kuid == 3) |>
    select(-kuid)
}

# Indeks: tase / baasaasta keskmine * 100. Nõuab baasaastasse 4 kvartalit.
index_base_mean <- function(df, nimi) {
  base <- df$values[year(df$time) == base_year]
  if (length(base) != 4) {
    stop(nimi, ": baasaastas ", base_year, " on ", length(base), " kvartalit, vaja 4.")
  }
  df |>
    mutate(index = base_index * values / mean(base)) |>
    select(time, !!nimi := index)
}

# ---- Seeriate kirjeldus ------------------------------------------------------
# Kõik on tasemed. Aheldatud mahuindeksi baasaasta (CLV20, I20) ei mõjuta
# tulemust, sest iga seeria normeeritakse ümber 2023. aasta keskmisele.
spec <- tribble(
  ~nimi,                    ~id,              ~sagedus, ~filters,
  # SKP, eksport, põhivara: aheldatud mahud, mln €, SCA
  "SKP",                    "namq_10_gdp",    "Q", list(geo = "EE", na_item = "B1GQ", unit = "CLV20_MEUR", s_adj = "SCA", freq = "Q"),
  "Eksport",                "namq_10_gdp",    "Q", list(geo = "EE", na_item = "P6",   unit = "CLV20_MEUR", s_adj = "SCA", freq = "Q"),
  "Põhivarainvesteeringud", "namq_10_gdp",    "Q", list(geo = "EE", na_item = "P51G", unit = "CLV20_MEUR", s_adj = "SCA", freq = "Q"),
  # Tootlikkus hõivatu kohta: indeks 2020=100, SCA
  "Tootlikkus",             "namq_10_lp_ulc", "Q", list(geo = "EE", na_item = "RLPR_PER", unit = "I20", s_adj = "SCA", freq = "Q"),
  # Hõive (siseriiklik kontseptsioon): tuhat inimest, SCA
  "Hõive",                  "namq_10_a10_e",  "Q", list(geo = "EE", na_item = "EMP_DC", unit = "THS_PER", nace_r2 = "TOTAL", s_adj = "SCA", freq = "Q"),
  # Otseinvesteeringute kohustuste positsioon, mln € (perioodi lõpp).
  # Eurostat seda hooajaliselt ei korrigeeri; varudel on hooajalisus tühine.
  "Välisinvesteeringud",    "bop_iip6_q",     "Q", list(geo = "EE", currency = "MIO_EUR", bop_item = "FA__D__F", sector10 = "S1",
                                                        sectpart = "S1", stk_flow = "L_LE", partner = "WRL_REST", freq = "Q"),
  # Tööstuse tootmisvõimsuste rakendamine, %, SA
  "Tootmisvõimsused",       "ei_bsin_q_r2",   "Q", list(geo = "EE", indic = "BS-ICU-PC", s_adj = "SA", freq = "Q")
)

# ---- Andmete laadimine -------------------------------------------------------
tasemed <- pmap(spec, \(nimi, id, sagedus, filters) {
  df <- fetch_series(id, filters)
  if (sagedus == "M") df <- to_quarterly(df)
  df
}) |> set_names(spec$nimi)

# Majandususaldus (ESI): Eurostati ametlik ESI; kuud, mida Eurostat veel ei
# avalda, arvutatakse statistikaameti baromeetritest EK metoodikaga.
esi <- esi_eesti()
tasemed$Majandususaldus <- esi$kuu |>
  select(time, values = Majandususaldus) |>
  to_quarterly()

esi_kvartal <- esi$kuu |>
  mutate(time = floor_date(time, "quarter")) |>
  group_by(time) |>
  summarise(
    kuid            = n(),
    Majandususaldus = mean(Majandususaldus),
    hinnatud_kuid   = sum(allikas != "Eurostat"),
    .groups = "drop"
  )

naitajad <- names(tasemed)

# Δlog eeldab katkematut kvartalirida igas seerias
iwalk(tasemed, \(df, nimi) {
  if (!all(diff(df$time) %in% 89:92)) stop(nimi, ": kvartalireas on auk.")
})

# Laiaks tabeliks: üks veerg näitaja kohta
lai <- function(lst) reduce(lst, full_join, by = "time") |> arrange(time)

# Üksikute näitajate indeksid (baasaasta keskmine = 100) — graafikuteks
indeksid <- imap(tasemed, index_base_mean) |> lai()

# ---- Kvartalimuutused ja standardiseerimine ----------------------------------
muutused <- tasemed |>
  imap(\(df, nimi) transmute(df, time, !!nimi := 100 * (log(values) - log(lag(values))))) |>
  lai()

std_mask <- with(muutused, time >= std_algus & time <= std_lopp & !year(time) %in% std_valja)

std_param <- muutused[std_mask, ] |>
  pivot_longer(-time, names_to = "naitaja", values_to = "d") |>
  group_by(naitaja) |>
  summarise(n = sum(!is.na(d)), keskmine = mean(d, na.rm = TRUE), sd = sd(d, na.rm = TRUE)) |>
  mutate(kaal = (1 / sd) / sum(1 / sd)) |>
  arrange(match(naitaja, naitajad))

if (any(std_param$n < 20)) {
  warning("Vähe vaatlusi standardiseerimiseks: ",
          paste(std_param$naitaja[std_param$n < 20], collapse = ", "))
}

D  <- as.matrix(muutused[naitajad])
mu <- set_names(std_param$keskmine, std_param$naitaja)[naitajad]
s  <- set_names(std_param$sd,       std_param$naitaja)[naitajad]
w  <- set_names(std_param$kaal,     std_param$naitaja)[naitajad]

Z <- sweep(sweep(D, 2, mu, `-`), 2, s, `/`)
z_skoorid <- bind_cols(time = muutused$time, as_tibble(Z))

# ---- Koondid -----------------------------------------------------------------
n_naitajaid <- rowSums(!is.na(D))
piisav      <- n_naitajaid >= min_naitajaid

# 1) z-skooride keskmine
koond_z <- if_else(piisav, rowMeans(Z, na.rm = TRUE), NA_real_)

# 2) Volatiilsusega kaalutud kvartalimuutus (log-punktides); kui mõni näitaja
#    puudub (min_naitajaid < 8), normeeritakse kaalud olemasolevatele.
kaalud_olemas <- sweep(!is.na(D), 2, w, `*`)
koond_dlog    <- rowSums(sweep(D, 2, w, `*`), na.rm = TRUE) / rowSums(kaalud_olemas)
koond_dlog[!piisav] <- NA

# Tasemeindeks: kumuleerime muutused alates esimesest kättesaadavast kvartalist
ok    <- !is.na(koond_dlog)
algus <- which(ok)[1]
lopp  <- max(which(ok))
if (any(!ok[algus:lopp])) stop("Koondi kvartalimuutuste reas on auk.")

tase <- rep(NA_real_, length(ok))
tase[(algus - 1):lopp] <- exp(cumsum(c(0, koond_dlog[algus:lopp])) / 100)

baas <- tase[year(muutused$time) == base_year]
if (length(baas) != 4 || anyNA(baas)) stop("Koondindeksil puudub täielik baasaasta.")
tase <- base_index * tase / mean(baas)

kokku <- tibble(
  time                   = muutused$time,
  n_naitajaid            = n_naitajaid,
  Majandusplaaniindeks   = tase,
  `Koond_kv_muutus_%`    = (exp(koond_dlog / 100) - 1) * 100,
  `Koond_aasta_muutus_%` = (tase / lag(tase, 4) - 1) * 100,
  Koond_z                = koond_z
) |>
  left_join(indeksid, by = "time") |>
  left_join(select(esi_kvartal, time, ESI_hinnatud_kuid = hinnatud_kuid), by = "time")

# Visualiseerimiseks
andmed <- kokku |> filter(time >= algus_visual)

# ---- Andmed veebilehe jaoks ---------------------------------------------------
# Repo versioon: Exceli, RDS-i ja PNG/PDF jooniste asemel kirjutatakse
# docs/data/majandusplaan.json (funktsioon on failis R/abi.R).
kirjuta_majandusplaan(
  kokku     = kokku,
  naitajad  = naitajad,
  esi       = esi,
  base_year = base_year,
  std_algus = std_algus,
  std_lopp  = std_lopp,
  std_valja = std_valja
)
