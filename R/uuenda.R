# =============================================================================
# Peaskript: uuendab kõik andmefailid kaustas docs/data.
#
# Käivitamine repo juurkaustast:   Rscript R/uuenda.R
#
# Kui mõni osa ebaõnnestub, jääb alles selle osa eelmine fail ja hoiatus
# kirjutatakse faili hoiatused.txt. GitHub Actions saadab siis teavituse.
# =============================================================================

options(warn = 1, timeout = 300)
Sys.setenv(TZ = "Europe/Tallinn")
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))  # täpitähed tabelite koodides
if (!file.exists("R/uuenda.R")) stop("Käivita skript repo juurkaustast: Rscript R/uuenda.R")

source("R/abi.R", encoding = "UTF-8")
source("R/naitajad.R", encoding = "UTF-8")
source("R/indeksid.R", encoding = "UTF-8")
source("R/vordlus.R", encoding = "UTF-8")

# ---- 1. Majandusplaaniindeks (sinu metoodika, sh ESI hinnang) ----------------
message("== Majandusplaaniindeks ==")
mp <- new.env()
mp_ok <- tryCatch({
  sys.source("R/majandusplaaniindeks_sca.R", envir = mp, keep.source = FALSE)
  TRUE
}, error = function(e) {
  hoiata("Majandusplaaniindeks: arvutus ebaõnnestus (", conditionMessage(e), "). Jäetakse alles eelmine versioon.")
  FALSE
})
esi_kuu <- if (mp_ok && !is.null(mp$esi)) as.data.frame(mp$esi$kuu) else NULL

# ---- 2. Eesti majanduse näitajad --------------------------------------------
message("== Majandusnäitajad ==")
naitajad <- koosta_naitajad(esi_kuu)
kirjuta_json(naitajad, "naitajad.json")

# ---- 3. Edetabelid -------------------------------------------------------------
message("== Edetabelid ==")
tryCatch(kirjuta_json(koosta_indeksid(), "indeksid.json"),
         error = function(e) hoiata("Edetabelid: ", conditionMessage(e), ". Jäetakse alles eelmine versioon."))

# ---- 4. Rahvusvaheline võrdlus (Eurostat) ---------------------------------------
message("== Rahvusvaheline võrdlus ==")
tryCatch(kirjuta_json(koosta_vordlus(), "vordlus.json"),
         error = function(e) hoiata("Rahvusvaheline võrdlus: ", conditionMessage(e), ". Jäetakse alles eelmine versioon."))

# ---- 5. Teiste asutuste ülevaated (käsitsi uuendatav nimekiri) -----------------
tryCatch({
  u <- read.csv("andmed/ulevaated.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
  if (!all(c("grupp", "pealkiri", "url") %in% names(u))) stop("puuduvad veerud grupp, pealkiri, url")
  u <- u[nzchar(trimws(u$pealkiri)) & nzchar(trimws(u$url)), c("grupp", "pealkiri", "url")]
  if (!all(grepl("^https?://", u$url))) stop("mõni link ei alga http(s)://")
  kirjuta_json(list(ulevaated = u), "ulevaated.json")
}, error = function(e) hoiata("Ülevaadete nimekiri (andmed/ulevaated.csv): ", conditionMessage(e), ". Jäetakse alles eelmine versioon."))

# ---- Viimase kontrolli aeg ----------------------------------------------------
# Andmefailid muutuvad ainult siis, kui muutusid andmed (vt kirjuta_json). Et leht
# saaks siiski näidata, millal andmeid viimati kontrolliti, kirjutatakse see eraldi
# väikesesse faili. seis.json on .gitignore'is: see läheb avaldatavale lehele, aga
# mitte repo ajalukku.
write_json(list(kontrollitud = aeg_utc(), hoiatusi = length(hoiatused)),
           file.path(DATA_DIR, "seis.json"), auto_unbox = TRUE)

# ---- Kokkuvõte --------------------------------------------------------------
if (length(hoiatused)) {
  writeLines(hoiatused, "hoiatused.txt", useBytes = TRUE)
  message("\nValmis, ", length(hoiatused), " hoiatust (vt hoiatused.txt).")
} else {
  if (file.exists("hoiatused.txt")) file.remove("hoiatused.txt")
  message("\nValmis, hoiatusi pole.")
}
