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

# ---- Kokkuvõte --------------------------------------------------------------
if (length(hoiatused)) {
  writeLines(hoiatused, "hoiatused.txt", useBytes = TRUE)
  message("\nValmis, ", length(hoiatused), " hoiatust (vt hoiatused.txt).")
} else {
  if (file.exists("hoiatused.txt")) file.remove("hoiatused.txt")
  message("\nValmis, hoiatusi pole.")
}
