# Eesti majandus numbrites

Avalik näidikulaud. Avaleht on **Ülevaade**: automaatne kokkuvõte, majandususaldus, neli põhinäitajat, SKP kasv ja Eesti koht edetabelites, kõik samadest andmefailidest. Sealt ja vahekaartidelt pääseb viide vaatesse:

1. **Majandusplaani näitajad**: majandusplaaniindeks ja selle 8 alamnäitajat (tasemed, muutused, soojuskaart, andmetabel).
2. **Eesti majanduse graafikud**: 37 näitajat (neist 21 põhinäitajat) kuu-, kvartali- ja aastavaatega.
3. **Eesti positsioon indeksites**: 13 rahvusvahelist indeksit (MAPO to-do faili leht „2.1 Indeksid“), jaotatud põhi- ja täiendavateks.
4. **Rahvusvaheline võrdlus**: 26 näitajat Eurostati ja Euroopa Keskpanga andmetel Eesti, Läti, Leedu, Soome ja Rootsi kohta. Need on vahekaardi 2 näitajad, millel on Eurostatis võrreldav vaste (sh Statistikaameti näitajate vasted, nt ühtlustatud THI, tööjõukulu indeks).
5. **Teiste asutuste ülevaated**: lingid teiste asutuste majandusnäitajatele, prognoosidele ja analüüsidele.

Andmed uuendatakse automaatselt tööpäeviti GitHub Actionsiga. Leht on staatiline: HTML, JavaScript ja JSON-failid, ilma serverita. Graafikud on tehtud ECharts'iga (Apache 2.0).

## Kuidas see töötab

```
Statistikaamet, Eurostat ──► R/uuenda.R ──► docs/data/*.json ──► docs/index.html
andmed/indeksid*.csv     ──┘   (GitHub Actions,                  (GitHub Pages või
                                tööpäeviti 8.05)                  MKM-i server)
```

* **R** laadib andmed ja arvutab majandusplaaniindeksi (sinu metoodika, failid `R/majandusplaaniindeks_sca.R` ja `R/esi_eesti.R`).
* **Leht** loeb JSON-failid. Kvartali- ja aastavaated, aastamuutused ja z-skoorid arvutab leht ise.
* **Eurostati päringud** tehakse kuni kolme katsega ja iga andmestik laaditakse käivituse jooksul üks kord (`get_eurostat`, `eurostat_plaan` failis `R/abi.R`).
* **Andmefail kirjutatakse ainult siis, kui andmed muutusid.** Päeval, mil midagi uut ei avaldatud, commit'i ei teki. Viimase kontrolli aeg on failis `docs/data/seis.json`, mis avaldatakse koos lehega, kuid ei lähe repo ajalukku (`.gitignore`).
* **Kaitse vigase uuenduse vastu:** kui mõne seeria laadimine ebaõnnestub või andmed tunduvad valed (tühi seeria, väärtused väljaspool lubatud vahemikku, viimane periood varasem kui eelmisel korral), jääb alles eelmine töötav versioon. Hoiatus läheb faili `hoiatused.txt` ja GitHub saadab teavituse.

## Failid

| Fail | Mis see on |
|---|---|
| `R/uuenda.R` | Peaskript. Käivitab kõik ja kirjutab `docs/data/*.json`. |
| `R/naitajad.R` | 15 majandusnäitaja päringud (Statistikaamet PXWeb, Eurostat). |
| `R/majandusplaaniindeks_sca.R` | Majandusplaaniindeksi arvutus. Sinu skript, ainus muudatus on lõpus: Exceli, RDS-i ja jooniste asemel kirjutatakse JSON. |
| `R/esi_eesti.R` | Majandususalduse indikaator EK metoodikaga (muutmata). |
| `R/vordlus.R` | Rahvusvahelise võrdluse andmed Eurostatist (`docs/data/vordlus.json`). Riike saab muuta muutujas `VORDLUS_RIIGID`. |
| `R/indeksid.R` | Teeb edetabelite CSV-failidest JSON-i. |
| `R/abi.R` | Ühised abifunktsioonid: päringud, kontrollid, JSON-i kirjutamine. |
| `andmed/indeksid.csv` | Edetabelid, üks rida edetabeli kohta. **Uuendatakse käsitsi.** |
| `andmed/indeksid_vordlus.csv` | Võrdlusriikide kohad edetabelites. **Uuendatakse käsitsi.** |
| `docs/data/kokkuvote.json` | Majanduse graafikute lehe sissejuhatav lõik. Kui väli `tekst` on tühi, koostab leht lõigu automaatselt viimastest andmetest; täidetuna näidatakse analüütiku teksti (lisaks `autor`, `kuupaev`). **Muudetakse käsitsi.** |
| `andmed/ulevaated.csv` | Teiste asutuste ülevaadete lingid (veerud `grupp`, `pealkiri`, `url`). **Uuendatakse käsitsi**, ka GitHubi veebis. Read on lehel samas järjekorras. |
| `docs/index.html` | Veebileht. |
| `docs/data/` | Andmefailid. Neid kirjutab robot, käsitsi ei muuda (v.a `kirjeldused.json`). |
| `docs/data/kirjeldused.json` | Näitajate nimed, valdkonnad, põhinäitaja märge ja selgitused (indikaatorite tabeli lehelt 2.1 AS-IS). **Muudetakse käsitsi.** |
| `docs/lib/`, `docs/fonts/` | ECharts ja fondid. Asuvad samas kohas, et leht ei laadiks midagi välistest teenustest. |
| `tools/echarts-kooste.mjs` | ECharts'i kohandatud kooste kirjeldus: `docs/lib/echarts.min.js` sisaldab ainult lehel kasutatavaid graafikutüüpe. Uue tüübi lisamisel ehita fail uuesti (juhis faili päises). |
| `.github/workflows/uuenda.yml` | Automaatne uuendamine ja avaldamine. |
| `mkm-ee/manustamine.html` | Koodijupp mkm.ee veebimeeskonnale. |

## Esmakordne seadistamine

### 1. Proovi oma arvutis

Vaja on R-i (4.3 või uuem) ja RStudio't.

```r
# RStudios: File > Open Project... või ava kaust ja sea see töökaustaks
install.packages(c("tidyverse", "eurostat", "httr", "jsonlite"))
source("R/uuenda.R")      # ~2–5 minutit
```

Kontrolli konsoolist, et lõpus oleks kirjas „Valmis, hoiatusi pole“. Kui on hoiatusi, loe `hoiatused.txt`.

Lehe vaatamiseks käivita kohalik veebiserver. Lihtsalt `index.html`-i topeltklõpsuga avamine ei tööta, sest brauser ei luba failist JSON-it laadida.

```r
# variant A (R):
install.packages("servr"); servr::httd("docs")
```
```bash
# variant B (Python):
python -m http.server 8000 --directory docs     # ava http://localhost:8000
```

### 2. Loo repo GitHubis

1. Loo MKM-i GitHubi organisatsiooni alla uus repo, näiteks `mkm-majandusnaitajad`. Ära kasuta isiklikku kontot.
2. Laadi kõik failid üles: `git init`, `git add .`, `git commit -m "Esimene versioon"`, `git remote add origin ...`, `git push -u origin main`. Teine võimalus on GitHub Desktop.
3. **Settings → Pages → Build and deployment → Source: GitHub Actions.**
4. **Settings → Actions → General → Workflow permissions: Read and write permissions.**
5. **Actions → Uuenda andmed ja avalda → Run workflow.** Esimene käivitus paigaldab R-i paketid, see võtab umbes 5–10 minutit. Järgmised käivitused on vahemälu tõttu kiiremad.
6. Leht on nüüd aadressil `https://<organisatsioon>.github.io/mkm-majandusnaitajad/`.

### 3. Turvalisus ja vastutus

* Lülita organisatsioonis sisse kahefaktoriline autentimine.
* **Settings → Branches → Add rule** `main`: muudatused tulevad pull requesti kaudu, kui repos on mitu inimest.
* **Settings → Notifications** (isiklik) või **Watch → Custom → Actions**: lisa teavitustesse vähemalt kaks inimest, et ebaõnnestunud uuendused ei jääks märkamata.

### 4. Oma domeen (valikuline)

Kui IT lubab alamdomeeni (näiteks `naitajad.mkm.ee`):

1. DNS-is: `CNAME naitajad.mkm.ee → <organisatsioon>.github.io`
2. **Settings → Pages → Custom domain:** `naitajad.mkm.ee`, seejärel lülita sisse **Enforce HTTPS**.

### 5. Lisamine mkm.ee lehele

Anna veebimeeskonnale fail `mkm-ee/manustamine.html`. Selles on:

* iframe koos kohustusliku `title` atribuudiga,
* väike skript, mis muudab iframe'i kõrgust vastavalt sisule (leht saadab oma kõrguse `postMessage`-iga; vastu võetakse ainult õigelt aadressilt),
* link eraldi lehele, kui iframe'i ei saa kasutada.

## Igapäevane töö

**Midagi ei ole vaja teha.** Robot käivitub tööpäeviti kell 8.05 Eesti aja järgi, nii suvel kui talvel. Uued andmed salvestatakse ja leht avaldatakse automaatselt.

Kui GitHub saadab teavituse „Run failed“:

1. Ava **Actions** ja vaata töö *uuenda* logi. Hoiatused on kollaselt märgitud.
2. Leht töötab edasi viimaste korrektsete andmetega. Pakilist ohtu pole.
3. Tavalised põhjused:
   * **API oli ajutiselt maas.** Järgmine käivitus parandab ise. Soovi korral vajuta **Run workflow**.
   * **Statistikaamet muutis tabelit** (uus tabeli kood, baasaasta, näitaja kood). Paranda vastav päring failis `R/naitajad.R`.
   * **Eurostati andmestik muutus.** Kontrolli filtreid failis `R/naitajad.R` või `R/majandusplaaniindeks_sca.R`.

## Edetabelite uuendamine (käsitsi, paar korda aastas)

Kui ilmub uus väljaanne, muuda faile `andmed/indeksid.csv` ja `andmed/indeksid_vordlus.csv`. Seda saab teha ka otse GitHubi veebis, pliiatsiikooniga. Pärast salvestamist (commit) käivitub uuendus ise.

| Edetabel | Ilmub tavaliselt |
|---|---|
| Korruptsioonitajumise indeks (CPI) | veebruar |
| Majandusvabaduse indeks | veebruar–märts |
| IMD konkurentsivõime | juuni |
| Euroopa innovatsiooni tulemustabel (EIS) | juuni–juuli |
| Ülemaailmne innovatsiooniindeks (GII) | september |
| Maksukonkurentsivõime indeks | oktoober |
| Euroopa iduettevõtete tulemustabel (ESSS) | mai |
| IKT arengu indeks (ITU IDI) | juuni |
| IMD talentide edetabel | september |
| Ülemaailmsed valitsemise indikaatorid (WGI) | september |
| Maailmapanga B-READY | sügis |
| IMD digitaalse konkurentsivõime edetabel | november |
| Talentide konkurentsivõime indeks (GTCI) | november |

`indeksid.csv` veerud on kirjeldatud failis `R/indeksid.R`. Olulisemad: `koht` ja `riike` = Eesti koht ja riikide arv (tühi, kui edetabelit pole); `skoor`/`skaala`, kui indeks avaldab ainult skoori; `ruhm` ja `ruhmad` (eraldaja `|`), kui indeks jagab riigid tulemusrühmadesse; `silt` = lühike lisainfo kaardil; `prioriteet` 1 või 2; `pikk`, `piirang`, `koosseis` = tekstid kaardi lisainfos. `indeksid_vordlus.csv`: võrdlusriikide `koht` või `skoor`.

## Näitajate muutmine

* **Nime, selgituse või põhinäitaja märke muutmine:** muuda faili `docs/data/kirjeldused.json` (võti on näitaja number tabelis).

* **Uue näitaja lisamine:** lisa päring faili `R/naitajad.R` (`lisa("nimi", function() ..., vahemik)`). Seejärel lisa kaart failis `docs/index.html` funktsioonis `koostaGrupid` (kopeeri olemasolev kaart eeskujuks).
* **Majandusplaaniindeksi metoodika muutmine:** muuda faili `R/majandusplaaniindeks_sca.R` samamoodi nagu varem. JSON-i kirjutab funktsioon `kirjuta_majandusplaan()` faili lõpus. Kui lisad indeksisse näitaja, tuleb see lehele automaatselt.

## Enne avalikustamist

- [ ] Kõik numbrid on võrreldud allikatega.
- [ ] Arvutatud aastaväärtused, mis pole ametlikud (mediaanpalga aastakeskmine, tarbijahindade kvartalivaade), on üle vaadatud: kas need jäävad lehele või asendatakse Statistikaameti tabelitega.
- [ ] Ligipääsetavuse test (WCAG 2.1 AA): axe või WAVE, klaviatuuriga läbimine, ekraanilugeja.
- [ ] Mobiilivaade on kontrollitud.
- [ ] Kommunikatsiooniosakond on tekstid kinnitanud.
- [ ] Veebimeeskond on iframe'i ja domeeni seadistanud.
- [ ] Teavitused lähevad vähemalt kahele inimesele.
