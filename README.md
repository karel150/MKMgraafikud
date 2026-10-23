# Eesti majandus numbrites

Avalik näidikulaud kolme vaatega:

1. **Majandusplaani näitajad**: majandusplaaniindeks ja selle 8 alamnäitajat (tasemed, muutused, soojuskaart, andmetabel).
2. **Eesti majanduse graafikud**: 15 näitajat kuu-, kvartali- ja aastavaatega.
3. **Eesti positsioon indeksites**: 6 rahvusvahelist edetabelit.

Andmed uuendatakse automaatselt tööpäeviti GitHub Actionsiga. Leht on staatiline: HTML, JavaScript ja JSON-failid, ilma serverita. Graafikud on tehtud ECharts'iga (Apache 2.0).

## Kuidas see töötab

```
Statistikaamet, Eurostat ──► R/uuenda.R ──► docs/data/*.json ──► docs/index.html
andmed/indeksid*.csv     ──┘   (GitHub Actions,                  (GitHub Pages või
                                tööpäeviti 09:15)                 MKM-i server)
```

* **R** laadib andmed ja arvutab majandusplaaniindeksi (sinu metoodika, failid `R/majandusplaaniindeks_sca.R` ja `R/esi_eesti.R`).
* **Leht** loeb JSON-failid. Kvartali- ja aastavaated, aastamuutused ja z-skoorid arvutab leht ise.
* **Kaitse vigase uuenduse vastu:** kui mõne seeria laadimine ebaõnnestub või andmed tunduvad valed (tühi seeria, väärtused väljaspool lubatud vahemikku, viimane periood varasem kui eelmisel korral), jääb alles eelmine töötav versioon. Hoiatus läheb faili `hoiatused.txt` ja GitHub saadab teavituse.

## Failid

| Fail | Mis see on |
|---|---|
| `R/uuenda.R` | Peaskript. Käivitab kõik ja kirjutab `docs/data/*.json`. |
| `R/naitajad.R` | 15 majandusnäitaja päringud (Statistikaamet PXWeb, Eurostat). |
| `R/majandusplaaniindeks_sca.R` | Majandusplaaniindeksi arvutus. Sinu skript, ainus muudatus on lõpus: Exceli, RDS-i ja jooniste asemel kirjutatakse JSON. |
| `R/esi_eesti.R` | Majandususalduse indikaator EK metoodikaga (muutmata). |
| `R/indeksid.R` | Teeb edetabelite CSV-failidest JSON-i. |
| `R/abi.R` | Ühised abifunktsioonid: päringud, kontrollid, JSON-i kirjutamine. |
| `andmed/indeksid.csv` | Edetabelid, üks rida edetabeli kohta. **Uuendatakse käsitsi.** |
| `andmed/indeksid_vordlus.csv` | Võrdlusriikide kohad edetabelites. **Uuendatakse käsitsi.** |
| `docs/index.html` | Veebileht. |
| `docs/data/` | Andmefailid. Neid kirjutab robot, käsitsi ei muuda. |
| `docs/lib/`, `docs/fonts/` | ECharts ja fondid. Asuvad samas kohas, et leht ei laadiks midagi välistest teenustest. |
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

**Midagi ei ole vaja teha.** Robot käivitub tööpäeviti 09:15 (talvel 08:15). Uued andmed salvestatakse ja leht avaldatakse automaatselt.

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

`indeksid.csv` veerud: `koht` = Eesti koht; `riike` = riikide arv (tühi, kui pole teada); `silt` = lühike lisainfo kaardil; `vordlus_aasta` ja `vordlus_riike` = mis väljaandest võrdlusriikide kohad pärinevad.

## Näitajate muutmine

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
