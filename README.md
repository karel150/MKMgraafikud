# Eesti majandus numbrites

Avalik näidikulaud. Avaleht on **Ülevaade**: automaatne kokkuvõte, majandususaldus, neli põhinäitajat, SKP kasv ja Eesti koht edetabelites, kõik samadest andmefailidest. Sealt ja vahekaartidelt pääseb viide vaatesse:

1. **Majandusplaani näitajad**: majandusplaaniindeks ja selle 8 alamnäitajat (tasemed, muutused, soojuskaart, andmetabel).
2. **Eesti majanduse graafikud**: 39 näitajat (neist 19 põhinäitajat, kuni 3 iga valdkonna kohta) kuu-, kvartali- ja aastavaatega.
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
* **Kokkuvõtvad lõigud kirjutab tehisaru** (vt „Tehisaru koostatud tekstid" allpool). Kui see ebaõnnestub või on välja lülitatud, koostab leht lõigud nagu varem lihtsate reeglite järgi.
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
| `andmed/indeksid.csv` | Edetabelid, üks rida edetabeli kohta. Arve ja silte muuda vormiga „Uuenda indeksit" (vt allpool); pikad tekstid muuda failis. WGI ja B-READY kohad laadib robot Maailmapanga API-st. |
| `andmed/indeksid_vordlus.csv` | Võrdlusriikide kohad edetabelites. **Uuendatakse käsitsi.** |
| `docs/data/kokkuvote.json` | Analüütiku kokkuvõte avalehele ja graafikute lehele. Täidetuna näidatakse seda (lisaks `autor`, `kuupaev`); tühjalt jäetuna kirjutab lõigu tehisaru ja selle puudumisel leht ise reeglite järgi. **Muudetakse käsitsi.** |
| `andmed/maksuvola_ulevaade.csv` | Maksu- ja Tolliameti maksuvõla ülevaate koopia. Robot laadib värske faili EMTA lingilt; seda koopiat kasutatakse ainult siis, kui allalaadimine ebaõnnestub (siis tekib hoiatus). |
| `andmed/ulevaated.csv` | Teiste asutuste ülevaadete lingid (veerud `grupp`, `pealkiri`, `url`). **Uuendatakse käsitsi**, ka GitHubi veebis. Read on lehel samas järjekorras. |
| `docs/index.html` | Veebileht. |
| `docs/data/` | Andmefailid. Neid kirjutab robot, käsitsi ei muuda (v.a `kirjeldused.json`). |
| `docs/data/kirjeldused.json` | Näitajate nimed, valdkonnad, põhinäitaja märge ja selgitused (indikaatorite tabeli lehelt 2.1 AS-IS). **Muudetakse käsitsi.** |
| `docs/lib/`, `docs/fonts/` | ECharts ja fondid. Asuvad samas kohas, et leht ei laadiks midagi välistest teenustest. |
| `docs/data/aitekstid.json` | Tehisaru koostatud kokkuvõtvad lõigud (avaleht, graafikud, indeksid, rahvusvaheline võrdlus). Kirjutab robot, käsitsi ei muuda. |
| `andmed/taie.csv` | TAIE arengukava mõõdikute nimed, sihttasemed, allikad ja käsitsi hoitavad väärtused. **Muudetakse käsitsi.** |
| `docs/data/taie.json` | TAIE mõõdikute leht: aegread koos sihttasemetega. Kirjutab robot (`R/taie.R`). |
| `docs/data/uuendused.json` | Andmemuudatuste logi: mis seeria sai uue perioodi ja mille varasem väärtus parandati. Sellest tehakse graafikute lehel kast „Viimati uuenenud". Kirjutab robot (`logi_muudatused`), hoitakse 21 päeva. Kui logi on kadunud või tühi, saab selle git-ajaloost taastada: `python3 tools/uuenduste_logi_ajaloost.py`. |
| `tools/ai_kokkuvotted.mjs` | Tellib tehisarult kokkuvõtvad lõigud ja kontrollib need üle (vt allpool). |
| `tools/ai_test.mjs` | Kontrollfunktsioonide testid: `node tools/ai_test.mjs`. |
| `tools/echarts-kooste.mjs` | ECharts'i kohandatud kooste kirjeldus: `docs/lib/echarts.min.js` sisaldab ainult lehel kasutatavaid graafikutüüpe. Uue tüübi lisamisel ehita fail uuesti (juhis faili päises). |
| `.github/workflows/uuenda.yml` | Automaatne uuendamine ja avaldamine. |
| `mkm-ee/manustamine.html` | Koodijupp mkm.ee veebimeeskonnale. |


## Elektri börsihind

Näitaja 37 on Nord Pooli järgmise päeva turu hind (€/MWh), mis laaditakse Eleringi avalikust
API-st (`dashboard.elering.ee/api/nps/price`, võtit pole vaja). Eleringi andmetes on Eesti, Läti,
Leedu ja Soome hinnapiirkond; Rootsi hindu seal ei ole, seetõttu Rootsit võrdluses pole.
Rootsi lisamiseks on vaja ENTSO-E läbipaistvusplatvormi API-võtit.

* Veerandtunni- ja tunnihinnad keskmistatakse esmalt tunniks, kuu keskmine on tundide keskmine
  (Eesti aja järgi). Jooksvat kuud ei näidata; kvartal ja aasta on kuude keskmised.
* Avalehe mõõdikus on Eesti tänase päeva keskmine hind ja see, kui suur osa viimase 365 päeva
  päevahindadest on sellest madalamad (protsentiil; poolringi keskel on aasta mediaan).
  Päevahinnad on `naitajad.json` väljal `elekter_paev` (viimased 400 päeva). Kui tänast hinda
  failis pole (nt nädalavahetusel), näidatakse viimast päeva koos kuupäevaga.
* Nord Pooli enda andmeportaalist andmeid ei võeta: avalik edasilevitamine vajab tasulist litsentsi.

## Sadamad (IMF PortWatch)

Näitajad 38 (sadamate hinnanguline eksport ja import, tuhat tonni) ja 39 (laevakülastused) tulevad
IMF PortWatchi avalikust ArcGIS-teenusest `Daily_Ports_Data` (võtit pole vaja). Viide:
„UN Global Platform; IMF PortWatch (portwatch.imf.org)“.

* Andmed on laevade satelliitpositsioonidel (AIS) põhinevad **hinnangud**, esialgsed ja muutuvad.
  Need hõlmavad ainult kaubalaevu (mitte reisiparvlaevu), seega on tase ametlikust
  sadamastatistikast tunduvalt madalam – kasulik on eelkõige muutus ja suund.
* Päringud tehakse serveris koondatuna (`outStatistics`), päevade kaupa riigi ja aasta kohta;
  kui aasta korraga ei õnnestu (nt Rootsi), küsitakse kuu kaupa. Ühe riigi rike ei mõjuta teisi.
* Kuu loetakse täielikuks, kui andmed katavad selle viimase päeva. PortWatch uueneb kord nädalas
  ja maha jääb umbes nädal.
* Võrdluses (Eesti, Läti, Leedu, Soome, Rootsi) on muutus aasta varasemaga, sest sadamate suurus
  on väga erinev.
* Avalehe ribal on „Sadamate eksport“: viimase 28 päeva hinnanguline eksport võrreldes sama 28
  päevaga 364 päeva varem (samad nädalapäevad); `naitajad.json` väli `sadamad_kiire`.

## TAIE mõõdikud

Vahekaart „TAIE mõõdikud" näitab teadus- ja arendustegevuse, innovatsiooni ning ettevõtluse arengukava 2021–2035 seirenäitajaid (arengukava lisa 3) nelja eesmärgi kaupa. Graafiku katkendjoon on 2035. aasta sihttase.

Kuue mõõdiku aegrea laeb `R/taie.R` ise: riigieelarveline TA rahastus ja tööjõu tootlikkus Eurostatist, erasektori TA kulutused ja eksport majandusnäitajate failist, mittemateriaalne põhivara ja teadlaste arv Statistikaametist. Euroopa innovatsiooni tulemustabeli koht võetakse indeksite failist, nii et see uueneb koos indeksite lehega.

Ülejäänud neljal mõõdikul masinloetavat allikat ei ole (lepingulise TA maht tuleb baasfinantseerimise alusandmetest, enamtsiteeritud artiklite osakaal innovatsiooni tulemustabelist, Harjumaa-väline SKP nõuab piirkondlike andmete kokkupanekut ja Doing Business'i lõpetas Maailmapank 2021. aastal). Nende viimane teada olev tase on failis `andmed/taie.csv` veergudes `aasta` ja `vaartus`; leht näitab neid ilma graafikuta koos selgitusega.

Uue mõõdiku või uue käsitsi väärtuse lisamiseks muuda `andmed/taie.csv`-d; aegrea lisamiseks tuleb `R/taie.R`-i kirjutada ka päring.

## Majandusprognoosid

Vahekaart „Prognoosid“ võrdleb Rahandusministeeriumi, Eesti Panga, Euroopa Komisjoni, IMF-i,
Swedbanki, SEB ja LHV viimaseid prognoose majanduskasvu, inflatsiooni ja töötuse määra kohta.
Andmed koostab `R/prognoosid.R` failiks `docs/data/prognoosid.json`.

* **IMF** laetakse automaatselt DataMapperi API-st (`NGDP_RPCH`, `PCPIPCH`, `LUR`). Kui arvud
  muutuvad, loetakse avaldamise kuupäevaks muutuse päev. API rikke korral kasutatakse CSV varurida.
* **Ülejäänud asutused** on failis `andmed/prognoosid.csv`, mida uuendatakse vormiga
  (vt allpool). Kui asutus mõnda arvu ei avalda, jääb see lehel tühjaks, mitte hinnatuks.

### Prognoosi uuendamine vormiga

1. Ava repo **Issues → New issue → Uuenda prognoosi**.
2. Vali asutus, sisesta avaldamise kuupäev (nt `22.09.2026`), link ja arvud – iga aasta eraldi
   reale kujul `2026: 2,5`.
3. **Uus kuupäev = uus prognoos:** asutuse senised arvud asendatakse ja tühjaks jäetud näitajat
   asutus ei avaldanud. **Sama kuupäev = parandus:** muudetakse ainult sisestatud aastaid;
   `2027: -` kustutab ühe aasta, lahtris ainult `-` terve näitaja.
4. Vajuta **Create**. Robot kirjutab muudatuse tabelisse, käivitab lehe uuenduse ja vastab
   kokkuvõttega (mis muutus millest milleks). Vea korral ütleb, mis on valesti; paranda
   sissekanne (Edit) ja robot proovib uuesti.

Vormi saavad kasutada ainult repo omanik ja liikmed. Vormi sisu töötleb `tools/prognoosi_vorm.py`,
tööd juhib `.github/workflows/prognoosi-vorm.yml`. Uue asutuse lisamiseks lisa see skripti
nimekirja `ASUTUSED` ja vormi rippmenüüsse.
* Näidatakse jooksva ja kahe järgmise aasta prognoose; „Keskmine“ arvutatakse, kui arv on
  vähemalt kolmelt asutuselt.

## Tehisaru koostatud tekstid

Avalehe, graafikute, indeksite ja rahvusvahelise võrdluse kokkuvõtvad lõigud kirjutab iga uuenduse järel tehisaru (Anthropicu API). Lehel on nende juures silt „Tehisaru kokkuvõte" ja selgitus, et teksti koostas tehisaru.

Mida skript `tools/ai_kokkuvotted.mjs` teeb:

1. Avab lehe peata brauseris (sama kood, mida näeb lugeja) ja võtab sealt **reeglipõhise mustandi** ning viimaste perioodide arvud. Nii on tekstis täpselt need arvud, mis on graafikutel – mudel ise arve ei arvuta ega otsi mujalt.
2. Küsib iga lõigu kohta ühe vastuse. Kui lähteandmed pole eelmisest korrast muutunud, vana tekst jääb ja päringut ei tehta.
3. **Kontrollib vastuse üle:** iga arv peab esinema lähtematerjalis, lauseid peab olema kokkulepitud arv, prognoose ja soovitusi ei tohi olla. Esimese ebaõnnestumise järel küsitakse uuesti; kui ka see ei kõlba, jääb kehtima eelmine tekst.
4. Kirjutab tulemuse faili `docs/data/aitekstid.json` ainult siis, kui tekst muutus.

Kui midagi ebaõnnestub (API, brauser, kontroll), läheb hoiatus faili `hoiatused.txt` ja GitHub saadab teate; leht näitab eelmist teksti või reeglipõhist kokkuvõtet. Analüütiku käsitsi kirjutatud tekst failis `docs/data/kokkuvote.json` on alati tähtsam kui tehisaru oma.

Seadistamine GitHubis (*Settings → Secrets and variables → Actions*):

| Nimi | Kus | Mille jaoks |
|---|---|---|
| `ANTHROPIC_API_KEY` | Secrets | API võti. Ilma selleta tekste ei uuendata (tuleb hoiatus). |
| `AI_MUDEL` | Variables (valikuline) | Mudeli nimi, vaikimisi `claude-sonnet-5-5`. |
| `AI_TEKSTID` | Variables (valikuline) | Väärtus `0` lülitab tehisaru tekstid välja ja leht läheb tagasi reeglipõhistele. |

Maht on väike (viis päringut päevas, ainult andmete muutumisel), seega kulu on sendid kuus.

Oma arvutis proovimiseks (nõuab Chrome'i ja Node.js-i):

```bash
npm install --no-save puppeteer-core@25
ANTHROPIC_API_KEY=... node tools/ai_kokkuvotted.mjs .
```

## Indeksite uuendamine vormiga

Edetabeli uue väljaande andmeid ei pea CSV-faili käsitsi kirjutama.

1. Ava repos **Issues → New issue → Uuenda indeksit**.
2. Vali indeks ja täida lahtrid, mida tahad muuta. Tühjaks jäetud lahtrit ei muudeta; lahtri sisu kustutamiseks kirjuta `-`.
3. Uue väljaande puhul märgi linnuke „See on uus väljaanne": senine aasta, koht ja skoor tõstetakse ise eelmise väljaande alla.
4. Vajuta **Create**. Robot kirjutab muudatuse tabelisse, käivitab lehe uuenduse ja vastab sissekandele kokkuvõttega (mis väli muutus millest milleks). Kui vormis on viga, ütleb vastus, mis on valesti; paranda sissekanne (Edit) ja robot proovib uuesti.

Vormi saavad kasutada ainult repo omanik ja liikmed. Vormis ei ole pikki tekste (kirjeldus, piirangud, „mida see näitab") ega WGI-d ja B-READY-d.

Uue indeksi lisamisel lisa rida faili `andmed/indeksid.csv` ja sama lühend vormi rippmenüüsse (`.github/ISSUE_TEMPLATE/indeksi-uuendus.yml`). Vormi sisu töötleb `tools/indeksi_vorm.py`, tööd juhib `.github/workflows/indeksi-vorm.yml`.

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
