# Juhised Claude'ile

Avalik Eesti majandusnäitajate töölaud: R laeb andmed, kirjutab JSON-failid, üks HTML-leht
joonistab need graafikuteks. Käib GitHub Actionsis ja GitHub Pagesil, inimese sekkumiseta.
Sisulised selgitused andmeallikate ja töövoo kohta on `README.md`-s – see fail on tööreeglite jaoks.

## Keel

Kõik on eesti keeles: vastused kasutajale, koodi kommentaarid, muutuja- ja funktsiooninimed,
commit'ide sõnumid, pull request'ide kirjeldused ja veateated. Inglise keelt kasutatakse ainult
seal, kus keel seda nõuab (API väljad, `git`, YAML-i võtmesõnad).

Lehe tekstid on avalikud ja neid loevad ajakirjanikud ja ametnikud: neutraalne toon, ei mingit
prognoosi ega poliitikasoovitust.

## Kuidas asjad liiguvad

```
andmeallikad (Statistikaamet, Eurostat, ECB, EMTA, Maailmapank)
   → R/uuenda.R ja selle osad       → docs/data/*.json
   → tools/ai_kokkuvotted.mjs       → docs/data/aitekstid.json
   → docs/index.html loeb JSON-id   → graafikud brauseris
```

| Koht | Mis seal on |
|---|---|
| `R/uuenda.R` | Peaskript, käivitab kõik osad järjekorras. Käivita repo juurkaustast. |
| `R/abi.R` | Abifunktsioonid: päringud, teisendused, kontrollid, failide kirjutamine. Uus abifunktsioon käib siia. |
| `R/naitajad.R`, `R/vordlus.R`, `R/indeksid.R` | Kolm väljundfaili koostajat. |
| `R/majandusplaaniindeks_sca.R`, `R/esi_eesti.R` | Majandusplaaniindeks ja ESI hinnang. |
| `andmed/*.csv` | Käsitsi hoitav sisend (indeksid, ülevaated, EMTA maksuvõlg). |
| `docs/index.html` | Terve veebileht ühes failis (~2000 rida): HTML, CSS ja JS koos. |
| `docs/data/*.json` | Lehe andmed. Kirjutab robot, käsitsi ei muuda – erand on `kokkuvote.json`. |
| `tools/` | Abiskriptid: tehisaru tekstid, nende testid, ECharts-i kooste, indeksivormi töötleja. |
| `mkm-ee/manustamine.html` | Koodijupp mkm.ee veebimeeskonnale lehe manustamiseks. |

## Kokkulepped, mida järgida

**R.** Kasuta `R/abi.R` olemasolevaid abifunktsioone, ära kirjuta neid uuesti:
`px_paring` (Statistikaamet), `get_eurostat` (kordustega), `http_json`, `seeria`, `turvaliselt`
(uue väärtuse mõistlikkuse kontroll), `kirjuta_json`, `hoiata`.

* Iga osa on `tryCatch`-is: kui üks allikas on maas, jääb alles selle osa **eelmine fail** ja
  hoiatus läheb faili `hoiatused.txt`. Uuendus ei tohi kunagi tervikuna katkeda ega kirjutada
  faili tühja või poolikut sisu.
* `kirjuta_json` kirjutab ainult siis, kui sisu tegelikult muutus (ajatemplit ei arvestata).
  Hoia seda käitumist: muidu tekib iga päev mõttetu commit ja uus deploy.
* Kodeering on UTF-8, ajavöönd `Europe/Tallinn`.

**Veebileht.** `docs/index.html` jääb üheks failiks.

* Graafikud joonistatakse laisalt (`IntersectionObserver`), vahekaardid ehitatakse alles avamisel.
* ECharts on kohandatud kooste (`tools/echarts-kooste.mjs`), kus on ainult vajalikud osad.
  Uut tüüpi graafik tähendab koostefaili täiendamist ja uut `docs/lib` koostet.
* Näitaja definitsioonid on `koostaGrupid`-is, nimed ja valdkonnad failis `docs/data/kirjeldused.json`.
* Kasutaja sisestatud või andmetest tulev tekst käib alati läbi `esc()`.

**Kokkuvõtvad tekstid.** Järjekord on: analüütiku tekst (`docs/data/kokkuvote.json`) →
tehisaru tekst (`docs/data/aitekstid.json`) → lehe enda reeglipõhine tekst. Kõik kolm peavad
alles jääma: kui tehisaru tekst puudub või ei läbi kontrolli, peab leht ilma veata tagasi
langema. Juhiste või kontrollide muutmisel tõsta `tools/ai_kokkuvotted.mjs` `VERSIOON`-i,
siis kirjutatakse tekstid uuesti ka muutumatute andmete korral.

## Töövoog

* Iga muudatus **eraldi harusse** (`git checkout -B <nimi> origin/main`) ja sealt pull request.
  Otse `main`-i ei pushita; erand on, kui GitHubi enda merge on katki ja kasutaja on mergemist palunud.
* Commit'i sõnum eesti keeles: esimene rida ütleb, mis muutus, edasi lõiguna miks.
* Pull request'i kirjeldus ütleb, mida muudeti, mida testiti ja mida peab kasutaja ise tegema
  (nt secret lisama).
* Pärast ühendamist kontrolli jooksu logi (`gh api repos/.../actions/runs`), ära jäta uuendust
  kontrollimata.

## Testimine

```bash
Rscript R/uuenda.R          # terve uuendus (vajab võrguühendust allikatele)
node tools/ai_test.mjs      # tehisaru vastuste kontrollfunktsioonid
python3 tools/indeksi_vorm.py --help
```

Lehe muudatusi kontrolli päriselt brauseris (peata Chrome, `docs/` kaust serveerituna) –
ainult koodi lugemisest ei piisa, sest vead tulevad välja alles joonistamisel.

## Mida tasub teada

* **Töökeskkonnal pole ligipääsu** stat.ee-le, Eurostatile, ECB-le ega EMTA-le. Päris andmetega
  saab proovida ainult GitHub Actionsis: `workflow_dispatch` mõnel harul käivitab arvutuse, aga
  ei avalda ega commiti midagi (`if: github.ref == 'refs/heads/main'`).
* **GitHubi `schedule` hilineb** selles repos tundide kaupa, seetõttu käivitab hommikuse jooksu
  väline ajastatud ülesanne. `uuenda.yml`-i `kell` töö valib kahest cron-ist selle, mis vastab
  Eesti aja järgi kella 8.05-le.
* `tools/indeksi_vorm.py` kirjutab CSV-d reavahetusega `\r\n`, ülejäänud failid on `\n`-ga.
* Kasutaja e-posti aadress on isiklik info: ära kasuta seda mujal kui tema enda töö tuvastamiseks.
