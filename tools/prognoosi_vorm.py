#!/usr/bin/env python3
"""Kirjutab GitHubi vormi „Uuenda prognoosi“ sisu faili andmed/prognoosid.csv.

Käivitab workflow .github/workflows/prognoosi-vorm.yml. Vormi tekst loetakse failist
(GitHubi sündmuse JSON või lihtne tekstifail), mitte käsurealt, sest see on kasutaja sisestatud.

  python3 tools/prognoosi_vorm.py --sundmus "$GITHUB_EVENT_PATH" --valjund kokkuvote.md
  python3 tools/prognoosi_vorm.py --tekst vorm.md --kuiv        # proov, faili ei muudeta

Reegel: kui avaldamise kuupäev erineb senisest, on see uus prognoos ja asutuse senised read
asendatakse täielikult (tühjaks jäetud näitajat asutus ei avaldanud). Kui kuupäev on sama,
parandatakse ainult sisestatud aastad ('2027: -' kustutab aasta, lahtris ainult '-' näitaja).

Lõpetab koodiga 0, kui muudatus on tehtud (või midagi ei muutunud), ja koodiga 1, kui
vormis on viga; mõlemal juhul on selgitus väljundfailis (Markdown, läheb kommentaariks).
"""
import argparse, csv, datetime, io, json, re, sys

PROGNOOSID = "andmed/prognoosid.csv"
VEERUD = ["asutus", "prognoos", "avaldatud", "aasta", "naitaja", "vaartus", "url", "markus"]
# Vormi lahtri pealkiri -> näitaja kood CSV-s ja mõistlik vahemik (sama mis R/prognoosid.R)
NAITAJAD = {
    "Majanduskasv": ("skp", -15, 15),
    "Inflatsioon": ("inflatsioon", -5, 30),
    "Töötuse määr": ("tootus", 1, 25),
}
NIMI = {kood: nimi for nimi, (kood, _, _) in NAITAJAD.items()}
# IMF tuleb API-st automaatselt, seda vormiga ei muudeta
ASUTUSED = ["Rahandusministeerium", "Eesti Pank", "Euroopa Komisjon", "Swedbank", "SEB", "LHV"]
TYHI = {"", "_No response_", "None"}
KUSTUTA = {"-", "–", "—"}


class VormiViga(Exception):
    pass


def loe_csv(tee):
    with open(tee, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def kirjuta_csv(tee, read):
    buf = io.StringIO(newline="")
    w = csv.DictWriter(buf, fieldnames=VEERUD, lineterminator="\n")
    w.writeheader()
    w.writerows(read)
    with open(tee, "w", newline="", encoding="utf-8") as f:
        f.write(buf.getvalue())


def osad(tekst):
    """Vormi keha -> {pealkiri: sisu}. GitHub kirjutab iga lahtri kujul '### Pealkiri\\n\\nsisu'."""
    out, nimi, read = {}, None, []
    for rida in tekst.replace("\r\n", "\n").split("\n"):
        m = re.match(r"^###\s+(.*\S)\s*$", rida)
        if m:
            if nimi is not None:
                out[nimi] = "\n".join(read).strip()
            nimi, read = m.group(1), []
        elif nimi is not None:
            read.append(rida)
    if nimi is not None:
        out[nimi] = "\n".join(read).strip()
    return out


def kuupaev(s):
    s = s.strip()
    m = re.fullmatch(r"(\d{1,2})\.(\d{1,2})\.(\d{4})", s)
    if m:
        p, k, a = map(int, m.groups())
    else:
        m = re.fullmatch(r"(\d{4})-(\d{1,2})-(\d{1,2})", s)
        if not m:
            raise VormiViga(f"Avaldamise kuupäev peab olema kujul 22.09.2026, aga on „{s}“.")
        a, k, p = map(int, m.groups())
    try:
        d = datetime.date(a, k, p)
    except ValueError:
        raise VormiViga(f"Kuupäeva „{s}“ ei ole olemas.")
    if d > datetime.date.today() + datetime.timedelta(days=1):
        raise VormiViga(f"Avaldamise kuupäev {d:%d.%m.%Y} on tulevikus.")
    return d.isoformat()


def loe_vaartused(tekst, nimi, alt, ylt, see_aasta):
    """Read kujul '2026: 2,5' -> {aasta: '2.5'}; '2027: -' -> {aasta: None} (kustuta see aasta).
    Eraldajaks sobib ka tühik, tabulaator või '='."""
    out = {}
    for rida in [r.strip() for r in tekst.split("\n") if r.strip()]:
        k = re.fullmatch(r"(\d{4})\s*[:=\t ]?\s*[-–—]", rida)
        if k:
            if int(k.group(1)) in out:
                raise VormiViga(f"„{nimi}“: aasta {k.group(1)} on kaks korda.")
            out[int(k.group(1))] = None
            continue
        m = re.fullmatch(r"(\d{4})\s*[:=\t ]\s*(-?\s*\d+(?:[.,]\d+)?)\s*%?", rida)
        if not m:
            raise VormiViga(f"„{nimi}“: rida „{rida}“ ei ole kujul „2026: 2,5“.")
        aasta, v = int(m.group(1)), float(m.group(2).replace(" ", "").replace(",", "."))
        if not see_aasta - 1 <= aasta <= see_aasta + 5:
            raise VormiViga(f"„{nimi}“: aasta {aasta} ei sobi (lubatud {see_aasta - 1}–{see_aasta + 5}).")
        if not alt <= v <= ylt:
            raise VormiViga(f"„{nimi}“: väärtus {m.group(2)} aastaks {aasta} ei ole usutav (lubatud {alt}…{ylt}).")
        if aasta in out:
            raise VormiViga(f"„{nimi}“: aasta {aasta} on kaks korda.")
        out[aasta] = f"{v:g}"
    return out


def rakenda(tekst, kuiv=False, tana=None):
    see_aasta = (tana or datetime.date.today()).year
    v = osad(tekst)
    asutus = v.get("Asutus", "").strip()
    if asutus in TYHI:
        raise VormiViga("Asutus on valimata.")
    if asutus not in ASUTUSED:
        raise VormiViga(f"Asutust „{asutus}“ selle vormiga muuta ei saa.")

    read = loe_csv(PROGNOOSID)
    vanad = [r for r in read if r["asutus"] == asutus]
    vana_kp = vanad[0]["avaldatud"] if vanad else ""

    kp_tekst = v.get("Avaldamise kuupäev", "").strip()
    if kp_tekst in TYHI:
        raise VormiViga("Avaldamise kuupäev on sisestamata.")
    kp = kuupaev(kp_tekst)
    uus_prognoos = kp != vana_kp

    def tekst_vali(pealkiri, vaikimisi, pikkus=200, link=False):
        s = v.get(pealkiri, "").strip()
        if s in TYHI:
            return vaikimisi
        if s in KUSTUTA:
            return ""
        s = " ".join(s.split())
        if link and not re.fullmatch(r"https?://\S+", s):
            raise VormiViga(f"„{pealkiri}“ peab algama https:// või http:// ja olema ilma tühikuteta.")
        if len(s) > pikkus:
            raise VormiViga(f"„{pealkiri}“ on liiga pikk (üle {pikkus} märgi).")
        return s

    prognoos = tekst_vali("Prognoosi nimi", vanad[0]["prognoos"] if vanad else "")
    url = tekst_vali("Link prognoosile", vanad[0]["url"] if vanad else "", 500, link=True)
    if not url:
        raise VormiViga("Link prognoosile on puudu.")
    if uus_prognoos and v.get("Link prognoosile", "").strip() in TYHI:
        raise VormiViga("Uue prognoosi puhul lisa link prognoosile (senine link viitab vanale prognoosile).")
    # Märkus kehtib uue prognoosi puhul ainult siis, kui see on uuesti sisestatud
    markus = tekst_vali("Märkus", "" if uus_prognoos else (vanad[0]["markus"] if vanad else ""), 300)

    # Väärtused: {kood: {aasta: väärtus}}
    senised = {}
    for r in vanad:
        senised.setdefault(r["naitaja"], {})[int(r["aasta"])] = r["vaartus"]
    uued = {} if uus_prognoos else {k: dict(x) for k, x in senised.items()}
    for pealkiri, (kood, alt, ylt) in NAITAJAD.items():
        s = v.get(pealkiri, "").strip()
        if s in TYHI:
            continue
        if s in KUSTUTA:
            uued[kood] = {}
            continue
        sisse = loe_vaartused(s, pealkiri, alt, ylt, see_aasta)
        # uue prognoosi puhul on sisestatu kogu näitaja; paranduse puhul muudetakse ainult antud aastaid
        x = {} if uus_prognoos else dict(uued.get(kood, {}))
        for a, val in sisse.items():
            if val is None:
                x.pop(a, None)
            else:
                x[a] = val
        uued[kood] = x
    uued = {k: x for k, x in uued.items() if x}
    if not uued:
        raise VormiViga("Sisesta vähemalt ühe näitaja prognoos (nt majanduskasv „2026: 2,5“).")

    uued_read = [{"asutus": asutus, "prognoos": prognoos, "avaldatud": kp, "aasta": str(a), "naitaja": kood,
                  "vaartus": val, "url": url, "markus": markus}
                 for kood in ["skp", "inflatsioon", "tootus"] if kood in uued for a, val in sorted(uued[kood].items())]

    muutus = [{k: r[k] for k in VEERUD} for r in vanad] != uued_read
    if muutus and not kuiv:
        # asutuse plokk jääb samale kohale (järjekord määrab lehel tabeli järjekorra)
        koht = next((i for i, r in enumerate(read) if r["asutus"] == asutus), len(read))
        ylej = [r for r in read if r["asutus"] != asutus]
        koht = min(koht, len(ylej))
        kirjuta_csv(PROGNOOSID, ylej[:koht] + uued_read + ylej[koht:])

    def kuva(d):
        return "–" if not d else ", ".join(f"{a}: {float(x):.1f}".replace(".", ",") for a, x in sorted(d.items()))
    md = [f"**{asutus}**: {prognoos} ({datetime.date.fromisoformat(kp):%d.%m.%Y})", ""]
    if not muutus:
        md.append("Midagi ei muutunud: sisestatud andmed on tabelis juba olemas.")
        return "\n".join(md), False
    md.append("Uus prognoos: senised arvud asendati." if uus_prognoos and vanad else
              "Uus asutus." if not vanad else "Sama prognoosi parandus: muudeti ainult sisestatud aastaid.")
    md += ["", "| Näitaja | Enne | Nüüd |", "|---|---|---|"]
    for kood in ["skp", "inflatsioon", "tootus"]:
        if senised.get(kood) == uued.get(kood):
            continue
        md.append(f"| {NIMI[kood]} | {kuva(senised.get(kood))} | {kuva(uued.get(kood))} |")
    if vanad and (vanad[0]["url"] != url or vanad[0]["prognoos"] != prognoos or vanad[0]["markus"] != markus):
        md += ["", f"Nimi: {prognoos or '–'} · Märkus: {markus or '–'}", f"Link: {url}"]
    return "\n".join(md), True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sundmus", help="GitHubi sündmuse JSON-fail (issue)")
    ap.add_argument("--tekst", help="vormi tekst failina (proovimiseks)")
    ap.add_argument("--valjund", help="kuhu kirjutada kokkuvõte (Markdown)")
    ap.add_argument("--kuiv", action="store_true", help="ära muuda faili")
    a = ap.parse_args()
    if a.sundmus:
        with open(a.sundmus, encoding="utf-8") as f:
            tekst = (json.load(f).get("issue") or {}).get("body") or ""
    elif a.tekst:
        with open(a.tekst, encoding="utf-8") as f:
            tekst = f.read()
    else:
        ap.error("anna --sundmus või --tekst")
    try:
        md, muutus = rakenda(tekst, a.kuiv)
        kood = 0
    except VormiViga as e:
        md, muutus, kood = f"Vormi ei saanud rakendada: {e}\n\nParanda sissekanne (paremal ülal „Edit“) ja robot proovib uuesti.", False, 1
    if a.valjund:
        with open(a.valjund, "w", encoding="utf-8") as f:
            f.write(md + "\n")
    else:
        print(md)
    print(f"muutus={'true' if muutus else 'false'}")
    sys.exit(kood)


if __name__ == "__main__":
    main()
