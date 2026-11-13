#!/usr/bin/env python3
"""Kirjutab GitHubi vormi „Uuenda indeksit“ sisu failidesse andmed/indeksid.csv ja
andmed/indeksid_vordlus.csv.

Käivitab workflow .github/workflows/indeksi-vorm.yml. Vormi tekst loetakse failist
(GitHubi sündmuse JSON või lihtne tekstifail), mitte käsurealt, sest see on kasutaja sisestatud.

  python3 tools/indeksi_vorm.py --sundmus "$GITHUB_EVENT_PATH" --valjund kokkuvote.md
  python3 tools/indeksi_vorm.py --tekst vorm.md --kuiv        # proov, faile ei muudeta

Lõpetab koodiga 0, kui muudatus on tehtud (või midagi ei muutunud), ja koodiga 1, kui
vormis on viga; mõlemal juhul on selgitus väljundfailis (Markdown, läheb kommentaariks).
"""
import argparse, csv, io, json, re, sys

INDEKSID = "andmed/indeksid.csv"
VORDLUS = "andmed/indeksid_vordlus.csv"

# Vormi lahtri pealkiri -> CSV veerg ja tüüp
VALJAD = {
    "Väljaande aasta": ("aasta", "aasta"),
    "Eesti koht": ("koht", "taisarv"),
    "Riikide arv edetabelis": ("riike", "taisarv"),
    "Skoor": ("skoor", "arv"),
    "Silt kaardil": ("silt", "tekst"),
    "Tulemusrühm": ("ruhm", "tekst"),
    "Raporti link": ("url", "link"),
    "Eelmise väljaande aasta": ("eelmine_aasta", "aasta"),
    "Eelmise väljaande koht": ("eelmine_koht", "taisarv"),
    "Eelmise väljaande skoor": ("eelmine_skoor", "arv"),
}
NIMED = {"aasta": "Väljaande aasta", "koht": "Koht", "riike": "Riikide arv", "skoor": "Skoor", "silt": "Silt", "ruhm": "Tulemusrühm",
         "url": "Raporti link", "eelmine_aasta": "Eelmise väljaande aasta", "eelmine_koht": "Eelmise väljaande koht",
         "eelmine_skoor": "Eelmise väljaande skoor", "eelmine_silt": "Eelmise väljaande silt",
         "vordlus_aasta": "Võrdluse aasta", "vordlus_riike": "Võrdluse riikide arv"}
TYHI = {"", "_No response_", "None"}
KUSTUTA = {"-", "–", "—"}


class VormiViga(Exception):
    pass


def loe_csv(tee):
    with open(tee, newline="", encoding="utf-8") as f:
        return list(csv.reader(f))


def kirjuta_csv(tee, read):
    # failid on CRLF-reavahetusega; sama kuju hoidmine jätab muudatuse ühe rea suuruseks
    buf = io.StringIO(newline="")
    csv.writer(buf, lineterminator="\r\n").writerows(read)
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


def arv(s, nimi):
    s = s.strip().replace(" ", "").replace(" ", "").replace(",", ".")
    if not re.fullmatch(r"-?\d+(\.\d+)?", s):
        raise VormiViga(f"„{nimi}“ peab olema arv, aga on „{s}“.")
    return s


def taisarv(s, nimi, alates=1, kuni=100000):
    s = s.strip().rstrip(".")
    if not re.fullmatch(r"\d+", s) or not alates <= int(s) <= kuni:
        raise VormiViga(f"„{nimi}“ peab olema täisarv vahemikus {alates}–{kuni}, aga on „{s}“.")
    return str(int(s))


def puhasta(s, nimi, tyyp):
    if tyyp == "aasta":
        return taisarv(s, nimi, 2000, 2100)
    if tyyp == "taisarv":
        return taisarv(s, nimi)
    if tyyp == "arv":
        return arv(s, nimi)
    if tyyp == "link":
        if not re.fullmatch(r"https?://\S+", s.strip()):
            raise VormiViga(f"„{nimi}“ peab algama https:// või http:// ja olema ilma tühikuteta.")
        return s.strip()
    s = " ".join(s.split())
    if len(s) > 200:
        raise VormiViga(f"„{nimi}“ on liiga pikk (üle 200 märgi).")
    return s


def loe_vordlus(tekst):
    """'Riik: koht', 'Riik: koht / skoor' või 'Riik: - / skoor' -> [(riik, koht, skoor)]"""
    out = []
    for nr, rida in enumerate([r.strip() for r in tekst.split("\n") if r.strip()], 1):
        m = re.fullmatch(r"(.+?)\s*:\s*([^/]*?)\s*(?:/\s*(.*?))?\s*", rida)
        if not m:
            raise VormiViga(f"Võrdlusriikide rida {nr} („{rida}“) ei ole kujul „Riik: koht“.")
        riik, koht, skoor = m.group(1).strip(), (m.group(2) or "").strip(), (m.group(3) or "").strip()
        koht = "" if koht in KUSTUTA | {""} else taisarv(koht, f"{riik} koht")
        skoor = "" if skoor in KUSTUTA | {""} else arv(skoor, f"{riik} skoor")
        if not koht and not skoor:
            raise VormiViga(f"Võrdlusriikide real {nr} („{rida}“) puudub nii koht kui ka skoor.")
        if len(riik) > 60:
            raise VormiViga(f"Võrdlusriikide rea {nr} riigi nimi on liiga pikk.")
        out.append((riik, koht, skoor))
    if len({r[0].lower() for r in out}) != len(out):
        raise VormiViga("Võrdlusriikides on sama riik kaks korda.")
    return out


def rakenda(tekst, kuiv=False):
    v = osad(tekst)
    valik = v.get("Indeks", "").strip()
    if valik in TYHI:
        raise VormiViga("Indeks on valimata.")
    iid = re.split(r"\s+[–-]\s+", valik, maxsplit=1)[0].strip().lower()

    read = loe_csv(INDEKSID)
    pais = read[0]
    idx = {n: i for i, n in enumerate(pais)}
    rida = next((r for r in read[1:] if r and r[0] == iid), None)
    if rida is None:
        raise VormiViga(f"Indeksit „{iid}“ failis {INDEKSID} ei ole.")
    vana = dict(zip(pais, rida))
    uus = dict(vana)

    # 1. Uus väljaanne: senised andmed eelmise väljaande alla
    nihuta = bool(re.search(r"^\s*-\s*\[[xX]\]", v.get("Uus väljaanne", ""), re.M))
    if nihuta:
        if v.get("Väljaande aasta", "").strip() in TYHI:
            raise VormiViga("Uue väljaande puhul tuleb sisestada väljaande aasta.")
        uus["eelmine_aasta"], uus["eelmine_koht"], uus["eelmine_skoor"] = vana["aasta"], vana["koht"], vana["skoor"]
        # eelmise väljaande silt on kasutusel ainult osal indeksitel (nt EIS: „104,8% EL-i keskmisest“);
        # neil tõstetakse sinna senine silt, teistel jääb lahter tühjaks
        uus["eelmine_silt"] = vana["silt"] if vana["eelmine_silt"] else ""
        # uue väljaande koht ja skoor ei tohi jääda vanaks, kui neid ei sisestata
        uus["koht"], uus["skoor"] = "", ""

    # 2. Vormi lahtrid
    for pealkiri, (veerg, tyyp) in VALJAD.items():
        s = v.get(pealkiri, "").strip()
        if s in TYHI:
            continue
        uus[veerg] = "" if s in KUSTUTA else puhasta(s, pealkiri, tyyp)

    # 3. Võrdlusriigid
    vrd_tekst = v.get("Võrdlusriigid", "").strip()
    vrd_uus = None
    if vrd_tekst not in TYHI:
        vrd_uus = [] if vrd_tekst in KUSTUTA else loe_vordlus(vrd_tekst)
        uus["vordlus_aasta"] = uus["aasta"] if vrd_uus else ""
        uus["vordlus_riike"] = uus["riike"] if vrd_uus else ""

    # 4. Kontrollid
    if uus["koht"] and uus["riike"] and int(uus["koht"]) > int(uus["riike"]):
        raise VormiViga(f"Koht ({uus['koht']}) ei saa olla suurem kui riikide arv ({uus['riike']}).")
    if uus["eelmine_aasta"] and uus["aasta"] and int(uus["eelmine_aasta"]) >= int(uus["aasta"]):
        raise VormiViga(f"Eelmise väljaande aasta ({uus['eelmine_aasta']}) peab olema varasem kui väljaande aasta ({uus['aasta']}).")
    if vrd_uus and uus["riike"]:
        for riik, koht, _ in vrd_uus:
            if koht and int(koht) > int(uus["riike"]):
                raise VormiViga(f"{riik} koht ({koht}) on suurem kui riikide arv ({uus['riike']}).")
    if nihuta and not (uus["koht"] or uus["skoor"] or uus["ruhm"]):
        raise VormiViga("Uue väljaande puhul sisesta vähemalt koht, skoor või tulemusrühm.")

    # 5. Muudatuste loetelu ja kirjutamine
    muutused = [(NIMED.get(k, k), vana[k], uus[k]) for k in pais if vana[k] != uus[k]]
    vrd_read = loe_csv(VORDLUS)
    vrd_vana = [tuple(r[1:4]) for r in vrd_read[1:] if r and r[0] == iid]
    vrd_muutus = vrd_uus is not None and [tuple(x) for x in vrd_uus] != vrd_vana
    if not kuiv:
        if muutused:
            for k in pais:
                rida[idx[k]] = uus[k]
            kirjuta_csv(INDEKSID, read)
        if vrd_muutus:
            # uued read senise ploki kohale (või faili lõppu, kui indeksil võrdlust polnud)
            koht = next((i for i, r in enumerate(vrd_read) if r and r[0] == iid), len(vrd_read))
            ylej = [r for r in vrd_read if not (r and r[0] == iid)]
            koht = min(koht, len(ylej))
            kirjuta_csv(VORDLUS, ylej[:koht] + [[iid, a, b, c] for a, b, c in vrd_uus] + ylej[koht:])

    def lahter(s):
        return (s or "–").replace("|", "\\|")
    md = [f"**{vana['pealkiri']}** (`{iid}`)", ""]
    if not muutused and not vrd_muutus:
        md.append("Midagi ei muutunud: sisestatud andmed on tabelis juba olemas.")
        return "\n".join(md), False
    if muutused:
        md += ["| Väli | Enne | Nüüd |", "|---|---|---|"] + [f"| {n} | {lahter(a)} | {lahter(b)} |" for n, a, b in muutused] + [""]
    if vrd_muutus:
        kuju = lambda l: ", ".join(" ".join(x for x in (r, k + "." if k else "", f"({s})" if s else "") if x) for r, k, s in l) or "–"
        md += [f"**Võrdlusriigid:** {kuju(vrd_vana)} → {kuju(vrd_uus)}", ""]
    return "\n".join(md), True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sundmus", help="GitHubi sündmuse JSON-fail (issue)")
    ap.add_argument("--tekst", help="vormi tekst failina (proovimiseks)")
    ap.add_argument("--valjund", help="kuhu kirjutada kokkuvõte (Markdown)")
    ap.add_argument("--kuiv", action="store_true", help="ära muuda faile")
    a = ap.parse_args()
    if a.sundmus:
        with open(a.sundmus, encoding="utf-8") as f:
            tekst = (json.load(f).get("issue") or {}).get("body") or ""
    else:
        with open(a.tekst, encoding="utf-8") as f:
            tekst = f.read()
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
