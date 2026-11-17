#!/usr/bin/env python3
"""Täidab muudatuste logi (docs/data/uuendused.json) tagantjärele git-ajaloost.

Tavaolukorras kirjutab logi R (logi_muudatused failis R/abi.R), võrreldes iga
käivituse tulemust eelmise failiga. Seda skripti läheb vaja ühekordselt – kui
logi on tühi või kadunud, aga andmefaili varasemad versioonid on git-ajaloos
alles. Commit'i kuupäev ütleb, mis päeval muudatus lehele jõudis.

Reeglid on samad, mis R-i poolel: uued perioodid (kuni kaks värskeimat seeria
kohta), parandused ainult kuues viimases perioodis ja ainult siis, kui vahe on
suurem kui ümardamismüra.

Käivitamine repo juurkaustast:
    python3 tools/uuenduste_logi_ajaloost.py            # kirjutab faili
    python3 tools/uuenduste_logi_ajaloost.py --kuiv     # näitab ainult tulemust
"""
import argparse
import datetime as dt
import json
import subprocess
import sys

FAIL = "docs/data/naitajad.json"
VALJUND = "docs/data/uuendused.json"
PAEVI = 21      # kui vanu kirjeid hoitakse (sama mis R-is)
MAX = 300
VAATA = 6       # mitut viimast perioodi paranduste osas vaadatakse


def git(*argumendid):
    return subprocess.run(["git", *argumendid], capture_output=True, text=True, check=True).stdout


def oluline_vahe(uus, vana):
    if uus is None or vana is None:
        return (uus is None) != (vana is None)
    return abs(uus - vana) > max(0.05, abs(vana) * 0.001)


def seeriaks(s):
    """Seeria perioodid ja väärtused paarideks; puuduvad väärtused jäävad None-iks."""
    return dict(zip(s.get("periods", []), s.get("values", []))), list(s.get("periods", []))


UUSI_MAX = 15   # rohkem uusi seeriaid korraga = töölaua muudatus, mitte andmeuudis


def muudatused(uus, vana, paev):
    kirjed, uued_seeriad = [], []
    for v6ti, u in sorted(uus.get("seeriad", {}).items()):
        if not u or not u.get("periods"):
            continue
        uv, up = seeriaks(u)
        v = vana.get("seeriad", {}).get(v6ti)
        if not v or not v.get("periods"):
            # failis uus seeria: ainult viimane periood, mitte kogu ajalugu
            olemas = [p for p in up if uv[p] is not None]
            if olemas:
                uued_seeriad.append({"paev": paev, "seeria": v6ti, "liik": "uus",
                                     "periood": olemas[-1], "v": uv[olemas[-1]]})
            continue
        vv, vp = seeriaks(v)
        for p in sorted(set(up) - set(vp))[-2:]:
            if uv[p] is not None:
                kirjed.append({"paev": paev, "seeria": v6ti, "liik": "uus", "periood": p, "v": uv[p]})
        for p in reversed([p for p in vp[-VAATA:] if p in uv]):
            if uv[p] is not None and oluline_vahe(uv[p], vv[p]):
                kirjed.append({"paev": paev, "seeria": v6ti, "liik": "parandus",
                               "periood": p, "v": uv[p], "vana": vv[p]})
                break
    if len(uued_seeriad) > UUSI_MAX:
        print(f"  {paev}: {len(uued_seeriad)} uut seeriat – töölaua muudatus, jäetakse logist välja")
        uued_seeriad = []
    return kirjed + uued_seeriad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--kuiv", action="store_true", help="ainult näita, ära kirjuta")
    args = ap.parse_args()

    read = [r.split("|", 1) for r in git("log", "--format=%H|%ad", "--date=format-local:%Y-%m-%d",
                                         "--reverse", "--", FAIL).splitlines() if r]
    if len(read) < 2:
        sys.exit("Ajaloos on alla kahe versiooni, võrrelda pole midagi.")
    print(f"Versioone ajaloos: {len(read)} ({read[0][1]} ... {read[-1][1]})")

    kirjed = []
    eelmine = json.loads(git("show", f"{read[0][0]}:{FAIL}"))
    for sha, paev in read[1:]:
        praegu = json.loads(git("show", f"{sha}:{FAIL}"))
        uued = muudatused(praegu, eelmine, paev)
        if uued:
            print(f"  {paev}: {len(uued)} muudatust")
        kirjed += uued
        eelmine = praegu

    alates = str(dt.date.today() - dt.timedelta(days=PAEVI))
    kirjed = [k for k in kirjed if k["paev"] >= alates][-MAX:]
    print(f"Kokku {len(kirjed)} kirjet alates {alates}")
    if args.kuiv:
        for k in kirjed:
            print("  ", k)
        return
    sisu = {"uuendatud": dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"), "kirjed": kirjed}
    with open(VALJUND, "w", encoding="utf-8") as f:
        json.dump(sisu, f, ensure_ascii=False, separators=(",", ":"))
    print("Kirjutatud:", VALJUND)


if __name__ == "__main__":
    main()
