#!/usr/bin/env node
/**
 * Koostab tehisaru (Anthropic API) abil veebilehe kokkuvõtvad lõigud ja kirjutab
 * need faili docs/data/aitekstid.json. Leht eelistab neid tekste reeglipõhistele;
 * kui faili pole või mõni tekst puudub, näitab leht endist automaatset teksti.
 *
 * Käivitamine repo juurkaustast, pärast seda kui R on andmefailid uuendanud:
 *   ANTHROPIC_API_KEY=... node tools/ai_kokkuvotted.mjs
 *
 * Tööpõhimõte (vt ka README):
 *  1. Leht avatakse peata brauseris (sama kood, mida näeb kasutaja) ja sealt
 *     võetakse reeglipõhine mustand ning viimaste perioodide arvud. Nii on
 *     tekstis täpselt need arvud, mis on graafikutel – mudel ise arve ei arvuta.
 *  2. Iga teksti kohta tehakse üks API-päring. Kui lähteandmed pole eelmisest
 *     korrast muutunud (räsi), jääb vana tekst alles ja päringut ei tehta.
 *  3. Vastus kontrollitakse üle: iga arv peab esinema lähtematerjalis, lauseid
 *     peab olema mõistlik arv ja keelatud sõnu (prognoos, soovitus) ei tohi olla.
 *     Kui kontroll ebaõnnestub, proovitakse üks kord uuesti; muidu jääb kehtima
 *     vana tekst või reeglipõhine variant.
 *
 * Keskkonnamuutujad:
 *   ANTHROPIC_API_KEY  API võti (ilma selleta skript ei tee midagi)
 *   AI_TEKSTID=0       lüliti: tühjendab tekstid, leht läheb tagasi reeglipõhisele
 *   AI_MUDEL           mudeli nimi (vaikimisi claude-sonnet-5-5)
 *   AI_URL             API aadress (testimiseks)
 *   CHROME_PATH        brauseri asukoht, kui see pole tavapärases kohas
 */
import { createServer } from 'node:http';
import { readFile, writeFile, rename } from 'node:fs/promises';
import { existsSync, readFileSync, appendFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { join, resolve, extname, normalize } from 'node:path';
import { fileURLToPath } from 'node:url';
import puppeteer from 'puppeteer-core';

const JUUR = resolve(process.argv[2] || '.');
const DOCS = join(JUUR, 'docs');
const FAIL = join(DOCS, 'data', 'aitekstid.json');
const AJUTINE = join(DOCS, 'data', '.aitekstid.json.tmp');
const MUDEL = process.env.AI_MUDEL || 'claude-sonnet-5-5';
const API = process.env.AI_URL || 'https://api.anthropic.com/v1/messages';
const VOTI = process.env.ANTHROPIC_API_KEY || '';

const logi = (...a) => console.log('[ai]', ...a);
const hoiata = (m) => { console.log('[ai] HOIATUS:', m); hoiatused.push(m); };
const hoiatused = [];

// ---------------------------------------------------------------- tekstide tellimus
// pikkus = mitu lauset küsida; silt = mida tekst katab (läheb juhisesse)
const TELLIMUS = [
  { votme: 'yldine', lauseid: '4-5',
    mida: 'Eesti majanduse hetkeseis: majanduskasv, tööstus, eksport, jaemüük, hinnad, tööturg ja usaldus.',
    lisaks: 'Alusta sellest, mis seisus majandus tervikuna on. Too eraldi välja see näitaja, mis kõige rohkem eristub või on muutunud, ja ütle, kas pilt on ühtlane või vastuoluline.' },
  { votme: 'indeksid', lauseid: '3-4',
    mida: 'Eesti koht rahvusvahelistes edetabelites (konkurentsivõime, innovatsioon, maksud, korruptsioon jm).',
    lisaks: 'Kirjuta ülevaatlikult, millistes valdkondades Eesti paistab silma ja kus jääb tahapoole. Kasuta arve säästlikult – täpsed kohad on allpool kaartidel.' },
  { votme: 'vordlus_M', freq: 'M', lauseid: '3-4',
    mida: 'Eesti võrdlus Läti, Leedu, Soome ja Rootsiga kuuandmete põhjal.',
    lisaks: 'Ütle, kus Eesti on võrdlusriikide seas ees ja kus taga. Arvesta, et kõrgem väärtus ei tähenda alati paremat tulemust.' },
  { votme: 'vordlus_Q', freq: 'Q', lauseid: '3-4',
    mida: 'Eesti võrdlus Läti, Leedu, Soome ja Rootsiga kvartaliandmete põhjal.',
    lisaks: 'Ütle, kus Eesti on võrdlusriikide seas ees ja kus taga. Arvesta, et kõrgem väärtus ei tähenda alati paremat tulemust.' },
  { votme: 'vordlus_A', freq: 'A', lauseid: '3-4',
    mida: 'Eesti võrdlus Läti, Leedu, Soome ja Rootsiga aastaandmete põhjal.',
    lisaks: 'Ütle, kus Eesti on võrdlusriikide seas ees ja kus taga. Arvesta, et kõrgem väärtus ei tähenda alati paremat tulemust.' },
];

const SYSTEM = `Oled Majandus- ja Kommunikatsiooniministeeriumi avaliku majandusnäitajate töölaua toimetaja. Kirjutad lühikesi eestikeelseid kokkuvõtteid, mida loevad ajakirjanikud, ametnikud ja majandushuvilised.

Reeglid:
- Kasuta ainult etteantud fakte ja arve. Ära arvuta uusi arve, ära ümarda teisiti ja ära lisa teadmisi väljastpoolt etteantud materjali.
- Iga arv, mille kirjutad, peab esinema etteantud materjalis täpselt samal kujul (koma kümnendkohtade eraldajana).
- Jälgi hoolega suunda: kui materjalis on langus või miinusmärgiga arv, ei tohi kirjutada kasvust, ja vastupidi.
- Ära ennusta tulevikku, ära anna poliitikasoovitusi ega hinda valitsuse või ametnike tööd. Kirjelda seda, mida andmed näitavad.
- Kirjuta selges ja neutraalses eesti keeles, täislausetega. Väldi ametnikukeelt ja klišeesid ("oluline on märkida", "kokkuvõttes võib öelda", "jätkuvalt").
- Ära loetle kõiki näitajaid ega korda sama arvu kaks korda. Too esile see, mis on muutunud või eristub, ja seo näitajad omavahel.
- Ära kasuta sõnu "tehisaru", "mudel" ega viita endale.

Vastuse vorming: iga lause omal real, ilma loendimärkide, pealkirjade ja jutumärkideta. Esimene rida on kõige olulisem üldpilt ja selles on kõige olulisem sõnarühm tärnide vahel, näiteks: Eesti *majandus kasvab aeglaselt*, kuid tööturg on nõrgenenud. Tärne kasuta ainult selles ühes kohas.`;

// ---------------------------------------------------------------- abifunktsioonid
const rasi = (s) => createHash('sha256').update(s, 'utf8').digest('hex').slice(0, 16);

// Arvud tekstist võrreldaval kujul: "1 234,5" -> "1234,5", "7.90" -> "7,9", "2,0" -> "2"
function arvud(t) {
  return (t.replace(/(\d)[   ](?=\d{3}\b)/g, '$1').match(/\d+(?:[.,]\d+)?/g) || [])
    .map((a) => { const [tais, murd = ''] = a.replace('.', ',').split(','); const m = murd.replace(/0+$/, ''); return m ? `${tais},${m}` : tais; });
}

function kontrolli(tekst, lahtematerjal, lauseid) {
  const read = tekst.split('\n').map((r) => r.trim()).filter(Boolean);
  const [min, max] = lauseid.split('-').map(Number);
  if (read.length < min || read.length > max + 1) return `lauseid on ${read.length}, oodati ${lauseid}`;
  if (read.some((r) => r.length < 15 || r.length > 320)) return 'mõni lause on liiga lühike või liiga pikk';
  if (read.some((r) => /^[-*•\d]+[.)\s]/.test(r))) return 'lause algab loendimärgiga';
  const keelatud = /prognoos|ennusta|eeldatavasti|tõenäoliselt|peaks valitsus|soovita|tulevikus/i.exec(tekst);
  if (keelatud) return `keelatud sõna "${keelatud[0]}" (prognoos või soovitus)`;
  const lubatud = new Set(arvud(lahtematerjal));
  const puudu = arvud(tekst).filter((a) => !lubatud.has(a));
  if (puudu.length) return `arve ei leia lähteandmetest: ${puudu.join(', ')}`;
  const t = read.join('\n');
  const tarnid = t.match(/\*/g) || [];
  if (tarnid.length !== 2) return `tärne on ${tarnid.length}, peab olema 2`;
  const rohk = /\*([^*]+)\*/.exec(t)[1].trim();
  if (!read[0].includes(`*${rohk}*`)) return 'rõhuasetus ei ole esimeses lauses';
  if (rohk.length < 8 || rohk.length > 70) return 'rõhuasetus on liiga lühike või liiga pikk';
  return null;
}

async function kysi(sonum) {
  const vastus = await fetch(API, {
    method: 'POST',
    headers: { 'content-type': 'application/json', 'x-api-key': VOTI, 'anthropic-version': '2023-06-01' },
    // temperature'i ei saadeta: uuemad mudelid seda enam ei toeta (API annab vea 400).
    // max_tokens on suur, sest mudeli mõttekäik läheb samasse arvestusse: väikese piiri
    // korral saab vastus otsa enne, kui tekst on kirjutatud.
    body: JSON.stringify({ model: MUDEL, max_tokens: 4000, system: SYSTEM, messages: [{ role: 'user', content: sonum }] }),
  });
  if (!vastus.ok) throw new Error(`API ${vastus.status}: ${(await vastus.text()).slice(0, 300)}`);
  const j = await vastus.json();
  const t = (j.content || []).filter((c) => c.type === 'text').map((c) => c.text).join('\n').trim();
  if (!t) throw new Error(`API vastus oli ilma tekstita (stop_reason: ${j.stop_reason}, plokid: ${(j.content || []).map((c) => c.type).join(',') || 'puuduvad'})`);
  return t;
}

// Küsib teksti ja kontrollib selle üle; vea korral teeb ühe parandusringi.
async function koostaTekst(t, lahtematerjal) {
  let sonum = lahtematerjal, viga = null;
  for (let katse = 1; katse <= 2; katse++) {
    const vastus = await kysi(sonum);
    viga = kontrolli(vastus, lahtematerjal, t.lauseid);
    if (!viga) {
      const read = vastus.split('\n').map((r) => r.trim()).filter(Boolean);
      const rohk = (/\*([^*]+)\*/.exec(read.join('\n')) || [])[1] || '';
      return { laused: read.map((r) => r.replace(/\*/g, '')), rohk: rohk.trim() };
    }
    logi(`${t.votme}: ${katse}. katse ei läbinud kontrolli – ${viga}`);
    sonum = `${lahtematerjal}\n\nEelmine vastus ei kõlvanud: ${viga}. Kirjuta kokkuvõte uuesti, järgides täpselt reegleid.`;
  }
  throw new Error(`vastus ei läbinud kontrolli (${viga})`);
}

// ---------------------------------------------------------------- leht peata brauseris
// Reeglipõhine mustand ja viimased arvud võetakse lehelt endalt, et tekstis
// oleksid täpselt samad arvud ja perioodid, mis graafikutel.
async function loeLeht() {
  const tyybid = { '.html': 'text/html', '.js': 'text/javascript', '.json': 'application/json', '.css': 'text/css', '.woff2': 'font/woff2', '.svg': 'image/svg+xml' };
  const server = createServer(async (req, res) => {
    const tee = join(DOCS, normalize(decodeURIComponent(req.url.split('?')[0])).replace(/^(\.\.[/\\])+/, ''));
    try {
      const sisu = await readFile(tee.endsWith('/') ? join(tee, 'index.html') : tee);
      res.writeHead(200, { 'content-type': tyybid[extname(tee)] || 'application/octet-stream' });
      res.end(sisu);
    } catch { res.writeHead(404); res.end(); }
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const port = server.address().port;

  const kroom = process.env.CHROME_PATH
    || ['/usr/bin/google-chrome', '/usr/bin/google-chrome-stable', '/usr/bin/chromium-browser', '/usr/bin/chromium', '/snap/bin/chromium'].find(existsSync);
  if (!kroom) throw new Error('Chrome’i ei leitud (vt CHROME_PATH)');
  const brauser = await puppeteer.launch({ executablePath: kroom, args: ['--no-sandbox', '--disable-dev-shm-usage'] });
  try {
    const leht = await brauser.newPage();
    const vead = [];
    leht.on('pageerror', (e) => vead.push(e.message));
    await leht.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'networkidle0', timeout: 60000 });
    await leht.waitForFunction('typeof KOKKUVOTE !== "undefined" && ULV', { timeout: 30000 });
    const andmed = await leht.evaluate(async () => {
      const lae = (f) => fetch(f).then((r) => r.json()).catch(() => null);
      const [nait, ix, meta, vrd, kasitsiF] = await Promise.all([lae('data/naitajad.json'), lae('data/indeksid.json'), lae('data/kirjeldused.json'), lae('data/vordlus.json'), lae('data/kokkuvote.json')]);
      AITEKSTID = null; // mustand peab tulema reeglitest, mitte eelmisest tehisaru tekstist
      if (!INDEXES) INDEXES = ix.indeksid;
      if (!VGROUPS) VGROUPS = koostaVordlus(vrd, meta.naitajad);
      const n1 = (v) => v.toLocaleString('et-EE', { maximumFractionDigits: 1 });
      // viimased perioodid ühel real: "2026 II kv 1,8; 2026 III kv 2,2"
      const saba = (s, n = 6) => {
        if (!s || !s.periods.length) return '';
        const i0 = Math.max(0, s.periods.length - n);
        return s.periods.slice(i0).map((p, j) => (s.values[i0 + j] == null ? null : `${silt(p)} ${n1(s.values[i0 + j])}`)).filter(Boolean).join('; ');
      };
      const S = (k) => nait.seeriad[k] || { periods: [], values: [] };
      const aastaga = (k, lag) => { const s = S(k); return { periods: s.periods.slice(lag), values: s.values.slice(lag).map((v, i) => (v == null || !s.values[i] ? null : (v / s.values[i] - 1) * 100)) }; };
      const skp = { periods: ULV.skpX, values: ULV.skpY };
      const lisaandmed = [
        ['SKP reaalkasv aastaga, %' + (ULV.kiirN ? ' (viimane kiirhinnang)' : ''), saba(skp)],
        ['Töötleva tööstuse toodang, muutus aastaga, %', saba(S('tootlev_m_yoy'))],
        ['Kaupade eksport, muutus aastaga, %', saba(aastaga('kaup_m', 12))],
        ['Jaemüügi maht, muutus aastaga, %', saba(aastaga('jaem_m_idx', 12))],
        ['Tarbijahinnaindeks, muutus aastaga, %', saba(S('thi_m'))],
        ['Töötuse määr, %', saba(S('tootus_q'), 5)],
        ['Keskmine brutokuupalk, eurot', saba(S('palk_q_lvl'), 4)],
        ['Keskmine brutokuupalk, muutus aastaga, %', saba(S('palk_q_yoy'), 4)],
        ['Majandususaldusindeks, punkti (pikaajaline keskmine 100)', saba(S('esi_m'))],
        ['Majandususaldusindeksi hinnang hilisematele kuudele, punkti', saba(S('esi_m_hinnang'), 3)],
        ['Töötukassa vahendatavad vabad töökohad', saba(S('tk_vabad_m'), 4)],
        ['Tootmisvõimsuse rakendatus töötlevas tööstuses, %', saba(S('voimsus_q'), 4)],
      ].filter(([, v]) => v).map(([k, v]) => `${k}: ${v}`).join('\n');
      const kv = (x) => (x && x.laused && x.laused.length ? x.laused.join('\n') : '');
      return {
        kasitsi: !!(kasitsiF && kasitsiF.tekst && kasitsiF.tekst.trim()),
        mustand: { yldine: kv(koostaKokkuvote(nait, null)), indeksid: kv(koostaIndeksiKokkuvote()),
          vordlus_M: kv(koostaVordluseKokkuvote('M')), vordlus_Q: kv(koostaVordluseKokkuvote('Q')), vordlus_A: kv(koostaVordluseKokkuvote('A')) },
        lisaandmed,
      };
    });
    if (vead.length) hoiata(`lehel oli JavaScripti viga: ${vead[0]}`);
    return andmed;
  } finally {
    await brauser.close().catch(() => {});
    server.close();
  }
}

// ---------------------------------------------------------------- fail
function loeVana() {
  try { return JSON.parse(readFileSync(FAIL, 'utf8')); } catch { return { tekstid: {} }; }
}
// Kirjutab ainult siis, kui sisu muutus (ajatemplit ei arvestata) – muidu tekiks iga päev uus commit.
async function kirjuta(uus) {
  const puhas = (s) => s.replace(/"uuendatud":\s*"[^"]*"/, '"uuendatud":""');
  const sisu = JSON.stringify(uus, null, 1);
  let vana = null;
  try { vana = readFileSync(FAIL, 'utf8'); } catch {}
  if (vana && puhas(vana) === puhas(sisu)) { logi('tekstid ei muutunud'); return false; }
  await writeFile(AJUTINE, sisu, 'utf8');
  await rename(AJUTINE, FAIL);
  logi('kirjutatud', FAIL);
  return true;
}

// ---------------------------------------------------------------- peaosa
async function main() {
  if (process.env.AI_TEKSTID === '0') {
    logi('AI_TEKSTID=0 – tehisaru tekstid lülitatud välja, leht kasutab reeglipõhiseid kokkuvõtteid');
    await kirjuta({ uuendatud: new Date().toISOString(), mudel: '', tekstid: {} });
    return;
  }
  if (!VOTI) { hoiata('ANTHROPIC_API_KEY puudub – tehisaru tekste ei uuendatud'); return; }

  const vana = loeVana();
  const leht = await loeLeht();
  const tekstid = {};
  const paev = new Date().toISOString().slice(0, 10);

  for (const t of TELLIMUS) {
    const mustand = leht.mustand[t.votme];
    const vanaT = vana.tekstid && vana.tekstid[t.votme];
    if (t.votme === 'yldine' && leht.kasitsi) { logi('yldine: analüütiku kokkuvõte on peal, tehisarult ei küsita'); if (vanaT) tekstid[t.votme] = vanaT; continue; }
    if (!mustand) { hoiata(`${t.votme}: reeglipõhist mustandit ei saanud, teksti ei uuendatud`); if (vanaT) tekstid[t.votme] = vanaT; continue; }

    const lisa = t.votme === 'yldine' ? `\n\nLisaandmed (viimased perioodid, varasem kõigepealt):\n${leht.lisaandmed}` : '';
    const lahtematerjal = `Ülesanne: kirjuta ${t.lauseid} lauset kokkuvõtet. Teema: ${t.mida}\n${t.lisaks}\n\n`
      + `Reeglipõhiselt koostatud mustand – kõik faktid ja arvud selles on õiged, aga sõnastus on kohmakas ja järjekord juhuslik:\n${mustand}${lisa}`;
    const r = rasi(`${MUDEL}\n${SYSTEM}\n${lahtematerjal}`);
    if (vanaT && vanaT.rasi === r) { logi(`${t.votme}: lähteandmed ei muutunud, vana tekst jääb`); tekstid[t.votme] = vanaT; continue; }

    try {
      const uus = await koostaTekst(t, lahtematerjal);
      tekstid[t.votme] = { ...uus, rasi: r, aeg: paev };
      logi(`${t.votme}: ${uus.laused.length} lauset`);
    } catch (e) {
      hoiata(`${t.votme}: ${e.message}`);
      if (vanaT) tekstid[t.votme] = vanaT;
    }
  }

  await kirjuta({ uuendatud: new Date().toISOString(), mudel: MUDEL, tekstid });
}

export { arvud, kontrolli }; // tools/ai_test.mjs kontrollib kontrollfunktsioone eraldi

// Peaosa käivitub ainult otse käivitamisel, mitte testist sisse lugemisel.
if (resolve(process.argv[1] || '') === fileURLToPath(import.meta.url)) {
  main().catch((e) => {
    // Tõrge ei tohi andmete uuendust katkestada: leht näitab lihtsalt eelmist või reeglipõhist teksti.
    console.log('[ai] HOIATUS: tekstide koostamine ebaõnnestus:', e.message);
    hoiatused.push(`tekstide koostamine ebaõnnestus: ${e.message}`);
  }).finally(() => {
    if (hoiatused.length) {
      try { appendFileSync(join(JUUR, 'hoiatused.txt'), hoiatused.map((h) => `Tehisaru kokkuvõtted: ${h}\n`).join('')); } catch {}
    }
  });
}
