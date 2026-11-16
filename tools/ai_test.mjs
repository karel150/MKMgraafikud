#!/usr/bin/env node
/**
 * Kontrollib tehisaru vastuste kontrollfunktsioone (tools/ai_kokkuvotted.mjs).
 * Käivitamine repo juurkaustast:  node tools/ai_test.mjs
 */
import { arvud, kontrolli } from './ai_kokkuvotted.mjs';

const LAHE = `Eesti majandus kasvas 2026. aasta III kvartalis 2,2 %.
Töötuse määr oli 7,9 %, aasta varem 7,1 %.
Keskmine brutokuupalk oli 2 243 eurot.`;

let vigu = 0;
const katse = (nimi, tingimus) => { if (!tingimus) { console.log('VIGA:', nimi); vigu++; } else console.log('ok  ', nimi); };
const ok = (t) => kontrolli(t, LAHE, '2-4') === null;
const viga = (t) => kontrolli(t, LAHE, '2-4');

katse('arvud: tuhandeliste eraldaja ja koma', JSON.stringify(arvud('2 243 eurot, 7,9 % ja 2.20')) === JSON.stringify(['2243', '7,9', '2,2']));
katse('korralik vastus läbib', ok('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\nTöötuse määr tõusis 7,9 protsendini.'));
katse('väljamõeldud arv ei läbi', /arve ei leia/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 3,4 %.\nTöötus oli 7,9 %.')));
katse('sama arv teises kirjapildis läbib', ok('Eesti *majandus kasvas kolmandas kvartalis* 2,20 %.\nTöötus oli 7,9 %.'));
katse('prognoos ei läbi', /keelatud sõna/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\nTõenäoliselt kasv jätkub.')));
katse('soovitus ei läbi', /keelatud sõna/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\nSoovitame tööturgu toetada.')));
katse('liiga vähe lauseid ei läbi', /lauseid on/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.')));
katse('liiga palju lauseid ei läbi', /lauseid on/.test(viga(Array(7).fill('Eesti majandus kasvas kolmandas kvartalis 2,2 %.').join('\n').replace('majandus', '*majandus*'))));
katse('tärnideta vastus ei läbi', /tärne on/.test(viga('Eesti majandus kasvas kolmandas kvartalis 2,2 %.\nTöötus oli 7,9 %.')));
katse('rõhk teises lauses ei läbi', /rõhuasetus/.test(viga('Eesti majandus kasvas 2,2 %.\nTöötus on *tõusnud 7,9 protsendini*.')));
katse('loendimärk ei läbi', /loendimärgiga/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\n- Töötus oli 7,9 %.')));
katse('aastaarv lähtematerjalist läbib', ok('*Eesti majandus kasvas* 2026. aasta III kvartalis.\nTöötus oli 7,9 %.'));

katse('katkenud lause ei läbi', /lõpetamata/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\nTöötus oli 7,9 % ja palk')));
katse('liiga pikk lause ei läbi', /liiga pikk/.test(viga('Eesti *majandus kasvas* 2,2 %.\n' + 'Töötuse määr oli 7,9 protsenti ja see on tõusnud, '.repeat(6) + 'nii on.')));
katse('liiga palju arve ei läbi', /arve on/.test(viga('Eesti *majandus kasvas kolmandas kvartalis* 2,2 %.\nTöötus 7,9 %, varem 7,1 %, palk 2 243 eurot, 2026, 2,2 %, 7,9 %, 7,1 %, 2 243, 2026 ja 2,2 %.')));

console.log(vigu ? `\n${vigu} viga` : '\nkõik katsed läbitud');
process.exit(vigu ? 1 : 0);
