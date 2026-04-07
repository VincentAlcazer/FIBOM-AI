// Placeholders d’unités (gris, disparaissent à la saisie)
$(function(){
  const ph = {
    age: 'years',
    hemoglobin: 'g/L',
    hematocrite: '%',
    hematies: 'T/L',
    MCV: 'fL',
    MCH: 'pg',
    MCHC: 'g/L',
    IDR: '%',
    platelets: 'G/L',
    mpv: 'fL',
    WBC: 'G/L',
    ANC: 'G/L',
    ALC: 'G/L',
    mono: 'G/L',
    eosino: 'G/L',
    baso: 'G/L',
    erythroblasts: '%',
    myelemia: '%',
    myeloblasts: '%'
  };
  Object.entries(ph).forEach(([id, unit])=>{
    const el = document.getElementById(id);
    if (el) el.setAttribute('placeholder', unit);
  });
});
