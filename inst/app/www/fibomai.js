// FIBOM-AI client-side helpers.
// Everything here runs in the browser only: the disclaimer and the
// out-of-range warnings never wait for the Shiny server connection.

(function () {
  // ---- Modals (see fib_modal() in R/functions.R) ----
  function openModal(id) {
    var el = document.getElementById(id);
    if (!el) return;
    el.className += ' fib-open';
    var btn = el.querySelector('.fib-modal-close');
    if (btn) btn.focus();
  }

  function closeModal(el) {
    el.className = el.className.replace(/\s*\bfib-open\b/g, '');
  }

  document.addEventListener('click', function (e) {
    var t = e.target;
    var opener = t.closest ? t.closest('[data-fib-open]') : null;
    if (opener) {
      e.preventDefault();
      openModal(opener.getAttribute('data-fib-open'));
      return;
    }
    // Close button, or click on the backdrop outside the box
    if (t.classList.contains('fib-modal-close')) {
      closeModal(t.closest('.fib-modal'));
    } else if (t.classList.contains('fib-modal')) {
      closeModal(t);
    }
  });

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape' || e.key === 'Esc') {
      var open = document.querySelectorAll('.fib-modal.fib-open');
      for (var i = 0; i < open.length; i++) closeModal(open[i]);
    }
  });

  // ---- Out-of-range warnings ----
  var ranges = null;
  var lastWarned = {};

  function getRanges() {
    if (ranges === null) {
      var el = document.getElementById('fib-ranges');
      try { ranges = el ? JSON.parse(el.textContent) : {}; } catch (err) { ranges = {}; }
    }
    return ranges;
  }

  // Returns the range if `input` holds a value outside it, null otherwise
  function outOfRange(input) {
    var rng = getRanges()[input.id];
    if (!rng || input.value === '') return null;
    var val = parseFloat(input.value);
    if (!isFinite(val)) return null;
    return (val < rng.min || val > rng.max) ? rng : null;
  }

  function escapeHtml(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }

  // Live highlight of the field while typing
  document.addEventListener('input', function (e) {
    var t = e.target;
    if (!t.id || !getRanges()[t.id]) return;
    t.classList.toggle('fib-out-of-range', outOfRange(t) !== null);
  });

  // Popup when leaving a field holding an outlier (once per value)
  document.addEventListener('focusout', function (e) {
    var t = e.target;
    if (!t.id || !getRanges()[t.id]) return;
    var rng = outOfRange(t);
    t.classList.toggle('fib-out-of-range', rng !== null);
    if (!rng || lastWarned[t.id] === t.value) return;
    lastWarned[t.id] = t.value;

    var box = document.querySelector('#fib-warning .fib-warning-text');
    if (!box) return;
    box.innerHTML = '<b>' + escapeHtml(rng.label) + '</b> is outside the training range (<b>' +
      rng.min + ' &ndash; ' + rng.max + '</b>).<br>' +
      'Please verify input data: predictions using outlier data should be interpreted with caution.';
    openModal('fib-warning');
  });

  // ---- Missing values: confirm before predicting ----
  var runAnyway = false;

  // Capture phase: runs before Shiny's handler on the button and can stop it
  document.addEventListener('click', function (e) {
    var run = e.target.closest ? e.target.closest('#run') : null;
    if (!run) return;
    if (runAnyway) { runAnyway = false; return; }

    var r = getRanges(), missing = [];
    for (var id in r) {
      var el = document.getElementById(id);
      if (el && el.value === '') missing.push(r[id].label);
    }
    if (!missing.length) return;

    e.preventDefault();
    e.stopImmediatePropagation();
    var list = document.querySelector('#fib-missing .fib-missing-list');
    if (list) {
      list.innerHTML = missing.map(function (m) { return '<li>' + escapeHtml(m) + '</li>'; }).join('');
    }
    openModal('fib-missing');
  }, true);

  document.addEventListener('click', function (e) {
    if (!e.target.classList.contains('fib-run-anyway')) return;
    closeModal(e.target.closest('.fib-modal'));
    runAnyway = true;
    document.getElementById('run').click();
  });

  // ---- "Load example values" ----
  document.addEventListener('click', function (e) {
    if (e.target.id !== 'fib-example') return;
    e.preventDefault();
    var src = document.getElementById('fib-example-values');
    var vals = {};
    try { vals = JSON.parse(src.textContent); } catch (err) { return; }
    for (var id in vals) {
      var el = document.getElementById(id);
      if (!el) continue;
      el.value = vals[id];
      el.classList.remove('fib-out-of-range');
      delete lastWarned[id];
      if (window.jQuery) jQuery(el).trigger('change');
    }
  });

  // ---- Unit placeholders (grey, disappear when typing) ----
  var ph = {
    age: 'years', hemoglobin: 'g/L', hematocrite: '%', hematies: 'T/L',
    MCV: 'fL', MCH: 'pg', MCHC: 'g/L', IDR: '%', platelets: 'G/L', mpv: 'fL',
    WBC: 'G/L', ANC: 'G/L', ALC: 'G/L', mono: 'G/L', eosino: 'G/L', baso: 'G/L',
    erythroblasts: '%', myelemia: '%', myeloblasts: '%'
  };

  document.addEventListener('DOMContentLoaded', function () {
    for (var id in ph) {
      var el = document.getElementById(id);
      if (el) el.setAttribute('placeholder', ph[id]);
    }
    // Keyboard focus on the disclaimer button (Enter closes it)
    var btn = document.querySelector('.fib-modal.fib-open .fib-modal-close');
    if (btn) btn.focus();
  });
})();
