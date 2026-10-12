// Navigation, theme and small interactions.
(function () {
  function setTheme(t) {
    document.documentElement.setAttribute('data-bs-theme', t);
    try { localStorage.setItem('sds-theme', t); } catch (e) {}
  }
  var saved = null;
  try { saved = localStorage.getItem('sds-theme'); } catch (e) {}
  setTheme(saved || (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'));
  window.toggleTheme = function () {
    setTheme(document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'light' : 'dark');
  };

  var chev = '<svg class="ic" width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M9 18l6-6-6-6"/></svg>';
  function esc(s) { var d = document.createElement('div'); d.textContent = s; return d.innerHTML; }

  function setCrumb(cr) {
    var c = document.getElementById('crumb');
    if (!c || !cr) return;
    c.innerHTML = cr.map(function (x, i) { return i === cr.length - 1 ? '<b>' + esc(x) + '</b>' : '<span>' + esc(x) + '</span>' + chev; }).join('');
  }

  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('crumb', function (m) { setCrumb(m.crumb); });
    Shiny.addCustomMessageHandler('ps_ready', function (m) { setTimeout(function () { Shiny.setInputValue(m.id, m.token, { priority: 'event' }); }, 50); });
    Shiny.addCustomMessageHandler('scroll_top', function (m) { window.scrollTo(0, 0); });
    Shiny.addCustomMessageHandler('scroll_to', function (m) {
      setTimeout(function () { var el = document.getElementById(m.id); if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' }); }, 250);
    });
    Shiny.addCustomMessageHandler('learn_on', function (m) {
      document.querySelectorAll('.lnav .lnk').forEach(function (a) { a.classList.toggle('on', a.getAttribute('data-k') === m.k); });
    });
    Shiny.addCustomMessageHandler('nav', function (m) {
      document.querySelectorAll('.side a.nav-item').forEach(function (a) {
        a.classList.toggle('on', a.getAttribute('data-page') === m.page);
      });
      var c = document.getElementById('crumb');
      setCrumb(m.crumb);
      document.querySelector('.app').classList.remove('nav-open');
      window.scrollTo(0, 0);
    });
    Shiny.addCustomMessageHandler('badge', function (m) {
      var b = document.getElementById(m.id || 'data-count');
      if (!b) return;
      b.textContent = m.n;
      b.style.display = m.n > 0 ? '' : 'none';
    });
  });

  // Drag pinned report items to reorder them.
  var dragEl = null;
  $(document).on('dragstart', '.pinrow', function (e) { dragEl = this; this.classList.add('dragging'); e.originalEvent.dataTransfer.effectAllowed = 'move'; try { e.originalEvent.dataTransfer.setData('text/plain', ''); } catch (x) {} });
  $(document).on('dragover', '.pinrow', function (e) {
    if (!dragEl || dragEl === this) return;
    e.preventDefault();
    var r = this.getBoundingClientRect();
    this.parentNode.insertBefore(dragEl, (e.originalEvent.clientY - r.top) > r.height / 2 ? this.nextSibling : this);
  });
  $(document).on('dragend', '.pinrow', function () {
    this.classList.remove('dragging');
    var list = this.parentNode; dragEl = null;
    var ids = Array.prototype.map.call(list.querySelectorAll('.pinrow'), function (x) { return x.getAttribute('data-id'); });
    Shiny.setInputValue(list.getAttribute('data-input'), ids, { priority: 'event' });
  });

  // Let the whole drop zone accept dropped files.
  $(document).on('dragover', '.dropzone', function (e) { e.preventDefault(); $(this).addClass('drag'); });
  $(document).on('dragleave drop', '.dropzone', function () { $(this).removeClass('drag'); });
  $(document).on('drop', '.dropzone', function (e) {
    e.preventDefault();
    var files = e.originalEvent.dataTransfer && e.originalEvent.dataTransfer.files;
    var input = this.querySelector('input[type=file]');
    if (!files || !files.length || !input) return;
    input.files = files;
    $(input).trigger('change');
  });

  // ---------- Ask Professor Riaz ----------
  var RZ = 'riaz-', RZKEY = 'sds-riaz';
  function rz(id) { return document.getElementById(RZ + id); }
  window.riazOpen = function (on) {
    document.body.classList.toggle('riaz-on', !!on);
    if (on) setTimeout(function () { var q = rz('q'); if (q && !document.body.classList.contains('riaz-set')) q.focus(); rzScroll(); }, 60);
  };
  window.riazView = function (v) { document.body.classList.toggle('riaz-set', v === 'settings'); if (v === 'settings') window.riazOpen(true); };
  window.riazSend = function () {
    var q = rz('q'); if (!q) return;
    var t = q.value.trim(); if (!t || document.body.classList.contains('riaz-busy')) return;
    Shiny.setInputValue(RZ + 'send', { text: t, n: Math.random() }, { priority: 'event' });
    q.value = ''; rzGrow(q);
  };
  window.riazAsk = function (t) { var q = rz('q'); if (q) { q.value = t; window.riazSend(); } };
  function rzGrow(q) { q.style.height = 'auto'; q.style.height = Math.min(q.scrollHeight, 140) + 'px'; }
  function rzScroll() { var m = rz('msgs'); if (m) m.scrollTop = m.scrollHeight; }
  $(document).on('keydown', '#riaz-q', function (e) {
    if (e.key === 'Enter' && !e.shiftKey && !e.isComposing) { e.preventDefault(); window.riazSend(); }
  });
  $(document).on('input', '#riaz-q', function () { rzGrow(this); });
  $(document).on('keydown', function (e) { if (e.key === 'Escape' && document.body.classList.contains('riaz-on')) window.riazOpen(false); });
  // Maths from the model: KaTeX when it has loaded, readable symbols otherwise.
  var TEXSYM = { alpha: 'α', beta: 'β', gamma: 'γ', delta: 'δ', epsilon: 'ε', mu: 'μ', sigma: 'σ', tau: 'τ', chi: 'χ', eta: 'η', rho: 'ρ', lambda: 'λ',
    le: '≤', leq: '≤', ge: '≥', geq: '≥', neq: '≠', ne: '≠', times: '×', cdot: '·', pm: '±', approx: '≈', sim: '~', infty: '∞', bar: '' };
  function rzMath(root) {
    root.querySelectorAll('.rz-math:not(.done)').forEach(function (el) {
      var tex = el.textContent; el.classList.add('done');
      if (window.katex) {
        try { katex.render(tex, el, { throwOnError: false, displayMode: el.classList.contains('d') }); return; } catch (x) {}
      }
      el.textContent = tex.replace(/\\[dt]?frac\{([^{}]*)\}\{([^{}]*)\}/g, '($1)/($2)')
        .replace(/\\(bar|overline)\{([^{}]*)\}/g, '$2̄').replace(/\\hat\{([^{}]*)\}/g, '$1̂')
        .replace(/\\(text|mathrm|mathbf|operatorname)\{([^{}]*)\}/g, '$2').replace(/\\sqrt\{([^{}]*)\}/g, '√($1)')
        .replace(/\\([a-zA-Z]+)/g, function (m, w) { return TEXSYM[w] !== undefined ? TEXSYM[w] : w; })
        .replace(/_\{?([0-9a-zA-Z]+)\}?/g, function (m, w) { return w.length === 1 && /[0-9]/.test(w) ? '₀₁₂₃₄₅₆₇₈₉'[+w] : '_' + w; })
        .replace(/[{}]/g, '');
    });
  }
  $(document).on('shiny:value', function (e) {
    if (e.name !== RZ + 'messages') return;
    setTimeout(function () {
      var box = document.getElementById('riaz-msgs'); if (box) rzMath(box);
      document.querySelectorAll('#riaz-msgs pre').forEach(function (pre) {
        if (pre.querySelector('.rz-copy')) return;
        var b = document.createElement('button'); b.className = 'rz-copy'; b.type = 'button'; b.textContent = 'Copy';
        b.onclick = function () {
          var code = pre.querySelector('code'); var txt = (code || pre).innerText;
          var done = function () { b.textContent = 'Copied'; setTimeout(function () { b.textContent = 'Copy'; }, 1500); };
          if (navigator.clipboard) navigator.clipboard.writeText(txt).then(done, done); else done();
        };
        pre.appendChild(b);
      });
      rzScroll();
    }, 30);
  });
  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('riaz_store', function (m) {
      try { if (m.clear) localStorage.removeItem(RZKEY); else localStorage.setItem(RZKEY, JSON.stringify(m)); } catch (e) {}
    });
    Shiny.addCustomMessageHandler('riaz_view', function (m) { window.riazView(m.view); });
    Shiny.addCustomMessageHandler('riaz_busy', function (m) {
      document.body.classList.toggle('riaz-busy', !!m.busy);
      var b = rz('sendbtn'); if (b) b.disabled = !!m.busy;
    });
    var s = null;
    try { s = JSON.parse(localStorage.getItem(RZKEY) || 'null'); } catch (e) {}
    if (s && s.provider) Shiny.setInputValue(RZ + 'restore', s, { priority: 'event' });
  });
})();
