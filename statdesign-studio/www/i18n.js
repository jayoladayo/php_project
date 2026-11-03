/* English / Arabic switch. The server sends a dictionary (window.SDS_I18N: exact sentences and patterns). When
   Arabic is on, every text on the screen is translated as it appears (menus, steps, results, notes, help), the page
   turns right-to-left, and the choice is remembered. English texts are kept so the switch can go back. Plots,
   formulas, the data and the problem statements are not translated. */
(function () {
  'use strict';
  var D = window.SDS_I18N || { exact: {}, rx: [] };
  var exact = D.exact, rxs = [], lang = 'en';
  for (var i = 0; i < D.rx.length; i++) { try { rxs.push([new RegExp('^' + D.rx[i][0] + '$'), D.rx[i][1]]); } catch (e) { /* skip a bad pattern */ } }
  var recs = new Map();      // text node -> { orig, ar }
  var attrs = new Map();     // element -> { attr: { orig, ar } }
  var SKIP = 'script, style, textarea, input, code, pre, .katex, .no-i18n, [data-no-i18n], .ps-data, .ps-statement, .ps-parts, .gridbox, .handsontable, .am-formula, .tour-count, #lang-btn, .tm-grid .tm-name';
  var ATTRS = ['placeholder', 'title', 'aria-label', 'data-tip'];

  function norm(s) { return s.replace(/\s+/g, ' ').trim(); }
  function tr(s, depth) {
    var t = norm(s); if (!t) return null;
    var e = exact[t]; if (e !== undefined) return e;
    for (var k = 0; k < rxs.length; k++) {
      var m = rxs[k][0].exec(t);
      if (m) return rxs[k][1].replace(/\$(\d)/g, function (_, n) { var g = m[+n]; if (g === undefined) return ''; var r = (depth || 0) < 4 ? tr(g, (depth || 0) + 1) : null; return r === null ? g : r; });
    }
    return null;
  }
  function keepSpace(orig, ar) { var a = orig.match(/^\s*/)[0], b = orig.match(/\s*$/)[0]; return a + ar + b; }

  function skipped(node) { var p = node.parentElement; return !p || p.closest(SKIP) !== null; }
  function doText(n) {
    var rec = recs.get(n);
    if (rec && n.data === rec.ar) return;                 // already translated
    var orig = n.data; if (!orig.trim()) return;
    if (skipped(n)) return;
    var ar = tr(orig); if (ar === null) { if (rec) recs.delete(n); return; }
    var out = keepSpace(orig, ar); recs.set(n, { orig: orig, ar: out }); n.data = out;
  }
  function doAttrs(el) {
    if (!el.getAttribute || el.closest(SKIP.replace(', input', '').replace(', textarea', ''))) return;
    var rec = attrs.get(el) || {};
    ATTRS.forEach(function (a) {
      var v = el.getAttribute(a); if (!v) return;
      if (rec[a] && v === rec[a].ar) return;
      var ar = tr(v); if (ar === null) { delete rec[a]; return; }
      rec[a] = { orig: v, ar: ar }; el.setAttribute(a, ar);
    });
    if (Object.keys(rec).length) attrs.set(el, rec);
    if (el.tagName === 'OPTION' && !el.closest('.no-i18n')) { var r2 = recs.get(el); if (!(r2 && el.text === r2.ar)) { var a2 = tr(el.text); if (a2 !== null) { recs.set(el, { orig: el.text, ar: a2 }); el.text = a2; } } }
  }
  function walk(root) {
    if (!root) return;
    if (root.nodeType === 3) { doText(root); return; }
    if (root.nodeType !== 1) return;
    if (root.closest && root.closest('.no-i18n, [data-no-i18n]')) return;
    doAttrs(root);
    var tw = document.createTreeWalker(root, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT, null);
    var n = tw.nextNode();
    while (n) { if (n.nodeType === 3) doText(n); else doAttrs(n); n = tw.nextNode(); }
  }

  var pending = new Set(), scheduled = false, obs = null;
  function flush() {
    scheduled = false; if (lang !== 'ar') { pending.clear(); return; }
    if (obs) obs.disconnect();
    pending.forEach(function (n) { if (document.contains(n)) walk(n); });
    pending.clear(); watch();
  }
  function queue(n) { pending.add(n); if (!scheduled) { scheduled = true; requestAnimationFrame(flush); } }
  function watch() {
    if (!obs) obs = new MutationObserver(function (list) {
      for (var i = 0; i < list.length; i++) {
        var m = list[i];
        if (m.type === 'childList') m.addedNodes.forEach(function (n) { queue(n); });
        else if (m.type === 'characterData') queue(m.target);
        else if (m.type === 'attributes') queue(m.target);
      }
    });
    obs.observe(document.body, { childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ATTRS });
  }

  function restore() {
    recs.forEach(function (rec, n) { if (n.nodeType === 3) { if (n.data === rec.ar) n.data = rec.orig; } else if (n.tagName === 'OPTION' && n.text === rec.ar) n.text = rec.orig; });
    attrs.forEach(function (rec, el) { Object.keys(rec).forEach(function (a) { if (el.getAttribute(a) === rec[a].ar) el.setAttribute(a, rec[a].orig); }); });
    recs.clear(); attrs.clear();
  }

  function setLang(l, silent) {
    lang = l === 'ar' ? 'ar' : 'en';
    var h = document.documentElement;
    h.setAttribute('lang', lang); h.setAttribute('dir', lang === 'ar' ? 'rtl' : 'ltr'); document.body && document.body.classList.toggle('rtl', lang === 'ar');
    try { localStorage.setItem('sds-lang', lang); } catch (e) { /* ignore */ }
    if (lang === 'ar') { if (obs) obs.disconnect(); walk(document.body); watch(); } else { if (obs) obs.disconnect(); restore(); watch(); }
    var b = document.getElementById('lang-btn'); if (b) { b.textContent = lang === 'ar' ? 'English' : 'العربية'; b.setAttribute('lang', lang === 'ar' ? 'en' : 'ar'); }
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('lang', lang, { priority: 'event' });
    window.dispatchEvent(new CustomEvent('sds-lang', { detail: lang }));
  }
  window.SDSLang = { set: setLang, toggle: function () { setLang(lang === 'ar' ? 'en' : 'ar'); }, get: function () { return lang; }, tr: tr,
                     // texts on screen that have no translation yet (used to complete the dictionary)
                     missing: function () {
                       var out = {}, tw = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null), n = tw.nextNode();
                       while (n) { var t = norm(n.data); if (t && /[A-Za-z]{3,}/.test(t) && !skipped(n) && !recs.has(n)) out[t] = (out[t] || 0) + 1; n = tw.nextNode(); }
                       return out; } };

  function init() {
    watch();
    var saved = null; try { saved = localStorage.getItem('sds-lang'); } catch (e) { /* ignore */ }
    if (saved === 'ar') setLang('ar', true);
    else { document.documentElement.setAttribute('dir', 'ltr'); if (!document.documentElement.getAttribute('lang')) document.documentElement.setAttribute('lang', 'en'); var b = document.getElementById('lang-btn'); if (b) b.textContent = 'العربية'; }
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init); else init();
  $(document).on('shiny:connected', function () { if (lang === 'ar' && window.Shiny) Shiny.setInputValue('lang', 'ar', { priority: 'event' }); else if (window.Shiny) Shiny.setInputValue('lang', 'en', { priority: 'event' }); });
})();
