/* Share links. A link is the address of the app plus #share=<code>. The code travels after the #, so it is never
   sent to the server in the address; when the page opens it hands the code to the app, which opens the analysis. */
(function () {
  'use strict';
  function topWin() { try { void window.top.location.href; return window.top; } catch (e) { return window; } }
  function base() { return topWin().location.href.split('#')[0]; }
  window.sdsShareLink = function (code) { return base() + '#share=' + code; };
  function copy(text) {
    if (navigator.clipboard && window.isSecureContext) return navigator.clipboard.writeText(text);
    return new Promise(function (ok, no) {
      var t = document.createElement('textarea'); t.value = text; t.style.position = 'fixed'; t.style.opacity = '0'; document.body.appendChild(t); t.select();
      try { document.execCommand('copy') ? ok() : no(); } catch (e) { no(e); } document.body.removeChild(t);
    });
  }
  // mode "link" copies the link, anything else the bare code
  window.sdsCopyShare = function (boxId, btn, mode) {
    var t = document.getElementById(boxId); if (!t) return false;
    var v = mode === 'link' ? window.sdsShareLink(t.value) : t.value, old = btn.innerHTML;
    copy(v).then(function () { btn.textContent = 'Copied'; }, function () { t.select(); btn.textContent = 'Press Ctrl+C'; });
    setTimeout(function () { btn.innerHTML = old; }, 2500);
    return false;
  };
  function fromHash() {
    var w = topWin(), m = /^#share=(.+)$/.exec(w.location.hash || '');
    if (!m) return null;
    try { w.history.replaceState(null, '', w.location.href.split('#')[0]); } catch (e) {}
    return m[1];
  }
  $(document).on('shiny:connected', function () {
    var code = fromHash(); if (!code) return;
    // wait until the app has built its pages, then open the analysis
    setTimeout(function () { Shiny.setInputValue('share_hash', code, { priority: 'event' }); }, 1800);
  });
})();
