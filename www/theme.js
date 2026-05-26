/* WebTrials — theme.js v4
   Light = white sidebar, black text
   Dark  = dark sidebar, white text
   Patches AdminLTE inline styles directly via JS
*/
(function () {
  "use strict";

  var KEY  = "webtrial_theme";
  var DARK = "dark";
  var LIGHT = "light";

  /* ── Colour maps per mode ──────────────────────────────────
     AdminLTE injects inline style on these selectors.
     We override each one directly.
  ─────────────────────────────────────────────────────────── */
  var COLORS = {
    light: {
      sidebar:     { bg: "#ffffff", color: "#334155" },
      sidebarMenu: { bg: "#ffffff", color: "#334155" },
      header:      { bg: "#ffffff", color: "#0f172a", border: "#e2e8f0" },
      body:        { bg: "#f1f5f9", color: "#0f172a" },
      content:     { bg: "#f1f5f9", color: "#0f172a" }
    },
    dark: {
      sidebar:     { bg: "#111827", color: "#e2e8f0" },
      sidebarMenu: { bg: "#111827", color: "#cbd5e1" },
      header:      { bg: "#111827", color: "#e2e8f0", border: "#1f2937" },
      body:        { bg: "#0d1117", color: "#e2e8f0" },
      content:     { bg: "#0d1117", color: "#e2e8f0" }
    }
  };

  function getSaved() {
    try { return localStorage.getItem(KEY); } catch(e) { return null; }
  }

  /* ── Patch a single element's inline style ─────────────── */
  function patch(el, styles) {
    if (!el) return;
    Object.keys(styles).forEach(function(prop) {
      el.style.setProperty(prop, styles[prop], "important");
    });
  }

  /* ── Apply everything for a given mode ─────────────────── */
  function applyTheme(mode) {
    var c = COLORS[mode] || COLORS[LIGHT];

    /* 1. HTML attribute — drives all CSS variable selectors */
    document.documentElement.setAttribute("data-theme", mode);

    /* 2. Persist */
    try { localStorage.setItem(KEY, mode); } catch(e) {}

    /* 3. Body */
    patch(document.body, {
      "background-color": c.body.bg,
      "color": c.body.color
    });

    /* 4. Sidebar container(s) — AdminLTE inline bg */
    [".main-sidebar", ".left-side"].forEach(function(sel) {
      document.querySelectorAll(sel).forEach(function(el) {
        patch(el, {
          "background-color": c.sidebar.bg,
          "color": c.sidebar.color,
          "transition": "background-color 0.25s, color 0.25s"
        });
      });
    });

    /* 5. Sidebar inner wrapper */
    document.querySelectorAll(".sidebar").forEach(function(el) {
      patch(el, { "background-color": c.sidebar.bg, "color": c.sidebar.color });
    });

    /* 6. Every sidebar menu link */
    document.querySelectorAll(".sidebar-menu li a, .sidebar-menu li span").forEach(function(el) {
      el.style.removeProperty("color"); /* let CSS vars take over */
    });

    /* 7. Header — changes with mode */
    document.querySelectorAll(".main-header .navbar").forEach(function(el) {
      patch(el, { "background-color": c.header.bg, "color": c.header.color });
    });
    document.querySelectorAll(".main-header .logo").forEach(function(el) {
      patch(el, {
        "background-color": c.header.bg,
        "color": c.header.color,
        "border-right-color": c.header.border
      });
    });
    /* Sidebar toggle icon and nav links */
    document.querySelectorAll(".main-header .sidebar-toggle, .main-header .navbar-nav > li > a").forEach(function(el) {
      el.style.setProperty("color", c.header.color, "important");
    });

    /* 8. Content wrapper */
    document.querySelectorAll(".content-wrapper, .right-side, .main-footer").forEach(function(el) {
      patch(el, { "background-color": c.content.bg, "color": c.content.color });
    });

    /* 9. Sync toggle button labels */
    document.querySelectorAll(".wt-theme-toggle").forEach(function(btn) {
      var lbl = btn.querySelector(".toggle-label");
      if (lbl) lbl.textContent = (mode === DARK) ? "☀ Light" : "🌙 Dark";
    });
  }

  /* ── Build toggle button ────────────────────────────────── */
  function makeBtn() {
    var btn = document.createElement("button");
    btn.className = "wt-theme-toggle";
    btn.innerHTML =
      '<span class="toggle-track"><span class="toggle-knob"></span></span>' +
      '<span class="toggle-label">🌙 Dark</span>';
    btn.addEventListener("click", function() {
      var cur = document.documentElement.getAttribute("data-theme");
      applyTheme(cur === DARK ? LIGHT : DARK);
    });
    return btn;
  }

  /* ── Inject into shinydashboard header ──────────────────── */
  function injectDashboard() {
    var ul = document.querySelector(
      ".main-header .navbar-custom-menu ul.navbar-nav, .main-header .navbar-right"
    );
    if (!ul) return false;
    if (ul.querySelector(".wt-theme-toggle")) return true;
    var li = document.createElement("li");
    li.style.cssText = "display:flex;align-items:center;padding:0 6px;";
    li.appendChild(makeBtn());
    ul.insertBefore(li, ul.firstChild);
    return true;
  }

  /* ── Inject into fixed div (user view) ──────────────────── */
  function injectFixed() {
    var el = document.getElementById("theme-toggle-fixed");
    if (!el) return false;
    if (el.querySelector(".wt-theme-toggle")) return true;
    el.appendChild(makeBtn());
    return true;
  }

  /* ── Retry injection ────────────────────────────────────── */
  var _tries = 0;
  function tryInject() {
    var ok = injectDashboard() || injectFixed();
    if (!ok && _tries++ < 30) {
      setTimeout(tryInject, 250);
    } else if (ok) {
      /* Re-sync labels after injection */
      var saved = getSaved();
      applyTheme(saved === DARK ? DARK : LIGHT);
    }
  }

  /* ── MutationObserver — re-patch when AdminLTE re-renders ─ */
  var _debounce = null;
  var _obs = new MutationObserver(function() {
    clearTimeout(_debounce);
    _debounce = setTimeout(function() {
      var saved = getSaved();
      applyTheme(saved === DARK ? DARK : LIGHT);
      /* Also try inject in case header was just rendered */
      if (!document.querySelector(".wt-theme-toggle")) {
        _tries = 0;
        tryInject();
      }
    }, 80);
  });

  /* ── Shiny message handler ──────────────────────────────── */
  function setupShiny() {
    if (!window.Shiny) { setTimeout(setupShiny, 500); return; }
    Shiny.addCustomMessageHandler("__themeReapply__", function() {
      _tries = 0;
      var saved = getSaved();
      setTimeout(function() { applyTheme(saved === DARK ? DARK : LIGHT); tryInject(); }, 200);
      setTimeout(function() { applyTheme(saved === DARK ? DARK : LIGHT); tryInject(); }, 600);
    });
  }

  /* ── Boot: apply before first paint ────────────────────── */
  var _boot = getSaved();
  applyTheme(_boot === DARK ? DARK : LIGHT);

  function init() {
    tryInject();
    setupShiny();
    _obs.observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ["style"] });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }

})();
