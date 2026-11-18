"use strict";

(() => {
  let parentRoot;
  try {
    if (window.parent !== window) parentRoot = window.parent.document.documentElement;
  } catch {}

  function updateTheme() {
    let theme = "light";
    try {
      theme = parentRoot
        ? (parentRoot.dataset.theme === "dark" || parentRoot.classList.contains("dark") ? "dark" : "light")
        : (window.localStorage.getItem("connect.theme") === "dark" ? "dark" : "light");
    } catch {}
    document.documentElement.dataset.theme = theme;
  }

  updateTheme();
  if (parentRoot) {
    new MutationObserver(updateTheme).observe(parentRoot, {
      attributes: true, attributeFilter: ["class", "data-theme"]
    });
  } else {
    window.addEventListener("storage", event => {
      if (event.key === "connect.theme" || event.key === null) updateTheme();
    });
  }
})();
