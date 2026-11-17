"use strict";

(() => {
  function safeLink(anchor) {
    const href = anchor.getAttribute("href");
    if (!href || href.startsWith("#")) return true;
    try {
      return ["http:", "https:", "mailto:"].includes(new URL(href, document.baseURI).protocol);
    } catch {
      return false;
    }
  }
  function sanitizeLinks() {
    for (const anchor of document.querySelectorAll("a[href]")) {
      if (!safeLink(anchor)) anchor.removeAttribute("href");
    }
  }
  for (const event of ["click", "auxclick"]) {
    document.addEventListener(event, event => {
      const anchor = event.target.closest && event.target.closest("a[href]");
      if (anchor && !safeLink(anchor)) event.preventDefault();
    }, true);
  }
  new MutationObserver(sanitizeLinks).observe(document.documentElement, {
    childList: true, subtree: true, attributes: true, attributeFilter: ["href"]
  });
  sanitizeLinks();
})();
