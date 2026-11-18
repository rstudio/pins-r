"use strict";

const viewLinks = document.querySelectorAll("[data-pin-view]");
const viewPanels = document.querySelectorAll("[data-pin-panel]");

function showPinView() {
  const requested = window.location.hash.slice(1);
  const view = Array.from(viewPanels).some(panel => panel.id === requested)
    ? requested : "overview";
  viewPanels.forEach(panel => { panel.hidden = panel.id !== view; });
  viewLinks.forEach(link => {
    if (link.dataset.pinView === view) link.setAttribute("aria-current", "page");
    else link.removeAttribute("aria-current");
  });
  window.dispatchEvent(new Event("resize"));
}

if (viewLinks.length) {
  window.addEventListener("load", showPinView);
  window.addEventListener("hashchange", showPinView);
}

document.querySelectorAll("[data-copy-code]").forEach(button => {
  button.hidden = false;
  button.addEventListener("click", async () => {
    const code = document.getElementById(button.dataset.copyCode);
    try {
      await navigator.clipboard.writeText(code.textContent);
      button.textContent = "Copied";
    } catch {
      const range = document.createRange();
      range.selectNodeContents(code);
      const selection = window.getSelection();
      selection.removeAllRanges();
      selection.addRange(range);
      button.textContent = "Selected — copy to clipboard";
    }
  });
});

if (window.hljs) {
  hljs.registerLanguage("python", highlight_python);
  hljs.registerLanguage("r", highlight_r);
  document.querySelectorAll(".pin-code code").forEach(code => hljs.highlightBlock(code));
}
