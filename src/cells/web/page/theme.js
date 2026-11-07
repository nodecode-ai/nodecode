// theme.js -- the page's look: Dark (the default), Light, or System, which
// follows the browser's own prefers-color-scheme as it changes. The choice is
// this browser's (localStorage); what it comes to is data-theme on the root,
// which app.css's light tokens key on. A classic script in the head, not a
// module, so the root carries its theme before the first paint; the choices
// behind the mark in the column's foot are wired once the page is parsed, and
// a choice made in one tab follows in the others.

(() => {
  const KEY = "nodecode-web-theme";
  const CHOICES = ["dark", "light", "system"];
  const light = matchMedia("(prefers-color-scheme: light)");
  const root = document.documentElement;

  function kept() {
    try {
      const choice = localStorage.getItem(KEY);
      return CHOICES.includes(choice) ? choice : "dark";
    } catch {
      return "dark";
    }
  }

  let choice = kept();

  function apply() {
    const theme = choice === "system" ? (light.matches ? "light" : "dark") : choice;
    root.dataset.theme = theme;
    const meta = document.querySelector('meta[name="color-scheme"]');
    if (meta) meta.content = theme;
    for (const button of document.querySelectorAll(".theme-choices [data-choice]")) {
      button.setAttribute("aria-checked", String(button.dataset.choice === choice));
    }
  }

  apply();
  light.addEventListener("change", () => { if (choice === "system") apply(); });
  window.addEventListener("storage", (event) => {
    if (event.key === KEY) { choice = kept(); apply(); }
  });
  document.addEventListener("DOMContentLoaded", () => {
    apply();
    document.addEventListener("click", (event) => {
      const button = event.target.closest(".theme-choices [data-choice]");
      if (!button) return;
      choice = button.dataset.choice;
      try { localStorage.setItem(KEY, choice); } catch {}
      apply();
    });
  });
})();
