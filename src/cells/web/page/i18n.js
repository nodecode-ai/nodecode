// i18n.js -- the page's words in the reader's language. English is the
// source: t(text) answers TEXT in the chosen language, or TEXT itself where
// the catalog has none, so a word never translated reads as it was written.
// A {name} in the text is filled from VALUES after the translation, so a
// translation can move it. The choice is this browser's (localStorage): Auto
// follows the browser's own languages, and a change reloads the page, which
// draws every word again. The page's own HTML says which of its words are
// words: data-i18n on an element whose text is one, data-i18n-attrs naming
// the attributes that hold one (title, placeholder, aria-label), each keyed
// by its English as written there. What the organism says itself -- a
// route's answer, a notice, a log line -- stays as it said it.

import { ZH } from "./zh.js";

const KEY = "nodecode-web-lang";
const CATALOGS = { "zh-Hans": ZH };
// Each named in its own language, so a reader finds theirs.
export const LANGUAGES = [["auto", "Auto"], ["en", "English"], ["zh-Hans", "中文"]];

function kept() {
  try {
    const choice = localStorage.getItem(KEY);
    return LANGUAGES.some(([id]) => id === choice) ? choice : "auto";
  } catch {
    return "auto";
  }
}

const choice = kept();
export const lang = choice !== "auto" ? choice
  : (navigator.languages || [navigator.language]).some((each) => /^zh\b/i.test(each || "")) ? "zh-Hans" : "en";
const catalog = CATALOGS[lang] || {};
document.documentElement.lang = lang;

export function t(text, values) {
  const said = catalog[text] ?? text;
  return values ? said.replace(/\{(\w+)\}/g, (whole, name) => (name in values ? String(values[name]) : whole)) : said;
}

// The page's own HTML in the reader's language, and the choices behind the
// mark in the column's foot that pick it.
export function translatePage(root = document) {
  if (lang !== "en") {
    for (const node of root.querySelectorAll("[data-i18n]")) node.textContent = t(node.textContent.trim());
    for (const node of root.querySelectorAll("[data-i18n-attrs]")) {
      for (const attr of node.dataset.i18nAttrs.split(" ")) {
        if (node.hasAttribute(attr)) node.setAttribute(attr, t(node.getAttribute(attr)));
      }
    }
  }
  for (const button of root.querySelectorAll?.(".lang-choices [data-lang]") || []) {
    button.setAttribute("aria-checked", String(button.dataset.lang === choice));
    button.addEventListener("click", () => {
      if (button.dataset.lang === choice) return;
      try { localStorage.setItem(KEY, button.dataset.lang); } catch {}
      location.reload();
    });
  }
}
