// Static MAIN-world snapshot implementation injected by the MV3 service worker.
// Keep this file free of eval/new Function so `chrome_snapshot` works on strict-CSP pages.
(() => {
  function getPiChromeState() {
    const state = window.__PI_CHROME_STATE__ || {
      nextElementUid: 1,
      elements: {},
      console: [],
      network: [],
      nextRequestId: 1,
      instrumentationInstalled: false,
      lastSnapshotDigest: null,
    };
    window.__PI_CHROME_STATE__ = state;
    return state;
  }

  function rememberElement(element) {
    const state = getPiChromeState();
    if (!element.__piChromeUid) element.__piChromeUid = "el-" + state.nextElementUid++;
    state.elements[element.__piChromeUid] = element;
    return element.__piChromeUid;
  }

  function isElementVisible(element) {
    if (!element || !element.getBoundingClientRect) return false;
    const style = getComputedStyle(element);
    if (style.visibility === "hidden" || style.display === "none") return false;
    const rect = element.getBoundingClientRect();
    if (rect.width === 0 || rect.height === 0) return false;
    if (rect.bottom < 0 || rect.right < 0) return false;
    if (rect.top > innerHeight || rect.left > innerWidth) return false;
    return true;
  }

  function occluderAt(x, y, expected) {
    const top = document.elementFromPoint(x, y);
    if (!top || top === expected) return null;
    if (expected && expected.contains(top)) return null;
    if (top.contains(expected)) return null;
    return {
      tag: top.tagName.toLowerCase(),
      id: top.id || undefined,
      className: typeof top.className === "string" ? top.className : undefined,
    };
  }

  function textOf(element, max) {
    return (element?.innerText || element?.textContent || "").replace(/\s+/g, " ").trim().slice(0, max || 500);
  }

  function accessibleLabel(element) {
    if (!element) return "";
    const labelledBy = element.getAttribute("aria-labelledby");
    if (labelledBy) {
      const text = labelledBy.split(/\s+/).map((id) => document.getElementById(id)?.innerText || "").join(" ").trim();
      if (text) return text;
    }
    const id = element.id;
    if (id) {
      try {
        const label = document.querySelector(`label[for="${cssEscape(id)}"]`);
        if (label?.innerText) return label.innerText;
      } catch {}
    }
    const wrappingLabel = element.closest?.("label");
    return (
      element.getAttribute("aria-label") ||
      element.getAttribute("title") ||
      element.getAttribute("placeholder") ||
      wrappingLabel?.innerText ||
      element.innerText ||
      element.textContent ||
      ""
    ).trim().replace(/\s+/g, " ").slice(0, 180);
  }

  function cssEscape(value) {
    return (window.CSS && CSS.escape) ? CSS.escape(value) : String(value).replace(/[^a-zA-Z0-9_-]/g, "\\$&");
  }

  function roleOf(element) {
    const explicit = element.getAttribute("role");
    if (explicit) return explicit.toLowerCase();
    const tag = element.tagName.toLowerCase();
    const type = (element.getAttribute("type") || "").toLowerCase();
    if (tag === "a" && element.href) return "link";
    if (tag === "button" || type === "button" || type === "submit" || type === "reset") return "button";
    if (tag === "textarea") return "textbox";
    if (tag === "select") return "combobox";
    if (tag === "input") {
      if (["checkbox", "radio", "range", "search", "email", "password", "tel", "url", "number"].includes(type)) return type === "checkbox" || type === "radio" || type === "range" ? type : "textbox";
      return "textbox";
    }
    if (element.isContentEditable) return "textbox";
    if (tag.match(/^h[1-6]$/)) return "heading";
    return tag;
  }

  function isSensitiveField(element) {
    if (!element) return false;
    const tag = element.tagName?.toLowerCase?.() || "";
    if (!/^(input|textarea|select)$/.test(tag) && !element.isContentEditable) return false;
    const type = (element.getAttribute("type") || "").toLowerCase();
    if (["password"].includes(type)) return true;
    const haystack = [
      type,
      element.getAttribute("name"),
      element.id,
      element.getAttribute("autocomplete"),
      element.getAttribute("aria-label"),
      element.getAttribute("placeholder"),
      element.getAttribute("data-testid"),
    ].filter(Boolean).join(" ").toLowerCase();
    return /password|passwd|\bpwd\b|secret|token|bearer|api[-_ ]?key|access[-_ ]?key|auth[-_ ]?code|one[-_ ]?time|otp|2fa|mfa|verification[-_ ]?code|recovery[-_ ]?code|credit[-_ ]?card|card[-_ ]?number|cc-number|cc-csc|cvc|cvv|security[-_ ]?code|ssn|social[-_ ]?security/.test(haystack);
  }

  function installPiChromeInstrumentation() {
    const state = getPiChromeState();
    if (state.instrumentationInstalled) return;
    state.instrumentationInstalled = true;
    const pushConsole = (level, args) => {
      state.console.push({
        id: state.console.length + 1,
        level,
        timestamp: Date.now(),
        url: location.href,
        args: Array.from(args).map((arg) => {
          try {
            if (typeof arg === "string") return arg;
            if (arg instanceof Error) return { name: arg.name, message: arg.message, stack: arg.stack };
            return JSON.parse(JSON.stringify(arg));
          } catch {
            return String(arg);
          }
        }),
      });
      if (state.console.length > 500) state.console.splice(0, state.console.length - 500);
    };
    for (const level of ["debug", "log", "info", "warn", "error"]) {
      const original = console[level];
      if (typeof original !== "function" || original.__piChromeWrapped) continue;
      const wrapped = function(...args) {
        pushConsole(level, args);
        return original.apply(this, args);
      };
      wrapped.__piChromeWrapped = true;
      console[level] = wrapped;
    }
    window.addEventListener("error", (event) => pushConsole("pageerror", [event.message, event.filename + ":" + event.lineno + ":" + event.colno]));
    window.addEventListener("unhandledrejection", (event) => pushConsole("unhandledrejection", [event.reason]));

    const record = (entry) => {
      state.network.push(entry);
      if (state.network.length > 1000) state.network.splice(0, state.network.length - 1000);
      return entry;
    };
    if (window.fetch && !window.fetch.__piChromeWrapped) {
      const originalFetch = window.fetch.bind(window);
      const wrappedFetch = async (...args) => {
        const id = "req-" + state.nextRequestId++;
        const startedAt = Date.now();
        const input = args[0];
        const init = args[1] || {};
        const url = typeof input === "string" ? input : input?.url;
        const method = (init.method || input?.method || "GET").toUpperCase();
        const entry = record({ id, type: "fetch", method, url: String(url || ""), startedAt, pageUrl: location.href, status: "pending" });
        try {
          const response = await originalFetch(...args);
          entry.status = response.status;
          entry.statusText = response.statusText;
          entry.ok = response.ok;
          entry.responseUrl = response.url;
          entry.durationMs = Date.now() - startedAt;
          entry.responseHeaders = Array.from(response.headers.entries());
          entry.responseBodyOmitted = "response body capture is disabled by default";
          return response;
        } catch (error) {
          entry.error = error?.message || String(error);
          entry.durationMs = Date.now() - startedAt;
          throw error;
        }
      };
      wrappedFetch.__piChromeWrapped = true;
      window.fetch = wrappedFetch;
    }
    if (window.XMLHttpRequest && !XMLHttpRequest.prototype.open.__piChromeWrapped) {
      const originalOpen = XMLHttpRequest.prototype.open;
      const originalSend = XMLHttpRequest.prototype.send;
      XMLHttpRequest.prototype.open = function(method, url, ...rest) {
        this.__piChromeRequest = { method: String(method || "GET").toUpperCase(), url: String(url || "") };
        return originalOpen.call(this, method, url, ...rest);
      };
      XMLHttpRequest.prototype.open.__piChromeWrapped = true;
      XMLHttpRequest.prototype.send = function(body) {
        const id = "req-" + state.nextRequestId++;
        const startedAt = Date.now();
        const info = this.__piChromeRequest || {};
        const entry = record({ id, type: "xhr", method: info.method || "GET", url: info.url || "", startedAt, pageUrl: location.href, status: "pending" });
        this.addEventListener("loadend", () => {
          entry.status = this.status;
          entry.statusText = this.statusText;
          entry.responseUrl = this.responseURL;
          entry.durationMs = Date.now() - startedAt;
          try { entry.responseHeadersText = this.getAllResponseHeaders(); } catch {}
          entry.responseBodyOmitted = "response body capture is disabled by default";
        });
        this.addEventListener("error", () => { entry.error = "XMLHttpRequest error"; entry.durationMs = Date.now() - startedAt; });
        return originalSend.call(this, body);
      };
    }
  }

  function hashString(text) {
    let h = 0;
    for (let i = 0; i < text.length; i++) h = (h * 31 + text.charCodeAt(i)) | 0;
    return h;
  }

  function selectorFor(element) {
    const unique = (selector) => {
      try { return document.querySelectorAll(selector).length === 1; } catch { return false; }
    };
    if (element.id && unique("#" + cssEscape(element.id))) return "#" + cssEscape(element.id);
    const attr = ["aria-label", "name", "placeholder", "data-testid", "role"].find((name) => element.getAttribute(name));
    if (attr) {
      const candidate = element.tagName.toLowerCase() + "[" + attr + "=" + JSON.stringify(element.getAttribute(attr)) + "]";
      if (unique(candidate)) return candidate;
    }
    const parts = [];
    let current = element;
    while (current && current.nodeType === Node.ELEMENT_NODE && parts.length < 5) {
      let part = current.tagName.toLowerCase();
      if (current.classList.length > 0) part += "." + Array.from(current.classList).slice(0, 2).map(cssEscape).join(".");
      const siblings = Array.from(current.parentElement?.children ?? []).filter((sibling) => sibling.tagName === current.tagName);
      if (siblings.length > 1) part += ":nth-of-type(" + (siblings.indexOf(current) + 1) + ")";
      parts.unshift(part);
      const candidate = parts.join(" > ");
      if (unique(candidate)) return candidate;
      current = current.parentElement;
    }
    return parts.join(" > ");
  }

  function directHeadingText(element) {
    const labelledBy = element.getAttribute?.("aria-labelledby");
    if (labelledBy) {
      const text = labelledBy.split(/\s+/).map((id) => document.getElementById(id)?.innerText || "").join(" ").replace(/\s+/g, " ").trim();
      if (text) return text.slice(0, 180);
    }
    const aria = element.getAttribute?.("aria-label");
    if (aria) return aria.trim().slice(0, 180);
    const heading = Array.from(element.querySelectorAll?.("h1,h2,h3,h4,[role='heading']") || []).find(isElementVisible);
    if (heading) return textOf(heading, 180);
    return "";
  }

  function meaningfulContainerFor(element) {
    let current = element.parentElement;
    let fallback = current;
    let depth = 0;
    while (current && current !== document.body && depth++ < 8) {
      if (!isElementVisible(current)) { current = current.parentElement; continue; }
      const tag = current.tagName.toLowerCase();
      const role = (current.getAttribute("role") || "").toLowerCase();
      const cls = typeof current.className === "string" ? current.className : "";
      const id = current.id || "";
      const named = Boolean(current.getAttribute("aria-label") || current.getAttribute("aria-labelledby") || directHeadingText(current));
      const semantic = /^(form|dialog|section|article|nav|header|main|aside|footer|li|tr|td|fieldset)$/.test(tag) ||
        /^(dialog|alertdialog|region|group|listitem|row|cell|tabpanel|menu|toolbar|navigation|main|banner|contentinfo|complementary)$/.test(role);
      const classHint = /card|panel|pane|modal|dialog|section|content|container|toolbar|menu|list|item|row|cell|header|footer|sidebar|drawer|popover|dropdown/i.test(`${id} ${cls}`);
      const rect = current.getBoundingClientRect();
      const childActions = current.querySelectorAll?.('a, button, input, textarea, select, [role="button"], [role="link"], [tabindex]:not([tabindex="-1"])').length || 0;
      if ((semantic || classHint || named) && rect.width > 20 && rect.height > 20 && childActions <= 80) return current;
      if (!fallback && rect.width > 20 && rect.height > 20) fallback = current;
      current = current.parentElement;
    }
    return fallback || document.body;
  }

  function contextForElement(element) {
    const container = meaningfulContainerFor(element);
    if (!container || container === document.body || container === element) return undefined;
    return {
      uid: rememberElement(container),
      tag: container.tagName.toLowerCase(),
      role: roleOf(container),
      label: directHeadingText(container) || accessibleLabel(container) || textOf(container, 140),
      rect: rectSummary(container),
    };
  }

  function summarizeElement(element, index) {
    const rect = element.getBoundingClientRect();
    const style = getComputedStyle(element);
    const cx = rect.left + rect.width / 2;
    const cy = rect.top + rect.height / 2;
    const occluded = occluderAt(cx, cy, element);
    const role = roleOf(element);
    const disabled = Boolean(element.disabled || element.getAttribute("aria-disabled") === "true");
    const rawValue = "value" in element && typeof element.value === "string" ? element.value : undefined;
    const sensitive = isSensitiveField(element);
    const value = rawValue && !sensitive ? rawValue.slice(0, 120) : undefined;
    const checked = "checked" in element ? Boolean(element.checked) : undefined;
    return {
      index,
      uid: rememberElement(element),
      tag: element.tagName.toLowerCase(),
      role,
      selector: selectorFor(element),
      label: accessibleLabel(element),
      href: element.href || undefined,
      type: element.getAttribute("type") || undefined,
      value: value || undefined,
      hasValue: rawValue ? rawValue.length > 0 : undefined,
      valueLength: rawValue && sensitive ? rawValue.length : undefined,
      valueRedacted: sensitive && rawValue ? true : undefined,
      checked,
      disabled,
      inert: Boolean(element.closest?.("[inert]")),
      pointerEvents: style.pointerEvents,
      occluded: occluded || undefined,
      context: contextForElement(element),
      rect: { x: Math.round(rect.x), y: Math.round(rect.y), width: Math.round(rect.width), height: Math.round(rect.height) },
    };
  }

  function isInViewport(element) {
    const rect = element.getBoundingClientRect();
    return rect.bottom >= 0 && rect.right >= 0 && rect.top <= innerHeight && rect.left <= innerWidth;
  }

  function formSummaries() {
    const fields = Array.from(document.querySelectorAll('input, textarea, select, [contenteditable="true"]'))
      .filter(isElementVisible)
      .slice(0, 80)
      .map((element, index) => ({
        ...summarizeElement(element, index),
        required: Boolean(element.required || element.getAttribute("aria-required") === "true"),
        invalid: Boolean(element.matches?.(":invalid") || element.getAttribute("aria-invalid") === "true"),
        autocomplete: element.getAttribute("autocomplete") || undefined,
      }));
    const submits = Array.from(document.querySelectorAll('button, input[type="submit"], [role="button"]'))
      .filter(isElementVisible)
      .filter((element) => /submit|save|continue|next|send|sign in|log in|create|update|done/i.test(accessibleLabel(element) + " " + (element.getAttribute("type") || "")))
      .slice(0, 30)
      .map((element, index) => summarizeElement(element, index));
    return { fields, submits };
  }

  function pageMap() {
    const landmarkSelectors = [
      ["header", 'header, [role="banner"]'],
      ["nav", 'nav, [role="navigation"]'],
      ["main", 'main, [role="main"]'],
      ["aside", 'aside, [role="complementary"]'],
      ["footer", 'footer, [role="contentinfo"]'],
      ["dialog", 'dialog, [role="dialog"], [aria-modal="true"]'],
      ["form", "form"],
    ];
    const regions = [];
    for (const [kind, selector] of landmarkSelectors) {
      for (const element of Array.from(document.querySelectorAll(selector)).filter(isElementVisible).slice(0, 12)) {
        const headings = Array.from(element.querySelectorAll("h1,h2,h3,[role='heading']")).filter(isElementVisible).slice(0, 6).map((h) => textOf(h, 120));
        const actions = Array.from(element.querySelectorAll('a, button, input, textarea, select, [role="button"], [role="link"], [tabindex]:not([tabindex="-1"])')).filter(isElementVisible).slice(0, 8).map((a) => {
          const summary = summarizeElement(a, 0);
          return { uid: summary.uid, role: summary.role, label: summary.label || summary.selector, disabled: summary.disabled || undefined };
        });
        regions.push({ kind, uid: rememberElement(element), label: accessibleLabel(element) || headings[0] || textOf(element, 100), headings, actions });
      }
    }
    const headings = Array.from(document.querySelectorAll("h1,h2,h3,[role='heading']")).filter(isElementVisible).slice(0, 30).map((element) => ({
      uid: rememberElement(element),
      level: Number(element.tagName?.slice(1)) || Number(element.getAttribute("aria-level")) || undefined,
      text: textOf(element, 180),
    }));
    return { regions, headings };
  }

  function layoutSections(elements, forms) {
    const byUid = new Map();
    const addToSection = (summary, kind) => {
      const source = getPiChromeState().elements[summary.uid];
      const container = source ? meaningfulContainerFor(source) : null;
      if (!container || container === document.body) return;
      const uid = rememberElement(container);
      let section = byUid.get(uid);
      if (!section) {
        const rect = rectSummary(container);
        section = {
          uid,
          tag: container.tagName.toLowerCase(),
          role: roleOf(container),
          label: directHeadingText(container) || accessibleLabel(container) || textOf(container, 160),
          text: textOf(container, 260),
          rect,
          actions: [],
          fields: [],
        };
        byUid.set(uid, section);
      }
      const item = { uid: summary.uid, role: summary.role, label: summary.label || summary.selector, disabled: summary.disabled || undefined };
      if (kind === "field") section.fields.push(item);
      else section.actions.push(item);
    };
    for (const el of (elements || []).slice(0, 80)) addToSection(el, ["textbox", "checkbox", "radio", "combobox"].includes(el.role) ? "field" : "action");
    for (const field of (forms?.fields || []).slice(0, 80)) addToSection(field, "field");
    const sections = Array.from(byUid.values())
      .filter((section) => section.actions.length || section.fields.length)
      .sort((a, b) => a.rect.y - b.rect.y || a.rect.x - b.rect.x)
      .slice(0, 18);
    for (const section of sections) {
      section.actions = section.actions.slice(0, 10);
      section.fields = section.fields.slice(0, 10);
    }
    return sections;
  }

  function tokenScore(haystack, query) {
    if (!query) return 0;
    const hay = String(haystack || "").toLowerCase();
    const tokens = String(query).toLowerCase().split(/\W+/).filter(Boolean);
    if (!tokens.length) return 0;
    let score = 0;
    for (const token of tokens) {
      if (hay.includes(token)) score += token.length <= 2 ? 1 : 3;
    }
    if (hay.includes(String(query).toLowerCase())) score += 8;
    return score;
  }

  function queryMatches(query, elements, map) {
    if (!query) return [];
    const candidates = [];
    for (const element of elements) {
      const hay = [element.role, element.label, element.selector, element.type, element.href].filter(Boolean).join(" ");
      const score = tokenScore(hay, query);
      if (score > 0) candidates.push({ score, kind: "element", ...element });
    }
    const textNodes = [];
    for (const block of Array.from(document.querySelectorAll("h1,h2,h3,h4,p,li,td,th,label,summary,[role='alert']")).filter(isElementVisible).slice(0, 300)) {
      const text = textOf(block, 300);
      const score = tokenScore(text, query);
      if (score > 0) textNodes.push({ score, kind: "text", uid: rememberElement(block), tag: block.tagName.toLowerCase(), role: roleOf(block), text, rect: rectSummary(block) });
    }
    for (const region of map.regions || []) {
      const score = tokenScore([region.kind, region.label, ...(region.headings || [])].join(" "), query);
      if (score > 0) candidates.push({ score, kind: "region", ...region });
    }
    return candidates.concat(textNodes).sort((a, b) => b.score - a.score).slice(0, 20);
  }

  function rectSummary(element) {
    const rect = element.getBoundingClientRect();
    return { x: Math.round(rect.x), y: Math.round(rect.y), width: Math.round(rect.width), height: Math.round(rect.height) };
  }

  function activeElementSummary() {
    const el = document.activeElement;
    if (!el || el === document.body || el === document.documentElement) return null;
    return summarizeElement(el, 0);
  }

  function modalSummary() {
    const selectors = 'dialog[open], [role="dialog"], [aria-modal="true"], [role="alertdialog"]';
    const modal = Array.from(document.querySelectorAll(selectors)).find(isElementVisible);
    if (!modal) return null;
    return { uid: rememberElement(modal), tag: modal.tagName.toLowerCase(), role: roleOf(modal), label: accessibleLabel(modal) || textOf(modal, 180), rect: rectSummary(modal) };
  }

  function digestFor(snapshot) {
    return {
      url: snapshot.url,
      title: snapshot.title,
      textHash: hashString(snapshot.text || ""),
      focusedUid: snapshot.focused?.uid || null,
      modalUid: snapshot.modal?.uid || null,
      labels: (snapshot.elements || []).slice(0, 50).map((el) => ({ uid: el.uid, role: el.role, label: el.label, disabled: el.disabled, value: el.value, checked: el.checked })),
    };
  }

  function diffSnapshot(previous, current) {
    if (!previous) return { firstSnapshot: true };
    const changes = [];
    if (previous.url !== current.url) changes.push({ kind: "url", before: previous.url, after: current.url });
    if (previous.title !== current.title) changes.push({ kind: "title", before: previous.title, after: current.title });
    if (previous.textHash !== current.textHash) changes.push({ kind: "textChanged" });
    if (previous.focusedUid !== current.focusedUid) changes.push({ kind: "focus", before: previous.focusedUid, after: current.focusedUid });
    if (previous.modalUid !== current.modalUid) changes.push({ kind: "modal", before: previous.modalUid, after: current.modalUid });
    const prevByUid = new Map((previous.labels || []).map((x) => [x.uid, x]));
    const curByUid = new Map((current.labels || []).map((x) => [x.uid, x]));
    const added = [];
    const removed = [];
    const updated = [];
    for (const cur of current.labels || []) {
      const prev = prevByUid.get(cur.uid);
      if (!prev) added.push(cur);
      else if (prev.label !== cur.label || prev.disabled !== cur.disabled || prev.value !== cur.value || prev.checked !== cur.checked) updated.push({ uid: cur.uid, before: prev, after: cur });
    }
    for (const prev of previous.labels || []) {
      if (!curByUid.has(prev.uid)) removed.push(prev);
    }
    return { changes, added: added.slice(0, 12), removed: removed.slice(0, 12), updated: updated.slice(0, 12) };
  }

  function visibleTextSnippets(maxChars) {
    const snippets = [];
    const blocks = Array.from(document.querySelectorAll("h1,h2,h3,h4,p,li,td,th,label,summary,[role='alert']")).filter(isElementVisible);
    let used = 0;
    for (const block of blocks) {
      if (!isInViewport(block) && snippets.length > 12) continue;
      const text = textOf(block, 500);
      if (!text || snippets.some((s) => s.text === text)) continue;
      const next = { uid: rememberElement(block), tag: block.tagName.toLowerCase(), text, rect: rectSummary(block) };
      snippets.push(next);
      used += text.length;
      if (used >= maxChars || snippets.length >= 40) break;
    }
    return snippets;
  }

  function snapshotPage(maxElements, containingText, roleFilter, nearUid, mode, query, maxTextChars) {
    installPiChromeInstrumentation();
    mode = ["auto", "interactive", "forms", "pageMap", "text", "changes", "full"].includes(mode) ? mode : "auto";
    const fullTextLimit = Number(maxTextChars || (mode === "full" ? 30000 : mode === "text" ? 18000 : 6000));
    let candidates = Array.from(document.querySelectorAll('a, button, input, textarea, select, summary, [role="button"], [role="link"], [role="menuitem"], [role="tab"], [role="checkbox"], [contenteditable="true"], [tabindex]:not([tabindex="-1"])'));
    if (containingText) {
      const needle = String(containingText).toLowerCase();
      candidates = candidates.filter((element) => accessibleLabel(element).toLowerCase().includes(needle));
    }
    if (roleFilter) {
      const wanted = String(roleFilter).toLowerCase();
      candidates = candidates.filter((element) => roleOf(element) === wanted || element.tagName.toLowerCase() === wanted);
    }
    let near;
    if (nearUid) near = getPiChromeState().elements[nearUid];
    if (near) {
      const nearRect = near.getBoundingClientRect();
      const cx = nearRect.left + nearRect.width / 2;
      const cy = nearRect.top + nearRect.height / 2;
      candidates.sort((a, b) => {
        const ra = a.getBoundingClientRect();
        const rb = b.getBoundingClientRect();
        const da = Math.hypot(ra.left + ra.width / 2 - cx, ra.top + ra.height / 2 - cy);
        const db = Math.hypot(rb.left + rb.width / 2 - cx, rb.top + rb.height / 2 - cy);
        return da - db;
      });
    } else {
      candidates.sort((a, b) => {
        const ar = a.getBoundingClientRect();
        const br = b.getBoundingClientRect();
        const avis = isInViewport(a) ? 0 : 1;
        const bvis = isInViewport(b) ? 0 : 1;
        return avis - bvis || ar.top - br.top || ar.left - br.left;
      });
    }
    const visibleCandidates = candidates.filter(isElementVisible);
    const elements = visibleCandidates.slice(0, maxElements).map((element, index) => summarizeElement(element, index));
    const queryElements = query
      ? visibleCandidates.slice(0, Math.max(maxElements, 500)).map((element, index) => summarizeElement(element, index))
      : elements;
    const map = pageMap();
    const forms = formSummaries();
    const layout = layoutSections(elements, forms);
    const focused = activeElementSummary();
    const modal = modalSummary();
    const bodyText = document.body ? document.body.innerText.replace(/\s+\n/g, "\n").trim() : "";
    const text = bodyText.slice(0, fullTextLimit);
    const snapshot = {
      title: document.title,
      url: location.href,
      mode,
      query: query || undefined,
      viewport: { width: innerWidth, height: innerHeight, scrollX, scrollY },
      summary: {
        visibleText: textOf(document.body, 500),
        visibleInteractiveCount: elements.filter((el) => el.rect.y >= 0 && el.rect.y <= innerHeight).length,
        totalInteractiveSampled: elements.length,
        totalInteractiveVisible: visibleCandidates.length,
        focused: focused ? { uid: focused.uid, role: focused.role, label: focused.label } : undefined,
        modal: modal ? { uid: modal.uid, label: modal.label } : undefined,
        hints: [],
      },
      focused: focused || undefined,
      modal: modal || undefined,
      text,
      textTruncated: bodyText.length > text.length,
      textSnippets: visibleTextSnippets(mode === "text" ? 12000 : 3000),
      elements,
      forms,
      layout,
      pageMap: map,
      matches: queryMatches(query, queryElements, map),
      filter: { containingText: containingText || undefined, roleFilter: roleFilter || undefined, nearUid: nearUid || undefined },
    };
    if (snapshot.modal) snapshot.summary.hints.push("A modal/dialog is visible; interact with it before the underlying page.");
    const disabledImportant = elements.find((el) => el.disabled && /submit|save|merge|continue|next|send|approve|login|sign in/i.test(el.label || ""));
    if (disabledImportant) snapshot.summary.hints.push(`${disabledImportant.uid} '${disabledImportant.label}' is disabled.`);
    const occluded = elements.find((el) => el.occluded);
    if (occluded) snapshot.summary.hints.push(`${occluded.uid} '${occluded.label || occluded.role}' appears occluded by ${occluded.occluded.tag}.`);

    const state = getPiChromeState();
    const currentDigest = digestFor(snapshot);
    snapshot.diff = diffSnapshot(state.lastSnapshotDigest, currentDigest);
    state.lastSnapshotDigest = currentDigest;

    if (mode === "interactive") {
      delete snapshot.text;
      delete snapshot.textSnippets;
      delete snapshot.pageMap;
    } else if (mode === "forms") {
      delete snapshot.text;
      delete snapshot.textSnippets;
      snapshot.elements = elements.filter((el) => ["textbox", "checkbox", "radio", "combobox", "button"].includes(el.role));
    } else if (mode === "pageMap") {
      delete snapshot.text;
      delete snapshot.textSnippets;
      snapshot.elements = elements.slice(0, 20);
    } else if (mode === "changes") {
      delete snapshot.text;
      delete snapshot.textSnippets;
      delete snapshot.elements;
      delete snapshot.forms;
      delete snapshot.layout;
      delete snapshot.pageMap;
    } else if (mode === "text") {
      snapshot.elements = elements.slice(0, 20);
    } else if (mode !== "full") {
      snapshot.elements = elements.slice(0, Math.min(maxElements, 40));
      snapshot.text = text.slice(0, Math.min(text.length, 6000));
    }
    return snapshot;
  }

  function inspectTarget(uid, selector, shouldScrollIntoView) {
    installPiChromeInstrumentation();
    const state = getPiChromeState();
    let element = null;
    if (uid) element = state.elements[uid];
    if (!element && selector) element = document.querySelector(selector);
    if (!element || !element.isConnected) throw new Error(uid ? `No live element for uid: ${uid}. Take a fresh (chrome:snapshot).` : `No element matches selector: ${selector}`);
    if (shouldScrollIntoView) element.scrollIntoView?.({ block: "center", inline: "center", behavior: "instant" });
    const summary = summarizeElement(element, 0);
    const ancestors = [];
    let current = element.parentElement;
    while (current && current !== document.body && ancestors.length < 6) {
      ancestors.push({ uid: rememberElement(current), tag: current.tagName.toLowerCase(), role: roleOf(current), label: accessibleLabel(current) || textOf(current, 100), selector: selectorFor(current) });
      current = current.parentElement;
    }
    const container = element.closest?.('form, dialog, [role="dialog"], [aria-modal="true"], section, article, main, aside') || element.parentElement || document.body;
    const nearbyText = Array.from(container.querySelectorAll("h1,h2,h3,h4,p,li,label,[role='alert']"))
      .filter(isElementVisible)
      .slice(0, 24)
      .map((node) => ({ uid: rememberElement(node), tag: node.tagName.toLowerCase(), text: textOf(node, 240), rect: rectSummary(node) }))
      .filter((entry) => entry.text);
    const nearbyActions = Array.from(container.querySelectorAll('a, button, input, textarea, select, [role="button"], [role="link"], [tabindex]:not([tabindex="-1"])'))
      .filter(isElementVisible)
      .slice(0, 30)
      .map((node, index) => summarizeElement(node, index));
    const form = element.closest?.("form");
    const formContext = form ? {
      uid: rememberElement(form),
      label: accessibleLabel(form) || textOf(form, 160),
      fields: Array.from(form.querySelectorAll('input, textarea, select, [contenteditable="true"]')).filter(isElementVisible).slice(0, 30).map((node, index) => summarizeElement(node, index)),
      actions: Array.from(form.querySelectorAll('button, input[type="submit"], [role="button"]')).filter(isElementVisible).slice(0, 12).map((node, index) => summarizeElement(node, index)),
    } : undefined;
    const rect = element.getBoundingClientRect();
    const center = { x: Math.round(rect.left + rect.width / 2), y: Math.round(rect.top + rect.height / 2) };
    const clickSuggestion = summary.disabled || summary.inert || summary.pointerEvents === "none"
      ? undefined
      : { uid: summary.uid, x: center.x, y: center.y };
    return { target: summary, ancestors, nearbyText, nearbyActions, formContext, clickSuggestion };
  }

  globalThis.__piChromeSnapshotPage = snapshotPage;
  globalThis.__piChromeInspectTarget = inspectTarget;
})();
