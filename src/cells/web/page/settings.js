// settings.js -- every declared setting on the Control pane, and config.jsonc
// whole for the engineer. The gateway serves the config's own description of
// itself (surface/settings.lisp): each section a folder declares, its fields
// with their types and docs, what it still needs, and when a write to it
// applies. A section draws as a form by its fields' types and writes through
// the gateway's one writer, which keeps the file's comments and refuses what
// the section's own cell would refuse at its start, under the form; a
// secret is pasted and never shown; a list whose field offers choices is
// picked by name from what its probe answers. Nothing here knows what a
// section is for: the Channels tab's setup is this same form under its guide.

import { el, button, plural } from "./observe.js";
import { block, note, form, formKey, confirmBox, about } from "./control.js";
import { t } from "./i18n.js";

const SAME = Symbol("same");
const REMOVE = Symbol("remove");

const words = (name) => { const text = name.replaceAll("_", " "); return text[0].toUpperCase() + text.slice(1); };
export const title = (s) => s.path.map(words).join(" · ");
// A guide written as a walk is its steps between semicolons, as the setup panel says it.
export const steps = (guide) => guide.split(";").map((step) => step.trim()).filter(Boolean);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// How a section stands, in a word or two; the word the row's color keys on,
// and the words it reads as.
function standing(s) {
  if (s.start_refusal) return "refused";
  if (s.present && !s.enabled) return "off";
  if (s.needs !== "complete") return "needs setting up";
  return s.present ? "set" : "defaults";
}
const STANDINGS = { refused: t("refused at start"), off: t("off"), "needs setting up": t("needs setting up"), set: t("set"), defaults: t("defaults") };

// A field's value as its control says it: SAME when the file says it already,
// REMOVE when the control was emptied of a value the file holds.
function valueOf(f, node) {
  const ids = node.dataset.pick && [...node.querySelectorAll("input:checked")].map((box) => box.value);
  const raw = ids ? ids.join(" ") : node.value.trim();
  // An emptied list with a default says none, which its absence would not.
  if (raw === "") return !f.set || f.secret ? SAME : f.type === "list" && f.default ? [] : REMOVE;
  let value = raw;
  if (f.type === "boolean") value = raw === "true";
  else if (f.type === "list") value = raw.split(/[\s,]+/).filter(Boolean);
  else if (f.type === "integer") {
    value = Number(raw);
    if (!Number.isInteger(value)) throw Object.assign(new Error(t("{field} takes a whole number", { field: words(f.name) })), { field: f.name });
  }
  return f.set && same(value, f.value) ? SAME : value;
}

export function makeSettings({ c, api, act, toast, redraw, refuse }) {
  const cfg = { list: null, raw: null, rawDraft: null, open: new Set(), more: new Set(), said: {}, probed: {}, picks: {}, drafts: {} };
  const find = (key) => cfg.list?.sections?.find((s) => s.key === key);

  async function load() {
    try { cfg.list = await api("GET", "/api/gateway/config"); } catch (error) { cfg.list = { error: error.message }; }
  }

  async function write(s, body) {
    cfg.list = await api("POST", "/api/gateway/config", { path: s.path, ...body });
    return cfg.list.said;
  }

  // --- one field ------------------------------------------------------------------

  function control(s, f) {
    const k = `${s.key}|${f.name}`;
    const draft = cfg.drafts[k];
    const was = f.set ? f.value : undefined;
    let node;
    if (Array.isArray(cfg.picks[k]) && f.type !== "list") {
      // One id picked by name; one the probe no longer offers stays a choice.
      node = el("select", "control");
      const options = [["", t("Not set")]].concat(cfg.picks[k].map((choice) => [choice.id, `${choice.label} · ${choice.id}`]));
      if (was && !options.some(([id]) => id === was)) options.push([was, `${was} · ${t("kept from the config")}`]);
      for (const [value, label] of options) node.append(Object.assign(el("option", "", label), { value }));
      node.value = draft ?? was ?? "";
    } else if (Array.isArray(cfg.picks[k])) {
      node = el("div", "ctl-cfg-pick");
      node.dataset.pick = "1";
      const marked = draft ?? was ?? [];
      const rows = cfg.picks[k].map((choice) => [choice.id, choice.label])
        .concat(marked.filter((id) => !cfg.picks[k].some((choice) => choice.id === id)).map((id) => [id, t("kept from the config")]));
      for (const [id, label] of rows) {
        const line = el("label", "ctl-cfg-check");
        const box = el("input");
        Object.assign(box, { type: "checkbox", value: id, checked: marked.includes(id) });
        line.append(box, el("span", "", label), el("span", "tag", id));
        node.append(line);
      }
      if (!rows.length) node.append(el("p", "obs-note", t("Nothing to pick from yet.")));
    } else if (f.type === "boolean" || f.type === "choice") {
      node = el("select", "control");
      const fallback = f.type === "boolean" ? (f.default ? t("on") : t("off")) : f.default;
      const options = [["", fallback != null ? t("Default ({value})", { value: fallback }) : t("Not set")]]
        .concat(f.type === "boolean" ? [["true", t("On")], ["false", t("Off")]] : f.options.map((option) => [option, option]));
      for (const [value, label] of options) node.append(Object.assign(el("option", "", label), { value }));
      node.value = draft ?? (was === undefined ? "" : String(was));
    } else {
      node = el("input", "control");
      node.type = f.secret ? "password" : f.type === "integer" ? "number" : "text";
      if (f.min != null) node.min = f.min;
      node.autocomplete = "off";
      node.spellcheck = false;
      node.placeholder = f.secret ? (f.set ? t("set; type a new one to replace it") : t("not set"))
        : f.type === "list" ? (f.default?.length ? t("default: {value}", { value: f.default.join(", ") }) : t("ids, separated by spaces or commas"))
        : f.default != null ? t("default: {value}", { value: f.default }) : f.type === "env" ? t("the variable's name") : t("not set");
      node.value = draft ?? (f.secret || was === undefined ? "" : Array.isArray(was) ? was.join(", ") : String(was));
    }
    node.dataset.field = f.name;
    node.id = `cfg-${k}`;
    return node;
  }

  function fieldRow(s, f) {
    const k = `${s.key}|${f.name}`;
    const row = el("div", "ctl-cfg-field");
    const label = el("label", "ctl-cfg-label");
    label.htmlFor = `cfg-${k}`;
    label.append(el("span", "", words(f.name)), el("span", "tag", f.name));
    const cell = el("div", "ctl-cfg-cell");
    const line = el("div", "ctl-cfg-line");
    line.append(control(s, f));
    if (f.choices) {
      line.append(Array.isArray(cfg.picks[k])
        ? button("word", t("Type ids"), { act: "cfg-unpick", key: s.key, field: f.name })
        : button("word", c.busy === `pick:${k}` ? t("Asking…") : t("Pick by name"), { act: "cfg-pick", key: s.key, field: f.name }));
    }
    cell.append(line);
    if (cfg.picks[k]?.error) cell.append(el("p", "ctl-cfg-doc refused", cfg.picks[k].error));
    const doc = [f.doc, f.type === "env" && f.set ? f.text : ""].filter(Boolean).join(" · ");
    if (doc) cell.append(el("p", "ctl-cfg-doc", doc));
    row.append(label, cell);
    return row;
  }

  // --- one section --------------------------------------------------------------------

  function applies(s) {
    const line = el("div", "ctl-cfg-applies");
    if (s.applies === "now") line.append(t("Used from its next read; nothing restarts."));
    else if (s.applies === "cell") {
      line.append(t("{cell} reads this when it starts.", { cell: words(s.cell.replace(/^nodecode-/, "")) }), " ",
        button("word", c.busy === `cell:${s.cell}` ? t("Restarting…") : t("Restart it"), { act: "restart-cell", name: s.cell }));
    } else {
      line.append(t("Read when the organism starts."), " ", button("word", t("Restart the organism"), { act: "cfg-ask", key: s.key }));
    }
    return line;
  }

  // The section as a form: what it needs, the paste row for a secret it takes
  // by reference, Check when it declares one, its fields by type -- the ones
  // it cannot do without first -- and what the last write did.
  function body(s, { guide = true, after } = {}) {
    const box = el("div", "ctl-cfg-body");
    if (guide && s.guide) box.append(el("p", "ctl-cfg-guide", s.guide));
    const head = el("div", "ctl-cfg-line");
    // Whole is not the same as running: a section its cell refused at start says that instead.
    head.append(s.start_refusal ? el("p", "ctl-cfg-needs refused", t("Refused at start: {why}", { why: s.start_refusal }))
      : el("p", `ctl-cfg-needs${s.needs === "complete" ? "" : " open"}`,
        s.needs === "complete" ? (s.present ? t("Complete.") : t("Complete, on its defaults.")) : `${s.needs[0].toUpperCase()}${s.needs.slice(1)}.`));
    if (s.check) head.append(button("quiet", c.busy === `check:${s.key}` ? t("Checking…") : t("Check"), { act: "cfg-check", key: s.key }));
    // A folder's section says "enabled": false to stand its folder down; the core's reads no such member.
    if (s.present && s.applies !== "now" && !s.fields.some((f) => f.name === "enabled")) {
      head.append(button("word", s.enabled ? t("Turn off") : t("Turn on"), { act: "cfg-enable", key: s.key }));
    }
    box.append(head);
    if (cfg.probed[s.key]) box.append(el("p", `ctl-said${cfg.probed[s.key].bad ? " refused" : ""}`, cfg.probed[s.key].text));
    const pair = s.secret;
    if (pair && !s.fields.find((f) => f.name === pair.env)?.set) {
      const replace = s.fields.find((f) => f.name === pair.file)?.set;
      const input = el("input", "control");
      Object.assign(input, { type: "password", name: "secret", autocomplete: "off", required: true, placeholder: replace ? t("a new one replaces the one kept") : t("paste it here") });
      const wrap = el("label", "ctl-field");
      wrap.append(el("span", "micro", replace ? t("Replace the {noun}", { noun: pair.noun }) : t("Paste the {noun}", { noun: pair.noun })), input);
      const keep = button("solid", c.busy === `cfg:${s.key}` ? t("Keeping…") : t("Keep it"), {});
      keep.type = "submit";
      const f = form("cfg-secret", wrap, keep);
      f.dataset.key = s.key;
      box.append(f, note(t("Kept in {to}, readable by you alone (mode 0600); {file} names the file, so config.jsonc never holds the {noun} itself.", { to: pair.to, file: pair.file, noun: pair.noun })));
    }
    const essential = s.fields.filter((f) => f.essential);
    const more = !essential.length || cfg.more.has(s.key);
    const fields = form("cfg-save");
    fields.classList.add("ctl-cfg-fields");
    fields.dataset.key = s.key;
    for (const f of essential.concat(more ? s.fields.filter((f) => !f.essential) : [])) fields.append(fieldRow(s, f));
    const actions = el("div", "ctl-actions ctl-cfg-actions");
    const save = button("solid", c.busy === `cfg:${s.key}` ? t("Saving…") : t("Save"), {});
    save.type = "submit";
    actions.append(save);
    const rest = s.fields.length - essential.length;
    if (essential.length && rest) actions.append(button("word", more ? t("Fewer settings") : plural(rest, t("{n} more setting"), t("{n} more settings")), { act: "cfg-more", key: s.key }));
    fields.append(actions);
    box.append(fields);
    if (cfg.said[s.key]) box.append(el("p", "ctl-said", cfg.said[s.key]));
    if (after || cfg.said[s.key] || s.start_refusal) box.append(after || applies(s));
    if (c.confirm === `cfg-restart:${s.key}`) {
      box.append(confirmBox(t("Restart this organism? It starts again on config.jsonc as it is now; this page reconnects when it is back."), t("Restart"), "restart"));
    }
    for (const b of box.querySelectorAll("button")) if (c.busy && b.dataset.act !== "cancel") b.disabled = true;
    return box;
  }

  // --- the tab's blocks ---------------------------------------------------------------

  function blocks() {
    const [infoMark, words] = about(c, "all-settings", t("Every part of config.jsonc that something here declares: open one to change it. A secret is never shown here; paste a new one to replace it."));
    const all = block(t("All settings"), infoMark);
    all.append(words);
    const list = cfg.list;
    if (!list || list.error && !list.sections) { all.append(el("p", "obs-note", list?.error || t("Looking…"))); return [all, rawBlock()]; }
    if (list.error) all.append(el("p", "ctl-said refused", t("config.jsonc does not read: {error}. Fix it in the editor below.", { error: list.error })));
    const rows = el("ol", "ctl-list");
    for (const s of list.sections.filter((s) => s.fields.length)) {
      const li = el("li", "ctl-cfg-section");
      li.id = anchor(s.key);
      const open = cfg.open.has(s.key);
      const head = button("ctl-cfg-head", "", { act: "cfg-open", key: s.key });
      head.setAttribute("aria-expanded", String(open));
      const state = el("span", "ctl-cfg-state", STANDINGS[standing(s)]);
      state.dataset.state = standing(s);
      head.append(el("span", "ctl-cfg-title", title(s)), el("span", "tag", s.key), state);
      li.append(head);
      if (open) li.append(body(s));
      rows.append(li);
    }
    all.append(rows);
    return [all, rawBlock()];
  }

  function rawBlock() {
    const [infoMark, words] = about(c, "config", t("The file itself, for the engineer: comments and all. Each secret reads [set], and saving puts it back from the file. A document that does not read, or that a setting above would refuse, is not saved, and why is said here."));
    const raw = block("config.jsonc", infoMark, button("word", cfg.raw ? t("Close") : t("Open the file"), { act: cfg.raw ? "cfg-raw-close" : "cfg-raw" }));
    raw.append(words);
    const r = cfg.raw;
    if (!r) return raw;
    if (!r.text && r.error) { raw.append(el("p", "ctl-said refused", r.error)); return raw; }
    const area = el("textarea", "control ctl-cfg-raw");
    area.dataset.input = "cfg-raw";
    area.spellcheck = false;
    area.setAttribute("aria-label", r.file);
    area.value = cfg.rawDraft ?? r.text;
    area.rows = Math.min(36, Math.max(10, area.value.split("\n").length + 1));
    const actions = el("div", "ctl-actions");
    actions.append(button("solid", c.busy === "cfg-raw" ? t("Saving…") : t("Save the file"), { act: "cfg-raw-save" }),
      button("word", t("Reload"), { act: "cfg-raw" }));
    for (const b of actions.children) b.disabled = !!c.busy;
    raw.append(el("p", "obs-note", r.file), area, actions);
    if (r.error) raw.append(el("p", "ctl-said refused", r.error));
    else if (r.said) raw.append(el("p", "ctl-said", t("{said}. A setting a cell reads applies when it starts again.", { said: r.said })));
    return raw;
  }

  // --- what the pane hands over -------------------------------------------------------

  function click(d) {
    const s = find(d.key);
    const k = `${d.key}|${d.field}`;
    switch (d.act) {
      case "cfg-open": cfg.open.has(d.key) ? cfg.open.delete(d.key) : cfg.open.add(d.key); redraw(); return true;
      case "cfg-more": cfg.more.has(d.key) ? cfg.more.delete(d.key) : cfg.more.add(d.key); redraw(); return true;
      case "cfg-ask": c.confirm = `cfg-restart:${d.key}`; redraw(); return true;
      case "cfg-unpick": delete cfg.picks[k]; delete cfg.drafts[k]; redraw(); return true;
      case "cfg-check":
        act(`check:${d.key}`, async () => {
          try { cfg.probed[d.key] = { text: (await api("POST", "/api/gateway/config/probe", { path: s.path })).text }; }
          catch (error) { cfg.probed[d.key] = { text: error.message, bad: true }; }
        });
        return true;
      case "cfg-pick":
        act(`pick:${k}`, async () => {
          try { cfg.picks[k] = (await api("POST", "/api/gateway/config/probe", { path: s.path, field: d.field })).choices; }
          catch (error) { cfg.picks[k] = { error: error.message }; }
        });
        return true;
      case "cfg-enable":
        act(`cfg:${d.key}`, async () => { cfg.said[d.key] = await write({ path: [...s.path, "enabled"] }, { value: !s.enabled }); });
        return true;
      case "cfg-raw":
        act("cfg-raw", async () => {
          try { cfg.raw = await api("GET", "/api/gateway/config/raw"); } catch (error) { cfg.raw = { error: error.message }; }
          cfg.rawDraft = null;
        });
        return true;
      case "cfg-raw-close": cfg.raw = null; cfg.rawDraft = null; redraw(); return true;
      case "cfg-raw-save":
        act("cfg-raw", async () => {
          try {
            cfg.raw = await api("POST", "/api/gateway/config/raw", { text: cfg.rawDraft ?? cfg.raw.text, stamp: cfg.raw.stamp });
            cfg.rawDraft = null;
            await load();
          } catch (error) { cfg.raw = { ...cfg.raw, error: error.message, said: null }; }
        });
        return true;
    }
    return false;
  }

  // What the hand types is kept, so a redraw -- a poll, another section's save --
  // never takes it back.
  function input(event) {
    const target = event.target;
    if (target.dataset.input === "cfg-raw") { cfg.rawDraft = target.value; if (cfg.raw) cfg.raw.error = null; return true; }
    const owner = target.closest("[data-field]");
    const key = target.closest("form[data-key]")?.dataset.key;
    if (!owner || !key) return false;
    cfg.drafts[`${key}|${owner.dataset.field}`] = owner.dataset.pick
      ? [...owner.querySelectorAll("input:checked")].map((box) => box.value) : target.value;
    return true;
  }

  function submit(node, values) {
    const s = find(node.dataset.key);
    if (!s) return false;
    if (node.dataset.act === "cfg-secret") {
      act(`cfg:${s.key}`, async () => { cfg.said[s.key] = await write(s, { secret: values.secret }); }, formKey(node));
      return true;
    }
    if (node.dataset.act !== "cfg-save") return false;
    const sets = {};
    const removes = [];
    try {
      for (const f of s.fields) {
        const input = node.querySelector(`[data-field="${f.name}"]`);
        const value = input && valueOf(f, input);
        if (!input || value === SAME) continue;
        if (value === REMOVE) removes.push(f.name); else sets[f.name] = value;
      }
    } catch (error) { refuse(formKey(node), error.message, error.field); return true; }
    if (!Object.keys(sets).length && !removes.length) { toast(t("Nothing changed.")); return true; }
    delete cfg.said[s.key];
    act(`cfg:${s.key}`, async () => {
      const said = [];
      if (Object.keys(sets).length) said.push(await write(s, { value: sets }));
      for (const name of removes) said.push(await write({ path: [...s.path, name] }, { remove: true }));
      cfg.said[s.key] = said.join("; ");
      for (const k of Object.keys(cfg.drafts)) if (k.startsWith(`${s.key}|`)) delete cfg.drafts[k];
      toast(said.join("; "));
    }, formKey(node));
    return true;
  }

  // Where a section's row stands in the page, and that section opened on the next draw.
  function anchor(key) { return `cfg-section-${key.replace(/[^\w-]/g, "_")}`; }
  function reveal(key) { cfg.open.add(key); }

  return { load, find, sections: () => cfg.list?.sections || [], blocks, body, click, input, submit, anchor, reveal };
}
