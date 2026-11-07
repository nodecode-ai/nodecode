// index.js -- the Control pane's Index tab: what the organism keeps -- its
// memories, its skills, its layer's definitions -- as the index ranks them
// (/api/gateway/index), one opened whole in an editor, a new memory made
// from a small form, one forgotten. A save is the definition's own text
// evaluated and kept as an eval keeps one, a forget is (unintern 'name): the
// file, the commit and the ledger are what the agent's own would leave, and a
// refusal is the definition's own reason, said where the write was made.

import { el, button, mark, when, waiting, headline, dot } from "./observe.js";
import { block, note, form, field, confirmBox, about, PHONE } from "./control.js";
import { t } from "./i18n.js";

const NAME = { pattern: "[a-z0-9][a-z0-9._\\-]{0,63}", title: t("a-z, 0-9, . _ and -, starting with a letter or digit") };
const KINDS = { memory: t("memory"), skill: t("skill"), definition: t("definition") };

// A string as the Lisp reader reads it back: a backslash and a double quote escaped, nothing else.
const PAGE = 50;
const lispString = (text) => `"${text.replace(/[\\"]/g, "\\$&")}"`;

export function makeIndex({ c, api, act, toast, redraw, settle }) {
  // SHOWN: how many rows a phone draws, fifty more at a time (the phone standard: never two thousand).
  const s = { list: null, open: null, draft: null, creating: false, fresh: {}, said: null, filter: "", shown: PAGE };

  async function load() {
    try { s.list = await api("GET", "/api/gateway/index"); } catch (error) { s.list = { error: error.message }; }
  }

  // A write answers the listing; a refusal stays where it was made, in the definition's words.
  function write(label, body, done) {
    act(label, async () => {
      try {
        const answer = await api("POST", "/api/gateway/index", body);
        s.list = answer;
        s.said = null;
        done?.(answer);
        if (answer.text) toast(answer.text);
      } catch (error) {
        s.said = { at: label, text: error.message };
      }
    });
  }

  function opened(name) {
    act(`index-open:${name}`, async () => {
      try {
        const answer = await api("GET", `/api/gateway/index?${new URLSearchParams({ name })}`);
        Object.assign(s, { open: answer.definition, draft: null, said: null, creating: false });
      } catch (error) { toast(t("Could not open {name}: {error}", { name, error: error.message }), { error }); }
    });
  }

  // --- the tab -----------------------------------------------------------------------

  // What it is and what the ledger knows of its use, in words.
  function facts(d) {
    return [KINDS[d.kind], d.type, d.project,
      d.last ? t("used {when}", { when: when(d.last) }) : t("never used")].filter(Boolean).join(" · ");
  }

  function refusal(at) {
    return s.said?.at === at ? el("p", "ctl-said refused", s.said.text) : null;
  }

  function editor(d) {
    const panel = el("div", "ctl-index-editor");
    panel.append(el("p", "obs-note", d.file));
    const area = el("textarea", "control ctl-index-text");
    area.dataset.input = "index-text";
    area.spellcheck = false;
    area.setAttribute("aria-label", t("{name}'s definition", { name: d.name }));
    area.value = s.draft ?? d.text;
    area.rows = Math.min(40, Math.max(8, area.value.split("\n").length + 2));
    const actions = el("div", "ctl-actions");
    actions.append(button("solid", c.busy === "index-save" ? t("Saving…") : t("Save"), { act: "index-save" }),
      button("word", t("Revert"), { act: "index-revert" }), button("word", t("Close"), { act: "index-close" }));
    for (const b of actions.children) b.disabled = !!c.busy;
    panel.append(area, actions);
    const said = refusal("index-save");
    if (said) panel.append(said);
    panel.append(note(t("The definition whole, as its file holds it: a save evaluates it and keeps it, one commit, as the agent's own eval would. The name stays its own; a new name is a new definition.")));
    return panel;
  }

  function creator() {
    const prose = field("prose", t("Why, and how to apply it (optional)"), { multiline: true, rows: "4" });
    prose.classList.add("wide");
    const f = form("index-create",
      field("name", t("Name"), { required: "", autocomplete: "off", spellcheck: "false", placeholder: "quiet-output", ...NAME }),
      field("description", t("One declarative sentence"), { required: "", autocomplete: "off", maxlength: "1024" }),
      field("type", t("Type (optional)"), { autocomplete: "off", placeholder: "feedback, user, project, reference or lesson" }),
      prose);
    for (const input of f.querySelectorAll("input, textarea")) input.value = s.fresh[input.name] ?? "";
    const go = button("solid", c.busy === "index-create" ? t("Creating…") : t("Create"), {});
    go.type = "submit";
    go.disabled = !!c.busy;
    f.append(go, button("word", t("Cancel"), { act: "index-new" }));
    const panel = el("div", "ctl-index-new ctl-rises");
    panel.append(f);
    const said = refusal("index-create");
    if (said) panel.append(said);
    panel.append(note(t("It is kept as a define-memory in your knowledge cell under the home, one file and one commit.")));
    return panel;
  }

  function row(rows, d) {
    const li = el("li", "ctl-row");
    li.dataset.level = d.hot ? "started" : "off";
    const name = el("span", "ctl-name");
    const link = button("cell-link", d.name, { act: "index-open", name: d.name });
    link.title = t("Open {name}", { name: d.name });
    name.append(dot(d.hot ? "" : "off"), link);
    const what = el("span", "ctl-cell-what", d.description);
    const tag = el("span", "tag", facts(d));
    tag.title = d.file;
    what.append(tag);
    const acts = el("span", "ctl-acts");
    const isOpen = s.open?.name === d.name;
    acts.append(isOpen ? mark("close", t("Close"), { act: "index-close", name: d.name }) : mark("pencil", t("Edit"), { act: "index-open", name: d.name }),
      mark("trash", t("Forget"), { act: "index-ask-forget", name: d.name }));
    for (const b of acts.children) b.disabled = !!c.busy;
    li.append(name, what, acts);
    rows.append(li);
    if (s.said?.at === `index-forget:${d.name}`) rows.append(el("li", "ctl-sub refused", s.said.text));
    if (isOpen) {
      const inset = el("li", "ctl-inset");
      inset.append(editor(s.open));
      rows.append(inset);
    }
    if (c.confirm === `index-forget:${d.name}`) {
      const inset = el("li", "ctl-inset");
      inset.append(confirmBox(t("Forget {name}? Its file leaves its cell in one commit, which git keeps; the agent no longer lists or finds it.", { name: d.name }), t("Forget"), "index-forget", { name: d.name }));
      rows.append(inset);
    }
  }

  function draw() {
    const root = el("div", "ctl-tab");
    const l = s.list;
    if (!l || l.error) {
      const line = waiting(l?.error ? t("Could not read the index: {error}", { error: l.error }) : t("Looking…"));
      if (l?.error) line.append(" ", button("word", t("Retry"), { act: "reread" }));
      root.append(line);
      return root;
    }
    const [infoMark, words] = about(c, "index", t("What the agent keeps and does not run — memories, skills in prose — and the definitions of its own layer, ranked by use: the index at the head of every conversation lists the hot ones, and the agent reads one before it follows it. A memory never cools; a skill or a definition unused for a while does. A new session lists a change at once, one already running after its next eviction, or at once after /index reload in it."));
    const cells = new Set(l.definitions.map((d) => d.cell)).size;
    const head = headline(t("Index"), t("{hot} hot, {cold} cold.", { hot: l.hot, cold: l.cold }), t("From {n} cells.", { n: cells }));
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head, words);
    const list = block("");
    const bar = el("div", "ctl-toolbar");
    const filter = el("input", "control");
    Object.assign(filter, { type: "search", placeholder: t("Filter what is kept"), value: s.filter });
    filter.dataset.input = "index-filter";
    filter.setAttribute("aria-label", t("Filter what is kept"));
    const fresh = button("quiet", s.creating ? t("Close the form") : t("New memory"), { act: "index-new" });
    fresh.disabled = !!c.busy;
    fresh.setAttribute("aria-expanded", String(s.creating));
    bar.append(filter, fresh);
    list.append(bar);
    if (s.creating) list.append(creator());
    const rows = el("ol", "ctl-list ctl-index-list");
    rows.id = "ctl-index-rows";
    fill(rows);
    list.append(rows);
    root.append(list);
    return root;
  }

  // The rows the filter leaves, a cell's under its name, redrawn alone while it is typed.
  function fill(rows) {
    const needle = s.filter.trim().toLowerCase();
    const shown = s.list.definitions.filter((d) => !needle || `${d.name} ${d.description} ${d.cell}`.toLowerCase().includes(needle));
    const page = PHONE.matches ? shown.slice(0, s.shown) : shown;
    rows.replaceChildren();
    for (const cell of new Set(page.map((d) => d.cell))) {
      rows.append(el("li", "ctl-index-cell micro", cell));
      for (const d of page.filter((d) => d.cell === cell)) row(rows, d);
    }
    if (page.length < shown.length) {
      const more = el("li", "ctl-index-more");
      more.append(button("quiet ctl-more", t("Show {n} more", { n: Math.min(PAGE, shown.length - page.length) }), { act: "index-more" }));
      rows.append(more);
    }
    settle?.(rows);
    if (!s.list.definitions.length) rows.append(el("li", "obs-note", t("Nothing kept yet. The agent keeps a memory or a skill when one is worth keeping; New memory keeps one here.")));
    else if (!shown.length) rows.append(el("li", "obs-note", t("Nothing kept matches.")));
  }

  // --- what the pane hands over ------------------------------------------------------

  function click(d) {
    switch (d.act) {
      case "index-open": opened(d.name); return true;
      case "index-close": Object.assign(s, { open: null, draft: null, said: null }); redraw(); return true;
      case "index-more": {
        s.shown += PAGE;
        const rows = document.getElementById("ctl-index-rows");
        if (rows) fill(rows);
        return true;
      }
      case "index-revert": s.draft = null; s.said = null; redraw(); return true;
      case "index-new": s.creating = !s.creating; s.said = null; redraw(); if (s.creating) document.querySelector(".ctl-index-new input[name=name]")?.focus(); return true;
      case "index-save":
        write("index-save", { op: "save", name: s.open.name, text: s.draft ?? s.open.text }, () => { opened(s.open.name); });
        return true;
      case "index-ask-forget": c.confirm = `index-forget:${d.name}`; s.said = null; redraw(); return true;
      case "index-forget":
        write(`index-forget:${d.name}`, { op: "forget", name: d.name }, () => { if (s.open?.name === d.name) Object.assign(s, { open: null, draft: null }); });
        return true;
    }
    return false;
  }

  // What the hand types is kept, so a redraw never takes it back.
  function input(event) {
    const node = event.target;
    if (node.dataset.input === "index-text") { s.draft = node.value; return true; }
    if (node.dataset.input === "index-filter") {
      s.filter = node.value;
      s.shown = PAGE;
      const rows = document.getElementById("ctl-index-rows");
      if (rows && s.list?.definitions) fill(rows);
      return true;
    }
    if (node.closest?.("form[data-act=index-create]")) { s.fresh[node.name] = node.value; return true; }
    return false;
  }

  function submit(node, values) {
    if (node.dataset.act !== "index-create") return false;
    const name = values.name.trim();
    const type = values.type.trim().replace(/^:/, "");
    const prose = values.prose.split("\n").map((line) => `  ;; ${line}`.trimEnd()).join("\n");
    const text = `(define-memory ${name}\n  ${lispString(values.description.trim())}${type ? `\n  :type :${type}` : ""}${values.prose.trim() ? `\n${prose}` : ""}\n  )`;
    write("index-create", { op: "save", name, text }, () => {
      Object.assign(s, { creating: false, fresh: {}, draft: null, filter: "" });
      opened(name);
    });
    return true;
  }

  return { load, draw, click, input, submit };
}
