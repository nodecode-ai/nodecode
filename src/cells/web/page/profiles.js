// profiles.js -- the Profiles tab of the Control pane: `nodecode profile' from
// the page. A profile is a home of its own -- its config, keys, store, layer
// and gateway -- and so an organism of its own. The gateway's profile routes
// (surface/control.lisp) are the kernel's verbs (organism/profile.lisp), and
// every refusal shown here is theirs, in their words. This page belongs to
// one gateway and never moves into another profile: a profile's panel says how
// to open that profile's own page from a terminal.
//
// The tab is one list, a row a profile: its name, what it started from, how
// many of its files it shares by link, its store. A row opens its panel beside
// the list (a phone raises it from the bottom): how to open it, what it is,
// its SOUL.md and its acts. New opens the builder in the same panel, and the
// builder shows what Create makes -- copied, linked, new -- before it is
// pressed. (The operator picked this of three designs, 2026-10-26: "less is
// more" -- no line of facts under a row, no command repeated under each.)
//
// Precedent: Hermes' profiles page and profile builder (hermes-agent
// web/src/pages/ProfilesPage.tsx, ProfileBuilderPage.tsx) -- the name rule
// checked as it is typed, clone-from with a clone-everything choice, the SOUL
// editor under its profile, the copyable terminal command, nothing written
// before Create. Its model, skills and MCP steps have no verb here.

import { el, button, mark, plural, headline, dot, PHONE } from "./observe.js";
import { block, note, form, formKey, field, confirmBox, bytes, about } from "./control.js";
import { icon } from "./icons.js";
import { t } from "./i18n.js";

// The name rule PROFILE-NAME-P holds a name to, checked as it is typed; the gateway still decides.
const NAME = /^[a-z0-9][a-z0-9_-]{0,63}$/;
const CLONES = [["settings", t("Settings")], ["all", t("Fork")], ["blank", t("Empty")]];
// What each start does with the home it starts from (the verb's own split):
// settings copies config.jsonc without its channels, SOUL.md, themes and the
// installed cells and links memory, skills, secrets and auth.json; a fork
// copies those, its cell folders and its layer with its git history too; an
// empty home is set up at its first launch. Each makes a store of its own.
const MAKES = {
  settings: [[t("config · SOUL.md · cells"), "copied"], [t("keys · memory · skills"), "linked"], [t("layer · store"), "new"]],
  all: [[t("config · SOUL.md · cells · layer"), "copied"], [t("keys · memory · skills"), "copied"], [t("store"), "new"]],
  blank: [[t("everything"), "new"]],
};
const HOW = { copied: t("copied"), linked: t("linked"), new: t("new") };

const fresh = (from = "") => ({ name: "", clone: "settings", from, description: "", soul: "", soulOpen: false });

export function makeProfiles({ c, api, act, toast, redraw, download }) {
  // PANEL is what stands beside the list: a profile by its NAME, or the
  // builder (NAME null); SOUL the SOUL.md open in it, with what the hand typed;
  // REFUSED the profile whose Delete was asked of one that stays; FIND the search.
  const p = { list: null, panel: null, soul: null, refused: null, find: "", draft: fresh() };
  const row = (name) => p.list?.profiles?.find((r) => r.name === name);
  const query = (name) => `name=${encodeURIComponent(name)}`;

  async function load() {
    try { p.list = await api("GET", "/api/gateway/profiles"); } catch (error) { p.list = { error: error.message }; }
    if (p.panel?.name && !row(p.panel.name)) p.panel = null;
  }

  function close() {
    p.panel = null;
    p.soul = null;
    p.refused = null;
    c.confirm = null;
  }

  // Esc closes the panel, as its x does.
  document.addEventListener("keydown", (event) => {
    if (event.key !== "Escape" || !p.panel || !document.querySelector(".control-pane .ctl-prof-panel")) return;
    event.preventDefault();
    close();
    redraw();
  });

  // Why the name typed cannot be made, or nothing.
  function problem(name) {
    if (!name) return "";
    if (!NAME.test(name)) return t("a-z, 0-9, _ and -, starting with a letter or digit");
    if (row(name)) return t("A profile named {name} already exists.", { name });
    return "";
  }

  // What Create makes, said before it is pressed.
  function makes() {
    const d = p.draft;
    const box = el("div", "ctl-prof-makes");
    box.id = "prof-makes";
    box.append(el("p", "ctl-prof-path", `${p.list.folder}${d.name.trim() || "…"}/`));
    for (const [what, how] of MAKES[d.clone]) {
      const line = el("p", "ctl-prof-make");
      line.append(el("span", "", what), el("span", `ctl-prof-how ${how}`, HOW[how]));
      box.append(line);
    }
    return box;
  }

  // Typing redraws only what it changes, so the field keeps the hand's place.
  function showReview() {
    const name = p.draft.name.trim();
    const bad = problem(name);
    const why = document.getElementById("prof-why");
    if (why) { why.textContent = bad; why.hidden = !bad; }
    document.querySelector('.ctl-prof-build input[name="name"]')?.setAttribute("aria-invalid", String(!!bad));
    document.getElementById("prof-makes")?.replaceWith(makes());
    const go = document.getElementById("prof-create");
    if (go) go.disabled = !name || !!bad || !!c.busy;
  }

  // --- the list -------------------------------------------------------------------

  const made = (r) => r.clone === "all" ? t("fork of {source}", { source: r.source })
    : r.source ? t("settings of {source}", { source: r.source }) : "";
  const shown = (r) => !p.find.trim() || `${r.name} ${r.description || ""}`.toLowerCase().includes(p.find.trim().toLowerCase());

  function drawRow(r) {
    const li = el("li", "ctl-prof-row");
    li.dataset.act = "prof-open";
    li.dataset.name = r.name;
    li.hidden = !shown(r);
    if (p.panel?.name === r.name) li.setAttribute("aria-current", "true");
    const who = el("span", "ctl-prof-who");
    const name = button("ctl-prof-name", "", { act: "prof-open", name: r.name });
    name.append(dot(r.pid ? "" : "off"), el("b", "", r.name));
    if (r.current) name.append(el("span", "ctl-prof-mark", t("this page")));
    if (r.sticky) name.append(el("span", "ctl-prof-mark", t("default")));
    name.title = r.home;
    who.append(name);
    if (r.description) who.append(el("span", "ctl-prof-says", r.description));
    const links = el("span", "ctl-prof-links");
    if (r.shares?.length) {
      links.append(icon("link", 12), String(r.shares.length));
      links.title = t("Linked to {source}: {names}", { source: r.source, names: r.shares.join(", ") });
    }
    li.append(who, el("span", "ctl-prof-from", made(r) || "—"), links,
      el("span", "ctl-prof-store", bytes(r.store_bytes)), icon("chevron", 14));
    return li;
  }

  function sift() {
    for (const li of document.querySelectorAll(".ctl-prof-row")) li.hidden = !shown(row(li.dataset.name) || {});
  }

  // --- the panel ----------------------------------------------------------------------

  // A phone raises the panel as its sheet (control.js rise); a wide window keeps it beside the list.
  function panel(title, ...marks) {
    const box = el("div", PHONE.matches ? "ctl-rises ctl-prof-panel" : "sheet ctl-prof-panel");
    const head = el("div", "ctl-prof-head");
    head.append(title, ...marks, mark("close", t("Close"), { act: "prof-close" }));
    box.append(head);
    return box;
  }

  function command(text) {
    const line = el("div", "ctl-prof-cmd");
    line.append(el("code", "", text), mark("copy", t("Copy"), { act: "prof-copy", text }));
    return line;
  }

  function drawProfile(r) {
    const title = el("h3", "ctl-prof-title");
    title.append(dot(r.pid ? "" : "off"), r.name);
    title.title = r.home;
    const box = panel(title, ...r.sticky ? [el("span", "ctl-prof-mark", t("default"))] : []);
    if (r.description) box.append(el("p", "ctl-prof-says", r.description));
    if (r.current) box.append(el("p", "ctl-prof-here", t("this page")));
    else {
      if (!r.pid) box.append(command(`nodecode -p ${r.name} gateway on`));
      box.append(command(`nodecode -p ${r.name} web`));
    }
    box.append(el("p", "ctl-prof-facts", [r.pid ? t("Running") : t("Stopped"), bytes(r.store_bytes), made(r)].filter(Boolean).join(" · ")));
    if (r.shares?.length) {
      const chips = el("div", "ctl-prof-chips");
      for (const s of r.shares) chips.append(el("span", "ctl-prof-chip", s));
      box.append(el("p", "micro", t("Linked to {source}", { source: r.source })), chips);
    }
    const soul = el("div", "ctl-prof-soul");
    const open = p.soul?.name === r.name;
    const top = el("div", "ctl-prof-soul-head");
    top.append(el("span", "micro", "SOUL.md"), button("word", open ? t("Close") : r.soul ? t("Edit") : t("Add"), { act: "prof-soul", name: r.name }));
    soul.append(top);
    if (open) {
      const text = el("textarea", "control");
      text.dataset.input = "prof-soul";
      text.rows = 10;
      text.value = p.soul.draft;
      text.placeholder = t("How the agent sounds in its channels.");
      text.setAttribute("aria-label", "SOUL.md");
      const save = button("solid", c.busy === "prof-soul" ? t("Saving…") : t("Save"), { act: "prof-soul-save" });
      save.disabled = !!c.busy;
      soul.append(text, save);
    }
    box.append(soul);
    if (p.refused === r.name && r.kept) box.append(el("p", "refused", t("Not deleted: {why}.", { why: r.kept })));
    if (c.confirm === `prof-drop:${r.name}`) {
      box.append(confirmBox(t("Delete {name}? Its store, layer and config go for good.", { name: r.name }), t("Delete"), "prof-drop", { name: r.name }));
      return box;
    }
    const acts = el("div", "ctl-prof-acts");
    if (!r.sticky) {
      const use = button("quiet", c.busy === `prof-use:${r.name}` ? t("Making default…") : t("Make default"), { act: "prof-use", name: r.name });
      use.title = t("The profile a bare nodecode runs: {command}", { command: `nodecode profile use ${r.name}` });
      acts.append(use);
    }
    acts.append(button("quiet", t("Clone"), { act: "prof-new", from: r.current ? "" : r.name }),
      button("quiet", c.busy === `prof-export:${r.name}` ? t("Saving…") : t("Export"), { act: "prof-export", name: r.name }));
    if (!r.current) acts.append(mark("trash", t("Delete"), { act: "prof-ask-drop", name: r.name }));
    for (const b of acts.children) b.disabled = !!c.busy;
    box.append(acts);
    return box;
  }

  function drawBuilder() {
    const d = p.draft;
    const box = panel(el("h3", "ctl-prof-title", t("New profile")));
    const f = form("prof-create");
    f.classList.add("ctl-prof-build");
    const name = field("name", t("Name"), { value: d.name, placeholder: "coder", autocomplete: "off", spellcheck: "false", required: "" });
    const why = el("span", "ctl-prof-why");
    why.id = "prof-why";
    why.hidden = true;
    name.append(why);

    const start = el("div", "ctl-field");
    const choices = el("div", "scope");
    choices.setAttribute("role", "radiogroup");
    choices.setAttribute("aria-label", t("Start from"));
    for (const [id, label] of CLONES) {
      const sw = button("sw", label, { act: "prof-clone", clone: id });
      sw.setAttribute("role", "radio");
      sw.setAttribute("aria-checked", String(d.clone === id));
      choices.append(sw);
    }
    start.append(el("span", "micro", t("Start from")), choices);
    if (d.clone !== "blank") {
      const select = el("select", "control");
      select.name = "from";
      select.setAttribute("aria-label", t("Start from"));
      const here = p.list.profiles.find((r) => r.current);
      // A home set by NODECODE_HOME is not on the roster; the verb's own default reaches it.
      if (!here) select.append(Object.assign(el("option", "", p.list.this), { value: "" }));
      for (const r of p.list.profiles) {
        const option = el("option", "", r.name);
        option.value = r.current ? "" : r.name;
        option.selected = option.value === d.from;
        select.append(option);
      }
      start.append(select);
    }

    const description = field("description", t("Description"), { value: d.description, autocomplete: "off", placeholder: t("Optional") });
    const soul = el("div", "ctl-field");
    soul.append(button("word ctl-prof-more", `${d.soulOpen ? "−" : "+"} SOUL.md`, { act: "prof-soul-new" }));
    if (d.soulOpen) {
      const text = el("textarea", "control");
      text.name = "soul";
      text.rows = 4;
      text.value = d.soul;
      text.placeholder = t("How the agent sounds in its channels.");
      text.setAttribute("aria-label", "SOUL.md");
      soul.append(text);
    }
    const go = button("solid", c.busy === "prof-create" ? t("Creating…") : t("Create"), {});
    go.type = "submit";
    go.id = "prof-create";
    f.append(name, start, description, soul, makes(), go);
    box.append(f);
    queueMicrotask(showReview);
    return box;
  }

  // --- the tab ----------------------------------------------------------------------

  function draw() {
    const root = el("div", "ctl-tab");
    const l = p.list;
    if (!l || l.error) {
      const line = note(l?.error ? t("Could not read the profiles: {error}", { error: l.error }) : t("Looking…"));
      if (l?.error) line.append(" ", button("word", t("Retry"), { act: "reread" }));
      root.append(line);
      return root;
    }
    const [infoMark, words] = about(c, "profiles",
      t("A profile is a home of its own: its config, keys, store, layer and gateway, so each one runs as an organism of its own. This page belongs to the gateway of {name} and stays there; another profile's page opens from a terminal with the command in its panel.", { name: l.this }),
      t("Make default writes the profile a bare nodecode runs from the next launch on; nothing running moves. Export saves a home as an archive without its store, secrets and credentials, which nodecode profile import brings back. Named profiles live in {folder}.", { folder: l.folder }));
    const head = headline(t("Profiles"), plural(l.profiles.length, t("{n} profile."), t("{n} profiles.")), t("This page belongs to {name}.", { name: l.this }));
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head, words);

    const find = el("input", "control ctl-prof-find");
    Object.assign(find, { type: "search", placeholder: t("Search"), value: p.find });
    find.dataset.input = "prof-find";
    find.setAttribute("aria-label", t("Search"));
    const add = button("solid", "", { act: "prof-new" });
    add.append(icon("plus", 14), t("New"));
    add.disabled = !!c.busy;
    const list = block("", find, add);
    list.classList.add("ctl-prof-block");
    const rows = el("ol", "ctl-prof-list");
    // This page's profile first, the rest by name.
    const sorted = l.profiles.slice().sort((a, b) => (!!b.current - !!a.current) || a.name.localeCompare(b.name, undefined, { numeric: true }));
    for (const r of sorted) rows.append(drawRow(r));
    list.append(rows);
    if (!l.profiles.some((r) => r.current)) list.append(note(t("This gateway runs {home}, a home NODECODE_HOME names, which is not among the profiles.", { home: l.home })));
    root.append(list);
    const open = p.panel && (p.panel.name ? row(p.panel.name) : true);
    if (open) root.append(p.panel.name ? drawProfile(row(p.panel.name)) : drawBuilder());
    return root;
  }

  async function copy(text) {
    try {
      await navigator.clipboard.writeText(text);
      toast(t("Copied: {text}", { text }));
    } catch {
      toast(t("Could not copy; select it by hand: {text}", { text }), { error: true });
    }
  }

  function click(d) {
    switch (d.act) {
      case "prof-open":
        if (p.panel?.name === d.name) close();
        else { close(); p.panel = { name: d.name }; }
        redraw();
        return true;
      case "prof-close": close(); redraw(); return true;
      case "prof-new":
        close();
        p.draft = fresh(d.from || "");
        p.panel = { name: null };
        redraw();
        queueMicrotask(() => document.querySelector('.ctl-prof-build input[name="name"]')?.focus());
        return true;
      case "prof-use":
        act(`prof-use:${d.name}`, async () => { p.list = await api("POST", "/api/gateway/profiles/use", { name: d.name }); toast(p.list.text); });
        return true;
      case "prof-soul":
        if (p.soul?.name === d.name) { p.soul = null; redraw(); return true; }
        act(`prof-soul:${d.name}`, async () => {
          const s = await api("GET", `/api/gateway/profiles/soul?${query(d.name)}`);
          p.soul = { ...s, draft: s.content };
        });
        return true;
      case "prof-soul-save":
        act("prof-soul", async () => {
          const s = await api("POST", "/api/gateway/profiles/soul", { name: p.soul.name, content: p.soul.draft });
          p.soul = null;
          toast(s.text);
          await load();
        });
        return true;
      case "prof-export":
        act(`prof-export:${d.name}`, async () => {
          await download(`/api/gateway/profiles/export?${query(d.name)}`, `nodecode-profile-${d.name}.tar.gz`);
          toast(t("Saved {file}", { file: `nodecode-profile-${d.name}.tar.gz` }));
        });
        return true;
      // A profile that stays says why at once; one that may go asks first.
      case "prof-ask-drop":
        if (row(d.name)?.kept) { p.refused = p.refused === d.name ? null : d.name; c.confirm = null; }
        else { c.confirm = `prof-drop:${d.name}`; p.refused = null; }
        redraw();
        return true;
      case "prof-drop":
        act("prof-drop", async () => {
          p.list = await api("DELETE", `/api/gateway/profiles?${query(d.name)}`);
          close();
          toast(p.list.text);
        });
        return true;
      case "prof-clone": p.draft.clone = d.clone; redraw(); return true;
      case "prof-soul-new": p.draft.soulOpen = !p.draft.soulOpen; redraw(); return true;
      case "prof-copy": copy(d.text); return true;
    }
    return false;
  }

  // What the hand types is kept, so a redraw never takes it back.
  function input(event) {
    const node = event.target;
    if (node.dataset.input === "prof-find") { p.find = node.value; sift(); return true; }
    if (node.dataset.input === "prof-soul") { if (p.soul) p.soul.draft = node.value; return true; }
    if (!node.name || !node.closest('form[data-act="prof-create"]')) return false;
    p.draft[node.name] = node.value;
    showReview();
    return true;
  }

  function submit(node) {
    if (node.dataset.act !== "prof-create") return false;
    const d = p.draft;
    const body = { name: d.name.trim(), clone: d.clone };
    if (d.clone !== "blank" && d.from) body.from = d.from;
    if (d.description.trim()) body.description = d.description.trim();
    if (d.soul.trim()) body.soul = d.soul;
    act("prof-create", async () => {
      p.list = await api("POST", "/api/gateway/profiles", body);
      p.draft = fresh();
      p.panel = { name: body.name };
      toast(p.list.text);
    }, formKey(node));
    return true;
  }

  return { load, draw, click, input, submit };
}
