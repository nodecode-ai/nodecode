// control.js -- the Control pane: the organism this page is attached to, run
// from the page the way a shell runs it with its own verbs. Gateway is
// `nodecode gateway' (how it runs, update, restart, stop), /doctor's health
// check and /backup's backups; Logs what `gateway logs' reads, Settings what
// /connect and /models move and every other setting as a declared section's
// form (settings.js), Cells what /cells and /setup move, with the hub
// beside them; Index what the organism keeps, as /index lists it (index.js),
// MCP that cell's own route (mcp.js), Profiles what `nodecode profile' moves
// (profiles.js); Channels and Scheduled jobs are the channel kit's and the
// cron cell's own routes, a channel set up by its section's
// form, who asks to be let in approved under it, and Link the link cell's
// (access.js); a tab whose cell is not loaded says so. Every control is a
// data-act the pane's own click handler reads; a change that waits for a
// restart says so where it is made.

import { el, button, mark, sectionHead, headline, banner, dot, when, uptime, waiting, plural } from "./observe.js";
import { makeSettings, title, steps } from "./settings.js";
import { makeIndex } from "./index.js";
import { makeMcp } from "./mcp.js";
import { makeProfiles } from "./profiles.js";
import { makeAccess } from "./access.js";
import { t } from "./i18n.js";
import { icon } from "./icons.js";

const TABS = [["gateway", t("Gateway")], ["logs", t("Logs")], ["settings", t("Config")], ["cells", t("Cells")], ["index", t("Index")], ["mcp", "MCP"], ["profiles", t("Profiles")], ["channels", t("Channels")], ["link", t("Link")], ["jobs", t("Scheduled jobs")]];
const LABELS = Object.fromEntries(TABS);
// A phone -- the page's own phone width, app.css's 720 px -- opens Control on
// its sections as one list, each saying how it stands, and a section as a page
// of its own under the way back, where a wide window has the strip of tabs (ten
// of them wrap into three rows on a phone).
export const PHONE = matchMedia("(max-width: 720px)");
// A wide window -- app.css's rail and list, 1024 px and up -- has the same list
// beside the rail, the open section lit, and no strip of tabs over the page.
export const WIDE = matchMedia("(min-width: 1024px)");
const GROUPS = [[t("This machine"), ["gateway", "logs"]], [t("What it runs"), ["settings", "cells", "mcp", "index"]],
  [t("Who reaches it"), ["channels", "link", "profiles", "jobs"]]];
const MARKS = { gateway: "server", logs: "lines", settings: "sliders", cells: "grid", index: "book", mcp: "plug",
  profiles: "person", channels: "chat", link: "globe", jobs: "clock" };
const FAILED = ["error", "refused"];
const RUNS = {
  unit: t("Runs as a systemd user unit, which starts it at login and after a crash."),
  serve: t("Runs headless in the foreground (nodecode serve): nothing starts it again if it stops."),
  shell: t("Runs inside a shell, in that shell's terminal."),
};
const SOURCES = { config: t("set in config.jsonc"), api_key: t("saved here"), env: t("from the environment"), oauth: t("signed in") };
// The ready keys' order: saved here, set in config.jsonc, the environment's, a sign-in.
const KEY_ORDER = ["api_key", "config", "env", "oauth"];
// What is wrong with a provider config.jsonc names with no key, as the
// gateway says it (its fix): the banner's line and why.
const FIXES = {
  pasted_key: (name) => [t("{name} is an API key saved as a provider name.", { name }), t("It names no provider. Forget takes it out of config.jsonc.")],
  keyless_copy: (name, of) => [t("{name} is a copy of {of} with no key.", { name, of }), t("{of} has the key. Forget takes this copy out of config.jsonc.", { of })],
};
// A provider's name as the page says it (a pasted key's id comes masked from the gateway).
const providerName = (p) => p.name || p.id;
const LEVELS = [["all", t("All")], ["warn", t("Warnings")], ["error", t("Errors")]];
// A channel's state and a job's in words; the state itself stays on the row for its color.
const STATES = { starting: t("starting"), running: t("running"), degraded: t("degraded"), stopped: t("stopped"),
  refused: t("refused"), unconfigured: t("not set up"), "not started": t("not started"), off: t("off") };
const JOB_STATES = { paused: t("paused"), done: t("done") };
const LOG_CAP = 2000;
// A phone draws the newest lines of the log, two hundred more at a time.
const LOG_PAGE = 200;

// Where a hub entry comes from, as host/owner/repo[/folder] @ the commit it is pinned to.
function pinned(row) {
  return `${row.git.replace(/^https:\/\//, "").replace(/\.git$/, "")}${row.path ? `/${row.path}` : ""} @ ${row.commit.slice(0, 7)}`;
}

// Its code at that commit: GitHub's tree view there, else the repository, whose commit the row names.
function codeUrl(row) {
  const repo = row.git.replace(/\.git$/, "");
  return repo.startsWith("https://github.com/") ? `${repo}/tree/${row.commit}${row.path ? `/${row.path}` : ""}` : repo;
}
// A finding the page can act on, by its key: the control that does it. One
// about a config section opens that section's form (sectionFix) instead.
const SETTINGS = [t("Open Config"), { act: "tab", tab: "settings" }];
const LOGS = [t("Open Logs"), { act: "tab", tab: "logs" }];
const REMEDIES = {
  build: [t("Restart"), { act: "ask", verb: "restart" }], update: [t("Update now"), { act: "update" }],
  model: SETTINGS, credential: SETTINGS, cell: LOGS, cells: LOGS, gateway: LOGS,
};
const sectionFix = (section) => [t("Open its settings"), { act: "tab", tab: "settings", section }];
// What a backup holds, in words; the store is said by its sessions.
const HOLDS = { "config.jsonc": t("config"), "auth.json": t("keys"), secrets: t("secrets"), memory: t("memory"),
  skills: t("skills"), cells: t("cells"), "SOUL.md": "SOUL.md", "themes.json": t("themes") };

// A level word the line says of itself wins; failing one, its words.
function levelOf(line) {
  const word = /\b(ERROR|WARNING|WARN|INFO|DEBUG)\b/.exec(line)?.[1];
  if (word) return word === "ERROR" ? "error" : word.startsWith("WARN") ? "warn" : "info";
  if (/\b(error|fatal|panic)\b/i.test(line)) return "error";
  return /\bwarn(ing)?\b/i.test(line) ? "warn" : "info";
}

export function bytes(n) {
  if (n == null) return "—";
  const units = ["B", "KB", "MB", "GB"];
  let i = 0;
  while (n >= 1024 && i < units.length - 1) { n /= 1024; i++; }
  return `${n.toFixed(i && n < 10 ? 1 : 0)} ${units[i]}`;
}

// A plain label over the value, the name an engineer searches for under it.
function fact(label, value, tag) {
  const row = el("div", "fact");
  const dd = el("dd");
  dd.append(el("span", "value", value));
  // The field's own name is for whoever debugs it: a tooltip, never a line (rule 7).
  if (tag) row.title = tag;
  row.append(el("dt", "", label), dd);
  return row;
}

// A section of a tab; TITLE empty where the strip already names it.
export function block(title, ...rest) {
  const node = el("section", "dash-section ctl-block");
  if (title || rest.length) node.append(sectionHead(title, ...rest));
  return node;
}

export function note(text) {
  return el("p", "obs-note", text);
}

export function field(name, label, attrs = {}) {
  const wrap = el("label", "ctl-field");
  const input = el(attrs.multiline ? "textarea" : "input", "control");
  input.name = name;
  for (const [key, value] of Object.entries(attrs)) if (key !== "multiline") input.setAttribute(key, value);
  wrap.append(el("span", "micro", label), input);
  return wrap;
}

export function form(act, ...children) {
  const node = el("form", "ctl-form");
  node.dataset.act = act;
  node.append(...children);
  return node;
}

// A pane form's key: its act, with the one thing it is for (a provider, a
// section, a backup) when it names one. What the hand typed and why a save
// was refused are kept under it.
export function formKey(node) {
  const d = node.dataset;
  return `${d.act}:${d.id || d.key || d.name || ""}`;
}

// Whether a failure never had the gateway's answer: nothing came back, or a
// proxy in front of it said it could not reach it. Only then is trying again
// the same thing worth offering; an answer is the gateway's word on the request.
const unreached = (error) => error.status === 0 || error.status >= 502;

// A list's own row around a form or a question asked in it.
export function item(node) {
  const li = el("li", "ctl-inset");
  li.append(node);
  return li;
}

// A switch drawn as one: a knob on a track, lit when ON. WORD, the state in
// words, stays in it for a reader without the picture.
export function pill(on, word, data) {
  const node = button("pill", "", data);
  node.setAttribute("role", "switch");
  node.setAttribute("aria-checked", String(on));
  node.append(el("span", "unseen", word));
  return node;
}

// What a section is for, folded behind an info mark by its heading: the mark,
// for the heading, and the notes it opens under it. C.about keeps which are
// open by ID, so the pane's redraws leave them as the hand left them.
export function about(c, id, ...texts) {
  const open = c.about.has(id);
  const words = el("div", "ctl-about");
  words.id = `ctl-about-${id}`;
  words.hidden = !open;
  words.append(...texts.map(note));
  const node = mark("info", t("What this is"), { act: "about", id });
  node.classList.add("info");
  node.setAttribute("aria-expanded", String(open));
  node.setAttribute("aria-controls", words.id);
  return [node, words];
}

export function confirmBox(title, go, act, data = {}) {
  const panel = el("div", "confirm");
  panel.append(el("p", "title", title));
  const actions = el("div", "actions");
  actions.append(button("solid", go, { act, ...data }), button("word", t("Cancel"), { act: "cancel" }));
  panel.append(actions);
  return panel;
}

// ONTAB is told when a click in the pane turns it to another tab, so the page's
// address follows; show(TAB) is the page turning it there, and OPENTAB the
// page opening the pane on TAB from outside it (the link panel).
export function makeControl({ api, download, toast, openSession, isOpen, folder, redraw, onTab, openTab }) {
  const c = {
    tab: "gateway",
    service: null, identity: null, said: null, busy: null, confirm: null, leaving: null,
    doctor: null, backups: null, restoring: null,
    log: { lines: [], cursor: null, source: null, where: null, error: null, follow: true, filter: "", level: "all", shown: LOG_PAGE },
    keys: null, cells: null, model: null, models: null, editing: null,
    channels: null, probed: {}, setup: null, jobs: null, hub: null, hubReading: false, about: new Set(),
    // By FORMKEY: what the hand typed into a form no tab keeps itself, and why its last save was refused.
    drafts: {}, refused: {},
    // A phone's list: whether it is on screen rather than a section, where it was
    // scrolled when a section was opened from it, the move a redraw slides in by,
    // and what only the list reads (the MCP servers, the profiles).
    home: true, homeAt: 0, motion: null, servers: null, profiles: null,
    // A phone's sheet: the row or the standing form it raises, by its key; and a
    // long list's search, state and how many of its rows it shows, by the list's key.
    sheet: null, lists: {},
  };
  // The list stands in for the strip on a phone only; a wide window keeps the tab.
  const atHome = () => c.home && PHONE.matches;
  const scroller = () => document.getElementById("observe");
  const cfgs = makeSettings({ c, api, act, toast, redraw, refuse });
  const index = makeIndex({ c, api, act, toast, redraw, settle: (rows) => { if (PHONE.matches) tapRows(rows); } });
  const mcp = makeMcp({ c, api, act, toast, redraw, reload: () => { if (shown && c.tab === "mcp") load(); } });
  const profs = makeProfiles({ c, api, act, toast, redraw, download });
  const access = makeAccess({ c, api, act, toast, redraw, unread, missing, openTab });
  let followTimer = null;
  let pollTimer = null;
  let jobTimer = null;
  let shown = false;

  // A read lands where it stands; a route a cell did not load is 404.
  async function read(key, path) {
    try { c[key] = await api("GET", path); } catch (error) { c[key] = { error: error.message, missing: error.status === 404 }; }
  }

  // The hub's index is read over the network, once and again on Read again, drawn when it lands.
  async function readHub() {
    if (c.hubReading) return;
    c.hubReading = true;
    await read("hub", "/api/gateway/cells/hub");
    c.hubReading = false;
    if (shown && c.tab === "cells") quietRedraw();
  }

  // What the list reads to say how every section stands (glance).
  const listReads = () => Promise.all([read("service", "/api/gateway/service"), read("identity", "/api/gateway/identity"),
    read("backups", "/api/gateway/backups"), read("model", "/api/gateway/model"), read("cells", "/api/gateway/cells"),
    read("channels", "/api/channels"), read("jobs", "/api/cron/jobs"), read("servers", "/api/mcp"),
    read("profiles", "/api/gateway/profiles"), access.load()]);
  // A wide window's list beside the open section: read again at most every half
  // minute, never waited on, drawn as it lands.
  let listAt = 0;
  function readList() {
    if (Date.now() - listAt < 30000) return;
    listAt = Date.now();
    listReads().then(() => { if (shown && WIDE.matches) quietRedraw(); });
  }

  async function load() {
    if (atHome()) {
      await listReads();
      if (shown && atHome()) quietRedraw();
      return;
    }
    if (WIDE.matches) readList();
    const tab = c.tab;
    if (tab === "gateway") {
      await Promise.all([read("service", "/api/gateway/service"), read("identity", "/api/gateway/identity"),
        read("backups", "/api/gateway/backups")]);
      watchJob();
    }
    else if (tab === "settings") await Promise.all([read("keys", "/api/gateway/keys"),
      read("model", "/api/gateway/model"), read("models", "/api/gateway/models"), cfgs.load()]);
    else if (tab === "cells") {
      if (!c.hub) readHub(); // the network's: never waited on
      await Promise.all([read("cells", "/api/gateway/cells"), read("service", "/api/gateway/service")]);
    } else if (tab === "index") await index.load();
    else if (tab === "mcp") await mcp.load();
    else if (tab === "profiles") await profs.load();
    else if (tab === "channels") await Promise.all([read("channels", "/api/channels"), cfgs.load()]);
    else if (tab === "link") await access.load();
    else if (tab === "jobs") await read("jobs", "/api/cron/jobs");
    else if (tab === "logs") { await readLog(true); return; }
    if (shown && tab === c.tab) quietRedraw();
  }

  // A poll never takes a field out from under the hand typing in it.
  function quietRedraw() {
    const active = document.activeElement;
    if (active?.closest?.("#observe") && active.matches("input, textarea, select")) return;
    if (!c.confirm && !document.querySelector(".control-pane .ctl-sheet")) redraw();
  }

  async function readLog(fresh) {
    const log = c.log;
    const path = fresh || log.cursor == null ? "/api/gateway/logs?lines=500"
      : `/api/gateway/logs?lines=2000&after=${encodeURIComponent(log.cursor)}`;
    let grew = true;
    try {
      const data = await api("GET", path);
      grew = fresh || data.lines.length > 0;
      log.lines = fresh ? data.lines : log.lines.concat(data.lines).slice(-LOG_CAP);
      Object.assign(log, { cursor: data.cursor, source: data.source, where: data.where, error: null });
    } catch (error) {
      log.error = error.message;
    }
    if (!shown || c.tab !== "logs" || !grew) return;
    if (fresh) redraw(); else drawLog();
  }

  function schedule() {
    clearInterval(followTimer);
    clearInterval(pollTimer);
    if (!shown) return;
    if (!atHome() && c.tab === "logs" && c.log.follow) followTimer = setInterval(() => { if (isOpen()) readLog(false); }, 2000);
    if (atHome() || ["gateway", "cells", "mcp", "channels", "link", "jobs"].includes(c.tab)) pollTimer = setInterval(() => { if (isOpen()) load(); }, 10000);
  }

  // A backup or a restore runs on the gateway: the page reads how it goes
  // until it ends, and says so when it does.
  function watchJob() {
    clearTimeout(jobTimer);
    if (c.backups?.job?.state !== "running") return;
    jobTimer = setTimeout(async () => {
      if (!shown || c.tab !== "gateway") return;
      await read("backups", "/api/gateway/backups");
      const job = c.backups?.job;
      if (job?.state === "done") toast(jobDone(job));
      else if (job?.state === "failed") toast(jobFailed(job), { error: true });
      quietRedraw();
      watchJob();
    }, 1500);
  }

  // FORM, the key of the form a save came from: a save that fails keeps what
  // was typed and says why under its form, the hand put back on the field
  // the gateway names; one that lands lets the form go blank. A control's
  // failure is a toast, with Retry only when the gateway never answered.
  async function act(label, work, form) {
    c.busy = label;
    redraw();
    try {
      await work();
      if (form) { delete c.drafts[form]; delete c.refused[form]; c.sheet = null; }
    } catch (error) {
      if (form) c.refused[form] = { text: error.message, field: error.field };
      else toast(t("Not done: {error}", { error: error.message }), { error, retry: unreached(error) && (() => act(label, work)) });
    }
    c.busy = null;
    c.confirm = null;
    redraw();
    if (form && c.refused[form]) refocus(form);
  }

  // A refusal the page makes itself, before anything is sent: said where the gateway's would be.
  function refuse(form, text, field) {
    c.refused[form] = { text, field };
    redraw();
    refocus(form);
  }

  // The field the refusal names, else the form's first.
  function refocus(key) {
    const node = [...document.querySelectorAll("#observe form.ctl-form")].find((each) => formKey(each) === key);
    if (!node) return;
    const field = c.refused[key]?.field;
    const named = field && node.querySelector(`[name="${CSS.escape(field)}"], [data-field="${CSS.escape(field)}"]`);
    const at = named?.matches("input, textarea, select") ? named
      : (named || node).querySelector("input:not([type=hidden]), textarea, select");
    at?.focus();
    at?.scrollIntoView({ block: "nearest" });
  }

  // Every pane form as the hand left it, whatever redrew it: what was typed,
  // back in its field, and why its last save was refused, under it.
  function kept(root) {
    for (const node of root.querySelectorAll("form.ctl-form")) {
      const key = formKey(node);
      for (const [name, value] of Object.entries(c.drafts[key] || {})) {
        const input = node.elements.namedItem(name);
        if (input) input.value = value;
      }
      const refused = c.refused[key];
      if (!refused) continue;
      const line = el("p", "ctl-said refused ctl-form-refused", `${refused.text[0].toUpperCase()}${refused.text.slice(1)}`);
      line.id = `ctl-form-refused-${key.replace(/[^\w-]/g, "_")}`;
      node.after(line);
      const field = refused.field && node.querySelector(`[name="${CSS.escape(refused.field)}"], [data-field="${CSS.escape(refused.field)}"]`);
      for (const input of field ? [field, ...field.querySelectorAll("input")] : []) {
        input.setAttribute("aria-invalid", "true");
        input.setAttribute("aria-describedby", line.id);
      }
    }
  }

  // --- the tabs ---------------------------------------------------------------

  // A read that failed says so where its list would stand, with a Retry.
  function unread(text) {
    const node = waiting(text);
    node.append(" ", button("word", t("Retry"), { act: "reread" }));
    return node;
  }

  function drawGateway() {
    const root = el("div", "ctl-tab");
    const s = c.service;
    const id = c.identity;
    if (!s || s.error) { root.append(s?.error ? unread(t("Could not read the gateway: {error}", { error: s.error })) : waiting(t("Looking…"))); return root; }
    const version = id?.version || "";
    root.append(headline(`pid ${s.pid} · ${s.runs}`,
      id?.started_at ? t("nodecode {version}, {uptime}.", { version: version.split("+")[0], uptime: uptime(id.started_at) })
        : t("nodecode {version}.", { version: version.split("+")[0] }),
      RUNS[s.runs] || ""));
    const facts = el("dl", "facts ctl-facts");
    facts.append(
      fact(t("Version"), version || "—", version.includes("+") ? `commit ${version.split("+")[1]}` : "version"),
      fact(t("Started"), id?.started_at ? when(new Date(id.started_at * 1000).toISOString()) : "—", "started_at"),
      fact(t("Process"), String(s.pid), "pid"),
      fact(t("How it runs"), s.runs === "unit" ? s.unit : s.runs === "serve" ? "nodecode serve" : t("a shell"), `runs ${s.runs}`),
      fact(t("Background gateway"), s.gateway, "nodecode gateway"),
      fact(t("Home"), s.home, "NODECODE_HOME"),
      fact(t("Store"), s.store ? `${s.store} · ${bytes(s.store_bytes)}` : t("not open"), t("the session store, with its log")),
    );
    root.append(facts);

    const [updateMark, updateAbout] = about(c, "update", t("A release that can go live does so at once, in this organism and every attached shell; one that changes what it boots with runs from the next restart."));
    const update = block(t("Update"), updateMark);
    update.append(updateAbout);
    const row = el("div", "ctl-actions");
    row.append(button("quiet", c.busy === "check" ? t("Checking…") : t("Check for an update"), { act: "check" }),
      button("quiet", c.busy === "update" ? t("Updating…") : t("Update now"), { act: "update" }));
    for (const b of row.children) b.disabled = !!c.busy;
    update.append(row);
    if (c.said) update.append(el("p", `ctl-said${c.said.bad ? " refused" : ""}`, c.said.text));
    root.append(update);

    const life = block(t("Restart and stop"));
    for (const [verb, word] of [["restart", t("Restart")], ["stop", t("Stop")]]) {
      const line = el("div", "ctl-line");
      const go = button("quiet", word, { act: "ask", verb });
      go.disabled = !s[verb].able || !!c.busy || !isOpen();
      line.append(go, el("span", s[verb].able ? "ctl-what" : "ctl-what refused", s[verb].text));
      life.append(line);
      if (c.confirm === verb) {
        life.append(confirmBox(verb === "stop"
          ? t("Stop this organism? {what}. This page and every attached shell lose it until it is back.", { what: s.stop.text })
          : t("Restart this organism? {what}. A turn that is running is picked up again after.", { what: s.restart.text }), word, verb));
      }
    }
    root.append(life, drawDoctor(), drawBackups());
    return root;
  }

  // What /doctor prints in a shell: the facts the Dashboard shows, then what
  // only a look can say. A finding the page can act on carries its control.
  function drawDoctor() {
    const d = c.doctor;
    const go = button("quiet", c.busy === "doctor" ? t("Checking…") : t("Check everything"), { act: "doctor" });
    go.disabled = !!c.busy;
    const node = block(t("Health check"), go);
    if (!d) {
      node.append(note(t("Reads the store back whole, then checks the build on disk, the free disk, config.jsonc and every section declared in it, the default provider's key and a release waiting: what /doctor prints in a shell.")));
      return node;
    }
    if (d.error) { node.append(el("p", "ctl-said refused", d.error)); return node; }
    const problems = d.facts.filter((f) => f.tone !== "ok").length;
    const found = problems ? plural(problems, t("{n} thing to look at"), t("{n} things to look at")) : t("Nothing to look at");
    node.append(note(`${found} · ${t("checked {when}, in {seconds} s", { when: when(d.at), seconds: d.seconds.toFixed(1) })}`));
    const rows = el("ol", "ctl-list");
    for (const f of d.facts) {
      const li = el("li", "ctl-row ctl-doctor-row");
      li.dataset.tone = f.tone;
      const name = el("span", "ctl-name");
      name.append(dot(f.tone === "ok" ? "" : f.tone === "danger" ? "fail" : "act"), ` ${f.label}`);
      name.title = f.key;
      const acts = el("span", "ctl-acts");
      const fix = f.tone !== "ok" && (f.section ? sectionFix(f.section) : REMEDIES[f.key]);
      if (fix) {
        const fixer = button("word", fix[0], fix[1]);
        fixer.disabled = !!c.busy;
        acts.append(fixer);
      }
      li.append(name, el("span", "ctl-detail", f.detail), acts);
      rows.append(li);
      if (f.remedy) rows.append(el("li", `ctl-sub${f.tone === "danger" ? " refused" : ""}`, f.remedy));
    }
    node.append(rows);
    return node;
  }

  function jobDone(job) {
    return job.verb === "backup" ? t("Backed up: {text}", { text: job.text })
      : t("Restored as the profile {profile}: {text}", { profile: job.profile, text: job.text });
  }

  function jobFailed(job) {
    return job.verb === "backup" ? t("The backup failed: {error}", { error: job.error }) : t("The restore failed: {error}", { error: job.error });
  }

  function jobLine(job) {
    const text = job.state === "running" ? (job.verb === "backup"
      ? t("Backing up: {step}… {seconds} s", { step: job.step, seconds: job.seconds })
      : t("Restoring as the profile {profile}: {step}… {seconds} s", { profile: job.profile, step: job.step, seconds: job.seconds }))
      : job.state === "done" ? jobDone(job)
      : jobFailed(job);
    return el("p", `ctl-said${job.state === "failed" ? " refused" : ""}`, text);
  }

  function drawBackups() {
    const b = c.backups;
    const running = b?.job?.state === "running";
    const go = button("quiet", running && b.job.verb === "backup" ? t("Backing up…") : t("Back up now"), { act: "backup" });
    go.disabled = !!c.busy || running;
    const [infoMark, words] = about(c, "backups", t("A backup is one file that holds this home whole: every session (a copy of the store taken while it runs), config.jsonc, SOUL.md, memory, skills and the cell folders."));
    const node = block(t("Backups"), infoMark, go);
    node.append(words);
    const keys = el("p", "ctl-notice", t("It holds your provider keys and pasted secrets too: keep the file where you keep passwords."));
    keys.dataset.level = "warning";
    node.append(keys);
    if (!b || b.error) { node.append(b?.error ? unread(b.error) : waiting(t("Looking…"))); return node; }
    if (b.job) node.append(jobLine(b.job));
    const rows = el("ol", "ctl-list");
    for (const k of b.backups) {
      const li = el("li", "ctl-row");
      const name = el("span", "ctl-name", k.created_at ? when(k.created_at) : k.name);
      name.title = k.name;
      const holds = (k.holds || []).map((h) => HOLDS[h]).filter(Boolean).join(t(", "));
      const what = [bytes(k.bytes), k.sessions != null ? plural(k.sessions, t("{n} session"), t("{n} sessions")) : "", holds].filter(Boolean);
      const acts = el("span", "ctl-acts");
      acts.append(button("word", t("Restore"), { act: "ask-restore", name: k.name }),
        mark("download", t("Download"), { act: "get-backup", name: k.name }, c.busy === `get:${k.name}` && t("Saving…")),
        mark("trash", t("Delete"), { act: "ask-drop", name: k.name }));
      for (const each of acts.children) each.disabled = !!c.busy || running;
      li.append(name, el("span", "ctl-detail", what.join(" · ")), acts);
      rows.append(li);
      if (c.restoring === k.name) {
        const restore = button("solid", t("Restore"), {});
        restore.type = "submit";
        const f = form("restore", field("profile", t("As the profile"), {
          value: `restored-${/\d{4}-\d\d-\d\d-\d{6}/.exec(k.name)?.[0] || "backup"}`, required: "", autocomplete: "off",
          pattern: "[a-z0-9][a-z0-9_\\-]{0,63}", title: t("a-z, 0-9, _ and -, starting with a letter or digit"),
        }), restore, button("word", t("Cancel"), { act: "cancel" }));
        f.dataset.name = k.name;
        rows.append(item(f), el("li", "ctl-sub", t("It comes back as a profile: a home of its own beside this one, which nothing here changes. nodecode -p NAME opens it; nodecode profile use NAME makes it the one a bare launch opens. It answers the same channels this home does, so run one of the two at a time.")));
      }
      if (c.confirm === `drop:${k.name}`) {
        rows.append(item(confirmBox(t("Delete the backup made {when}? The file is gone for good; this home is untouched.", { when: when(k.created_at) }), t("Delete"), "drop-backup", { name: k.name })));
      }
    }
    if (!b.backups.length) rows.append(el("li", "obs-note", t("No backup yet.")));
    node.append(rows, note(t("Kept in {folder}, readable by you alone.", { folder: b.folder })));
    return node;
  }

  function logMatches(line) {
    const log = c.log;
    if (log.filter && !line.toLowerCase().includes(log.filter.toLowerCase())) return false;
    const level = levelOf(line);
    return log.level === "all" || level === log.level || (log.level === "warn" && level === "error");
  }

  // How the log stands, in one sentence: its errors and warnings as Level counts them.
  function logState() {
    const lines = c.log.lines || [];
    if (!lines.length) return c.log.source ? t("Nothing written yet.") : t("Reading…");
    const levels = lines.map(levelOf);
    return t("{errors} errors, {warnings} warnings.", { errors: levels.filter((l) => l === "error").length, warnings: levels.filter((l) => l !== "info").length });
  }

  function drawLog() {
    const box = document.getElementById("control-log");
    if (!box) return;
    // A phone reads the log in the page's one scroll: following keeps to its end
    // while the reader is there, and a scroll up to read stops it moving.
    const phone = PHONE.matches;
    const pane = phone ? scroller() || box : box;
    const atEnd = pane.scrollHeight - pane.scrollTop - pane.clientHeight < (phone ? 120 : 40);
    const fresh = !box.querySelector(".ctl-logline");
    // Each level's count stands on its segment; Warnings counts the errors with it, as it shows them.
    const levels = c.log.lines.map(levelOf);
    const counts = { all: levels.length, warn: levels.filter((l) => l !== "info").length, error: levels.filter((l) => l === "error").length };
    for (const sw of document.querySelectorAll('.control-pane [data-act="level"] .n')) sw.textContent = phone ? ` ${counts[sw.parentElement.dataset.level]}` : "";
    const kept = c.log.lines.filter(logMatches);
    const drawn = phone ? kept.slice(-c.log.shown) : kept;
    box.replaceChildren(...drawn.map((line) => {
      const row = el("div", "ctl-logline", line);
      row.dataset.level = levelOf(line);
      return row;
    }));
    if (drawn.length < kept.length) {
      const earlier = button("quiet ctl-more", t("Show {n} earlier", { n: Math.min(LOG_PAGE, kept.length - drawn.length) }), { act: "log-earlier" });
      box.prepend(earlier);
    }
    if (!kept.length) box.append(el("div", "obs-note", c.log.lines.length ? t("No line matches.") : t("Nothing written yet.")));
    const state = document.getElementById("control-log-state");
    if (state) state.textContent = logState();
    const count = document.getElementById("control-log-count");
    if (count) count.textContent = ` · ${t("{kept} of {total} lines", { kept: kept.length, total: c.log.lines.length })}`;
    if (phone ? c.log.follow && (atEnd || fresh) : atEnd || c.log.follow) pane.scrollTop = pane.scrollHeight;
  }

  function drawLogs() {
    const root = el("div", "ctl-tab ctl-logs");
    const log = c.log;
    root.append(headline(t("Logs"), logState(), log.lines?.length ? t("In the last {n} lines.", { n: log.lines.length }) : ""));
    root.lastChild.querySelector(".display").id = "control-log-state";
    const bar = el("div", "ctl-toolbar");
    const filter = el("input", "control");
    Object.assign(filter, { type: "search", placeholder: t("Filter lines"), value: log.filter });
    filter.dataset.input = "filter";
    filter.setAttribute("aria-label", t("Filter the log"));
    const levels = el("div", "scope");
    levels.setAttribute("role", "radiogroup");
    levels.setAttribute("aria-label", t("Level"));
    for (const [id, label] of LEVELS) {
      const sw = button("sw", label, { act: "level", level: id });
      sw.append(el("span", "n", ""));
      sw.setAttribute("role", "radio");
      sw.setAttribute("aria-checked", String(log.level === id));
      levels.append(sw);
    }
    const follow = button("sw", t("Follow"), { act: "follow" });
    follow.setAttribute("role", "switch");
    follow.setAttribute("aria-checked", String(log.follow));
    follow.title = t("Read what is written as it is written");
    bar.append(filter, levels, follow);
    root.append(bar);
    if (log.error && !log.source) { root.append(unread(log.error)); return root; }
    const where = el("p", "obs-note");
    where.append(log.source === "journal" ? t("systemd's journal, {where}", { where: log.where }) : log.where || t("Reading…"),
      el("span", "ctl-count", ""));
    where.lastChild.id = "control-log-count";
    root.append(where);
    const box = el("div", "ctl-log");
    box.id = "control-log";
    box.setAttribute("role", "log");
    root.append(box);
    if (log.error) root.append(el("p", "ctl-said refused", log.error));
    return root;
  }

  function keyCell(p) {
    const cell = el("span", "ctl-key");
    cell.dataset.source = p.source;
    cell.append(el("span", "ctl-key-from", SOURCES[p.source] || p.source));
    if (p.tail) cell.append(el("span", "tag", ` ····${p.tail}`));
    return cell;
  }

  function drawSettings() {
    const root = el("div", "ctl-tab");
    const [infoMark, words] = about(c, "settings", t("A key or the default model is used from the next turn on; nothing restarts. A cell reads its section of config.jsonc when it starts: after changing that section, restart the cell under Cells."));
    const m = c.model;
    const list = c.models?.models || [];
    const named = m && !m.error && (list.find((each) => each.provider === m.provider && each.model === m.model)?.name || m.model);
    const head = headline(t("Config"), named ? t("New sessions use {model}.", { model: named }) : m && !m.error ? t("No default model.") : t("Reading…"), m?.refusal || "");
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head, words);
    const model = block(t("Default model"));
    if (!m || m.error) model.append(m?.error ? unread(m.error) : waiting(t("Looking…")));
    else {
      const select = el("select", "control ctl-select");
      select.dataset.input = "model";
      select.setAttribute("aria-label", t("Default model"));
      const current = `${m.provider}\t${m.model}`;
      const options = list.map((each) => [`${each.provider}\t${each.model}`, `${each.name || each.model} · ${each.provider}`]);
      if (!options.some(([value]) => value === current)) options.unshift([current, `${m.model} · ${m.provider}`]);
      for (const [value, label] of options) {
        const option = el("option", "", label);
        option.value = value;
        option.selected = value === current;
        select.append(option);
      }
      model.append(select, note(t("New sessions, and sessions that follow the default, use it; a session given its own model keeps it.")));
    }
    root.append(model);

    root.append(drawKeys(), ...cfgs.blocks());
    return root;
  }

  // Provider keys (the provider keys variants artifact's A with C's strip; the
  // operator, 2026-10-20: "Land your pick A with C needs fixing strip"). What is
  // wrong comes first, the kit's banner with its Forget; then the ready ones, a
  // row each -- the name, where its key comes from and its last four, the acts
  // under the hand; then those with no key, a chip each. The head holds the
  // count, the filter and Add provider.
  function drawKeys() {
    const k = c.keys;
    if (!k || k.error) {
      const keys = block(t("Provider keys"));
      keys.append(k?.error ? unread(k.error) : waiting(t("Looking…")));
      return keys;
    }
    const phone = PHONE.matches;
    const fixes = k.providers.filter((p) => FIXES[p.fix]);
    const none = k.providers.filter((p) => p.source === "none" && !FIXES[p.fix]);
    const ready = k.providers.filter((p) => p.source !== "none")
      .sort((a, b) => KEY_ORDER.indexOf(a.source) - KEY_ORDER.indexOf(b.source) || providerName(a).localeCompare(providerName(b)));
    const find = el("input", "control ctl-keys-find");
    Object.assign(find, { type: "search", placeholder: t("Filter {n} providers", { n: k.providers.length }), value: c.keysFind || "" });
    find.dataset.input = "keys-find";
    find.setAttribute("aria-label", find.placeholder);
    const count = el("span", "reading", t("{ready} ready · {none} with no key", { ready: ready.length, none: none.length + fixes.length }));
    const keys = block(t("Provider keys"), count, find, ...phone ? [] : [button("quiet", t("Add provider"), { act: "add-key" })]);
    keys.classList.add("ctl-keys-block");
    for (const p of fixes) {
      const name = providerName(p);
      const [what, why] = FIXES[p.fix](name, p.of);
      const go = button("quiet", t("Forget"), { act: "forget-provider", id: p.id });
      go.disabled = !!c.busy;
      keys.append(banner("need", what, why, go));
      if (c.confirm === `forget:${p.id}`) keys.append(confirmBox(t("Take {name} out of config.jsonc?", { name }), t("Forget"), "drop-provider", { id: p.id, name }));
    }
    if (!phone && c.editing === "+") keys.append(addKey(k, true));
    const rows = el("ol", "ctl-list ctl-keys ctl-keys-ready");
    for (const p of ready) keyRow(rows, p);
    if (!k.providers.length) rows.append(el("li", "obs-note", t("No provider is named yet.")));
    if (ready.length) keys.append(el("p", "micro ctl-keys-group", t("Ready")));
    keys.append(rows);
    if (none.length) {
      const label = el("p", "micro ctl-keys-group");
      label.append(dot("act"), t("No key"));
      const chips = el("ol", "ctl-list ctl-keys ctl-keys-none");
      for (const p of none) keyRow(chips, p);
      keys.append(label, chips);
    }
    if (phone) keys.append(addKey(k, false));
    keys.append(note(t("Saved to {file}, as /connect saves it, once the provider has not refused the key. A key set in config.jsonc wins over one saved here. This page is never sent a key: only its last four characters.", { file: k.file })));
    const pages = k.suggest.filter((s) => s.page).map((s) => `${s.id}: ${s.page}`).join(" · ");
    if (pages) keys.append(note(t("Where keys are made: {pages}", { pages })));
    siftKeys(keys);
    return keys;
  }

  // One provider: a line of the ledger in the ready list, a chip in the no-key one.
  function keyRow(rows, p) {
    const li = el("li", "ctl-row");
    li.dataset.find = `${p.name || ""} ${p.id}`.toLowerCase();
    const name = el("span", "ctl-name", p.name || p.id);
    if (p.name) name.append(el("span", "tag", ` ${p.id}`));
    const acts = el("span", "ctl-acts");
    if (p.source === "api_key") acts.append(button("word", t("Replace"), { act: "edit-key", id: p.id }), mark("trash", t("Remove"), { act: "remove-key", id: p.id }));
    else if (p.source === "none") acts.append(button("word", t("Add key"), { act: "edit-key", id: p.id }));
    li.append(name, ...p.source === "none" ? [] : [keyCell(p)], acts);
    rows.append(li);
    if (c.editing === p.id) {
      const save = button("solid", c.busy === "key" ? t("Checking…") : t("Save"), {});
      save.type = "submit";
      const f = form("save-key", field("key", t("Key for {provider}", { provider: p.id }), { type: "password", autocomplete: "off", required: "" }),
        save, button("word", t("Cancel"), { act: "cancel" }));
      f.dataset.id = p.id;
      rows.append(item(f));
    }
    if (c.confirm === `key:${p.id}`) rows.append(item(confirmBox(t("Remove the key saved for {provider}? A turn on {provider} has none after, unless the environment gives one.", { provider: p.id }), t("Remove"), "drop-key", { id: p.id })));
  }

  // Another provider and its key: under the head once Add provider is pressed,
  // with its Cancel; a phone keeps it at the foot, a row that raises it.
  function addKey(k, cancel) {
    const known = el("datalist");
    known.id = "ctl-providers";
    for (const s of k.suggest) known.append(Object.assign(el("option"), { value: s.id }));
    const provider = field("provider", t("Another provider"), { list: "ctl-providers", placeholder: "anthropic", autocomplete: "off", required: "" });
    const secret = field("key", t("Its key"), { type: "password", autocomplete: "off", required: "" });
    const save = button("quiet", c.busy === "key" ? t("Checking…") : t("Save key"), {});
    save.type = "submit";
    save.disabled = !!c.busy;
    return form("save-key", provider, secret, save, ...cancel ? [button("word", t("Cancel"), { act: "cancel" })] : [], known);
  }

  // The keys' filter: the rows and chips whose name or id holds what was typed.
  function siftKeys(scope) {
    const needle = (c.keysFind || "").trim().toLowerCase();
    for (const li of scope.querySelectorAll(".ctl-keys > .ctl-row")) li.hidden = !!needle && !li.dataset.find.includes(needle);
  }

  // One cell's row, and the lines under it: why its start was refused, why
  // it stays on, why it cannot be turned on, what waits for a restart, and the
  // question a switch asks.
  function cellRow(rows, r, hub) {
    const li = el("li", "ctl-row");
    li.dataset.level = r.level === "error" ? "error" : r.on && r.loaded ? "started" : "off";
    li.dataset.seg = r.on ? "on" : "off";
    const name = el("span", "ctl-name");
    name.append(dot(r.refused || r.level === "error" ? "fail" : r.on && r.level === "started" ? "" : "off"), ` ${r.label}`);
    name.title = r.name;
    const what = el("span", "ctl-cell-what", r.description || (r.source === "own" ? t("Your own folder.") : ""));
    // A refused start is said under the row; the engineer's line keeps to what the cell is.
    const tech = el("span", "tag", r.git ? `${t("from the hub, {pin}", { pin: pinned(r) })} · ` : [r.name, r.refused ? r.kind : r.detail].filter(Boolean).join(" · "));
    if (r.git) tech.append(Object.assign(el("a", "", t("read the code")), { href: codeUrl(r), target: "_blank", rel: "noopener noreferrer" }));
    if (!r.on && r.brings?.length) tech.append(` · ${t("brings {cells}", { cells: r.brings.join(", ") })}`);
    what.append(tech);
    const acts = el("span", "ctl-acts");
    if (r.on && r.loaded && r.kind === "peripheral") {
      acts.append(mark("refresh", t("Restart"), { act: "restart-cell", name: r.name }, c.busy === `cell:${r.name}` && t("Restarting…")));
    }
    // On, and refused: its folder is in, and it does nothing until its start is not refused.
    if (r.refused) {
      const badge = el("span", "ctl-cfg-badge", t("refused"));
      badge.dataset.state = "refused";
      acts.append(badge);
    }
    // The operator's own folder is never the page's to move; a hub folder taken out turns on again
    // from the hub's list, where its pin shows.
    if (!["own", "gone"].includes(r.source)) {
      const sw = pill(r.on, r.refused ? t("Refused") : r.on ? t("On") : t("Off"), { act: "switch", name: r.name, on: String(!r.on), hub: hub ? "1" : "" });
      sw.setAttribute("aria-label", r.label);
      if (r.refused) sw.dataset.state = "refused";
      sw.title = r.on ? t("Turn {name} off", { name: r.label }) : t("Turn {name} on", { name: r.label });
      sw.disabled = (!r.on && !!r.missing) || (r.on && !!r.kept);
      acts.append(sw);
    }
    for (const b of acts.querySelectorAll("button")) b.disabled ||= !!c.busy;
    li.append(name, what, acts);
    rows.append(li);
    const sub = (text, refused) => rows.append(el("li", `ctl-sub${refused ? " refused" : ""}`, text));
    if (r.refused) {
      const line = el("li", "ctl-sub refused", t("Refused at start: {why}", { why: r.refused }));
      if (r.section) line.append(" ", button("word", t("Open its settings"), { act: "tab", tab: "settings", section: r.section }));
      rows.append(line);
    }
    if (r.on && r.kept) sub(t("Stays on: {why}.", { why: r.kept }));
    if (!r.on && r.missing) sub(t("Cannot be turned on here: it needs {needs}, which this build does not have.", { needs: r.missing.join(", ") }), true);
    if (r.on && !r.loaded && r.level !== "error") {
      const line = el("li", "ctl-sub", r.source === "own" ? t("In the folder; it loads at the next restart.") : t("In the folder, not loaded in this organism yet."));
      if (r.source !== "own") line.append(" ", button("word", t("Load now"), { act: "switch", name: r.name, on: "true" }));
      rows.append(line);
    }
    if (!r.on && r.loaded) sub(t("Off. Its code stays loaded in this organism until a restart."));
    if (c.confirm === `switch:${r.name}`) {
      rows.append(item(r.on
        ? confirmBox(t("Turn {name} off? Its folder leaves {folder} and it stops now; its code stays loaded in this organism until a restart.", { name: r.label, folder: c.cells.folder }), t("Turn off"), "switch-go", { name: r.name, on: "false" })
        : confirmBox(t("Install {name} from the hub? It is someone else's code, and it runs inside this organism with full access to your files, keys and sessions. It is pinned to {pin}: read the code at that commit before you say yes.", { name: r.label, pin: pinned(r) }), t("Install"), "switch-go", { name: r.name, on: "true" })));
    }
  }

  function drawCells() {
    const root = el("div", "ctl-tab");
    const a = c.cells;
    if (!a || a.error) { root.append(a?.error ? unread(t("Could not read the cells: {error}", { error: a.error })) : waiting(t("Looking…"))); return root; }
    const [infoMark, words] = about(c, "cells", a.folder
      ? t("A cell is a folder in {folder}. On puts it there, with the cells it needs, and loads it into this organism now; off takes it out and stops it. Code already loaded stays in this organism until a restart.", { folder: a.folder })
      : t("A cell is a folder in the cell folder. On puts it there, with the cells it needs, and loads it into this organism now; off takes it out and stops it. Code already loaded stays in this organism until a restart."));
    const on = a.cells.filter((r) => r.on).length;
    const failed = a.cells.filter((r) => r.refused || r.level === "error").length;
    const head = headline(plural(a.cells.length, t("{n} cell"), t("{n} cells")),
      t("{on} on, {off} off.", { on, off: a.cells.length - on }),
      t("{n} loaded at this boot.", { n: a.cells.filter((r) => r.loaded).length }));
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head);
    if (failed) {
      root.append(banner("fail", plural(failed, t("{n} cell failed to load."), t("{n} cells failed to load.")),
        a.cells.filter((r) => r.refused || r.level === "error").map((r) => r.label || r.name).join(", "),
        button("quiet", t("Read the log"), { act: "tab", tab: "logs" })));
    }
    root.append(words);
    const list = block("");
    const rows = el("ol", "ctl-list");
    for (const r of a.cells) cellRow(rows, r, false);
    if (!a.cells.length) rows.append(el("li", "obs-note", a.folder ? t("{folder} holds no cell.", { folder: a.folder }) : t("The cell folder is off in this process.")));
    list.append(rows);
    // A folder taken out keeps its code, and one put in by another process waits to be loaded: both wait for a restart.
    const waits = a.cells.filter((r) => r.on !== r.loaded && r.level !== "error").length;
    const s = c.service;
    if (waits && s?.restart) {
      const line = el("div", "ctl-line");
      const go = button("quiet", t("Restart now"), { act: "ask", verb: "restart" });
      go.disabled = !s.restart.able || !!c.busy || !isOpen();
      line.append(go, el("span", s.restart.able ? "ctl-what" : "ctl-what refused",
        `${plural(waits, t("{n} change waits for a restart."), t("{n} changes wait for a restart."))} ${s.restart.able ? t("A restart: {what}.", { what: s.restart.text }) : s.restart.text}`));
      list.append(line);
      if (c.confirm === "restart") list.append(confirmBox(t("Restart this organism? {what}. A turn that is running is picked up again after.", { what: s.restart.text }), t("Restart"), "restart"));
    }
    // A refused start is said on its own row; the board's line for it would say it twice.
    const refused = new Set(a.cells.filter((r) => r.refused).map((r) => r.name));
    for (const n of a.notices.filter((each) => !refused.has(each.key))) {
      const line = el("p", "ctl-notice", n.text);
      line.dataset.level = n.level;
      list.append(line);
    }
    root.append(list);
    root.append(drawHub(new Set(a.cells.filter((r) => r.on).map((r) => r.name))));
    return root;
  }

  function drawHub(listed) {
    const box = block(t("From the hub"), mark("refresh", t("Read again"), { act: "hub" }, c.hubReading && t("Reading…")));
    box.append(note(t("A hub cell is someone else's code. It runs inside this organism with full authority, your files, keys and sessions included, and is pinned to the commit shown: read the code at that commit before you turn it on.")));
    const h = c.hub;
    if (!h) box.append(waiting(t("Reading the hub's index…")));
    else if (h.error) box.append(el("p", "dash-line", t("The hub could not be read: {error}", { error: h.error })));
    else if (!h.url) box.append(waiting(t("This build names no hub.")));
    else {
      const rows = el("ol", "ctl-list");
      const offered = h.cells.filter((r) => !listed.has(r.name));
      for (const r of offered) cellRow(rows, r, true);
      if (!offered.length) rows.append(el("li", "obs-note", h.cells.length ? t("Every hub cell this platform runs is on.") : t("The hub offers nothing for this platform yet.")));
      box.append(rows, note(t("The hub's index: {url}", { url: h.url })));
      if (h.stale) box.append(note(t("This is the index as last read; reading it now failed: {error}", { error: h.stale })));
    }
    return box;
  }

  function missing(what, folder, section) {
    const box = el("div", "ctl-missing");
    box.append(el("p", "dash-line", t("The {what} cell is not loaded in this organism, so there is nothing to show here.", { what })),
      note(section ? t("Turn on {folder} under Cells, then give it a {section} section in config.jsonc.", { folder, section })
        : t("Turn on {folder} under Cells.", { folder })));
    return box;
  }

  function drawChannels() {
    const root = el("div", "ctl-tab");
    const ch = c.channels;
    if (ch?.missing) { root.append(missing("channels", t("a channel (discord, telegram) with the channel kit"), "channels.<name>")); return root; }
    if (!ch || ch.error) { root.append(ch?.error ? unread(t("Could not read the channels: {error}", { error: ch.error })) : waiting(t("Looking…"))); return root; }
    const [infoMark, words] = about(c, "channels", t("Set up walks a channel from its bot's token to who may talk to it. Check asks the platform what this channel's token can see. Restart channels starts every channel again on config.jsonc as it is now: what a changed section needs."));
    const running = ch.channels.filter((ch1) => ch1.state === "running").map((ch1) => `${ch1.id[0].toUpperCase()}${ch1.id.slice(1)}`);
    const asking = ch.pairing.reduce((n, p) => n + (p.asks?.length || 0), 0);
    const head = headline(t("Channels"),
      !running.length ? t("No channel is running.")
        : running.length === 1 ? t("{name} is running.", { name: running[0] })
        : t("{names} and {last} are running.", { names: running.slice(0, -1).join(", "), last: running.at(-1) }),
      asking ? plural(asking, t("{n} is asking to pair."), t("{n} are asking to pair.")) : t("Nobody is asking to pair."));
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head, words);
    // The page's drastic act stands at its foot (the standard's sixth slot).
    const foot = el("div", "ctl-foot");
    foot.append(button("quiet", c.busy === "channels" ? t("Restarting…") : t("Restart channels"), { act: "restart-channels" }));
    const list = block("");
    const rows = el("ol", "ctl-list");
    // A lane the kit reports, then one whose section is declared and quiet (turned off).
    const quiet = cfgs.sections().filter((s) => s.path.length === 2 && s.path[0] === "channels"
      && !ch.channels.some((ch1) => ch1.id === s.path[1])).map((s) => ({ id: s.path[1], state: s.enabled ? "not started" : "off" }));
    for (const ch1 of ch.channels.concat(quiet)) {
      const li = el("li", "ctl-row channel");
      const bad = ["stopped", "refused"].includes(ch1.state);
      li.dataset.level = bad ? "error" : ch1.connected ? "started" : "warn";
      const name = el("span", "ctl-name");
      name.append(dot(ch1.connected ? "" : bad ? "fail" : "off"), ` ${ch1.id[0].toUpperCase()}${ch1.id.slice(1)}`);
      const state = el("span", "ctl-detail");
      const badge = el("span", "ctl-cfg-badge", STATES[ch1.state] || ch1.state);
      badge.dataset.state = ch1.state;
      state.append(badge, " ", ch1.connected ? t("connected") : t("not connected"));
      const seen = el("span", "ctl-seen", ch1.last_event_at_ms ? t("last message {when}", { when: when(new Date(ch1.last_event_at_ms).toISOString()) }) : t("no message yet"));
      seen.title = "last_event_at_ms";
      const acts = el("span", "ctl-acts");
      const section = cfgs.find(`channels.${ch1.id}`);
      if (section) {
        const setup = button("word", c.setup === ch1.id ? t("Close") : section.needs === "complete" ? t("Edit") : t("Set up"), { act: "setup", id: ch1.id });
        setup.setAttribute("aria-expanded", String(c.setup === ch1.id));
        acts.append(setup);
      }
      acts.append(button("word", c.busy === `probe:${ch1.id}` ? t("Checking…") : t("Check"), { act: "probe", id: ch1.id }));
      li.append(name, state, seen, acts);
      rows.append(li);
      const more = [ch1.delivered != null ? t("{n} delivered", { n: ch1.delivered }) : "", ch1.sessions != null ? t("{n} sessions", { n: ch1.sessions }) : ""].filter(Boolean).join(" · ");
      if (ch1.detail) {
        const setup = ch1.state === "unconfigured";
        rows.append(el("li", `ctl-sub${setup ? "" : " refused"}`, setup ? ch1.detail : t("Last error: {error}", { error: ch1.detail })));
      }
      if (ch1.last_rejection) rows.append(el("li", "ctl-sub", t("Last refused: {what}", { what: ch1.last_rejection })));
      if (more) rows.append(el("li", "ctl-sub", more));
      if (c.probed[ch1.id]) rows.append(el("li", "ctl-sub probe", c.probed[ch1.id]));
      if (section && c.setup === ch1.id) rows.append(item(channelSetup(section)));
    }
    // Shipped beside this organism and not in its home: added from the Cells tab, not here.
    for (const id of ch.absent || []) {
      const li = el("li", "ctl-row channel");
      const name = el("span", "ctl-name");
      name.append(dot("off"), ` ${id[0].toUpperCase()}${id.slice(1)}`);
      const acts = el("span", "ctl-acts");
      acts.append(button("word", t("Add it"), { act: "tab", tab: "cells" }));
      li.dataset.level = "off";
      li.append(name, el("span", "ctl-detail", t("not installed: add nodecode-channel-{id} from the Cells tab, then set it up here", { id })), acts);
      rows.append(li);
    }
    if (!ch.channels.length && !quiet.length) rows.append(el("li", "obs-note", t("No channel is installed. Add one, then Set up walks it here.")));
    list.append(rows);
    root.append(list, access.pairing(ch), foot);
    return root;
  }

  // One channel's section under its guide, the steps a person takes on the
  // platform first; Start starts the channels again on what was saved.
  function channelSetup(s) {
    const panel = el("div", "ctl-cfg-setup");
    panel.append(el("h4", "micro", t("Set up {name}", { name: title(s).split(" · ").pop() })));
    const ol = el("ol", "ctl-cfg-steps");
    for (const step of steps(s.guide)) ol.append(el("li", "", step));
    const start = el("div", "ctl-cfg-applies");
    start.append(t("The channels start again on what is saved:"), " ",
      button("word", c.busy === "channels" ? t("Starting…") : t("Start it"), { act: "restart-channels" }));
    panel.append(ol, cfgs.body(s, { guide: false, after: start }));
    return panel;
  }

  function drawJobs() {
    const root = el("div", "ctl-tab");
    const j = c.jobs;
    if (j?.missing) { root.append(missing("cron", "nodecode-cron")); return root; }
    if (!j || j.error) { root.append(j?.error ? unread(t("Could not read the jobs: {error}", { error: j.error })) : waiting(t("Looking…"))); return root; }
    const [infoMark, words] = about(c, "jobs", t("A job runs in a session of its own, cron-<id>: open its name to read what it did."));
    const count = (state) => j.jobs.filter((job) => job.state === state).length;
    const next = j.jobs.filter((job) => job.state === "active" && job.next_at).sort((a, b) => a.next_at - b.next_at)[0];
    const head = headline(plural(j.jobs.length, t("{n} job"), t("{n} jobs")),
      t("{active} active, {paused} paused.", { active: count("active"), paused: count("paused") }),
      next ? t("Next: {name}, {when}.", { name: next.name, when: when(new Date(next.next_at * 1000).toISOString()) }) : "");
    (head.querySelector(".micro") || head).append(" ", infoMark);
    root.append(head, words);
    const list = block("");
    const rows = el("ol", "ctl-list");
    for (const job of j.jobs) {
      const li = el("li", "ctl-row");
      li.dataset.level = job.state === "active" ? "started" : "off";
      li.dataset.seg = job.state;
      const name = el("span", "ctl-name");
      const open = button("cell-link", job.name, { act: "open", id: job.session });
      open.title = t("{id} · open its session, {session}", { id: job.id, session: job.session });
      name.append(open);
      const timing = el("span", "ctl-detail");
      timing.append(el("span", "mono", job.schedule.display), ` · ${job.state === "active"
        ? (job.next_at ? t("next {when}", { when: when(new Date(job.next_at * 1000).toISOString()) }) : t("no run left")) : JOB_STATES[job.state] || job.state}`);
      const acts = el("span", "ctl-acts");
      // On a phone, whether it runs is its switch, the row's one thing on its right.
      const paused = job.state === "paused";
      const turn = { act: "job", op: paused ? "resume" : "pause", id: job.id };
      const runs = PHONE.matches && job.state !== "done" ? pill(!paused, paused ? t("Paused") : t("Active"), turn)
        : mark(paused ? "play" : "pause", paused ? t("Resume") : t("Pause"), turn);
      if (runs.classList.contains("pill")) runs.title = paused ? t("Resume") : t("Pause");
      acts.append(mark("next", t("Run now"), { act: "job", op: "run", id: job.id }), runs,
        mark("trash", t("Delete"), { act: "ask-job", id: job.id }));
      for (const b of acts.children) b.disabled = !!c.busy;
      li.append(name, timing, acts);
      rows.append(li);
      const last = job.last;
      rows.append(el("li", `ctl-sub ctl-job-last ctl-sheet-also${last && last.status !== "ok" ? " refused" : ""}`, last
        ? (last.line ? t("Last run {when}: {status}, {line}", { when: when(new Date(last.at * 1000).toISOString()), status: last.status, line: last.line })
          : t("Last run {when}: {status}", { when: when(new Date(last.at * 1000).toISOString()), status: last.status }))
        : t("Never run. It will ask: {prompt}", { prompt: job.prompt })));
    }
    if (!j.jobs.length) rows.append(el("li", "obs-note", t("No job is scheduled.")));
    list.append(rows);
    if (c.confirm?.startsWith?.("job:")) {
      const id = c.confirm.slice(4);
      list.append(confirmBox(t("Delete the job {id}? It fires no more; its session keeps what it did.", { id }), t("Delete"), "job", { op: "remove", id }));
    }
    root.append(list);

    const add = block(t("Add a job"));
    const prompt = field("prompt", t("What to do"), { multiline: true, rows: "3", placeholder: t("Summarize what changed in this folder since yesterday"), required: "" });
    prompt.classList.add("wide");
    const f = form("add-job",
      field("schedule", t("When"), { placeholder: "every day at 9am", required: "", autocomplete: "off" }),
      field("name", t("Name (optional)"), { autocomplete: "off" }),
      field("cwd", t("Folder"), { value: folder() || "", autocomplete: "off" }),
      prompt,
    );
    const go = button("quiet", c.busy === "add" ? t("Adding…") : t("Add job"), {});
    go.type = "submit";
    go.disabled = !!c.busy;
    f.append(go);
    add.append(f, note(t("When reads as 30m, every 2h, in 45m, at 17:30, every day at 9am, weekdays at 9:30, every mon,wed at 18:00, five-field cron (0 9 * * 1-5), or a time (2026-10-01T09:00); local time.")));
    root.append(add);
    return root;
  }

  // --- what the pane is -----------------------------------------------------------

  // How section ID stands, in a few words, from what the list read: [words, tone],
  // the tone "warn" or "danger" when it wants the reader.
  function glance(id) {
    const got = (v) => v && !v.error;
    switch (id) {
      case "gateway": return isOpen() ? [uptime(c.identity?.started_at) || "", ""] : [t("away"), "warn"];
      case "logs": return [t("what it writes"), ""];
      case "settings": return got(c.model) ? [c.model.model || t("no default model"), c.model.refusal || !c.model.model ? "warn" : ""] : ["", ""];
      case "cells": {
        if (!got(c.cells)) return ["", ""];
        const refused = c.cells.cells.filter((r) => r.refused).length;
        return refused ? [t("{n} refused", { n: refused }), "danger"] : [t("{n} on", { n: c.cells.cells.filter((r) => r.on).length }), ""];
      }
      case "index": return [t("what it keeps"), ""];
      case "mcp": {
        if (!got(c.servers)) return ["", ""];
        if (!c.servers.enabled) return [t("off"), ""];
        const failed = c.servers.servers.filter((s) => FAILED.includes(s.state)).length;
        return failed ? [t("{n} failing", { n: failed }), "danger"]
          : [t("{n} ready", { n: c.servers.servers.filter((s) => s.state === "ready").length }), ""];
      }
      case "profiles": return got(c.profiles) ? [plural(c.profiles.profiles.length, t("{n} profile"), t("{n} profiles")), ""] : ["", ""];
      case "channels": {
        if (!got(c.channels)) return ["", ""];
        const asking = c.channels.pairing.reduce((n, p) => n + (p.asks?.length || 0), 0);
        if (asking) return [t("{n} asking", { n: asking }), "warn"];
        const down = c.channels.channels.filter((ch) => !["running", "off", "unconfigured"].includes(ch.state)).length;
        return down ? [t("{n} not running", { n: down }), "danger"]
          : [t("{n} running", { n: c.channels.channels.filter((ch) => ch.state === "running").length }), ""];
      }
      case "link": {
        const l = c.link;
        if (!got(l)) return ["", ""];
        return l.state === "on" ? [[t("on"), plural(l.browsers?.length || 0, t("{n} browser"), t("{n} browsers"))].join(" · "), ""]
          : [STATES[l.state] || l.state, ""];
      }
      case "jobs": return got(c.jobs) ? [t("{n} active", { n: c.jobs.jobs.filter((j) => j.state === "active").length }), ""] : ["", ""];
    }
    return ["", ""];
  }

  // What wants the reader, each with the one control that answers it: a backup
  // never made, a default model refused, a cell, server or channel that will
  // not run, someone asking to be let in.
  function needsYou() {
    const rows = [];
    // Each is the kit's banner: needs you, or failed when something will not run.
    const need = (text, detail, word, data, tone = "need") => {
      const go = button("quiet", word, data);
      go.disabled = !!c.busy;
      rows.push(banner(tone, text, detail, go));
    };
    const b = c.backups;
    if (b && !b.error && !b.backups.length) {
      const job = b.job?.verb === "backup" ? b.job : null;
      need(t("No backup yet."), job?.state === "running" ? t("Backing up: {step}… {seconds} s", { step: job.step, seconds: job.seconds })
        : job?.state === "failed" ? jobFailed(job) : t("One file holds this home whole, its keys too."),
        job?.state === "running" ? t("Backing up…") : t("Back up now"), { act: "backup" });
      if (job?.state === "running") rows.at(-1).querySelector("button").disabled = true;
    }
    if (c.model && !c.model.error && c.model.refusal) need(t("The default model is refused"), c.model.refusal, t("Open Config"), { act: "tab", tab: "settings" }, "fail");
    for (const r of (c.cells && !c.cells.error ? c.cells.cells : []).filter((each) => each.refused)) {
      need(t("{name} refused to start: {why}", { name: r.label || r.name, why: r.refused }), "", t("Open"), { act: "tab", tab: "cells" }, "fail");
    }
    for (const s of (c.servers && !c.servers.error && c.servers.enabled ? c.servers.servers : []).filter((each) => FAILED.includes(each.state))) {
      need(t("MCP server {name}: {state}", { name: s.name, state: STATES[s.state] || s.state }), "", t("Open"), { act: "tab", tab: "mcp" }, "fail");
    }
    if (c.channels && !c.channels.error) {
      for (const ch of c.channels.channels.filter((each) => !["running", "off", "unconfigured"].includes(each.state))) {
        need(t("Channel {name}: {state}", { name: ch.id, state: STATES[ch.state] || ch.state }), "", t("Open"), { act: "tab", tab: "channels" }, "fail");
      }
      for (const p of c.channels.pairing.filter((each) => each.asks?.length)) {
        need(t("{n} asking to be let in on {name}", { n: p.asks.length, name: p.id }), "", t("Open"), { act: "tab", tab: "channels" });
      }
    }
    return rows;
  }

  // The phone's list: what wants the reader first, then every section under its
  // group, a finger tall, saying how it stands.
  function drawHome() {
    const root = el("div", "ctl-tab ctl-home");
    const up = isOpen() && uptime(c.identity?.started_at);
    root.append(headline([t("This machine"), c.identity?.version?.split("+")[0]].filter(Boolean).join(" · "),
      !isOpen() ? t("Reconnecting…") : up ? t("Running, {uptime}.", { uptime: up }) : t("Running.")));
    const group = (label, rows, cls) => {
      const node = el("section", `ctl-group${cls ? ` ${cls}` : ""}`);
      const list = el("ul", "ctl-group-list");
      list.append(...rows);
      node.append(el("h3", "ctl-group-head", label), list);
      root.append(node);
    };
    const needs = needsYou();
    root.append(...needs);
    for (const [label, ids] of GROUPS) {
      group(label, ids.map((id) => {
        const li = el("li");
        const go = button("ctl-go", "", { act: "tab", tab: id });
        const [words, tone] = glance(id);
        const marked = el("span", "ctl-go-mark");
        marked.append(icon(MARKS[id], 17));
        const state = el("span", "ctl-go-state", words);
        if (tone) state.dataset.tone = tone;
        go.append(marked, el("span", "ctl-go-name", LABELS[id]), state, icon("chevron", 14));
        li.append(go);
        return li;
      }));
    }
    return root;
  }

  // --- a phone's sheets -----------------------------------------------------------
  // The phone standard: on a
  // phone a row says one thing and keeps one thing on its right -- its switch,
  // or its one word -- and a tap on it raises its details and every other action
  // in a sheet; a form standing in a section is a row that raises it; what the
  // hand opened -- a form, a question, a section's settings -- rises from the
  // bottom over the page, the scrim behind it the way out; a long list searches
  // first, filters by state with its counts, and shows fifty rows at a time.
  // One pass over what a tab drew, after it drew it, so every tab, a module's
  // too, follows it alike; a wide window keeps every tab as it is drawn.
  // CLOSER is what closes the risen panel as its own controls would.
  let closer = null;

  // An act by everything its control carries: the same act twice is offered once.
  const actKey = (node) => JSON.stringify({ ...node.dataset });

  // Each row of SCOPE that carries more than its one thing becomes a tap that
  // raises them (its one act, when that is all it has); a row turned once is left.
  function tapRows(scope) {
    for (const row of scope.querySelectorAll(".ctl-row:not(.ctl-tap)")) {
      const acts = row.querySelector(":scope > .ctl-acts");
      const link = row.querySelector(":scope > .ctl-name .cell-link[data-act]");
      const marks = [...acts?.children || []].filter((b) => b.matches("button[data-act], a[href]") && !b.classList.contains("pill"));
      // One word and nothing else keeps its place on the row: "Add key", "Add".
      const kept = !link && marks.length === 1 && !marks[0].classList.contains("icon") && !acts.querySelector(".pill") && marks[0];
      const moving = marks.filter((b) => b !== kept);
      // A link in the row's words is too small to aim at: it is an act of the sheet.
      const words = [...row.querySelectorAll(":scope > :not(.ctl-acts, .ctl-name) a[href]")];
      const all = [...link && !moving.some((b) => actKey(b) === actKey(link)) ? [link] : [], ...moving, ...words];
      if (!all.length) continue;
      // A line under the row that is the sheet's (a terminal's command) stands in it;
      // one the row cuts to a line (a job's last run) stands there too, whole.
      for (let next = row.nextElementSibling; next && !next.classList.contains("ctl-row");) {
        const after = next.nextElementSibling;
        if (next.classList.contains("ctl-sheet-part") || next.classList.contains("ctl-sheet-also")) {
          const part = el("div", next.className);
          part.append(...(next.classList.contains("ctl-sheet-part") ? next.childNodes : next.cloneNode(true).childNodes));
          if (next.classList.contains("ctl-sheet-part")) next.remove();
          row.append(part);
        }
        next = after;
      }
      const name = row.querySelector(".ctl-name").textContent.trim();
      if (link) link.replaceWith(link.textContent);
      for (const b of [...moving, ...words]) b.remove();
      row.classList.add("ctl-tap");
      row.tabIndex = 0;
      row.setAttribute("role", "button");
      // A row's one act is its tap when it opens the thing or asks a question; one that
      // does something at once (a restart, a run) waits in the sheet for the second tap.
      if (all.length === 1 && all[0].dataset.act && (all[0].classList.contains("cell-link") || /(^|-)(ask|open)(-|$)/.test(all[0].dataset.act))) {
        Object.assign(row.dataset, all[0].dataset);
        row.setAttribute("aria-label", `${all[0].classList.contains("cell-link") ? t("Open") : all[0].getAttribute("aria-label") || all[0].textContent.trim()}: ${name}`);
      } else {
        Object.assign(row.dataset, { act: "sheet", sheet: `${c.tab}:${name}` });
        row.sheetActs = all;
      }
      if (!acts?.querySelector(".pill, button, a")) (acts || row.appendChild(el("span", "ctl-acts"))).append(icon("chevron", 18));
    }
  }

  // A row's sheet: its name, what the row says of it whole, and every act it had, a word each.
  function rowSheet(row) {
    const box = el("div", "ctl-rises ctl-sheet-row");
    box.append(el("h4", "ctl-sheet-title", row.querySelector(".ctl-name").textContent.trim()));
    for (const part of row.querySelectorAll(":scope > :not(.ctl-name, .ctl-acts)")) box.append(part.cloneNode(true));
    const list = el("div", "ctl-sheet-acts");
    for (const b of row.sheetActs) {
      const go = b.tagName === "A" ? b.cloneNode(true) : button("quiet", "", { ...b.dataset });
      go.classList.add("quiet", "ctl-sheet-act");
      if (b.tagName === "A") go.textContent = `${go.textContent[0].toUpperCase()}${go.textContent.slice(1)}`;
      if (b.classList.contains("drop")) go.classList.add("danger");
      if (b.tagName !== "A") {
        const drawn = b.querySelector("svg");
        if (drawn) go.append(drawn.cloneNode(true));
        go.append(el("span", "", b.classList.contains("cell-link") ? t("Open") : b.getAttribute("aria-label") || b.textContent.trim()));
        go.disabled = b.disabled;
      }
      list.append(go);
    }
    list.append(button("word ctl-sheet-cancel", t("Cancel"), { act: "sheet-close" }));
    box.append(list);
    return box;
  }

  // What rises on a phone: a question, a list's opened row, a panel the hand
  // opened, a section's settings opened from their row.
  const RISES = ".confirm, .ctl-inset, .ctl-rises, .ctl-cfg-section > .ctl-cfg-body";
  const PAGE = 50;
  const SEGS = { active: t("Active"), paused: t("Paused"), done: t("Done"), on: t("On"), off: t("Off") };

  function rise(root) {
    closer = null;
    const tab = root.querySelector(".ctl-tab");
    if (!tab) return;
    tapRows(tab);
    const tapped = c.sheet && tab.querySelector(`.ctl-tap[data-sheet="${CSS.escape(c.sheet)}"]`);
    let raised = tapped?.sheetActs ? rowSheet(tapped) : null;
    // A form standing in a section, with more than one thing to fill, is a row that raises it.
    for (const f of tab.querySelectorAll("form.ctl-form")) {
      if (f.closest(RISES) || f.querySelectorAll("input:not([type=hidden]), textarea, select").length < 2) continue;
      const block = f.closest(".ctl-block");
      const whole = block && !block.querySelector(".ctl-list, .ctl-row");
      const words = whole ? block.querySelector(".obs-head h3").textContent : f.querySelector("[type=submit]")?.textContent.trim() || t("Open");
      const key = `${c.tab}:form:${words}`;
      const go = button("ctl-go ctl-raise", "", { act: "sheet", sheet: key });
      go.append(el("span", "ctl-go-name", words), icon("chevron", 18));
      const at = el("ul", "ctl-group-list ctl-raises");
      at.append(el("li"));
      at.firstChild.append(go);
      let part = block;
      if (!whole) {
        part = el("div", "ctl-rises-form");
        const refused = f.nextElementSibling?.classList.contains("ctl-form-refused") && f.nextElementSibling;
        f.replaceWith(part);
        part.append(f, ...refused ? [refused] : []);
      }
      part.replaceWith(at);
      if (c.sheet === key && !raised) raised = part;
    }
    if (!raised) {
      const open = [...tab.querySelectorAll(RISES)].filter((n) => !n.parentElement.closest(RISES));
      raised = open.find((n) => n.matches(".confirm") || n.querySelector(".confirm")) || open[0] || null;
      for (const n of open) if (n !== raised) n.remove();
      if (raised) {
        const head = raised.matches(".ctl-cfg-body") && raised.parentElement.querySelector(":scope > .ctl-cfg-head");
        if (head) raised.prepend(el("h4", "ctl-sheet-title", head.querySelector(".ctl-cfg-title").textContent));
        closer = raised.querySelector('[data-act="cancel"], [data-act$="-close"], [data-act$="-cancel"]') || head
          || tab.querySelector('[data-act][aria-expanded="true"]:not(.info)');
        raised.remove();
      }
    }
    for (const list of tab.querySelectorAll("ol.ctl-list")) lists(list);
    if (!raised) return;
    const sheet = el("div", "ctl-sheet");
    sheet.setAttribute("role", "dialog");
    sheet.setAttribute("aria-modal", "true");
    const scrim = button("ctl-scrim", "", { act: "sheet-close" });
    scrim.setAttribute("aria-label", t("Close"));
    sheet.append(el("div", "ctl-grab"), raised);
    root.append(scrim, sheet);
  }

  // A list's rows, each with the lines under it, up to the next row.
  function groups(list) {
    const out = [];
    for (const li of list.children) {
      if (li.classList.contains("ctl-row")) out.push([li]);
      else if (out.length && !li.classList.contains("ctl-index-cell")) out.at(-1).push(li);
    }
    return out;
  }

  // A long list, or one whose rows say a state, searches first, filters by
  // state with each state's count, and shows fifty rows at a time.
  function lists(list) {
    const rows = groups(list);
    const segs = new Set(rows.map(([r]) => r.dataset.seg).filter(Boolean));
    const block = list.closest(".ctl-block");
    const own = block?.querySelector("input[type=search]");
    if (own || rows.length < 15 && !segs.size) return;
    const key = `${c.tab}:${block?.querySelector(".obs-head h3")?.textContent || ""}`;
    const s = c.lists[key] ||= { find: "", seg: "all", shown: PAGE };
    list.dataset.list = key;
    const bar = el("div", "ctl-find");
    if (rows.length >= 15) {
      const find = el("input", "control");
      Object.assign(find, { type: "search", placeholder: t("Search"), value: s.find });
      Object.assign(find.dataset, { input: "find", list: key });
      find.setAttribute("aria-label", t("Search"));
      bar.append(find);
    }
    if (segs.size) {
      const scope = el("div", "scope ctl-segs");
      scope.setAttribute("role", "radiogroup");
      for (const id of ["all", ...Object.keys(SEGS).filter((k) => segs.has(k))]) {
        const n = id === "all" ? rows.length : rows.filter(([r]) => r.dataset.seg === id).length;
        const sw = button("sw", id === "all" ? t("All") : SEGS[id], { act: "list-seg", list: key, seg: id });
        sw.append(el("span", "n", ` ${n}`));
        sw.setAttribute("role", "radio");
        sw.setAttribute("aria-checked", String(s.seg === id));
        scope.append(sw);
      }
      bar.append(scope);
    }
    list.before(bar);
    list.after(button("quiet ctl-more", "", { act: "list-more", list: key }));
    sift(list);
  }

  // The rows a list's search and state leave, the first SHOWN of them.
  function sift(list) {
    const s = c.lists[list.dataset.list];
    const needle = s.find.trim().toLowerCase();
    let hits = 0;
    for (const group of groups(list)) {
      const [row] = group;
      const hit = (s.seg === "all" || row.dataset.seg === s.seg) && (!needle || row.textContent.toLowerCase().includes(needle));
      if (hit) hits++;
      for (const li of group) li.hidden = !hit || hits > s.shown;
    }
    const more = list.nextElementSibling;
    if (!more?.classList.contains("ctl-more")) return;
    more.hidden = hits <= s.shown;
    more.textContent = t("Show {n} more", { n: Math.min(PAGE, hits - s.shown) });
  }

  function draw() {
    const root = el("div", "observe-usage control-pane");
    const phone = PHONE.matches;
    root.classList.toggle("ctl-phone", phone);
    // A move between the list and a section slides once; a poll's redraw stands still.
    if (phone && c.motion) root.dataset.motion = c.motion;
    c.motion = null;
    const bar = el("div", "dash-bar");
    // The strip names the place (Control, or Control / its tab, the way back on a
    // phone); a wide window adds the tabs here, a phone the list of its sections.
    if (!phone && !WIDE.matches) {
      const tabs = el("div", "scope ctl-tabs");
      tabs.setAttribute("role", "tablist");
      for (const [id, label] of TABS) {
        const sw = button("sw", label, { act: "tab", tab: id });
        sw.setAttribute("role", "tab");
        sw.setAttribute("aria-checked", String(id === c.tab));
        tabs.append(sw);
      }
      bar.append(tabs);
    }
    // On the bar, so it stays in sight wherever the pane is scrolled: why the
    // link is down when this pane took it down; the link bar says the rest.
    if (!isOpen() && c.leaving) {
      const away = el("p", "ctl-away");
      away.append(el("i", "led off"), " ", c.leaving === "restart" ? t("Restarting. This page reconnects when the gateway is back.")
        : t("Stopped. This page reconnects once it runs again."));
      bar.append(away);
    }
    // A phone's bar holds only that line: with nothing to say it takes no room.
    if (bar.childElementCount) root.append(bar);
    if (atHome()) {
      root.append(drawHome());
      return root;
    }
    const draws = { gateway: drawGateway, logs: drawLogs, settings: drawSettings, cells: drawCells, index: index.draw, mcp: mcp.draw, profiles: profs.draw, channels: drawChannels, link: access.draw, jobs: drawJobs };
    root.append(draws[c.tab]());
    kept(root);
    if (phone) rise(root);
    if (c.tab === "logs") queueMicrotask(drawLog);
    return root;
  }

  async function lifecycle(verb) {
    await act(verb, async () => {
      const answer = await api("POST", `/api/gateway/${verb}`);
      c.leaving = verb;
      toast(verb === "stop" ? t("Stopping: {text}", { text: answer.text }) : t("Restarting: {text}", { text: answer.text }));
    });
  }

  async function jobOp(op, fields, form) {
    const query = new URLSearchParams({ op, ...fields });
    await act(op === "add" ? "add" : `job:${fields.id}`, async () => {
      c.jobs = await api("POST", `/api/cron/jobs?${query}`);
      if (c.jobs.text) toast(c.jobs.text);
    }, form);
  }

  // A switch answers the listing, then the tab is read again.
  async function switchCell(name, on) {
    await act(`switch:${name}`, async () => {
      c.cells = await api("POST", "/api/gateway/cells", { name, on });
      toast(c.cells.text);
      await load();
    });
  }

  function click(event) {
    const target = event.target.closest("[data-act]");
    if (!target || target.disabled || target.tagName === "FORM") return;
    const d = target.dataset;
    switch (d.act) {
      case "sheet": c.sheet = d.sheet; redraw(); return;
      // The scrim is the way out: what the sheet holds closes as its own Cancel closes it.
      case "sheet-close": if (closer?.isConnected) closer.click(); else { c.sheet = null; redraw(); } return;
      case "list-seg": case "list-more": {
        const s = c.lists[d.list];
        if (d.act === "list-seg") {
          Object.assign(s, { seg: d.seg, shown: PAGE });
          for (const b of target.parentElement.children) b.setAttribute("aria-checked", String(b === target));
        } else s.shown += PAGE;
        const list = document.querySelector(`.control-pane ol[data-list="${CSS.escape(d.list)}"]`);
        if (list) sift(list);
        return;
      }
    }
    // Whatever a sheet offered, it leaves the sheet as it acts.
    c.sheet = null;
    if (cfgs.click(d) || index.click(d) || mcp.click(d) || profs.click(d) || access.click(d)) return;
    switch (d.act) {
      // A health finding about a config section opens Settings on that section's form.
      case "tab": {
        const from = atHome();
        if (from) c.homeAt = scroller()?.scrollTop || 0;
        turnTo(d.tab);
        if (d.section) cfgs.reveal(d.section);
        if (from) c.motion = "in";
        redraw();
        if (from && scroller()) scroller().scrollTop = 0;
        load().then(() => { if (d.section) document.getElementById(cfgs.anchor(d.section))?.scrollIntoView({ block: "start" }); });
        schedule();
        onTab(c.tab);
        return;
      }
      case "setup": c.setup = c.setup === d.id ? null : d.id; redraw(); return;
      // Opened and closed in place: a redraw would take the hand's focus off the mark.
      case "about": {
        const open = !c.about.delete(d.id);
        if (open) c.about.add(d.id);
        target.setAttribute("aria-expanded", String(open));
        document.getElementById(target.getAttribute("aria-controls")).hidden = !open;
        return;
      }
      case "cancel":
        for (const key of [`save-key:${c.editing}`, `restore:${c.restoring}`]) { delete c.drafts[key]; delete c.refused[key]; }
        c.confirm = null; c.editing = null; c.restoring = null; redraw(); return;
      case "reread": load(); return;
      case "check": case "update":
        act(d.act, async () => {
          try {
            c.said = { text: (await api("POST", "/api/gateway/update", d.act === "check" ? { check: true } : {})).text };
          } catch (error) {
            c.said = { text: error.message, bad: true };
          }
          await load();
        });
        return;
      // Asked from a health finding too, whose row sits below the question.
      case "ask": c.confirm = d.verb; redraw(); document.querySelector(".control-pane .confirm")?.scrollIntoView({ block: "nearest" }); return;
      case "restart": case "stop": lifecycle(d.act); return;
      case "doctor":
        act("doctor", async () => {
          try {
            c.doctor = { ...(await api("POST", "/api/gateway/doctor")), at: new Date().toISOString() };
          } catch (error) {
            c.doctor = { error: t("Could not check: {error}", { error: error.message }) };
          }
        });
        return;
      case "backup":
        act("backup", async () => { c.backups = await api("POST", "/api/gateway/backups"); watchJob(); });
        return;
      case "get-backup":
        act(`get:${d.name}`, async () => { await download(`/api/gateway/backups/file?name=${encodeURIComponent(d.name)}`, d.name); toast(t("Saved {name}", { name: d.name })); });
        return;
      case "ask-restore": c.restoring = d.name; c.confirm = null; redraw(); document.querySelector(".ctl-form input[name=profile]")?.focus(); return;
      case "ask-drop": c.confirm = `drop:${d.name}`; c.restoring = null; redraw(); return;
      case "drop-backup":
        act("drop", async () => { c.backups = await api("DELETE", `/api/gateway/backups?name=${encodeURIComponent(d.name)}`); toast(t("The backup is deleted.")); });
        return;
      case "level": c.log.level = d.level; c.log.shown = LOG_PAGE; redraw(); return;
      case "log-earlier": c.log.shown += LOG_PAGE; drawLog(); return;
      case "follow": c.log.follow = !c.log.follow; redraw(); schedule(); if (c.log.follow) readLog(false); return;
      case "edit-key": c.editing = d.id; c.confirm = null; redraw(); document.querySelector(".ctl-form input[name=key]")?.focus(); return;
      case "remove-key": c.confirm = `key:${d.id}`; c.editing = null; redraw(); return;
      case "add-key": c.editing = "+"; c.confirm = null; redraw(); document.querySelector(".ctl-keys-block input[name=provider]")?.focus(); return;
      case "forget-provider": c.confirm = `forget:${d.id}`; c.editing = null; redraw(); return;
      // Forget takes a flagged entry out of config.jsonc, named as the listing says it.
      case "drop-provider":
        act("key", async () => {
          c.keys = await api("POST", "/api/gateway/keys", { provider: d.id, forget: true });
          await cfgs.load();
          toast(t("{name} is out of config.jsonc.", { name: d.name }));
        });
        return;
      case "drop-key":
        act("key", async () => { c.keys = await api("POST", "/api/gateway/keys", { provider: d.id, key: null }); toast(t("The key for {provider} is removed.", { provider: d.id })); });
        return;
      // A start refused again says so, and Settings reads the section's standing again.
      case "restart-cell":
        act(`cell:${d.name}`, async () => {
          c.cells = await api("POST", "/api/gateway/cells", { name: d.name });
          const refused = c.cells.cells.find((r) => r.name === d.name)?.refused;
          if (refused) toast(t("{name} refused to start: {why}", { name: d.name, why: refused }), { error: true });
          else toast(t("{name} started again.", { name: d.name }));
          if (c.tab === "settings") await cfgs.load();
        });
        return;
      case "restart-channels":
        act("channels", async () => { c.channels = await api("POST", "/api/channels?op=restart"); });
        return;
      case "probe":
        act(`probe:${d.id}`, async () => { c.probed[d.id] = (await api("POST", `/api/channels?op=probe&id=${encodeURIComponent(d.id)}`)).text; });
        return;
      case "switch":
        if (d.on === "true" && !d.hub) switchCell(d.name, true);
        else { c.confirm = `switch:${d.name}`; redraw(); }
        return;
      case "switch-go": switchCell(d.name, d.on === "true"); return;
      case "hub": c.hub = null; readHub(); redraw(); return;
      case "ask-job": c.confirm = `job:${d.id}`; redraw(); return;
      case "job": jobOp(d.op, { id: d.id }); return;
      case "open": openSession(d.id); return;
    }
  }

  function input(event) {
    if (event.target.dataset.input === "keys-find") {
      c.keysFind = event.target.value;
      const scope = event.target.closest(".ctl-keys-block");
      if (scope) siftKeys(scope);
      return;
    }
    if (event.target.dataset.input === "find") {
      const s = c.lists[event.target.dataset.list];
      Object.assign(s, { find: event.target.value, shown: PAGE });
      const list = document.querySelector(`.control-pane ol[data-list="${CSS.escape(event.target.dataset.list)}"]`);
      if (list) sift(list);
      return;
    }
    if (cfgs.input(event) || index.input(event) || mcp.input(event) || profs.input(event)) return;
    // A form no tab keeps itself is kept here, so no redraw takes back what the hand typed.
    const owner = event.target.name && event.target.closest?.("form.ctl-form");
    if (owner) (c.drafts[formKey(owner)] ||= {})[event.target.name] = event.target.value;
    const kind = event.target.dataset.input;
    if (kind === "filter") { c.log.filter = event.target.value; drawLog(); }
    if (kind === "model" && event.type === "change") {
      const [provider, model] = event.target.value.split("\t");
      act("model", async () => { c.model = await api("POST", "/api/gateway/model", { provider, model }); toast(t("The default model is {model} on {provider}.", { model, provider })); });
    }
  }

  function submit(event) {
    const node = event.target.closest("form[data-act]");
    if (!node) return;
    event.preventDefault();
    const values = Object.fromEntries(new FormData(node));
    if (cfgs.submit(node, values) || index.submit(node, values) || mcp.submit(node, values, event.submitter)
        || profs.submit(node) || access.submit(node, values)) return;
    if (node.dataset.act === "save-key") {
      const provider = node.dataset.id || values.provider.trim();
      act("key", async () => {
        c.keys = await api("POST", "/api/gateway/keys", { provider, key: values.key });
        c.editing = null;
        // A key nothing answered may still be right: it is saved, and the toast says it was not checked.
        toast(c.keys.unchecked
          ? t("The key for {provider} is saved, not checked: {why}", { provider, why: c.keys.unchecked })
          : t("The key for {provider} is saved.", { provider }));
      }, formKey(node));
    } else if (node.dataset.act === "restore") {
      act("restore", async () => {
        c.backups = await api("POST", "/api/gateway/backups/restore", { name: node.dataset.name, profile: values.profile.trim() });
        c.restoring = null;
        watchJob();
      }, formKey(node));
    } else if (node.dataset.act === "add-job") {
      const fields = Object.fromEntries(Object.entries(values).filter(([, value]) => value.trim()));
      jobOp("add", fields, formKey(node));
    }
  }

  // The pane on TAB; a question or an edit left open on the last one is let go.
  function turnTo(tab) {
    c.tab = tab;
    c.home = false;
    c.confirm = null;
    c.editing = null;
  }

  // A phone's way back: the list, where it was scrolled when a section was opened.
  function toHome() {
    c.home = true;
    c.confirm = null;
    c.editing = null;
  }

  // A wide window and a phone draw the pane apart: a turn of the phone, or a
  // window drawn narrow, draws it again as the width now asks.
  for (const width of [PHONE, WIDE]) width.addEventListener("change", () => {
    if (!shown) return;
    redraw();
    load();
    schedule();
    onTab(c.tab);
  });

  return {
    draw, click, input, submit,
    // The wide window's list: each group's label and its sections, each with its
    // mark, its name, how it stands ([words, tone], glance) and whether it is open.
    places() {
      return GROUPS.map(([label, ids]) => [label, ids.map((id) => {
        const [words, tone] = glance(id);
        return { id, label: LABELS[id], mark: MARKS[id], words, tone, open: id === c.tab };
      })]);
    },
    // NULL for a phone's list, so its address names no tab.
    get tab() { return atHome() ? null : c.tab; },
    // The open tab's name for the strip's way back, NULL on a phone's list.
    get label() { return atHome() ? null : LABELS[c.tab]; },
    // The strip's way back, on a phone: the list, where it was scrolled.
    back() {
      if (atHome() || !PHONE.matches) return;
      toHome();
      c.motion = "back";
      redraw();
      if (scroller()) scroller().scrollTop = c.homeAt;
      load();
      schedule();
      onTab(null);
    },
    // Whether show(TAB) moves the pane: to another tab, or, on a phone, to the
    // list from a section, when the address names no tab.
    moves(tab) { return tab ? tab !== (atHome() ? null : c.tab) : PHONE.matches && !c.home; },
    // The link as last read, read again, its panel by the gateway line, and a
    // browser asking, for the page's notices to hold.
    link: { get now() { return c.link; }, read: access.load, panel: access.panel, askNotice: access.askNotice },
    // Opened on TAB when it names one of the pane's tabs; a name it does not
    // have (an address from another build) leaves it on the tab it was on, and says so.
    show(tab) {
      const was = atHome() ? null : c.tab;
      if (tab && (tab !== c.tab || atHome())) {
        if (TABS.some(([id]) => id === tab)) turnTo(tab);
        else {
          c.home = false;
          toast(t("Settings has no {tab} tab, so it opens on {label}.", { tab, label: LABELS[c.tab] }));
        }
      } else if (!tab && PHONE.matches && !c.home) toHome();
      // Back and Forward between the list and a section, on a phone: a section
      // opens at its top, the list where it was left.
      const now = atHome() ? null : c.tab;
      if (shown && now !== was) {
        if (was === null) c.homeAt = scroller()?.scrollTop || 0;
        queueMicrotask(() => { if (scroller()) scroller().scrollTop = now ? 0 : c.homeAt; });
      }
      shown = true;
      load();
      schedule();
    },
    hide() { shown = false; c.confirm = null; schedule(); },
    reconnected() { c.leaving = null; if (shown) { load(); schedule(); } },
  };
}
