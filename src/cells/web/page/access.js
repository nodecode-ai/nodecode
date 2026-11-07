// access.js -- who is let in, on the Control pane: the people a channel's bot
// paired (the Channels tab's Pairing block, over the channel kit's own
// /api/channels) and the browsers the link allowed (the Link tab, over the
// link cell's own /api/link). Every verb is one a shell runs -- /channels
// pair and unpair, /link on, off, allow, deny and remove -- called through
// the cell's route: nothing here decides who may come in. The pairing page
// is never sent a code; it approves an ask by the id the kit gave it, as
// Hermes' Pairing page does (web/src/pages/PairingPage.tsx: pending asks with
// Approve, the approved with a confirmed revoke). The Link tab draws the
// address's QR code from the rows the cell encodes (qr.lisp), the same code
// the /link panel paints, and reads the link again while it is open, so a
// phone that scanned the code and asked is here to allow with no reload. The
// gateway line's link mark opens the tab's first acts in a panel of its own
// (panel), the same verbs over the same reading, and a browser asking while
// neither is on screen is held in a notice of its own (askNotice) until it is
// answered or its two minutes are out. A link whose dials keep failing says
// why in the cell's words, when it tries next, and offers Try now.

import { el, button, mark, waiting, when, headline, dot } from "./observe.js";
import { block, note, form, formKey, field, item, confirmBox, about, pill, PHONE } from "./control.js";
import { t } from "./i18n.js";

const STATES = {
  on: [t("On."), t("This machine's page answers at its address, for every browser you allowed.")],
  connecting: [t("Connecting."), t("It is on, and the relay has not answered yet; this machine keeps trying.")],
  failing: [t("Failing."), t("It is on, and the relay cannot be reached; this machine keeps trying.")],
  off: [t("Off."), t("Nothing reaches this machine through the relay.")],
};

const named = (id) => `${id[0].toUpperCase()}${id.slice(1)}`;

// The address as a QR code: one path, a rectangle to each run of dark modules.
// The slash reply draws a command's picture with it too (app.js slashPanel).
export function qrCode(rows) {
  const ns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(ns, "svg");
  svg.setAttribute("viewBox", `0 0 ${rows[0].length} ${rows.length}`);
  svg.setAttribute("class", "ctl-link-qr");
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", t("The address as a QR code"));
  svg.setAttribute("shape-rendering", "crispEdges");
  let d = "";
  rows.forEach((row, y) => {
    for (const run of row.matchAll(/1+/g)) d += `M${run.index} ${y}h${run[0].length}v1h-${run[0].length}z`;
  });
  const path = document.createElementNS(ns, "path");
  path.setAttribute("d", d);
  svg.append(path);
  return svg;
}

// What a re-read must differ in to be drawn again: not the ask's clock nor the
// next try's, which count down where they are shown, nor the last verb's words.
const shape = (l) => JSON.stringify({ ...l, text: null, ask: l.ask && { ...l.ask, seconds_left: 0 },
  failure: l.failure && { ...l.failure, next_try_seconds: 0 } });

function ago(seconds) {
  if (seconds < 60) return t("just now");
  return seconds < 3600 ? t("{n} min ago", { n: Math.floor(seconds / 60) }) : t("{n} h ago", { n: Math.floor(seconds / 3600) });
}

// OPENTAB opens Control on a tab, from the panel.
export function makeAccess({ c, api, act, toast, redraw, unread, missing, openTab }) {
  let readAt = 0;
  let ticker = null;
  let poller = null;

  async function load() {
    try { c.link = await api("GET", "/api/link"); readAt = Date.now(); } catch (error) { c.link = { error: error.message, missing: error.status === 404 }; }
  }

  const since = () => (Date.now() - readAt) / 1000;
  const left = () => Math.max(0, Math.round(c.link.ask.seconds_left - since()));
  const nextTry = () => Math.max(0, Math.round(c.link.failure.next_try_seconds - since()));
  const nextText = () => (nextTry() ? t("next try in {n} s", { n: nextTry() }) : t("trying now"));

  // The tab or the panel, where one is on screen.
  const seen = () => ["ctl-link", "ctl-link-panel"].some((id) => document.getElementById(id)?.checkVisibility());

  // While a browser asks its seconds count down where they are shown, and so
  // does a failing link's next try; at none left the ask is read again, and gone.
  function watchClock() {
    if (ticker || !(c.link?.ask || c.link?.failure)) return;
    ticker = setInterval(() => {
      const asks = document.querySelectorAll(".ctl-link-left");
      const tries = document.querySelectorAll(".ctl-link-next");
      if ((!asks.length || !c.link?.ask) && (!tries.length || !c.link?.failure)) { clearInterval(ticker); ticker = null; return; }
      if (c.link.failure) for (const node of tries) node.textContent = nextText();
      if (!c.link.ask || !asks.length) return;
      const n = left();
      for (const node of asks) node.textContent = t("{n} s left", { n });
      if (!n) { clearInterval(ticker); ticker = null; load().then(redraw); }
    }, 1000);
  }

  // While the tab or the panel is open and the link on, or a browser asks, it
  // is read again every few seconds and drawn again when it changed: a browser
  // that asks shows up to allow, and one answered at a shell or gone leaves.
  // A verb under way or a question open waits for the next read.
  function watchLink() {
    if (poller || !c.link || c.link.error || c.link.state === "off") return;
    poller = setInterval(async () => {
      if (!(seen() || c.link?.ask) || !c.link || c.link.error || c.link.state === "off") {
        clearInterval(poller);
        poller = null;
        return;
      }
      if (c.busy || c.confirm) return;
      const before = shape(c.link);
      await load();
      if (shape(c.link) !== before) redraw();
    }, 2500);
  }

  // --- pairing, in the Channels tab ---------------------------------------------------

  function pairing(ch) {
    const hosts = ch.pairing || [];
    const asks = hosts.flatMap((h) => h.asks.map((a) => ({ ...a, host: h.id })));
    const paired = hosts.flatMap((h) => h.paired.map((p) => ({ ...p, host: h.id })));
    const reading = el("span", "reading", hosts.length ? t("{asking} asking · {paired} let in", { asking: asks.length, paired: paired.length }) : "");
    if (!hosts.length) {
      const box = block(t("Pairing"), reading);
      box.append(note(t("No channel is running, so nobody can ask to be let in. A running bot answers a direct message from someone no allowlist names with a pairing code, and the ask waits here.")));
      return box;
    }
    const [infoMark, words] = about(c, "pairing", t("A bot answers a direct message from someone no allowlist names with a pairing code, and the ask waits here. Let in admits them by the name and id the platform shows; only the person who wrote holds the code, so when someone asks you to let them in, have them read you theirs and type it below."));
    const box = block(t("Pairing"), infoMark, reading);
    box.append(words);
    for (const h of hosts.filter((each) => !each.on)) {
      box.append(el("p", "ctl-notice", t("{platform} gives no code: pairing is false in channels.{id}, so a stranger's direct message goes unanswered.", { platform: named(h.id), id: h.id })));
    }

    box.append(el("h4", "micro ctl-pair-head", t("Asking to be let in")));
    const asking = el("ol", "ctl-list");
    for (const a of asks) {
      const li = el("li", "ctl-row");
      const name = el("span", "ctl-name");
      name.append(dot("act pulse"), ` ${a.name}`, el("span", "tag", ` ${a.user}`));
      const go = button("word", c.busy === `pair:${a.id}` ? t("Letting in…") : t("Let in"), { act: "pair-ask", ask: a.id });
      go.disabled = !!c.busy;
      const acts = el("span", "ctl-acts");
      acts.append(go);
      li.append(name, el("span", "ctl-detail", t("{platform} · asked {ago} · its code lapses in {n} min", { platform: named(a.host), ago: ago(a.age_seconds), n: Math.ceil(a.seconds_left / 60) })), acts);
      asking.append(li);
    }
    if (!asks.length) asking.append(el("li", "obs-note ctl-pair-none", t("Nobody is asking.")));
    box.append(asking);

    box.append(el("h4", "micro ctl-pair-head", t("Let in")));
    const rows = el("ol", "ctl-list");
    for (const p of paired) {
      const li = el("li", "ctl-row");
      const name = el("span", "ctl-name");
      name.append(` ${p.name}`, el("span", "tag", ` ${p.id}`));
      const acts = el("span", "ctl-acts");
      const drop = mark("trash", t("Unpair"), { act: "ask-unpair", user: p.id });
      drop.disabled = !!c.busy;
      acts.append(drop);
      li.append(name, el("span", "ctl-detail", t("{platform} · let in {when}", { platform: named(p.host), when: when(p.at) })), acts);
      rows.append(li);
      if (c.confirm === `unpair:${p.id}`) {
        rows.append(item(confirmBox(t("Unpair {name}? They can no longer talk to the {platform} bot; writing to it again gives them a new code to ask with.", { name: p.name, platform: named(p.host) }),
          t("Unpair"), "unpair", { user: p.id })));
      }
    }
    if (!paired.length) rows.append(el("li", "obs-note ctl-pair-none", t("Nobody is let in by pairing yet.")));
    box.append(rows);

    const go = button("quiet", c.busy === "pair" ? t("Letting in…") : t("Let in"), {});
    go.type = "submit";
    go.disabled = !!c.busy;
    box.append(form("pair-code", field("code", t("A code someone gave you"), {
      placeholder: "XK4P7M2Q", required: "", autocomplete: "off", spellcheck: "false", maxlength: "12",
    }), go));
    box.append(note(t("A code nobody lets in lapses an hour after the bot gave it; there is no refusing one here. The person may ask for a new code ten minutes after their last, and three can wait at once: past that, a stranger is told you have too many waiting.")));
    return box;
  }

  async function pair(label, query, form) {
    await act(label, async () => {
      c.channels = await api("POST", `/api/channels?${new URLSearchParams(query)}`);
      toast(c.channels.text);
    }, form);
  }

  // --- the Link tab -------------------------------------------------------------------

  function draw() {
    const root = el("div", "ctl-tab");
    root.id = "ctl-link";
    const l = c.link;
    if (l?.missing) { root.append(missing("link", "nodecode-link")); return root; }
    if (!l || l.error) { root.append(l?.error ? unread(t("Could not read the link: {error}", { error: l.error })) : waiting(t("Looking…"))); return root; }
    const [word, sub] = STATES[l.state] || STATES.off;
    const head = headline(t("This machine's page, from anywhere"), word, sub);
    const scope = el("div", "scope ctl-link-switch");
    scope.setAttribute("role", "radiogroup");
    scope.setAttribute("aria-label", t("The link"));
    // The chosen word does nothing; the other is the verb.
    for (const [on, label] of [[false, t("Off")], [true, t("On")]]) {
      const chosen = on === (l.state !== "off");
      const sw = button("sw", label, { act: chosen ? "" : on ? "link-on" : "ask-link-off" });
      sw.setAttribute("role", "radio");
      sw.setAttribute("aria-checked", String(chosen));
      sw.disabled = !chosen && !!c.busy;
      scope.append(sw);
    }
    if (l.failure) head.append(failing(l));
    // On a phone the link's on and off is one switch, on a row of its own.
    if (PHONE.matches) {
      const on = l.state !== "off";
      const line = el("div", "ctl-switch-row");
      const sw = pill(on, on ? t("On") : t("Off"), { act: on ? "ask-link-off" : "link-on" });
      sw.setAttribute("aria-label", t("The link"));
      sw.disabled = !!c.busy;
      line.append(el("span", "", t("The link")), sw);
      head.append(line);
    } else head.append(scope);
    if (c.confirm === "link-off") head.append(offQuestion(l));
    root.append(head);

    // A browser asking is what wants an answer now, so it stands first.
    if (l.ask) {
      const ask = block(t("A browser is asking"), countdown());
      ask.append(...asking(l));
      root.append(ask);
    }

    const [addressMark, addressAbout] = about(c, "address", t("One line this machine dials out to the relay at {relay}: nothing listens here, and the sessions, the store and the turns stay on this machine. On, the link keeps the gateway running while no shell is open.", { relay: l.relay }));
    const address = block(t("Address"), addressMark);
    address.append(addressAbout);
    const where = el("div", "ctl-link-where");
    const said = el("div", "ctl-link-said");
    said.append(l.address ? addressLine(l) : note(l.state === "off"
      ? t("The relay names this machine's address the first time the link is on.")
      : t("The relay names this machine's address once it answers.")));
    // The phone in the hand needs no code to scan: on one, the code is a row that raises it.
    if (l.qr && PHONE.matches) {
      const go = button("ctl-go", "", { act: "link-qr" });
      go.append(el("span", "ctl-go-name", t("Show the code")), el("span", "ctl-go-state", t("for another phone")));
      const rows = el("ul", "ctl-group-list ctl-raises");
      rows.append(el("li"));
      rows.firstChild.append(go);
      said.append(rows);
      if (c.showQr) {
        const sheet = el("div", "ctl-rises ctl-link-qr-sheet");
        sheet.append(el("h4", "ctl-sheet-title", t("Open on another phone")), qrCode(l.qr),
          note(t("Point a phone's camera at the code to open the address there. The phone asks to open this machine; allow its code here, or with /link allow in a shell.")),
          button("word ctl-sheet-cancel", t("Close"), { act: "link-qr-close" }));
        where.append(sheet);
      }
    } else if (l.qr) {
      where.append(qrCode(l.qr));
      said.append(note(t("Point a phone's camera at the code to open the address there. The phone asks to open this machine; allow its code here, or with /link allow in a shell.")));
    }
    where.append(said);
    address.append(where);
    root.append(address);

    const [allowedMark, allowedAbout] = about(c, "browsers", t("A browser asks from the address, shows a code, and waits two minutes; allowing it here or with /link allow in a shell lets it in for good, until it is removed."));
    const allowed = block(t("Allowed browsers"), allowedMark, el("span", "reading", t("{n} allowed", { n: l.browsers.length })));
    allowed.append(allowedAbout);
    const rows = el("ol", "ctl-list");
    for (const b of l.browsers) {
      const li = el("li", "ctl-row");
      const name = el("span", "ctl-name");
      name.append(` ${b.agent}`);
      if (b.id === l.here) name.append(el("span", "tag", ` ${t("this browser")}`));
      const acts = el("span", "ctl-acts");
      const drop = mark("trash", t("Remove"), { act: "ask-link-remove", id: b.id });
      drop.disabled = !!c.busy;
      acts.append(drop);
      li.append(name, el("span", "ctl-detail", [b.place, t("allowed {when}", { when: when(b.allowed) }), b.id].filter(Boolean).join(" · ")), acts);
      rows.append(li);
      if (c.confirm === `link-remove:${b.id}`) {
        rows.append(item(confirmBox(b.id === l.here
          ? t("Remove this browser? This page came through it, so it is cut off now, and coming back takes allowing it again at the machine.")
          : t("Remove {agent}? It can no longer open this machine, and a page it has open is cut off now.", { agent: b.agent }), t("Remove"), "link-remove", { id: b.id })));
      }
    }
    if (!l.browsers.length) {
      rows.append(el("li", "obs-note", l.state === "off"
        ? t("No browser is allowed. Turn the link on, open its address in a browser, and allow it here when it asks.")
        : l.qr
          ? t("No browser is allowed yet. Scan the code with a phone, or open the address in any browser, and allow it here when it asks.")
          : t("No browser is allowed yet. Open the address in a browser, and allow it here when it asks.")));
    }
    allowed.append(rows);
    root.append(allowed);
    queueMicrotask(watchLink);
    return root;
  }

  // --- what the tab and the panel both draw ------------------------------------------

  function offQuestion(l) {
    return confirmBox(l.here
      ? t("Turn the link off? This page came through it, so it is cut off now; turning it on again takes a browser on this machine, or /link on in a shell.")
      : t("Turn the link off? Every browser you allowed is cut off until it is on again; the list of them is kept."), t("Turn off"), "link-off");
  }

  function countdown() {
    queueMicrotask(watchClock);
    return el("span", "reading ctl-link-left", t("{n} s left", { n: left() }));
  }

  // Why the dials keep failing, in the cell's words, when the next one is,
  // and a try now.
  function failing(l) {
    queueMicrotask(watchClock);
    const box = el("div", "ctl-link-failing");
    const when = el("p", "ctl-link-when");
    const go = button("word", c.busy === "link:retry" ? t("Trying…") : t("Try now"), { act: "link-retry" });
    go.disabled = !!c.busy;
    when.append(el("span", "ctl-link-next", nextText()), go);
    box.append(el("p", "ctl-link-why", t("The last try: {why}.", { why: l.failure.why })), when);
    return box;
  }

  // The asking browser's code, who it is, and Allow and Deny.
  function asking(l) {
    const row = el("div", "ctl-actions");
    row.append(button("solid", c.busy === "link:allow" ? t("Allowing…") : t("Allow"), { act: "link-allow", code: l.ask.code }),
      button("quiet", t("Deny"), { act: "link-deny" }));
    for (const b of row.children) b.disabled = !!c.busy;
    return [el("p", "ctl-link-code", l.ask.code),
      el("p", "ctl-what", l.ask.place
        ? t("{agent} in {place} asks to open this machine. Allow it only if its page shows this same code.", { agent: l.ask.agent, place: l.ask.place })
        : t("{agent} asks to open this machine. Allow it only if its page shows this same code.", { agent: l.ask.agent })),
      row];
  }

  // BARE, in the narrow panel, says it without its https://.
  function addressLine(l, bare = false) {
    const line = el("div", "ctl-link-address");
    const link = Object.assign(el("a", "ctl-link-url", bare ? l.address.replace(/^https:\/\//, "") : l.address),
      { href: l.address, target: "_blank", rel: "noopener noreferrer" });
    line.append(link, mark("copy", t("Copy"), { act: "link-copy" }));
    return line;
  }

  // --- the panel by the gateway line --------------------------------------------------

  // The Link tab's first acts, a click from any pane: the switch, a browser
  // asking, the address and its code; the browsers allowed stay in the tab. With
  // no link cell loaded, its one act turns the cell on and the link with it.
  function panel() {
    const root = el("div", "ctl-link-panel");
    root.id = "ctl-link-panel";
    const l = c.link;
    const head = el("div", "ctl-link-panel-head");
    head.append(el("span", "micro", t("Link")));
    root.append(head);
    if (l?.missing) {
      const go = button("solid", c.busy === "link:cell" ? t("Turning on…") : t("Turn on"), { act: "link-cell" });
      go.disabled = !!c.busy;
      root.append(note(t("The link cell is off. On, it gives this machine's page an address you can open from anywhere, a phone too.")), go);
      return root;
    }
    if (!l || l.error) {
      const said = waiting(l?.error ? t("Could not read the link: {error}", { error: l.error }) : t("Looking…"));
      if (l?.error) said.append(" ", button("word", t("Retry"), { act: "link-read" }));
      root.append(said);
      return root;
    }
    const on = l.state !== "off";
    const sw = pill(on, on ? t("On") : t("Off"), { act: on ? "ask-link-off" : "link-on" });
    sw.setAttribute("aria-label", t("The link"));
    sw.disabled = !!c.busy;
    head.append(sw);
    const [word, sub] = STATES[l.state] || STATES.off;
    const state = el("p", "ctl-link-state");
    state.append(el("span", "", word), ` ${sub}`);
    root.append(state);
    if (l.failure) root.append(failing(l));
    if (c.confirm === "link-off") root.append(offQuestion(l));
    // A browser asking has the code already: its ask stands where the code was.
    if (l.ask) {
      const ask = el("div", "ctl-link-ask");
      const [code, what, row] = asking(l);
      const top = el("div", "ctl-link-panel-head");
      top.append(code, countdown());
      ask.append(top, what, row);
      root.append(ask);
    }
    if (on && l.address) {
      if (l.qr && !l.ask) root.append(qrCode(l.qr));
      root.append(addressLine(l, true));
      if (l.qr && !l.ask) root.append(note(t("Scan the code with a phone; when it asks, allow it here.")));
    } else if (on) root.append(note(t("The relay names this machine's address once it answers.")));
    const foot = el("div", "ctl-link-panel-head");
    foot.append(el("span", "reading", t("{n} allowed", { n: l.browsers.length })), button("word", t("Manage"), { act: "link-manage" }));
    root.append(foot);
    queueMicrotask(watchLink);
    return root;
  }

  // A browser asking while neither the tab nor the panel is on screen: its
  // code, its seconds, Allow and Deny, for the page's notices to hold until it
  // is answered here or at a shell, or its two minutes are out; null when
  // there is none to hold.
  function askNotice() {
    const l = c.link;
    if (!l?.ask || l.error || seen()) return null;
    const box = el("div", "ctl-link-asknote");
    const top = el("div", "ctl-link-panel-head");
    top.append(el("span", "micro", t("A browser is asking")), countdown());
    const [code, what, row] = asking(l);
    box.append(top, code, what, row);
    queueMicrotask(watchLink);
    return box;
  }

  // A verb that cuts the link this page came through never hears back: that
  // is the verb done, not failed.
  async function linkOp(op, fields = {}, cuts = false) {
    await act(`link:${op}`, async () => {
      try {
        c.link = await api("POST", `/api/link?${new URLSearchParams({ op, ...fields })}`);
        readAt = Date.now();
        toast(c.link.text);
      } catch (error) {
        if (!cuts) throw error;
        toast(t("Done, and this page, which came through the link, is cut off with it."));
      }
    });
  }

  function click(d) {
    switch (d.act) {
      case "pair-ask": pair(`pair:${d.ask}`, { op: "pair", ask: d.ask }); return true;
      case "ask-unpair": c.confirm = `unpair:${d.user}`; redraw(); return true;
      case "unpair": pair("unpair", { op: "unpair", user: d.user }); return true;
      case "link-on": linkOp("on"); return true;
      case "link-qr": c.showQr = true; redraw(); return true;
      case "link-qr-close": c.showQr = false; redraw(); return true;
      case "link-retry": linkOp("retry"); return true;
      case "ask-link-off": c.confirm = "link-off"; redraw(); return true;
      case "link-off": linkOp("off", {}, !!c.link?.here); return true;
      case "link-allow": linkOp("allow", { code: d.code }); return true;
      case "link-deny": linkOp("deny"); return true;
      case "ask-link-remove": c.confirm = `link-remove:${d.id}`; redraw(); return true;
      case "link-remove": linkOp("remove", { id: d.id }, d.id === c.link?.here); return true;
      case "link-read": load().then(redraw); return true;
      case "link-manage": openTab("link"); return true;
      // The cell loaded now (SWITCH-CELL-ON), then the link on through it.
      case "link-cell":
        act("link:cell", async () => {
          toast((await api("POST", "/api/gateway/cells", { name: "nodecode-link", on: true })).text);
          c.link = await api("POST", "/api/link?op=on");
          readAt = Date.now();
          toast(c.link.text);
        });
        return true;
      case "link-copy":
        navigator.clipboard.writeText(c.link.address).then(() => toast(t("Copied the address")),
          () => toast(t("Couldn't copy: the browser refused the clipboard"), { error: true }));
        return true;
    }
    return false;
  }

  function submit(node, values) {
    if (node.dataset.act !== "pair-code") return false;
    pair("pair", { op: "pair", code: values.code.trim() }, formKey(node));
    return true;
  }

  return { load, pairing, draw, panel, askNotice, click, submit };
}
