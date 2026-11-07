// app.js -- the organism in a tab. It speaks the gateway's own sync protocol
// to the gateway that served it: the handshake with the operator token, a
// subscription that asks for rows (the transcript folded at the gateway by the
// shell's own reducer), and start_turn / steer_turn / cancel_turn the way a
// shell sends them. It folds nothing; it draws the rows it is sent, grouped
// into the turns they belong to: the operator's line on the right, the work
// folded to one line, the answer standing under it, and the line the turn
// rested on. Checkpoints are the gateway's own (session/checkpoint.lisp): the
// listing and files routes, and navigate_checkpoint the way the shell's /undo,
// /redo and /tree send it.

import { diffHtml, markdown } from "./md.js";
import { t, translatePage } from "./i18n.js";
import { drawRun, drawInspector, drawDashboard, drawTimeline, turnSpan, meterOf, ms, usd, plural, bytes, RANGES, JUMPS, INSPECTOR_TABS, ORIGIN_WORDS, PHONE } from "./observe.js";
import { makeControl, WIDE } from "./control.js";
import { qrCode } from "./access.js";
import { makeFiles, putFile } from "./files.js";
import { icon, iconButton } from "./icons.js";

const $ = (id) => document.getElementById(id);
const app = $("app");
const rowsEl = $("rows");
const ledger = $("ledger");
const promptEl = $("prompt");
const composer = $("composer");

const TOKEN_KEY = "nodecode-web-token";
const FOLD_KEY = "nodecode-web-fold";
const DRAFT_KEY = "nodecode-web-draft:";
const NOTICES_KEY = "nodecode-web-notices";
const SOURCE_KEY = "nodecode-web-source";
const SCOPE_KEY = "nodecode-web-scope";
const SEEN_KEY = "nodecode-web-seen";

const state = {
  token: null,
  ws: null,
  open: false,
  retry: 0,
  retryAt: 0, // when the socket tries again, while it is down
  retryTimer: null,
  sessionsUnread: null, // the error the last read of the sessions list failed with
  listed: false, // the sessions list has been read: the opener's folder waits for it
  sessions: [],
  sessionsById: new Map(),
  running: new Set(),
  seen: { since: null, at: {} }, // what this browser has read of each session (SEEN_KEY)
  left: null, // the session just left standing idle, seen as far as the next listing goes
  branches: new Set(), // sessions whose forks and seats the list draws under them
  kids: new Map(), // session id -> the listed sessions right under it, oldest first
  rooms: new Set(), // the sessions a channel's chat keeps, its threads and asks under them
  reveal: null, // the session whose line opens once the list knows it
  current: null,
  filter: "",
  scope: "folder", // the list: "folder" the sessions of the folder here, "all" every one; remembered
  source: null, // the column's source choice: null every source, else a kind ("" none recorded)
  doing: null, // the list's state choice: null every state, else "ask", "run" or "done"
  doneAll: false, // the Done band lists every session, not its newest DONE_ROWS
  folder: null, // the folder a new session opens in
  gatewayFolder: null, // where a session opens that names none: the gateway's own folder
  folderCheck: null, // { path, exists, why }: the gateway's word on state.folder
  cwdPick: null, // { text, answer, asking } while the opener's folder field is open
  views: new Map(), // session id -> { rows: Map, order: [], turn }
  sent: new Map(), // command id -> { text, session }
  drawn: new Map(), // turn key -> { el, sig } for the session on screen
  toggled: new Map(), // fold key -> open, where the reader chose
  rewind: null, // { session, key, turnId, number, files, busy, error } while a turn's rewind is asked
  history: null, // { session, entries, pick, busy, error } while the history sheet is open
  redo: new Set(), // sessions this tab rewound, until a turn starts or redo lands
  undone: null, // the prompt the last rewind put back in the box
  recalled: null, // how far back Up has walked this session's prompts
  images: [], // { media_type, data, url } waiting to ride the next prompt
  undonePictures: [], // the pictures the last rewind put back in the box
  files: [], // { file, name, size, status, loaded, path, why }: any other file, riding it as its path
  drafts: new Map(), // session id ("" the opener) -> { text, images, files } left there by a switch
  target: null, // GET /api/gateway/target for the session on screen (or the default)
  aim: null, // { provider, model, effort }: the opener's picks, for the session its first message opens
  models: null, // the catalog, fetched when the model sheet first opens
  found: [], // conversation search hits for the filter
  execSeen: new Map(), // execKey -> when this tab first saw it running
  stopping: new Set(), // execKeys this tab asked to interrupt, until they leave
  withdrawing: new Set(), // queued prompt ids this tab asked to take back, until the gateway answers
  shown: null,
  arriving: false,
  pane: "chat", // chat | run | files: the session on screen; dashboard: every session; control: the organism
  filesPath: "", // the Files view's place in the session's folder, relative to it ("" the folder): the address's path
  timeline: null, // { session, data, error }: the session's timeline read
  runOpen: null, // the turn number opened in the Run view
  runZoom: null, // { n, from, to }: the window of turn n's clock shown, its whole span when null
  runAll: false, // every finding shown, not the first four
  runPaged: false, // on a phone, the open turn is a page of its own (observe.js phoneTurn), not a row opened in the list
  drawer: null, // { turn, call, tab, open } while a model call is inspected; open: the request's unfolded paths
  request: null, // { key, status, bytes, text, value, shared, divergedIn, previous, error }: the inspected call's body
  usage: null, // the Dashboard's reads for state.range, each the read or { range, error }
  activity: null,
  health: null, // what is on right now, or { error }
  identity: null, // the gateway's version and start
  runWant: null, // the turn a Dashboard row asked the Run view to open
  range: "7d",
  dashSheet: null, // the section a phone's Dashboard has open in its sheet (observe.js SHEETS)
  dashEnter: false, // that sheet rises on the next draw alone, not on each read landing under it
  phoneOpen: new Set(), // what a phone's rows hold open in place: a row's +, a group's Show all (observe.js prow, pgroup)
  picking: null, // { ids, confirm } while the list is ticked to export or delete
  dropping: null, // { id, answers, busy } while one row's trash mark asks to delete its session
  prune: null, // { days, empty, answer, error, busy, failed } while the prune form is open
};

// --- the token, and where the page is ----------------------------------------------
// The token arrives after the # of the link `nodecode web` prints, which a
// browser never sends to a server; it is kept for this tab only and taken out
// of the address bar. The rest of the address says where the page is, so a
// reload or a link lands there and Back and Forward walk the places it has been:
//   #s=ID                              a session's chat
//   #s=ID&v=run&turn=N&call=K&tab=T    its Run view, turn N open, its Kth model call inspected on tab T
//   #s=ID&v=files&path=REL             its Files view on REL, relative to its folder
//   #v=dashboard&range=R               the Dashboard over R
//   #v=control&tab=NAME                the Control pane on its tab NAME
// A move to another place pushes an entry; a change that only filters a place
// -- the Dashboard's range, the inspector's tab -- replaces the one there.

function readLink() {
  const params = new URLSearchParams(location.hash.slice(1));
  const token = params.get("t");
  if (token) {
    try { sessionStorage.setItem(TOKEN_KEY, token); } catch {}
  }
  state.token = token || safeGet(sessionStorage, TOKEN_KEY);
  land(params);
}

// The panes over every session or over the organism, which name no session.
const BOARDS = ["dashboard", "control"];

// The views the strip offers a session -- its switch's Chat and Run, Files
// among the acts (under More on a phone), and any a module puts there: the
// one list of what `v' may name beside a session.
const sessionViews = () => [...document.querySelectorAll(".strip [data-view]")].map((sw) => sw.dataset.view);

// A number an address spells in digits, or null.
const whole = (text) => (/^\d+$/.test(text || "") ? Number(text) : null);

// Where the page is, as an address says it; `t' is never part of it.
function addressOf() {
  const parts = [];
  const put = (key, value) => {
    if (value != null && value !== "") parts.push(`${key}=${encodeURIComponent(value).replace(/%2F/g, "/")}`);
  };
  if (state.pane === "dashboard") {
    put("v", "dashboard");
    put("range", state.range);
  } else if (state.pane === "control") {
    put("v", "control");
    put("tab", control.tab);
  } else if (state.current) {
    put("s", state.current);
    if (state.pane !== "chat") put("v", state.pane);
    if (state.pane === "run") {
      put("turn", state.drawer?.turn ?? (PHONE.matches && !state.runPaged ? null : state.runOpen));
      if (state.drawer) {
        put("call", state.drawer.call + 1);
        put("tab", state.drawer.tab);
      }
    }
    if (state.pane === "files") put("path", state.filesPath);
  }
  return parts.join("&");
}

// An address without what only filters its place: two addresses of one place
// are one entry in the history.
function placeOf(hash) {
  const params = new URLSearchParams(hash);
  params.delete("t");
  params.delete("range");
  if (params.get("v") === "run") params.delete("tab");
  params.sort();
  return params.toString();
}

let landing = false; // while the page is taken to an address, what it moves writes none
let linkDue = null; // "push" or "replace": the write waiting for the end of this task

// The address of where the page now is, written once however many moves one
// task made. HOW "push" -- a move the reader made -- pushes an entry when the
// place changed and replaces the one there when it did not; "replace" -- the
// page settling, on a default or on a place it could not reach -- never pushes.
function writeLink(how = "push") {
  if (landing) return;
  if (!linkDue) queueMicrotask(flushLink);
  if (linkDue !== "push") linkDue = how;
}

function flushLink() {
  const how = linkDue;
  linkDue = null;
  const hash = addressOf();
  const now = location.hash.slice(1);
  if (hash === now) return;
  const url = `${location.pathname}${hash ? `#${hash}` : ""}`;
  if (how === "push" && placeOf(hash) !== placeOf(now)) history.pushState(null, "", url);
  else history.replaceState(null, "", url);
}

// The page taken to the place PARAMS name, pushing nothing on its way: the
// address it lands on replaces the one it came from, so a place it could not
// reach is put right in the bar rather than kept.
function land(params) {
  landing = true;
  try {
    const session = params.get("s");
    let view = params.get("v") || "chat";
    if (BOARDS.includes(view)) {
      if (session && session !== state.current) select(session);
      if (view === "dashboard") landRange(params.get("range"));
      setPane(view, params.get("tab"));
    } else if (!session) {
      if (state.current || state.pane !== "chat") openNew();
    } else {
      if (!sessionViews().includes(view)) {
        toast(t("A session has no {view} view on this page, so this is its chat.", { view }));
        view = "chat";
      }
      if (session !== state.current) select(session);
      setPane(view);
      if (view === "run") {
        state.runOpen = whole(params.get("turn"));
        state.runPaged = state.runOpen != null && PHONE.matches;
        const call = whole(params.get("call"));
        const tab = params.get("tab");
        state.drawer = state.runOpen != null && call
          ? { turn: state.runOpen, call: call - 1, tab: INSPECTOR_TABS.some(([id]) => id === tab) ? tab : "summary", open: new Set() }
          : null;
      }
      if (view === "files") state.filesPath = params.get("path") || "";
      renderObserve();
      if (!state.sessionsById.has(session)) held(session);
    }
  } finally {
    landing = false;
  }
  writeLink("replace");
}

// A range the Dashboard does not have leaves it on the one it was on, and says so.
function landRange(range) {
  const known = RANGES.some(([id]) => id === range);
  if (range && !known) toast(t("The Dashboard has no range {range}, so it shows {shown}.", { range, shown: RANGES.find(([id]) => id === state.range)?.[1] }));
  if (!known || range === state.range) return;
  state.range = range;
  if (state.pane === "dashboard") {
    loadDashboard();
    renderObserve();
  }
}

// A session an address names that the list does not hold may still be in the
// store -- the list is the newest 300 -- so the gateway is asked. One it does
// not hold is said so, and a new session opens in its place.
async function held(id) {
  try {
    await api("GET", sessionPath(id));
  } catch (error) {
    if (error.status !== 404 || state.current !== id) return;
    toast(t("There is no session {id} in this home, so this is a new one.", { id }), { error: true });
    landing = true;
    try { openNew(); } finally { landing = false; }
    writeLink("replace");
  }
}

// Back, Forward, a link followed or an address typed: the page goes where the
// address says. A token that arrives so is taken the way a load takes it.
function arrive() {
  const hash = location.hash.slice(1);
  const params = new URLSearchParams(hash);
  if (params.has("t")) { location.reload(); return; }
  if (hash !== addressOf()) land(params);
}

function safeGet(store, key) {
  try { return store.getItem(key); } catch { return null; }
}

function gate(detail) {
  $("gate").hidden = false;
  const line = $("gate-detail");
  line.hidden = !detail;
  line.lastElementChild.textContent = detail || "";
}

// --- HTTP: the session directory --------------------------------------------------

// A request with this tab's token, or with none when the page came without one
// (a proxy in front of the gateway adds it). One that never reached the gateway
// fails with status 0 and says so in a sentence, where the browser says "Failed to fetch".
async function reach(path, init = {}) {
  const authorization = state.token ? { Authorization: `Bearer ${state.token}` } : {};
  try {
    return await fetch(path, { ...init, headers: { ...authorization, ...init.headers } });
  } catch {
    throw Object.assign(new Error(t("the gateway isn't answering")), { status: 0 });
  }
}

// A BODY is sent as JSON, or as it is (a file) under TYPE when one is given.
async function api(method, path, body, type) {
  const response = await reach(path, {
    method,
    headers: body ? { "Content-Type": type || "application/json" } : {},
    body: body && !type ? JSON.stringify(body) : body,
  });
  const data = await response.json().catch(() => null);
  if (!response.ok) {
    const error = new Error(data?.error?.message || `${response.status}`);
    error.status = response.status;
    // The request member a refusal is about, when it names one: where a form puts the hand back.
    error.field = data?.error?.field;
    throw error;
  }
  return data;
}

let sessionsTimer = null;
async function loadSessions() {
  clearTimeout(sessionsTimer);
  try {
    const list = await api("GET", "/api/gateway/sessions?limit=300");
    const known = new Map(list.map((entry) => [entry.id, entry]));
    // A session minted here and not yet spoken in is standing by, not listed.
    for (const [id, entry] of state.sessionsById) {
      if (entry.standby && !known.has(id)) known.set(id, entry);
    }
    state.sessionsById = known;
    state.sessions = [...known.values()];
    noteSeen(list);
    for (const id of state.picking?.ids || []) if (!known.has(id)) state.picking.ids.delete(id);
    state.sessionsUnread = null;
    state.listed = true;
    renderSessions();
    renderHead();
    if (!state.current) renderOpener();
  } catch (error) {
    if (error.status === 401 && !state.token) location.reload();
    else if (error.status === 401) gate(t("{host} refused this tab's token", { host: location.host }));
    else state.sessionsUnread = error;
  }
  renderUnread();
  sessionsTimer = setTimeout(loadSessions, 30000);
}

// A list that could not be read says so where it stands, with a Retry; while
// the link is down the link bar has already said why.
function renderUnread() {
  const error = state.sessionsUnread;
  $("sessions-unread").hidden = !error || (error.status === 0 && !state.open);
  $("sessions-why").textContent = error ? sentence(error.message) : "";
}

let sessionsSoon = null;
function loadSessionsSoon() {
  clearTimeout(sessionsSoon);
  sessionsSoon = setTimeout(loadSessions, 400);
}

// --- the socket ----------------------------------------------------------------------

const newId = (prefix) => `${prefix}-${crypto.randomUUID()}`;
const instanceId = newId("web");

// The retry ladder: five quick tries (0.4 s doubling to 5 s, about 11 s in
// all) pass under the link bar alone; after them one every 15 s, and one at
// once when the device is back online, the tab back in view, or Try again.
const QUICK_TRIES = 5;
const retryDelay = (n) => (n < QUICK_TRIES ? Math.min(5000, 400 * 2 ** n) : 15000);

// A tab without a token still dials: behind a proxy that signs every request
// in (the link cell) the gateway takes the socket at its upgrade, and on the
// gateway's own port a refusal brings up the gate.
function connect() {
  const ws = new WebSocket(`${location.protocol === "https:" ? "wss" : "ws"}://${location.host}/gateway`);
  state.ws = ws;
  app.dataset.state = "connecting";
  $("link").textContent = t("connecting");
  ws.onmessage = (event) => receive(JSON.parse(event.data));
  ws.onclose = () => {
    if (state.ws !== ws) return;
    state.open = false;
    app.dataset.state = "closed";
    $("link").textContent = t("reconnecting");
    renderComposer();
    if (state.pane === "control") renderObserve();
    const delay = retryDelay(state.retry++);
    state.retryAt = Date.now() + delay;
    state.retryTimer = setTimeout(connect, delay);
    renderLink();
    traceLine();
  };
}

// Whether this page came down the link (the relay's address, an allowed
// browser): the link's reading names the browser HERE only then.
const viaLink = () => !!control.link.now?.here;

// Where the line breaks, each hop read where it answers: the device's own
// online flag, then /healthz, which the gateway answers anyone. No answer is
// the host the page came from -- the relay when the page came down the link,
// the machine itself on its own port; the relay's offline page
// (x-uplink-machine: offline) is the machine off its line, and its 502 or 504
// the machine on it but silent; any other answer is the machine up and its
// socket refusing this tab. A tab without a token that the link no longer lets
// in (a 401) reloads to say where it goes instead of retrying forever.
async function traceLine() {
  let broken = "device";
  if (navigator.onLine) {
    try {
      const response = await fetch("/healthz", { cache: "no-store", signal: AbortSignal.timeout(8000) });
      if (response.status === 401 && !state.token) { location.reload(); return; }
      broken = response.headers.get("x-uplink-machine") === "offline" ? "off"
        : [502, 504].includes(response.status) ? "silent" : "socket";
    } catch (error) {
      broken = !viaLink() ? "silent" : error?.name === "TimeoutError" ? "silent" : "relay";
    }
  }
  if (state.open) return;
  state.broken = broken;
  renderOffline();
}

// The offline sheet: once the quick tries are spent, the hops between this
// tab and Nodecode -- this device, the relay when the page came down the
// link, the machine -- each with what it answered and the broken one lit;
// Try again at its foot, the command that starts Nodecode when the machine is
// the break, a reload when its socket refuses. Put away (Close, the scrim), it
// stays away until the break moves or the link is back.
function renderOffline() {
  const b = state.broken;
  const show = !state.open && state.retry > QUICK_TRIES && !!b && state.offlineAway !== b;
  $("offline").hidden = $("offline-scrim").hidden = !show;
  if (!show) return;
  const link = viaLink();
  const machine = link ? location.hostname.split(".")[0] : location.host;
  let relay = "";
  try { relay = new URL(control.link.now.relay).hostname; } catch {}
  const unseen = b === "device" || b === "relay";
  const hops = [[t("This device"), b === "device" ? "bad" : "ok", b === "device" ? t("offline") : t("online")]];
  if (link) hops.push([relay || t("The relay"), b === "relay" ? "bad" : b === "device" ? "" : "ok",
    b === "relay" ? t("not reachable") : b === "device" ? t("not checked") : t("reachable")]);
  hops.push([machine, unseen ? "" : b === "socket" ? "ok" : "bad",
    unseen ? t("not checked") : b === "off" ? t("not connected") : b === "silent" ? t("not answering") : t("answering")]);
  $("offline-hops").replaceChildren(...hops.map(([name, hop, said]) => {
    const li = el("li");
    if (hop) li.dataset.hop = hop;
    li.append(el("i", hop === "ok" ? "led ok" : hop ? "led" : "led off"), el("span", "name", name), el("span", "said", said));
    return li;
  }));
  $("offline-tries").textContent = t("Not connected · tried {n} times", { n: state.retry });
  $("offline-title").textContent = b === "device" ? t("This device is offline")
    : b === "relay" ? t("Can't reach the relay")
    : b === "socket" ? t("{machine} answers, but Nodecode won't connect", { machine })
    : t("{machine} isn't answering", { machine });
  $("offline-why").textContent = b === "device" ? t("It tries again the moment this device is back online.")
    : b === "relay" ? t("This device is online, but the relay doesn't answer.")
    : b === "socket" ? t("Its gateway may be restarting. If Nodecode was updated, reload the page.")
    : t("Start Nodecode on {machine}, or wake the machine; this tab connects on its own:", { machine });
  $("offline-cmd").hidden = b !== "off" && b !== "silent";
  $("offline-reload").hidden = b !== "socket";
}

// The command that starts Nodecode, copied; where the browser keeps the
// clipboard from the page (a page over plain http), it is selected to copy.
function copyStart() {
  const code = $("offline-cmd").querySelector("code");
  const said = () => {
    $("offline-copy").textContent = t("Copied");
    setTimeout(() => { $("offline-copy").textContent = t("Copy"); }, 2000);
  };
  (navigator.clipboard ? navigator.clipboard.writeText(code.textContent) : Promise.reject())
    .then(said, () => getSelection().selectAllChildren(code));
}

// The link bar: shown from the first drop until the link is back, counting down
// to the next try; on the way back it says so for two seconds and goes.
let linkTick = null;
let linkGone = null;
function renderLink() {
  const bar = $("linkbar");
  clearInterval(linkTick);
  clearTimeout(linkGone);
  renderOffline();
  if (state.open) {
    if (bar.hidden) return;
    bar.classList.add("back");
    $("linkbar-said").textContent = t("Connected again");
    $("linkbar-when").textContent = "";
    $("reconnect").hidden = true;
    linkGone = setTimeout(() => { bar.hidden = true; }, 2000);
    return;
  }
  bar.hidden = false;
  bar.classList.remove("back");
  $("linkbar-said").textContent = t("Not connected to the gateway");
  $("reconnect").hidden = false;
  const tick = () => {
    const left = Math.ceil((state.retryAt - Date.now()) / 1000);
    const trying = left <= 0 || state.ws?.readyState === WebSocket.CONNECTING;
    const when = trying ? t("trying now") : t("next try in {n} s", { n: left });
    $("linkbar-when").textContent = `${location.host} · ${when}`;
    $("offline-fine").textContent = when;
    $("offline-retry").disabled = trying;
  };
  tick();
  linkTick = setInterval(tick, 250);
}

// A try now, between tries only: not while one dials, and never after the
// gateway refused this tab (the gate holds the page then, and the socket is
// gone). It keeps the ladder's place, so a sheet up stays up.
function reconnectNow() {
  if (state.open || state.ws?.readyState !== WebSocket.CLOSED) return;
  clearTimeout(state.retryTimer);
  connect();
  renderLink();
}

function send(message) {
  if (state.ws?.readyState === WebSocket.OPEN) state.ws.send(JSON.stringify(message));
}

function command(type, fields) {
  const id = newId("command");
  send({ type: "client_command", command: { type, command_id: id, correlation_id: id, ...fields } });
  return id;
}

function subscribe() {
  const subscriptions = state.current ? [{ session_id: state.current, rows: true }] : [];
  send({ type: "client_command", command: { type: "set_session_subscriptions", subscriptions } });
}

function receive(message) {
  switch (message.type) {
    case "gateway_connect_challenge":
      send({
        type: "gateway_connect",
        protocol_min: message.protocol_min,
        protocol_max: message.protocol_max,
        challenge_nonce: message.challenge_nonce,
        client: { id: "nodecode-web", version: "0.1.0", mode: "web", platform: "browser", instance_id: instanceId },
        ...(state.token ? { auth: { operator_token: state.token } } : {}),
        requested_scopes: ["sync.read", "sync.write"],
        transport: { forwarded: false, secure_channel_observed: false },
      });
      return;
    case "gateway_connect_ack":
      state.open = true;
      state.retry = 0;
      state.broken = state.offlineAway = null;
      app.dataset.state = "open";
      $("link").textContent = "";
      $("link").parentElement.title = message.version
        ? t("The gateway this tab is attached to, v{version}", { version: message.version })
        : t("The gateway this tab is attached to");
      renderComposer();
      renderLink();
      for (const node of $("toasts").querySelectorAll("[data-link]")) node.remove();
      subscribe();
      loadSessions();
      loadGatewayFolder();
      loadTarget();
      loadAccount();
      loadSlash();
      loadTimeline();
      loadDashboard();
      control.reconnected();
      control.link.read().then(paintLink);
      return;
    case "gateway_connect_rejected":
      state.ws = null;
      gate(state.token ? message.message || t("{host} refused this tab's token", { host: location.host }) : "");
      return;
  }
  const payload = message.payload;
  if (!payload) return;
  switch (payload.type) {
    case "session_rows": foldRows(payload); break;
    case "gateway_turn_command_result": commandResult(payload.result || {}); break;
    case "organism.notice":
      if (payload.text && freshNotice(payload)) toast(payload.text, { error: payload.level === "error", key: payload.key });
      readLinkSoon();
      // A sign-in from a shell's /login ends in a notice; the meter is read again (cached there).
      loadAccount();
      break;
    case "organism.activity": {
      const before = state.running;
      state.running = new Set((payload.sessions || []).map((entry) => entry.id));
      // A session running that the list holds standing by, or not at all, has
      // just been spoken in: the gateway's listing names it by that prompt now.
      // One that stopped has news the listing's stamp is not yet showing.
      if ([...state.running].some((id) => !state.sessionsById.get(id) || state.sessionsById.get(id).standby)
        || [...before].some((id) => !state.running.has(id))) loadSessionsSoon();
      // A turn that ended may have spent the account's requests or credit.
      if ([...before].some((id) => !state.running.has(id))) loadAccount();
      renderSessions();
      break;
    }
    // A pick in another tab or shell, or the default moved in Control: the
    // chip names what the next turn here runs on, so it reads that again.
    case "model_changed": if (!payload.session_id || payload.session_id === state.current) loadTarget(); break;
    case "sync_protocol_error": toast(t("The gateway refused what this tab sent: {why}", { why: payload.message || payload.code }), { error: true }); break;
  }
}

// --- rows ------------------------------------------------------------------------------

function view(id) {
  if (!state.views.has(id)) state.views.set(id, { rows: new Map(), order: [], turn: { active: false } });
  return state.views.get(id);
}

function foldRows(payload) {
  const v = view(payload.session_id);
  if (payload.reset) {
    v.rows = new Map();
    v.order = [];
    if (payload.session_id === state.current) state.arriving = true;
  }
  for (const row of payload.rows || []) {
    const held = v.rows.get(row.id);
    if (row.append !== undefined) row.text = (held?.text || "") + row.append;
    if (!held) v.order.push(row.id);
    v.rows.set(row.id, row);
  }
  for (const id of payload.gone || []) {
    v.rows.delete(id);
  }
  if (payload.gone?.length) v.order = v.order.filter((id) => v.rows.has(id));
  if (payload.order) v.order = payload.order.slice();
  const wasActive = !!v.turn?.active;
  if (payload.turn) v.turn = payload.turn;
  if (payload.session_id === state.current && !payload.reset
      && ((wasActive && !v.turn?.active) || state.pane === "run")) {
    loadTimelineSoon(wasActive && !v.turn?.active ? 300 : 1500);
  }
  if (payload.execs) v.execs = payload.execs;
  if (payload.context) v.context = payload.context;
  if (payload.session_id === state.current) schedule();
}

let frame = 0;
function schedule() {
  if (!frame) frame = requestAnimationFrame(() => { frame = 0; renderTranscript(); renderHead(); });
}

// The rows of a session as the turns they belong to: a turn opens on the
// operator's line and holds everything up to the next one. A turn no one
// typed -- an exit wake, opening on its (eval-status ID) reads -- opens on its
// first row of a turn id not seen before, with no line of its own. A row of a
// turn already seen -- a call the gateway moved to the end when it exited in
// the background -- goes back to the turn that made it. Rows before the first
// turn (a notice, an eviction's divider) stand on their own.
function turnsOf(v) {
  const loose = [];
  const turns = [];
  const seen = new Map(); // turn id -> its group
  let turn = null;
  for (const id of v.order) {
    const row = v.rows.get(id);
    if (!row) continue;
    if (row.kind === "text" && row.role === "user") {
      turn = { key: `turn-${row.id}`, prompt: row, rows: [], turnId: row.turn_id };
      if (row.turn_id) seen.set(row.turn_id, turn);
      turns.push(turn);
      continue;
    }
    // A prompt painted before its turn was admitted learns the id from its work.
    if (turn && !turn.turnId && row.turn_id) {
      turn.turnId = row.turn_id;
      seen.set(row.turn_id, turn);
    }
    const owner = row.turn_id && seen.get(row.turn_id);
    if (owner && owner !== turn && row.kind === "tool_call") {
      owner.rows.push(row);
    } else if (row.turn_id && !owner && row.kind !== "pending_input") {
      // An exit wake's first row is the line naming what woke it (drawWoke).
      const woke = row.metadata?.wake ? row : null;
      turn = { key: `turn-${row.turn_id}`, prompt: null, woke, rows: woke ? [] : [row], turnId: row.turn_id };
      seen.set(row.turn_id, turn);
      turns.push(turn);
    } else if (turn) {
      turn.rows.push(row);
    } else {
      loose.push(row);
    }
  }
  return { loose, turns };
}

// --- drawing -------------------------------------------------------------------------------

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

// The page's letters, on the icon alphabet's 16-cell grid.
const GLYPHS = {
  think: "M7 2h2v3h-2zM3 5h1v1h-1zM12 5h1v1h-1zM4 6h1v1h-1zM11 6h1v1h-1zM7 5h2v2h-2zM2 7h12v2h-12zM4 9h1v1h-1zM11 9h1v1h-1zM3 10h1v1h-1zM12 10h1v1h-1zM7 9h2v5h-2z",
  run: "M2 3h12v1h-12zM2 4h1v8h-1zM13 4h1v8h-1zM4 5h2v1h-2zM5 6h2v1h-2zM6 7h2v1h-2zM5 8h2v1h-2zM4 9h2v1h-2zM7 10h4v1h-4zM2 12h12v1h-12z",
  said: "M2 4h12v1h-12zM2 6h12v1h-12zM2 8h10v1h-10zM2 10h7v1h-7z",
  read: "M2 1h12v1h-12zM2 2h1v12h-1zM13 2h1v12h-1zM4 3h4v1h-4zM9 3h3v1h-3zM4 5h4v1h-4zM10 5h2v1h-2zM4 7h5v1h-5zM10 7h2v1h-2zM4 9h3v1h-3zM10 9h2v1h-2zM4 11h8v1h-8zM4 13h4v1h-4zM9 13h3v1h-3zM2 14h12v1h-12z",
  wrote: "M11 1h3v3h-3zM10 4h2v2h-2zM9 5h2v2h-2zM8 6h2v2h-2zM7 7h2v2h-2zM6 8h2v2h-2zM5 9h2v2h-2zM4 10h2v2h-2zM3 12h1v1h-1zM2 13h1v1h-1zM7 14h7v1h-7z",
};

function glyph(name) {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("class", "glyph");
  svg.setAttribute("viewBox", "0 0 16 16");
  svg.setAttribute("width", "16");
  svg.setAttribute("height", "16");
  svg.setAttribute("fill", "currentColor");
  svg.setAttribute("aria-hidden", "true");
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("d", GLYPHS[name]);
  svg.append(path);
  return svg;
}

function gutter(name) {
  const gut = el("span", "gut");
  gut.append(el("i"), glyph(name), el("i"));
  return gut;
}

// A running thing's mark is the red that means running (the standard's rule
// 5, the operator's balls); one that has ended keeps its place as the hollow
// ring.
function lamp(pulse) {
  return el("i", pulse ? "led live pulse" : "led off");
}

// What a session is doing, as its ball says it (the operator, 2026-10-21:
// "green, orange or red"): red while a turn runs in it, orange with a halo
// while its answer waits on you, green once done -- a pulse, a halo and still,
// so the shape says it to an eye the hue does not reach.
const DOING_WORDS = { ask: t("Needs you"), run: t("Running"), done: t("Done") };
const DONE_ROWS = 5;

function ball(kind) {
  const node = el("i", `ball is-${kind}`);
  node.setAttribute("role", "img");
  node.setAttribute("aria-label", DOING_WORDS[kind]);
  return node;
}

// What a session's answer waits on, as its row says it: the question its turn
// named, else that it waits; a newest turn that failed, that it failed.
const waitsLine = (waits) => (waits?.why === "failed" ? t("its last turn failed") : waits?.question || t("waits on you"));

// A fold the reader can open: native <details>, open where they left it, else
// where DEFAULT says. KEY is what their choice is remembered by.
function fold(className, key, openByDefault) {
  const details = el("details", `fold ${className}`);
  details.dataset.fold = key;
  details.open = state.toggled.has(key) ? state.toggled.get(key) : openByDefault;
  return details;
}

function showClose() {
  const chev = el("span", "chev");
  chev.append(icon("chevron", 13));
  return chev;
}

// Where a fold holds more lines than it shows: how many, beside a mark that
// opens them; open, a mark that folds them back.
function moreLines(n) {
  const word = el("span", "more-lines");
  const open = el("span", "open");
  open.append(icon("expand", 13), el("span", "", t("{n} lines", { n })));
  const shut = el("span", "shut");
  shut.append(icon("collapse", 13), el("span", "sr", t("Show less")));
  word.append(open, shut);
  return word;
}

function toolArguments(row) {
  const args = row.tool?.args || "";
  try {
    const parsed = JSON.parse(args);
    const form = parsed.form ?? parsed.command ?? parsed.cmd;
    if (typeof form === "string" && Object.keys(parsed).length === 1) return form;
    return JSON.stringify(parsed, null, 2);
  } catch {
    return args;
  }
}

const ONE_LINE = 96;
const oneLine = (text) => (text || "").replace(/\s+/g, " ").trim();
// Markdown on one line, as the words it says: no heading, quote or list marks,
// no bold or code ticks, a link its text. A cut attempt's row began `### Part 1'.
const plainLine = (text) => oneLine((text || "")
  .replace(/^[ \t]*(?:#{1,6}[ \t]+|>[ \t]?|[-*+][ \t]+(?:\[[ xX]\][ \t]+)?|\d+[.)][ \t]+)/gm, "")
  .replace(/!?\[([^\]]*)\]\([^)]*\)/g, "$1")
  .replace(/(\*\*|\*|`)(?=\S)(.*?\S)\1/g, "$2"));

// A line of prose inside the work: a thought, or a note the machine said on
// its way past. It opens only when the row could not hold it.
function proseAct(row, kind) {
  const text = row.text || "";
  const line = proseOf(`title ${kind} prose`, text.split("\n")[0]);
  if (line.textContent.length <= ONE_LINE && !text.includes("\n")) {
    const div = el("div", "act act-line");
    div.append(gutter(kind === "thought" ? "think" : "said"), line);
    return div;
  }
  const details = fold("act", `act-${row.id}`, false);
  const summary = el("summary");
  summary.append(gutter(kind === "thought" ? "think" : "said"), line);
  details.append(summary, proseOf(`prose prose-note ${kind}`, text));
  return details;
}

// A thought while it is being thought: the tail of it, under a fixed window.
function thinking(row) {
  const div = el("div", "act thinking");
  const body = el("div");
  const tail = el("div", "tail");
  tail.append(proseOf("prose thought", row.text || ""));
  body.append(el("span", "head", t("Thinking…")), tail);
  div.append(gutter("think"), body);
  return div;
}

// What the shell says of an attempt that was cut off ("Attempt 2 cut off after
// 2.1s: ... Trying again."), on the rail under the attempt it closes. The words
// are the gateway's, the same row a reload replays from the log.
function attemptNote(said) {
  const note = el("div", "act act-line att-note");
  const rail = el("span", "gut");
  rail.append(el("i"), el("i"));
  note.append(rail, el("span", "att-said", said));
  return note;
}

// A call: the shell's own words for it, and what came back under the fold. A
// failed one opens itself -- the reason the answer under it may be wrong. What
// came back is the gateway's reading for the operator (tool.shown); the text
// the model read stays in the Run view. A stopped call keeps what it wrote and
// says who stopped it; a call that handed its job to the background points at
// the job's box; a read of a background job folds what the job wrote under how
// it ended.
function toolAct(row) {
  const tool = row.tool || {};
  const status = tool.status || (row.live ? "running" : "");
  const failed = status === "error" || row.error;
  const stop = row.metadata?.stopped;
  const read = row.metadata?.background_read;
  const details = fold("act", `act-${row.id}`, failed);
  details.dataset.status = failed ? "error" : status;
  const summary = el("summary");
  const title = el("span", tool.title ? "title" : "title tool-name", tool.title || tool.name || t("tool"));
  if (tool.code) title.append(" ", el("code", "act-code", tool.code));
  const shown = (tool.shown || "").replace(/\n+$/, "");
  const note = failed ? t("failed")
    : status === "stopped" ? t("stopped")
    : status === "background" ? t("moved to the background")
    : status === "running" ? t("running")
    : status !== "exited" && tool.exit_code ? t("exit {code}", { code: tool.exit_code }) : "";
  const changed = row.metadata?.workspace_diff?.files || [];
  // A write or an edit whose diff stands under the card is that diff: its
  // form and its receipt said the same thing twice more.
  const diffed = changed.length > 0 && (tool.family === "write" || tool.family === "edit");
  summary.append(gutter(tool.name === "read" ? "read" : changed.length ? "wrote" : "run"), title);
  if (changed.length) summary.append(countsOf(changed));
  const lines = shown && !diffed && !read ? shown.split("\n").length : 0;
  if (lines > 1) summary.append(el("span", "act-lines", t("{n} lines", { n: lines })));
  if (note) summary.append(el("span", "state", note));
  const spelled = el("div", "spelled");
  // What ran: a shell command as a shell prints it, else the snippet -- unless
  // the title already carries the whole snippet as its code.
  const snippet = toolArguments(row);
  if (tool.commands) for (const command of tool.commands) spelled.append(el("div", "act-command", `$ ${command}`));
  else if (!diffed && !read && snippet.trim() !== tool.code) spelled.append(el("div", "", snippet));
  if (stop) spelled.append(...stoppedBody(stop));
  else if (row.metadata?.background_shell) spelled.append(el("div", "result", t("Its output is over the message box while it runs, and under the turn once it ends.")));
  else if (read) spelled.append(readBody(row, read));
  else if (shown && !diffed) spelled.append(el("div", "result", shown));
  details.append(summary);
  if (spelled.childNodes.length) details.append(spelled);
  return details;
}

// Who stopped a call and after how long, from the stop the engine keeps on
// its result (or on its exit, for a job that ran on in the background).
function stopWords(stop) {
  const after = stop.after_ms != null ? ms(stop.after_ms) : null;
  if (stop.by === "operator") return after ? t("Stopped by you after {time}", { time: after }) : t("Stopped by you");
  if (stop.by === "shutdown") return t("Stopped: the gateway shut down while this ran");
  if (stop.by === "model") return after ? t("Stopped by the model after {time}", { time: after }) : t("Stopped by the model");
  return stop.reason ? t("Stopped: {reason}", { reason: stop.reason }) : t("Stopped");
}

// A stopped call's lines as it wrote them before the stop, then the stop.
function stoppedBody(stop) {
  const nodes = [];
  // A tail kept by bytes begins partway into its first line.
  const tail = stop.tail_cut ? stop.tail.slice(stop.tail.indexOf("\n") + 1) : stop.tail || "";
  if (tail.trim()) {
    const lines = el("div", "result");
    if (stop.tail_cut) lines.append(el("span", "kept", `${t("Earlier output is not kept")}\n`));
    lines.append(tail.replace(/\n+$/, ""));
    nodes.push(lines);
  }
  nodes.push(el("div", "act-stop-said", stopWords(stop)));
  return nodes;
}

// A read of a background job: one line -- how it ended, how many lines it
// wrote -- over what it wrote, which its box already shows; a job someone
// stopped, who stopped it.
function readBody(row, read) {
  if (read.stopped) return el("div", "act-stop-said", stopWords(read.stopped));
  const text = (read.text || "").replace(/\n+$/, "");
  const n = text ? text.split("\n").length : 0;
  const how = read.status === "running" ? t("running")
    : read.end === "failed" || read.end === "stopped" ? execEnd(read.end)
    : read.shell_exit != null ? t("exit {code}", { code: read.shell_exit }) : t("done");
  const said = n ? `${how} · ${plural(n, t("{n} line"), t("{n} lines"))}` : how;
  if (!n) return el("div", "result", said);
  const more = fold("act-read", `read-${row.id}`, false);
  const summary = el("summary");
  summary.append(el("span", "", said), showClose());
  more.append(summary, el("div", "result", text));
  return more;
}

// The lines FILES added and removed, as the shell's card header counts them.
function countsOf(files) {
  const added = files.reduce((sum, file) => sum + (file.added || 0), 0);
  const removed = files.reduce((sum, file) => sum + (file.removed || 0), 0);
  const counts = el("span", "card-stat");
  if (added) counts.append(el("span", "diff-add", `+${added}`));
  if (removed) counts.append(el("span", "diff-del", `-${removed}`));
  return counts;
}

// A patch from its first hunk on: git's own header lines name what the card
// already names, and its no-newline note is not a line of the file.
function hunksOf(patch) {
  const lines = (patch || "").replace(/\n$/, "").split("\n");
  const start = lines.findIndex((line) => line.startsWith("@@"));
  return start < 0 ? "" : lines.slice(start).filter((line) => !line.startsWith("\\")).join("\n");
}

// What a call changed, under its card: the workspace diff its result carries
// (the gateway's row metadata, git's patch per file between the folder before
// the call and after it -- what the shell draws under its card too), so a
// write over a file that was there reads as the lines it changed and a new
// file as every line added. Never the call's own text read back: the diff is
// what landed on disk. Past DIFF_LINES the rest folds under a word that opens
// it; a file past the patch caps keeps its counts alone.
const DIFF_LINES = 12;
function toolDiff(row) {
  const diff = row.metadata?.workspace_diff;
  const files = diff?.files || [];
  const many = files.length > 1 || diff?.truncated;
  const body = el("div", "card-diff-body");
  let lines = 0;
  for (const file of files) {
    if (many) {
      const head = el("div", "card-diff-file");
      head.append(el("span", "path", file.path || ""), file.binary ? el("span", "", t("binary")) : countsOf([file]));
      body.append(head);
    }
    const hunks = hunksOf(file.patch);
    if (!hunks) continue;
    body.insertAdjacentHTML("beforeend", diffHtml(hunks));
    lines += hunks.split("\n").length;
  }
  if (diff?.truncated) {
    body.append(el("div", "card-diff-file", plural(diff.truncated, t("{n} more file"), t("{n} more files"))));
  }
  if (!lines && !many) return [];
  const block = el("div", "card-diff");
  if (lines > DIFF_LINES) {
    // The word comes first so a closed one can clip the lines after it; it
    // stands under them all the same.
    const more = fold("card-diff-more", `diff-${row.id}`, false);
    const summary = el("summary");
    summary.append(moreLines(lines));
    more.append(summary);
    block.style.setProperty("--card-diff-lines", DIFF_LINES);
    block.append(more);
  }
  block.append(body);
  return [block];
}

function looseRow(row) {
  const text = row.text || (row.kind === "eviction_divider" ? t("• context evicted above") : "");
  return el("p", `loose${row.error ? " error" : ""}${row.evicted ? " dimmed" : ""}`, text);
}

// A prompt too long to read at a glance -- a pasted file, an exit's output --
// shows its head and opens on a click.
const BUBBLE_LINES = 10;
function bubbleOf(row) {
  const text = row.text || "";
  const lines = text.split("\n");
  if (lines.length <= BUBBLE_LINES + 2 && text.length <= 1600) return el("div", "bubble", text);
  const key = `bubble-${row.id}`;
  const details = fold("bubble long", key, false);
  const summary = el("summary");
  summary.append(el("span", "head", lines.slice(0, BUBBLE_LINES).join("\n").slice(0, 1600)));
  summary.append(moreLines(lines.length));
  details.append(summary, el("div", "rest-text", lines.slice(BUBBLE_LINES).join("\n")));
  return details;
}

function drawTurn(turn, number, live, v, again) {
  const article = el("article", turn.prompt ? "turn" : "turn unprompted");
  if (turn.prompt && (turn.prompt.evicted || turn.prompt.voided)) article.classList.add("dimmed");
  if (turn.prompt) article.append(drawAsk(turn, number, live));
  if (turn.woke) article.append(drawWoke(turn.woke));
  return drawEnd(drawClosedExecs(drawWork(article, turn, live, v), turn), turn, again);
}

// A pair written whole -- a reflection's recap, a prompt an import read from
// another agent -- was not asked here: its input stands quiet, on the
// machine's side, under what recorded it.
function drawAsk(turn, number, live) {
  const recorded = turn.prompt.recorded;
  const ask = el("div", recorded ? "ask recorded" : "ask");
  if (recorded) ask.append(el("p", "by", typeof recorded === "string" ? t("recorded · {by}", { by: recorded }) : t("recorded")));
  const under = el("p", "under");
  if (live) under.append(lamp(true));
  under.append(el("span", live ? "turn-n" : "turn-n rewind", turn.prompt.steer ? t("turn {n} · steer", { n: number }) : t("turn {n}", { n: number })));
  if (turn.prompt.turn_id && !live) {
    const rewind = iconButton("rewind", t("Rewind"), "icon rewind");
    rewind.dataset.act = "rewind";
    rewind.dataset.key = turn.key;
    rewind.dataset.turn = turn.prompt.turn_id;
    rewind.dataset.number = String(number);
    const fork = iconButton("fork", t("Fork"), "icon rewind");
    fork.dataset.act = "fork";
    fork.dataset.turn = turn.prompt.turn_id;
    under.append(rewind, fork);
  }
  ask.append(bubbleOf(turn.prompt));
  // The pictures sent with it, as the gateway kept them (exec.lisp
  // PROMPT-IMAGES): the same on the live stream and after a reload.
  const sent = (turn.prompt.metadata?.images || []).map((image) => figureOf(image, image.id));
  if (sent.length) {
    const strip = el("div", "ask-pics");
    strip.append(...sent);
    ask.append(strip);
  }
  ask.append(under);
  const asking = state.rewind?.key === turn.key && state.rewind.session === state.current ? state.rewind : null;
  if (asking) ask.append(confirmPanel(t("Rewind to before turn {n}?", { n: number }), asking));
  return ask;
}

// --- a picture, whole --------------------------------------------------------------------
// Over the page, not in place of it: Esc, the mask or the close button put the
// page back as it was, focus where it stood.

let lightboxOpener = null;

function openLightbox(src, caption) {
  if (!src) return;
  lightboxOpener = document.activeElement;
  $("lightbox-image").src = src;
  $("lightbox-caption").textContent = caption || "";
  $("lightbox-caption").hidden = !caption;
  $("lightbox").hidden = false;
  $("lightbox-close").focus();
}

function closeLightbox() {
  if ($("lightbox").hidden) return;
  $("lightbox").hidden = true;
  $("lightbox-image").removeAttribute("src");
  lightboxOpener?.focus?.();
  lightboxOpener = null;
}

// A picture a fact names: a LOOK's, one an answer wrote as ![alt](path), or
// one sent with a prompt. The fact names a file, never bytes, and a page
// cannot open a path: the gateway serves the bytes, fetched once per key --
// the call that looked, or the id of a message's picture. The bytes are kept
// beside their object URL: the page may not fetch a blob: URL back.
const pictures = new Map(); // "session key" -> promise of { blob, url }, or null

function pictureHeld(session, key) {
  const slot = `${session} ${key}`;
  if (!pictures.has(slot)) {
    const path = `/api/gateway/sessions/${encodeURIComponent(session)}/images/${encodeURIComponent(key)}`;
    pictures.set(slot, reach(path)
      .then((response) => (response.ok ? response.blob() : null))
      .then((blob) => blob && { blob, url: URL.createObjectURL(blob) })
      .catch(() => null));
  }
  return pictures.get(slot);
}

const picture = (session, key) => pictureHeld(session, key).then((held) => held?.url);

// IMAGE, an image fact whose bytes KEY fetches, as a figure: the picture, its
// name and size beneath, the whole of it a click away. ALT is what an answer
// called it.
function figureOf(image, key, alt) {
  const name = (image.path || "").split(/[\\/]/).pop() || t("image");
  const figure = el("figure", "look");
  const frame = el("button", "frame");
  frame.type = "button";
  frame.setAttribute("aria-label", t("View {name}", { name }));
  const img = el("img");
  img.alt = alt || name;
  // The size the fact knows holds the picture's place before its bytes land
  // (app.css .look img): an img's own width and height do not, under auto.
  if (image.width && image.height) { img.style.setProperty("--w", image.width); img.style.setProperty("--h", image.height); }
  frame.append(img);
  const caption = el("figcaption");
  const size = [image.width && image.height && `${image.width}×${image.height}`,
    image.bytes && `${Math.max(1, Math.round(image.bytes / 1024))} KB`].filter(Boolean).join(" · ");
  // A picture that came as bytes -- a prompt's -- has no file to name.
  if (image.path) caption.append(el("span", "name", name));
  caption.append(el("span", "reading", size));
  figure.append(frame, caption);
  if (image.path) figure.title = image.path;
  frame.addEventListener("click", () => { if (img.src) openLightbox(img.src, `${name}${size ? ` · ${size}` : ""}`); });
  picture(state.current, key).then((url) => {
    if (url) {
      img.src = url;
    } else {
      figure.classList.add("gone");
      caption.lastChild.textContent = t("no longer on disk");
    }
  });
  return figure;
}

// A line a turn rests on that no turn ended on: the context's eviction
// count, carrying neither an outcome nor a turn: a footnote like a good end's.
function noteRow(row) {
  return row.kind === "finish_divider" && !row.outcome && !row.turn_id;
}

// One mark per call, in the order they ran: how the work went, read with the
// fold shut -- a call that failed, the one still running.
function callTicks(calls) {
  const ticks = el("span", "ticks");
  ticks.setAttribute("aria-hidden", "true");
  for (const row of calls) {
    const status = row.tool?.status || (row.live ? "running" : "");
    ticks.append(el("i", status === "error" || row.error ? "failed" : status === "running" ? "running" : ""));
  }
  return ticks;
}

// The thinking the model has finished in thought ROW: all of it once the
// thought is done; while it arrives, up to the paragraph being written -- a
// paragraph is done once a blank line closes it -- so words still arriving
// never show.
function finishedThought(row) {
  const text = row.text || "";
  if (!row.live || /\n[ \t]*\n\s*$/.test(text)) return text.trim();
  const cut = text.search(/\n[ \t]*\n(?![\s\S]*\n[ \t]*\n)/);
  return cut < 0 ? "" : text.slice(0, cut).trim();
}

// The last paragraph of TEXT, under the heading that opens it when one does:
// a summary's **Finding the label** says nothing alone.
function thoughtParagraph(text) {
  const parts = text.split(/\n[ \t]*\n/).map((part) => part.trim()).filter(Boolean);
  const heading = (part) => /^(\*\*[^\n]+\*\*|#{1,6} [^\n]+)$/.test(part);
  const last = parts.length - 1;
  if (last > 0 && heading(parts[last - 1]) && !heading(parts[last])) return `${parts[last - 1]}\n${parts[last]}`;
  return parts[last] || "";
}

// When the paragraph under each running turn's bar last changed (turn key ->
// { text, at }), so a turn drawn again mid-fade carries its fade on, never
// starts it over.
const thoughtShown = new Map();
const THOUGHT_FADE_MS = 420;

// Under a running turn's bar, the newest thinking the model finished: one
// paragraph, no label, a step quieter than the work, faded in as it takes the
// place of the one before -- the TUI's live thought (LIVE-THOUGHT-ROWS).
// Only while the turn runs: an ended turn's thinking
// is in its work.
function liveThought(work, key) {
  for (let at = work.length - 1; at >= 0; at--) {
    const row = work[at];
    if (row.kind !== "reasoning" || row.voided) continue;
    const said = thoughtParagraph(finishedThought(row));
    if (!said) continue;
    const now = performance.now();
    if (thoughtShown.get(key)?.text !== said) thoughtShown.set(key, { text: said, at: now });
    const since = now - thoughtShown.get(key).at;
    const thought = proseOf("prose thought live-thought", said);
    if (since < THOUGHT_FADE_MS) thought.style.setProperty("--faded", `-${Math.round(since)}ms`);
    else thought.classList.add("settled");
    return thought;
  }
  return null;
}

function drawWork(article, turn, live, v) {
  // The answer is the last thing said with no work after it; every message
  // before it is work. Decided by position, never by reading the text.
  let answer = null;
  for (const row of turn.rows) {
    // A cut-off attempt's text is kept, but it is never the answer.
    if (row.kind === "text" && row.role === "assistant") answer = row.voided ? null : row;
    // A call that exited in the background after the turn is not work the
    // answer came before: the gateway only moved it to where it finished.
    else if ((row.kind === "tool_call" && row.tool?.status !== "exited") || row.kind === "reasoning") answer = null;
  }
  const work = [];
  const notes = [];
  const rests = [];
  const parked = [];
  for (const row of turn.rows) {
    if (row === answer) continue;
    switch (row.kind) {
      case "reasoning": case "tool_call": work.push(row); break;
      case "text": if (row.role === "assistant") work.push(row); else notes.push(row); break;
      case "finish_divider": rests.push(row); break;
      case "pending_input": parked.push(row); break;
      default: notes.push(row);
    }
  }

  if (work.length) {
    const calls = work.filter((row) => row.kind === "tool_call");
    const failed = calls.filter((row) => row.tool?.status === "error" || row.error).length;
    // The row that closes a cut-off attempt carries the shell's sentence for it.
    const cut = work.filter((row) => typeof row.voided === "string").length;
    // Folded, live or done, until a click opens it -- the TUI's Standard run:
    // the running bar and the thought under it say
    // what the turn is doing.
    const details = fold("work", `work-${turn.key}`, false);
    const summary = el("summary");
    summary.append(el("span", "micro", live ? t("Working") : t("Worked")));
    if (calls.length) summary.append(el("span", "count", `· ${plural(calls.length, t("{n} tool call"), t("{n} tool calls"))}`));
    if (failed) summary.append(el("span", "failed", t("{n} failed", { n: failed })));
    if (cut) summary.append(el("span", "att-cut", `· ${plural(cut, t("{n} attempt cut off"), t("{n} attempts cut off"))}`));
    if (calls.length) summary.append(callTicks(calls));
    summary.append(showClose());
    const acts = el("div", "acts");
    for (const row of work) {
      if (row.kind === "tool_call") acts.append(toolAct(row), ...toolDiff(row));
      else {
        const act = row.kind === "reasoning" ? (row.live ? thinking(row) : proseAct(row, "thought")) : proseAct(row, "said");
        if (row.voided) act.classList.add("att-void");
        acts.append(act);
        if (typeof row.voided === "string") acts.append(attemptNote(row.voided));
      }
      // What a call looked at, or a word on the way named, stands under that
      // act, inside the work: the answer draws a picture only where it names it.
      const looked = row.kind === "tool_call"
        ? (row.metadata?.image && row.tool?.call_id ? [figureOf(row.metadata.image, row.tool.call_id)] : [])
        : (row.metadata?.images || []).map((image) => figureOf(image, image.id));
      if (looked.length) {
        const strip = el("div", "looked");
        strip.append(...looked);
        acts.append(strip);
      }
    }
    details.append(summary, acts);
    article.append(details);
  }
  for (const row of notes) article.append(looseRow(row));
  if (answer) {
    const prose = proseOf(`answer prose${answer.live ? " live-answer" : ""}`, answer.text, answer.metadata?.images);
    duotone(prose);
    if (answer.live) glint(prose, turn.key);
    else chipsArrived.delete(turn.key);
    article.append(prose);
  }
  if (live) {
    const bar = el("div", "running");
    bar.append(lamp(true), el("span", "", liveVerb(v.turn.verb, work)));
    article.append(bar);
    const thought = liveThought(work, turn.key);
    if (thought) article.append(thought);
  } else thoughtShown.delete(turn.key);
  for (const row of rests) {
    // A turn that ended well keeps what it ran on, how long and when as its
    // footnote, there under the pointer, and so does a line no turn ended on
    // (the context's eviction count); a failed or stopped end stays in view.
    const meta = row.outcome === "completed" || row.outcome === "steered" || noteRow(row);
    const line = el("div", meta ? "rest meta" : "rest");
    const text = row.text || "";
    if (text.startsWith("•")) line.append(el("span", "", "•"), el("span", "", text.slice(1).trim()));
    else line.textContent = text;
    article.append(line);
  }
  if (parked.length) {
    const list = el("div", "parked");
    for (const row of parked) {
      const item = el("div");
      item.append(el("span", "mark", "⌎"), el("span", "text", oneLine(row.text)), el("span", "how", row.steer ? t("— steer") : t("— queued")));
      // A queued prompt, steer or follow-up, comes back into the box to be
      // edited (the gateway's withdraw_prompt; ↑ on an empty box does the same).
      if (row.prompt_id) {
        const back = el("button", "word", t("Take back"));
        back.type = "button";
        back.title = t("Back into the box, to edit or send again");
        back.dataset.act = "withdraw";
        back.dataset.prompt = row.prompt_id;
        back.disabled = state.withdrawing.has(row.prompt_id);
        item.append(back);
      }
      list.append(item);
    }
    article.append(list);
  }
  return article;
}

// How the turn ended, read off the row it rested on -- its outcome, never its
// words. A stopped answer carries the mark where it was cut, or, stopped
// before it said anything, the mark stands where the work stopped. A failed
// turn whose prompt is the last one asked (AGAIN) offers it again.
function drawEnd(article, turn, again) {
  const outcome = turn.rows.findLast((row) => row.outcome)?.outcome;
  if (outcome === "cancelled") {
    const mark = el("span", "cut", t("Stopped"));
    const answer = article.querySelector(":scope > .answer");
    const line = answer && cutLine(answer);
    if (line) line.append(" ", mark);
    else if (answer) answer.append(mark);
    else {
      const alone = el("div", "cut-alone");
      alone.append(mark);
      article.insertBefore(alone, article.querySelector(":scope > .rest"));
    }
  } else if (outcome === "failed" && again) {
    const retry = el("button", "quiet retry", t("Retry"));
    retry.type = "button";
    retry.prepend(icon("refresh", 13));
    retry.title = t("Send this prompt again");
    retry.dataset.act = "retry";
    retry.dataset.key = turn.key;
    const line = el("div", "again");
    line.append(retry);
    article.append(line);
  }
  return article;
}

// The line a cut answer ends on: its last paragraph, heading or list item,
// inside the lists and quotes that hold it. A code block or a table has no
// such line, and the mark stands under it.
function cutLine(prose) {
  let at = prose.lastElementChild;
  while (/^(UL|OL|LI|BLOCKQUOTE)$/.test(at?.tagName) && /^(P|H[1-6]|UL|OL|LI|BLOCKQUOTE)$/.test(at.lastElementChild?.tagName)) {
    at = at.lastElementChild;
  }
  return /^(P|H[1-6]|LI)$/.test(at?.tagName) ? at : null;
}

// Which characters of a code span stand back, the TUI's CODE-SPANS: an id all
// quiet; a path's folders quiet under its last segment (PATH true); a name
// bright but for its package and its punctuation.
function codeQuiet(body) {
  const quiet = new Array(body.length).fill(false);
  if (/^(?:[0-9a-f]{7,40}|\d{3,}|[0-9A-Z]{8,}(?:\/[0-9a-f]+…?)?)$/.test(body)) return { path: false, quiet: quiet.fill(true) };
  if (!/\s/.test(body) && (/^(?:~|\.\.?\/)|[^/]\//.test(body) || /\.(?:lisp|asd|py|json|jsonc|md|js|ts|sh|txt|toml|ya?ml|rs|go|zig|c|h|html|css)$/.test(body))) {
    const last = body.endsWith("/") ? body.length - 1 : body.length;
    quiet.fill(true, 0, body.lastIndexOf("/", last - 1) + 1);
    if (last < body.length) quiet[last] = true;
    return { path: true, quiet };
  }
  const at = body.lastIndexOf("::");
  quiet.fill(true, 0, at >= 0 ? at + 2 : /^[a-z][a-z0-9.-]*:[^:\s]/.test(body) ? body.indexOf(":") + 1 : 0);
  for (let i = 0; i < body.length; i++) if ("*+{},…".includes(body[i])) quiet[i] = true;
  return { path: false, quiet };
}

// An answer in the TUI's two hues (app.css, .answer.prose). md.js keeps the
// HTML the shared color goldens lock; the grounds are laid here: # and ## and
// a paragraph that opens on a bold run take a .duo-band, its fade half its words
// in cells, 8 to 36 (GROUND-SEGS); each code span outside an inline color is
// split into its quiet and bright runs.
function duotone(prose) {
  for (const head of prose.querySelectorAll(":scope > :is(h1, h2)")) {
    const band = el("span", "duo-band");
    band.append(...head.childNodes);
    head.append(band);
  }
  for (const p of prose.querySelectorAll(":scope > p")) {
    if (p.firstChild?.nodeName !== "STRONG") continue;
    const strong = p.firstChild, band = el("span", "duo-band");
    strong.replaceWith(band);
    band.append(strong);
    p.classList.add("lead");
  }
  for (const band of prose.querySelectorAll(".duo-band")) {
    band.style.setProperty("--fade", Math.max(8, Math.min(36, Math.ceil(band.textContent.length / 2))));
  }
  for (const code of prose.querySelectorAll(":not(pre) > code")) {
    if (code.closest(".inline-color")) continue;
    const body = code.textContent;
    const { path, quiet } = codeQuiet(body);
    if (path) code.classList.add("path");
    if (!quiet.includes(true)) continue;
    const runs = [];
    for (let i = 0; i < body.length; i++) {
      if (i && quiet[i] === quiet[i - 1]) runs[runs.length - 1][1] += body[i];
      else runs.push([quiet[i], body[i]]);
    }
    code.replaceChildren(...runs.map(([still, text]) => (still ? el("span", "quiet", text) : text)));
  }
}

// A bold run that has just come into a running answer glints once across its
// chip, left to right (the TUI's GLINT-OVERLAY, 800 ms): each chip's arrival
// is kept per turn, so a repaint mid-glint carries on where it was.
const GLINT_MS = 800;
const chipsArrived = new Map();
function glint(prose, key) {
  const now = performance.now();
  const arrived = chipsArrived.get(key) ?? [];
  chipsArrived.set(key, arrived);
  [...prose.querySelectorAll("strong")].filter((chip) => !chip.closest(".duo-band, .inline-color")).forEach((chip, at) => {
    const since = now - (arrived[at] ??= now);
    if (since >= GLINT_MS) return;
    chip.classList.add("glint");
    chip.style.setProperty("--glinted", `-${Math.round(since)}ms`);
  });
}

// The model's markdown, as marks on this surface; every code block carries a
// way to copy it, and each picture of IMAGES, the message's image facts,
// stands where the text names it.
function proseOf(className, text, images = []) {
  const prose = el("div", className);
  prose.innerHTML = markdown(text || "", images.map((image) => image.target));
  // A hex-only foreground fact, not an inline style attribute: assigning the
  // property keeps the page's style-src 'self' policy intact.
  for (const span of prose.querySelectorAll(".inline-color[data-color]")) {
    if (/^#[0-9a-fA-F]{6}$/.test(span.dataset.color)) span.style.color = span.dataset.color;
  }
  for (const mark of prose.querySelectorAll("span.picture")) {
    const image = images.find((each) => each.target === mark.dataset.target);
    mark.replaceWith(figureOf(image, image.id, mark.textContent));
  }
  for (const pre of prose.querySelectorAll("pre")) {
    const copy = iconButton("copy", t("Copy"), "icon copy");
    copy.dataset.act = "copy";
    pre.append(copy);
  }
  return prose;
}

// An exec is known by its call: exec ids start again with each gateway, and a
// session's transcript outlives them.
function execKey(exec) {
  return `${state.current} ${exec.call_id ?? exec.exec_id ?? exec.command}`;
}

function elapsedText(since) {
  const seconds = Math.max(0, Math.round((Date.now() - since) / 1000));
  return seconds < 60 ? `${seconds}s` : `${Math.floor(seconds / 60)}m ${String(seconds % 60).padStart(2, "0")}s`;
}

function execEnd(end, code) {
  return end === "exited" ? t("exited {code}", { code })
    : end === "failed" ? t("failed")
    : end === "stopped" ? t("stopped")
    : t("done");
}

// Every line an exec kept, the newest last; what came before them is gone,
// said once above them.
function execTail(exec, lines) {
  const tail = el("pre", "exec-tail");
  if (exec.cut) tail.append(el("span", "kept", t("Earlier output is not kept")));
  for (const line of lines) tail.append(el("span", "", line || " "));
  return tail;
}

// A running exec is a line of the band over the message box, as the shell's
// band is, never a box in the transcript: its lamp, what it runs, the newest
// line it wrote, how long it has run, and an x that interrupts it (the shell's
// band ✕); once asked it says so until it leaves. The line opens on what it
// kept in a window of eight lines that scrolls, the newest last, so a burst
// of output never moves the page under the reader.
const band = $("band");
let bandSig = null;

function drawRunning(exec) {
  const key = execKey(exec);
  const lines = exec.tail ? exec.tail.split("\n") : [];
  const open = lines.length > 0 && !!state.toggled.get(`bg ${key}`);
  const box = el("div", open ? "bg-exec open" : "bg-exec");
  if (open) box.append(execTail(exec, lines));
  const line = el("div", "bg-line");
  const opener = el("button", "bg-open");
  opener.type = "button";
  opener.dataset.act = "bg-open";
  opener.dataset.key = `bg ${key}`;
  opener.disabled = !lines.length;
  opener.setAttribute("aria-expanded", String(open));
  opener.append(el("span", "command", oneLine(exec.command)), el("span", "newest", lines.at(-1) || t("no output yet")), showClose());
  if (!state.execSeen.has(key)) state.execSeen.set(key, Date.now());
  const elapsed = el("span", "elapsed", elapsedText(state.execSeen.get(key)));
  elapsed.dataset.since = String(state.execSeen.get(key));
  line.append(lamp(exec.status === "running"), opener, elapsed);
  if (state.stopping.has(key)) line.append(el("span", "state", t("stopping")));
  else if (exec.exec_id != null) {
    const stop = el("button", "exec-stop");
    stop.type = "button";
    stop.title = t("Stop");
    stop.setAttribute("aria-label", t("Stop {command}", { command: oneLine(exec.command) }));
    stop.dataset.act = "interrupt";
    stop.dataset.exec = String(exec.exec_id);
    stop.dataset.key = key;
    stop.innerHTML = '<svg viewBox="0 0 15 15" width="12" height="12" fill="none" aria-hidden="true"><path d="M3.5 3.5L11.5 11.5M11.5 3.5L3.5 11.5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>';
    line.append(stop);
  }
  box.append(line);
  return box;
}

// The band holds the session on screen's running execs, drawn again only when
// they or a stop asked of one change, the reader's focus kept on the mark it
// was on.
function renderBand() {
  const v = state.current ? state.views.get(state.current) : null;
  const execs = v?.execs || [];
  if (v) for (const key of state.stopping) {
    if (key.startsWith(`${state.current} `) && !execs.some((exec) => execKey(exec) === key)) state.stopping.delete(key);
  }
  const sig = [state.current, v?.execs, [...state.stopping].join()];
  if (bandSig?.every((part, at) => part === sig[at])) return;
  bandSig = sig;
  const focused = band.contains(document.activeElement) ? document.activeElement.dataset : null;
  band.replaceChildren(...execs.map(drawRunning));
  band.hidden = !execs.length;
  for (const tail of band.querySelectorAll(".exec-tail")) tail.scrollTop = tail.scrollHeight;
  if (focused) band.querySelector(`[data-act="${focused.act}"][data-key="${CSS.escape(focused.key)}"]`)?.focus();
}

// An exec that ran on in the background and has ended is its call's row under
// the turn that ran it, one line as a call's is, drawn from the exit the
// gateway recorded there (persisted history), so a reload shows it as it
// closed: what it ran, its last line, how many it kept and how it ended, in
// the shell's own exit status, never read out of its output. It opens on every
// line the organism kept, wrapped whole, and on what it failed on.
function drawClosedExec(exec) {
  const lines = exec.tail ? exec.tail.split("\n") : [];
  const opens = lines.length > 0 || !!exec.error;
  const row = opens ? fold(`exec ${exec.end}`, `exec ${execKey(exec)}`, false) : el("div", `exec exec-line ${exec.end}`);
  const head = opens ? el("summary") : row;
  head.append(glyph("run"), el("span", "command", oneLine(exec.command)), el("span", "last", lines.at(-1) || ""));
  if (lines.length > 1) head.append(el("span", "count", t("{n} lines", { n: lines.length })));
  head.append(el("span", "state", exec.stopped ? stopWords(exec.stopped) : execEnd(exec.end, exec.shell_exit)));
  if (!opens) return row;
  head.append(showClose());
  row.append(head);
  if (lines.length) row.append(execTail(exec, lines));
  // What it failed on, last and whole, never a bare "failed".
  if (exec.error) row.append(el("pre", "exec-error", t("error: {report}", { report: exec.error })));
  return row;
}

// The execs a turn left running that have exited since, under what the turn
// said: their calls' rows carry how they ended.
function drawClosedExecs(article, turn) {
  const closed = turn.rows.filter((row) => row.tool?.exec);
  if (closed.length) {
    const block = el("div", "execs");
    for (const row of closed) block.append(drawClosedExec(row.tool.exec));
    article.append(block);
  }
  return article;
}

// A turn no one typed opens on what woke it: the evaluations whose exits it
// reads, as its start fact names them -- never a line in the operator's mouth.
function drawWoke(row) {
  const wake = row.metadata.wake;
  const line = el("p", "woke");
  // What woke it in the operator's words; the eval's number is the model's.
  line.title = wake.map((each) => t("eval {id}", { id: each.eval_id })).join(", ");
  line.append(el("span", "micro", t("Woke")),
    el("span", "", wake.map((each) => t("background job {end}", { end: execEnd(each.end, each.shell_exit) })).join(", ")));
  return line;
}

// --- checkpoints --------------------------------------------------------------------

// The confirm stage: what the move is, the files a restore would touch when
// the gateway can say, and the two scopes the shell offers -- conversation
// and files, or the conversation alone.
function confirmPanel(title, ask) {
  const panel = el("div", "confirm");
  panel.append(el("p", "title", title));
  if (ask.files === "loading") panel.append(el("p", "files", t("Reading the files it would restore…")));
  else if (Array.isArray(ask.files)) {
    const n = ask.files.length;
    const names = `${ask.files.slice(0, 3).join(", ")}${n > 3 ? ` +${n - 3}` : ""}`;
    panel.append(el("p", "files", !n ? t("No files to restore.")
      : n === 1 ? t("Restores {n} file: {names}", { n, names })
      : t("Restores {n} files: {names}", { n, names })));
  }
  if (ask.error) panel.append(el("p", "error", ask.error));
  const actions = el("div", "actions");
  const go = el("button", "solid", ask.go || t("Rewind"));
  const conversation = el("button", "quiet", t("Conversation only"));
  const cancel = el("button", "word", t("Cancel"));
  [[go, "go"], [conversation, "conversation"], [cancel, "cancel"]].forEach(([button, act]) => {
    button.type = "button";
    button.dataset.act = act;
    button.disabled = !!ask.busy && act !== "cancel";
  });
  actions.append(go, conversation, cancel);
  panel.append(actions);
  return panel;
}

const sessionPath = (session) => `/api/gateway/sessions/${encodeURIComponent(session)}`;

async function checkpointsOf(session) {
  const listing = await api("GET", `${sessionPath(session)}/checkpoints?limit=64`);
  return listing.checkpoints || [];
}

// Rewinding a turn takes the session back to before its prompt (the /undo
// move, aimed at any turn): the turns from it on stay in the tree as a branch.
async function askRewind(key, turnId, number) {
  const session = state.current;
  state.rewind = { session, key, turnId, number, files: "loading" };
  schedule();
  let files = null;
  try {
    const entry = (await checkpointsOf(session)).find((each) => each.kind === "user_message" && each.turn_id === turnId);
    if (entry?.checkpoint_id) {
      files = (await api("GET", `${sessionPath(session)}/checkpoints/${encodeURIComponent(entry.checkpoint_id)}/files`)).files || [];
    }
  } catch {}
  if (state.rewind?.key === key) {
    state.rewind = { ...state.rewind, files };
    schedule();
  }
}

// PROMPT, the row of the turn a rewind takes back, rides the ask: its
// pictures go back in the box with its words.
function navigate(fields, prompt) {
  const id = command("navigate_checkpoint", { session_id: state.current, ...fields });
  state.sent.set(id, { navigate: true, session: state.current, redo: fields.direction === "redo", prompt });
}

function answerRewind(act) {
  const ask = state.rewind;
  if (!ask) return;
  if (act === "cancel") { state.rewind = null; schedule(); return; }
  state.rewind = { ...ask, busy: true, error: null };
  schedule();
  const turn = turnsOf(view(ask.session)).turns.find((each) => each.key === ask.key);
  navigate({ target_turn_id: ask.turnId, boundary: "user_message", ...(act === "conversation" ? { restore_files: false } : {}) },
    turn?.prompt && { row: turn.prompt, number: ask.number });
}

// The history sheet: every turn the session holds, branches included, in the
// gateway's tree order; the shell's marks -- ● the head, ○ on its line, ◇ a
// branch off it. Going to a turn makes it the head, answer and all.
async function openHistory() {
  const session = state.current;
  if (!session) return;
  $("evict").hidden = true;
  state.history = { session, entries: null };
  renderHistory();
  try {
    const entries = (await checkpointsOf(session)).filter((each) => each.kind === "user_message");
    if (state.history?.session === session) state.history = { ...state.history, entries };
  } catch (error) {
    if (state.history?.session === session) state.history = { ...state.history, entries: [], error: error.message };
  }
  renderHistory();
}

function closeHistory() {
  state.history = null;
  renderHistory();
}

function renderHistory() {
  const sheet = $("history");
  const h = state.history;
  sheet.hidden = !h || h.session !== state.current;
  if (sheet.hidden) return;
  const list = $("history-list");
  if (!h.entries) { list.replaceChildren(el("li", "none", t("Reading the tree…"))); return; }
  if (!h.entries.length) { list.replaceChildren(el("li", "none", h.error || t("No turns yet."))); return; }
  // A reflection's recap is its note on the turn before it, not a turn: it
  // leaves the list, and the head it sits on is that turn's. A wake ran a
  // round of its own and stays, saying what it is.
  const shown = h.entries.filter((entry) => entry.origin !== "reflection");
  const at = h.entries.findIndex((entry) => entry.is_head);
  const here = at < 0 ? null
    : h.entries.slice(0, at + 1).reverse().find((entry) => entry.origin !== "reflection" && entry.on_head_chain)?.turn_id;
  list.replaceChildren(...shown.map((entry) => {
    // A turn no one asked says what it is, has no number, and is not a turn
    // to go to.
    const said = ORIGIN_WORDS[entry.origin];
    const head = entry.turn_id === here;
    const li = el("li", [head ? "head" : entry.on_head_chain ? "" : "branch", said && "lst-note"].filter(Boolean).join(" "));
    const pick = el("button", "pick");
    pick.type = "button";
    pick.dataset.turn = entry.turn_id;
    pick.disabled = !!said;
    pick.style.paddingLeft = `${18 + (entry.depth || 0) * 16}px`;
    pick.append(
      el("span", "mark", head ? "●" : entry.on_head_chain ? "○" : "◇"),
      el("span", "name", said || oneLine(entry.prompt) || t("(no prompt)")),
      el("span", "reading", head ? t("here") : said ? "" : t("turn {n}", { n: entry.turn_number ?? "?" })),
    );
    li.append(pick);
    if (h.pick === entry.turn_id) {
      const n = entry.turn_number ?? "?";
      li.append(confirmPanel(entry.on_head_chain ? t("Go to turn {n}?", { n }) : t("Go to turn {n} on its branch?", { n }),
        { busy: h.busy, error: h.error, go: t("Go there") }));
    }
    return li;
  }));
}

function answerHistory(act) {
  const h = state.history;
  if (!h?.pick) return;
  if (act === "cancel") { state.history = { ...h, pick: null, error: null }; renderHistory(); return; }
  state.history = { ...h, busy: true, error: null };
  renderHistory();
  navigate({ target_turn_id: h.pick, boundary: "turn", ...(act === "conversation" ? { restore_files: false } : {}) });
}

// Where a move landed: the transcript arrives again on its own (the gateway's
// resync); the prompt a rewind took back returns to the box, as in the shell.
function navigated(result, sent) {
  const response = result.response || {};
  const session = sent?.session || result.session_id;
  if (result.status === "accepted") {
    state.rewind = null;
    state.history = null;
    if (sent?.redo) state.redo.delete(session); else state.redo.add(session);
    // Redo takes back the prompt the rewind handed over, if it is untouched.
    if (sent?.redo && state.undone && promptEl.value === state.undone) {
      promptEl.value = "";
      if (state.images.length && state.images.every((image) => state.undonePictures.includes(image))) {
        state.images.forEach((image) => URL.revokeObjectURL(image.url));
        state.images = [];
        renderAttachments();
      }
      fit();
    }
    state.undone = null;
    state.undonePictures = [];
    if (response.undone_prompt && !promptEl.value.trim()) {
      promptEl.value = state.undone = response.undone_prompt;
      fit();
      promptEl.focus();
      if (sent?.prompt && !state.images.length) putBackPictures(session, sent.prompt);
    }
    if (response.workspace === "degraded") {
      toast(t("The conversation moved; the files did not: {why}", { why: (response.degradation_reason || t("restore failed")).replace(/_/g, " ") }), { error: true });
    } else if (response.conflicts) {
      const n = response.conflicts;
      toast(n === 1 ? t("{n} file was changed outside the session and left as it is", { n })
        : t("{n} files were changed outside the session and left as they are", { n }));
    }
  } else if (result.status === "rejected") {
    const message = result.message || result.code || t("refused");
    if (state.rewind) state.rewind = { ...state.rewind, busy: false, error: message };
    else if (state.history?.pick) state.history = { ...state.history, busy: false, error: message };
    else toast(message, { error: true });
    if (sent?.redo) state.redo.delete(session);
  }
  renderHistory();
  renderHead();
  schedule();
}

// The pictures the rewound prompt was sent with, back in the box beside its
// words; one no longer on disk is said, never dropped quietly.
async function putBackPictures(session, { row, number }) {
  const { images, lost } = await promptPictures(session, row);
  if (state.current !== session || state.undone !== promptEl.value) {
    images.forEach((image) => URL.revokeObjectURL(image.url));
    return;
  }
  state.images.push(...images);
  state.undonePictures = images;
  renderAttachments();
  if (lost) {
    toast(lost === 1 ? t("The picture sent with turn {n} is no longer on disk: attach it again", { n: number })
      : t("{count} pictures sent with turn {n} are no longer on disk: attach them again", { count: lost, n: number }), { error: true });
  }
}

function renderTranscript() {
  const v = state.current ? state.views.get(state.current) : null;
  renderBand();
  if (state.shown !== state.current) {
    rowsEl.replaceChildren();
    state.drawn.clear();
    state.shown = state.current;
    following = true;
  }
  $("opener").hidden = !!state.current;
  if (!state.current) return;
  if (!v) return;
  if (state.arriving) following = true;
  const { loose, turns } = turnsOf(v);
  const groups = [];
  if (loose.length) groups.push({ key: "loose", sig: loose, draw: () => {
    const block = el("div", "turn");
    loose.forEach((row) => block.append(looseRow(row)));
    return block;
  } });
  // The last turn the operator asked -- a recorded recap after it is no one's.
  const asked = turns.findLastIndex((turn) => turn.prompt && !turn.prompt.recorded);
  // The turns the operator asked are numbered in a row, as History and the Run
  // view number them: a recap or a wake between two of them is counted by no one.
  let number = 0;
  turns.forEach((turn, index) => {
    // A reflection's recap is the machine's note to itself, not the reader's:
    // the transcript draws none, as the TUI's Standard viewport draws none.
    // History leaves it out too; the Run view keeps it.
    if (turn.prompt?.recorded === "reflection") return;
    const live = !!v.turn?.active && index === turns.length - 1;
    const nth = turn.prompt && turn.prompt.recorded !== "reflection" ? ++number : 0;
    groups.push({
      key: turn.key,
      sig: [turn.prompt, ...turn.rows, live, live && v.turn.verb, index,
        state.rewind?.key === turn.key ? state.rewind : null, index === asked, nth],
      draw: () => drawTurn(turn, nth, live, v, index === asked),
    });
  });
  const keep = new Set(groups.map((group) => group.key));
  for (const [key, drawn] of state.drawn) {
    if (!keep.has(key)) { drawn.el.remove(); state.drawn.delete(key); }
  }
  let before = null;
  for (let index = groups.length - 1; index >= 0; index--) {
    const group = groups[index];
    let drawn = state.drawn.get(group.key);
    const same = drawn && drawn.sig.length === group.sig.length && drawn.sig.every((part, at) => part === group.sig[at]);
    if (!same) {
      const built = group.draw();
      if (drawn) drawn.el.replaceWith(built);
      drawn = { el: built, sig: group.sig };
      state.drawn.set(group.key, drawn);
    }
    if (drawn.el.nextSibling !== before || drawn.el.parentNode !== rowsEl) rowsEl.insertBefore(drawn.el, before);
    before = drawn.el;
  }
  follow();
  state.arriving = false;
}

// A reader at the transcript's end stays at it whatever grows or moves after
// the rows are drawn -- a picture that loads, the box getting taller, a turn's
// marks back when it ends -- and one who scrolled away from it gets the way
// back. Where the reader is comes from their own scrolling, never from a
// measure taken before the page settles: one taken then left the answer that
// had just ended under the box, with no way back shown. One distance decides
// both.
const NEAR_END = 80;
let following = true;

function follow() {
  if (following && !ledger.hidden) ledger.scrollTop = ledger.scrollHeight;
  placeJump();
}

function toEnd() {
  following = true;
  follow();
}

function placeJump() {
  $("jump").hidden = ledger.hidden || !state.current || following;
}

const settling = new ResizeObserver(follow);
settling.observe(rowsEl);
settling.observe(ledger);

// --- the nodecode.ai account's meter ----------------------------------------------------------

// The organism reads the account's usage from api.nodecode.ai with this home's key
// (/api/gateway/account, nodecode's surface/account-login.lisp) and this chip draws its badge:
// the plan's name, then its requests left while the plan pays or the credit once it does, on a
// chip of the meter's tone. Hidden on a home that is not signed in. Its title carries the lines
// /usage prints; a click opens the account.
let accountRead = null;
async function loadAccount() {
  if (accountRead) return accountRead;
  accountRead = (async () => {
    try {
      const account = await api("GET", "/api/gateway/account");
      const chip = $("account");
      const meter = account?.meter;
      chip.hidden = !meter;
      chip.replaceChildren(...(meter ? [["k", meter.label], ["v", meter.value]] : []).map(([part, text]) => {
        const span = document.createElement("span");
        span.className = part;
        span.textContent = text || "";
        return span;
      }));
      chip.title = [t("nodecode.ai account"), ...(account?.lines || [])].join("\n");
      if (meter?.tone) chip.dataset.tone = meter.tone; else delete chip.dataset.tone;
    } catch {} finally { accountRead = null; }
  })();
  return accountRead;
}

// --- the model a turn runs on ------------------------------------------------------------

// The target the next turn runs on, as the gateway resolves it for the
// session on screen -- or, before a session exists, the default, or the model
// the opener's pick aims the new session at, with its reasoning pick laid over it.
async function loadTarget() {
  const session = state.current;
  const aim = session ? null : state.aim;
  const query = session ? `?session_id=${encodeURIComponent(session)}`
    : aim?.model ? `?provider=${encodeURIComponent(aim.provider)}&model=${encodeURIComponent(aim.model)}` : "";
  try {
    const target = await api("GET", `/api/gateway/target${query}`);
    if (state.current !== session || (!session && aim !== state.aim)) return;
    if (aim) aimAt(target, aim);
    state.target = target;
    renderModel();
  } catch {}
}

// The opener's picks drawn as the session's own: the model as its pin, and a
// reasoning pick (Default, the model's own, among them) while the model aimed
// at still offers that rung; without one the default effort shows.
function aimAt(target, aim) {
  if (aim.model) target.pin = { provider: aim.provider, model: aim.model };
  if (aim.effort && !target.efforts?.includes(aim.effort)) delete aim.effort;
  if ("effort" in aim) target.effort = aim.effort;
}

const effortOf = (target) => target?.effort || null;

// Whether the target reasons at all: a model with no rungs has no effort to pick.
const reasons = (target) => !!target?.efforts?.length;

const effortLabel = (target) => effortOf(target) || t("Default");

const pinned = (target) => !!(target?.pin?.provider || target?.pin?.model);

function renderModel() {
  const aim = state.target;
  const chip = $("model");
  if (!aim?.model) chip.textContent = t("Choose a model");
  else chip.replaceChildren(aim.model, ...(reasons(state.target) ? [el("span", "tone", ` · ${effortLabel(state.target)}`)] : []));
  if (!$("models").hidden) renderModels();
}

// The model and its reasoning are two choices, not one sheet: the chip opens
// a pair of rows -- Model, Reasoning -- and each goes into its own list. Esc
// comes back out one level; a choice closes it.
async function openModels() {
  const sheet = $("models");
  // Anchored at the chip wherever the composer stands: the dock, or the opener.
  const slab = document.querySelector(".slab").getBoundingClientRect();
  const box = composer.getBoundingClientRect();
  sheet.style.setProperty("--anchor", `${Math.max(20, $("model").getBoundingClientRect().left - slab.left - 18)}px`);
  sheet.style.setProperty("--above", `${slab.bottom - box.top + 8}px`);
  sheet.style.setProperty("--room", `${box.top - slab.top - 16}px`);
  closeSlash();
  sheet.hidden = false;
  showPane("root");
  if (!state.models) {
    try { state.models = await api("GET", "/api/gateway/models"); }
    catch (error) { state.models = { models: [], unlisted: [], error: error.message }; }
    renderModels();
  }
}

function showPane(pane) {
  const sheet = $("models");
  sheet.dataset.pane = pane;
  for (const name of ["root", "model", "effort"]) $(`pane-${name}`).hidden = name !== pane;
  if (pane === "model") $("models-filter").value = "";
  renderModels();
  if (pane === "model") $("models-filter").focus();
  else {
    const shown = $(`pane-${pane}`);
    (shown.querySelector("[aria-checked='true']") || shown.querySelector(".cell, .pick"))?.focus();
  }
}

function closeModels(restoreFocus = false) {
  if ($("models").hidden) return;
  $("models").hidden = true;
  if (restoreFocus) $("model").focus();
}

// One row of a list in the sheet: a radio, checked when ON.
function pickRow(on, name, data) {
  const li = el("li", on ? "head" : "");
  const pick = el("button", "pick");
  pick.type = "button";
  pick.setAttribute("role", "radio");
  pick.setAttribute("aria-checked", String(on));
  Object.assign(pick.dataset, data);
  pick.append(el("span", "mark", on ? "●" : "○"), el("span", "name", name));
  li.append(pick);
  return li;
}

function renderModels() {
  const pane = $("models").dataset.pane;
  const target = state.target;
  const aim = target;
  const fallback = target?.default;
  const standing = fallback?.model ? `${fallback.provider}/${fallback.model}` : t("None");
  if (pane === "root") {
    // Where a pick lands, said where it is made: this session, or the one the
    // first message opens -- and the default every later session starts on.
    $("pickscope-head").textContent = state.current ? t("This session") : t("The new session");
    $("model-now").textContent = !aim?.model ? t("None")
      : pinned(target) ? `${aim.provider}/${aim.model}` : t("Default ({model})", { model: aim.model });
    $("effort-cell").hidden = !reasons(target);
    $("effort-now").textContent = effortLabel(target);
    $("pickscope-note").textContent = state.current
      ? t("A pick here changes this session and becomes the default for new sessions. Sessions already begun keep their own model.")
      : t("A pick here goes to the session your first message opens and becomes the default for new sessions.");
  } else if (pane === "effort") {
    const now = effortOf(target);
    const rows = (target?.efforts || []).map((rung) => pickRow(rung === now, rung, { effort: rung }));
    $("effort-list").replaceChildren(pickRow(!now, t("Default (the model's own)"), { effort: "" }), ...rows);
  } else {
    renderCatalog(aim, pinned(target), standing);
  }
}

// ON: the target is this session's own pick, not the default it follows.
function renderCatalog(aim, on, standing) {
  const list = $("models-list");
  if (!state.models) { list.replaceChildren(el("li", "none", t("Reading the catalog…"))); return; }
  const needle = $("models-filter").value.trim().toLowerCase();
  const hits = state.models.models.filter((row) => !needle
    || `${row.provider}/${row.model} ${row.name || ""}`.toLowerCase().includes(needle));
  const items = hits.slice(0, 200).map((row) => {
    const here = on && aim && row.provider === aim.provider && row.model === aim.model;
    const li = el("li", here ? "head" : "");
    const pick = el("button", "pick");
    pick.type = "button";
    pick.setAttribute("role", "radio");
    pick.setAttribute("aria-checked", String(here));
    pick.dataset.provider = row.provider;
    pick.dataset.model = row.model;
    pick.append(el("span", "mark", here ? "●" : "○"), el("span", "name", `${row.provider}/${row.model}`),
      el("span", "reading", row.context ? `${Math.round(row.context / 1000)}k` : ""));
    if (row.name) pick.title = row.name;
    li.append(pick);
    return li;
  });
  for (const row of needle ? [] : state.models.unlisted) {
    items.push(el("li", "none", t("{provider}/ — no listing ({why})", { provider: row.provider, why: row.reason })));
  }
  if (!items.length) items.push(el("li", "none", state.models.error || t("Nothing matches.")));
  // Following the default is a choice of its own, first, while nothing is typed.
  if (!needle) items.unshift(pickRow(!on, t("Default ({model})", { model: standing }), { model: "", provider: "" }));
  list.replaceChildren(...items);
}

// SESSION is the one on screen when the pick was made, so a Retry after a switch
// still lands on it; with none the pick waits for the session the first
// message opens. An empty MODEL is the default: the session follows it again.
async function chooseModel(provider, model, session = state.current) {
  if (!session) {
    state.aim = { ...state.aim, provider: model ? provider : null, model: model || null };
    closeModels();
    promptEl.focus();
    await loadTarget();
    return;
  }
  try {
    await api("POST", `/api/gateway/sessions/${encodeURIComponent(session)}/model`, model ? { provider, model } : { provider: null, model: null });
    closeModels();
    promptEl.focus();
    await loadTarget();
  } catch (error) {
    toast(t("Couldn't switch to {model}: {why}", { model: model || t("the default"), why: error.message }), { error, retry: () => chooseModel(provider, model, session) });
  }
}

// An empty VALUE is Default: the pick is cleared, and the model's own reasoning runs.
async function chooseEffort(value, session = state.current) {
  closeModels();
  promptEl.focus();
  if (session === state.current && (value || null) === effortOf(state.target)) return;
  if (!session) {
    state.aim = { ...state.aim, effort: value || null };
    await loadTarget();
    return;
  }
  try {
    const target = await api("POST", `/api/gateway/sessions/${encodeURIComponent(session)}/effort`, { effort: value || null });
    if (session === state.current && target?.model) { state.target = target; renderModel(); }
  } catch (error) {
    toast(t("Couldn't set reasoning to {value}: {why}", { value: value || t("Default"), why: error.message }), { error, retry: () => chooseEffort(value, session) });
  }
}

// The opener's picks, landed on the session its first message just opened,
// before that message is sent. => false when the gateway refused one.
async function landAim(session) {
  const aim = state.aim;
  if (!aim) return true;
  try {
    if (aim.model) await api("POST", `/api/gateway/sessions/${encodeURIComponent(session)}/model`, { provider: aim.provider, model: aim.model });
    if ("effort" in aim) await api("POST", `/api/gateway/sessions/${encodeURIComponent(session)}/effort`, { effort: aim.effort });
    state.aim = null;
    loadTarget();
    return true;
  } catch (error) {
    toast(t("Couldn't open the session on your pick: {why}", { why: error.message }), { error: true });
    return false;
  }
}

// --- the context window ------------------------------------------------------------------

const tokensText = (n) => (n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1000 ? `${Math.round(n / 1000)}k` : String(n));

function renderContext(v) {
  const c = v?.context;
  const button = $("context");
  button.hidden = !c || !!v.turn?.active;
  if (!c) { $("evict").hidden = true; return; }
  const used = tokensText(c.used);
  const size = c.window && tokensText(c.window);
  const pct = c.window ? Math.min(100, Math.round((100 * c.used) / c.window)) : 0;
  const said = c.window ? t("{pct}% context", { pct }) : t("{used} context", { used });
  button.querySelector(".label").textContent = said;
  button.querySelector(".fill").setAttribute("stroke-dasharray", `${(33 * pct) / 100} 33`);
  button.toggleAttribute("data-full", pct >= 80);
  button.title = !c.window ? said
    : `${said} · ${c.model ? t("{used} of {window} on {model}", { used, window: size, model: c.model })
      : t("{used} of {window}", { used, window: size })}`;
}

function openEvict() {
  const sheet = $("evict");
  const v = state.views.get(state.current);
  const c = v?.context;
  const panel = el("div", "confirm");
  panel.append(el("p", "title", t("Drop the oldest half of this session's context?")));
  if (c) panel.append(el("p", "files", c.window
    ? t("{used} of {window} in use. The turns stay in the transcript, dimmed; the model stops seeing them.", { used: tokensText(c.used), window: tokensText(c.window) })
    : t("{used} in use. The turns stay in the transcript, dimmed; the model stops seeing them.", { used: tokensText(c.used) })));
  const actions = el("div", "actions");
  const go = el("button", "solid", t("Evict"));
  go.type = "button";
  go.dataset.act = "evict";
  const cancel = el("button", "word", t("Cancel"));
  cancel.type = "button";
  cancel.dataset.act = "cancel";
  actions.append(go, cancel);
  panel.append(actions);
  sheet.replaceChildren(panel);
  sheet.hidden = false;
}

async function evict(session = state.current) {
  $("evict").hidden = true;
  try {
    const out = await api("POST", `${sessionPath(session)}/evict`, {});
    const n = out.evicted_turn_count;
    toast(n === 1 ? t("Dropped {n} turn from the context", { n }) : t("Dropped {n} turns from the context", { n }));
  } catch (error) {
    if (error.status === 409) toast(error.message);
    else toast(t("Couldn't drop turns from the context: {why}", { why: error.message }), { error, retry: () => evict(session) });
  }
}

// --- a session of its own from a turn -----------------------------------------------------

async function forkAt(turnId, parent = state.current) {
  const id = `s-${Math.floor(Math.random() * 36 ** 8).toString(36).padStart(8, "0")}`;
  try {
    const entry = await api("POST", "/api/gateway/sessions", { session_id: id, parent, anchor_turn_id: turnId });
    const from = sessionName(state.sessionsById.get(parent));
    const depth = (state.sessionsById.get(parent)?.depth || 0) + 1;
    const standby = { id, title: t("fork: {name}", { name: from }), working_directory: entry?.cwd || "", updated_at: new Date().toISOString(), standby: true, parent_id: parent, depth };
    state.sessionsById.set(id, standby);
    state.sessions = [standby, ...state.sessions.filter((each) => each.id !== id)];
    select(id);
  } catch (error) {
    toast(t("Couldn't fork the session: {why}", { why: error.message }), { error, retry: () => forkAt(turnId, parent) });
  }
}

// --- search through what the sessions said --------------------------------------------------

let searchTimer = null;
function searchSoon() {
  clearTimeout(searchTimer);
  const query = state.filter.trim();
  if (query.length < 3) { state.found = []; renderSessions(); return; }
  searchTimer = setTimeout(async () => {
    try {
      const out = await api("POST", "/api/gateway/sessions/search", { query, limit: 40 });
      if (state.filter.trim() !== query) return;
      const seen = new Set();
      state.found = (out.rows || []).filter((row) => !seen.has(row.session_id) && seen.add(row.session_id));
    } catch { state.found = []; }
    renderSessions();
  }, 300);
}

// --- what rides a prompt: images, and any other file as its path ------------------------------
// An image the model can see rides the prompt itself. Any other file goes up
// through the Files view's route (surface/files.lisp) into the session's
// folder, and the prompt carries its path, which the model reads with its own
// verbs. Nothing goes up until Send: the chip names where the file will land,
// and a chip removed before then leaves nothing behind -- one removed on its
// way up stops it. It never replaces a file there: a name already taken goes
// in as name-2.ext, name-3.ext, and the chip says so before it is sent; a name
// taken since, found at Send, holds the send for the chip to say it first.
// The paperclip, a paste and a drop all come here, and what cannot come along
// says why.

const IMAGE_TYPES = ["image/png", "image/jpeg", "image/gif", "image/webp"];
const IMAGE_BUDGET = 10 * 1024 * 1024; // raw bytes; a sync frame holds 16 MiB of base64

function attach(files) {
  let budget = IMAGE_BUDGET - state.images.reduce((sum, image) => sum + image.size, 0);
  for (const file of files) {
    if (!IMAGE_TYPES.includes(file.type)) { addFile(file); continue; }
    if (file.size > budget) { toast(t("That image would make the prompt too large to send"), { error: true }); continue; }
    budget -= file.size;
    const image = { media_type: file.type, size: file.size, url: URL.createObjectURL(file), data: null };
    state.images.push(image);
    const reader = new FileReader();
    reader.onload = () => { image.data = String(reader.result).split(",")[1]; renderComposer(); };
    reader.readAsDataURL(file);
  }
  renderAttachments();
}

function addFile(file) {
  const entry = { file, name: file.name, size: file.size, status: "waiting", loaded: 0 };
  state.files.push(entry);
  if (state.current) placeFiles([entry], state.current);
}

// The folder the box's files go into: the session's, or the one the first
// message will open it in.
const composerFolder = () => (state.current ? state.sessionsById.get(state.current)?.working_directory : state.folder) || "";

// The name to try once NAME is taken, the Nth time: notes.txt, notes-2.txt.
function nthName(name, n) {
  const dot = name.lastIndexOf(".");
  return dot > 0 ? `${name.slice(0, dot)}-${n}${name.slice(dot)}` : `${name}-${n}`;
}

// Where FILES will land in SESSION's folder, read off its listing: each one's
// own name, or the first name-N.ext nothing there and no other chip holds.
// SENDING, the read at Send: => true when a chip now names another place
// than the one it showed.
async function placeFiles(files, session, sending) {
  let listing = null;
  try { listing = await api("GET", `/api/gateway/sessions/${encodeURIComponent(session)}/files?path=`); } catch { return false; }
  const taken = new Set((listing.entries || []).map((each) => each.name));
  let moved = false;
  for (const entry of files) {
    if (entry.status !== "waiting") continue;
    const held = new Set(state.files.filter((each) => each !== entry && each.folder).map((each) => each.name));
    let name = entry.file.name;
    for (let n = 2; taken.has(name) || held.has(name); n++) name = nthName(entry.file.name, n);
    entry.moved = !!sending && name !== entry.name;
    moved ||= entry.moved;
    Object.assign(entry, { name, folder: listing.folder, taken: name !== entry.file.name });
  }
  renderAttachments();
  return moved;
}

// ENTRY's file put into SESSION's folder under the name its chip shows, the
// chip counting the bytes as they go; its x stops it. => a promise that
// settles once it is up, refused, or stopped, kept on the entry.
function putUp(entry, session) {
  const stop = new AbortController();
  Object.assign(entry, { status: "going", loaded: 0, why: null, refused: false, stop: () => stop.abort() });
  renderAttachments();
  entry.going = (async () => {
    const tried = new Set();
    let name = entry.name;
    for (;;) {
      tried.add(name);
      const result = await putFile({ token: state.token, session, dir: "", name, file: entry.file, signal: stop.signal }, (loaded) => {
        entry.loaded = loaded;
        paintFile(entry);
      });
      if (result.ok) {
        Object.assign(entry, { status: "up", name, folder: result.data.folder, path: `${result.data.folder}/${name}` });
        break;
      }
      if (result.stopped) { entry.status = "stopped"; break; }
      // Taken in the moment since the chip read the folder: the next free name.
      if (result.code === "file_exists" || result.code === "is_folder") {
        for (let n = 2; tried.has(name); n++) name = nthName(entry.file.name, n);
        continue;
      }
      // A refusal says what is wrong with the file; only one the route never
      // answered is worth sending again, from the chip.
      Object.assign(entry, { status: "failed", why: result.message, refused: !!result.code });
      break;
    }
    renderAttachments();
  })();
  return entry.going;
}

// The paths of the files riding a prompt to SESSION once every one is up;
// null when one is not -- refused, or its chip removed on the way -- and the
// send holds, the box keeping the words and the rest.
async function filesUp(files, session) {
  await Promise.all(files.map((entry) => (entry.status === "up" ? null : putUp(entry, session))));
  return files.every((entry) => entry.status === "up" && !entry.gone) ? files.map((entry) => entry.path) : null;
}

// Every image read and no file on its way up or refused.
const attachedReady = () => state.images.every((image) => image.data)
  && state.files.every((entry) => entry.status === "up" || entry.status === "waiting");

// The line under the chips: why what rides the box keeps it from sending, or
// that a send held for a name taken since the chip was read; else "".
function attachWhy() {
  const failed = state.files.find((entry) => entry.status === "failed");
  if (failed) {
    return failed.refused ? t("{name} was refused: {why}. Remove it to send.", { name: failed.file.name, why: failed.why })
      : t("Retry or remove {name} to send", { name: failed.file.name });
  }
  const moved = state.files.find((entry) => entry.moved && entry.status === "waiting");
  return moved ? t("{name} is taken in {folder}, so this one goes in as {as}: send again", { name: moved.file.name, folder: folderName(moved.folder), as: moved.name }) : "";
}

function fileChip(entry, index) {
  const item = el("div", "attachment attach-file");
  const icon = el("span", "attach-icon");
  icon.innerHTML = `<svg viewBox="0 0 15 15" width="15" height="15" fill="none" aria-hidden="true"><path d="M3.5 1.5H8.5L11.5 4.5V13.5H3.5Z M8.5 1.5V4.5H11.5" stroke="currentColor" stroke-width="1.2" stroke-linejoin="round"/></svg>`;
  const words = el("span", "attach-words");
  words.append(el("span", "attach-name", entry.file.name), el("span", "attach-meta"));
  const drop = el("button", "remove", "×");
  drop.type = "button";
  drop.setAttribute("aria-label", t("Remove this file"));
  drop.dataset.file = String(index);
  item.append(icon, words, drop);
  entry.chip = item;
  paintFile(entry);
  return item;
}

// What ENTRY's chip says of it: where it will land, how much of it is up, or
// that it is not attached, with a Retry when sending it again can help.
function paintFile(entry) {
  const item = entry.chip;
  if (!item) return;
  const share = entry.size ? Math.floor((100 * entry.loaded) / entry.size) : 100;
  const where = `${folderName(entry.folder || composerFolder())}/${entry.name}`;
  item.dataset.status = entry.status;
  item.style.setProperty("--share", `${share}%`);
  item.title = entry.status === "failed" ? entry.why
    : entry.status === "up" ? home(entry.path)
    : [entry.taken ? t("{name} is already in {folder}: this one goes in as {as}", { name: entry.file.name, folder: folderName(entry.folder), as: entry.name })
      : entry.folder ? t("It goes into {path} when the message is sent", { path: home(`${entry.folder}/${entry.name}`) })
      : t("It goes into the session's folder when the first message opens the session"), bytes(entry.size)].join(" · ");
  const meta = item.querySelector(".attach-meta");
  if (entry.status === "failed") {
    meta.replaceChildren(t("not attached"));
    if (!entry.refused) {
      const again = el("button", "attach-retry", t("Retry"));
      again.type = "button";
      again.dataset.file = String(state.files.indexOf(entry));
      meta.append(" · ", again);
    }
  } else meta.textContent = entry.status === "going" ? t("uploading {share}%", { share }) : `→ ${where}`;
}

function renderAttachments() {
  const box = $("attachments");
  box.hidden = !state.images.length && !state.files.length;
  box.replaceChildren(...state.images.map((image, index) => {
    const item = el("div", "attachment");
    const view = el("button", "view");
    view.type = "button";
    view.setAttribute("aria-label", t("View this image"));
    view.dataset.index = String(index);
    const img = el("img");
    img.src = image.url;
    img.alt = "";
    view.append(img);
    const drop = el("button", "remove", "×");
    drop.type = "button";
    drop.setAttribute("aria-label", t("Remove this image"));
    drop.dataset.index = String(index);
    item.append(view, drop);
    return item;
  }), ...state.files.map(fileChip));
  const why = attachWhy();
  if (why) {
    const line = el("p", state.files.some((entry) => entry.status === "failed") ? "attach-why fail" : "attach-why", why);
    line.id = "attach-why";
    line.setAttribute("role", "status");
    box.append(line);
  }
  renderComposer();
}

function clearAttachments() {
  state.images.forEach((image) => URL.revokeObjectURL(image.url));
  state.images = [];
  state.files = [];
  renderAttachments();
}

// --- where the time and money went ---------------------------------------------------
// The gateway's two reads (surface/observe.lisp): the session's timeline feeds
// the strip's meter and the Run view, re-read when a turn ends and, while the
// Run view is up, as the turn runs; the usage and the activity across sessions,
// with the gateway's health, feed the Dashboard, and the week's usage the
// column's reading.

let timelineAsk = 0;
async function loadTimeline() {
  const session = state.current;
  if (!session) return;
  const ask = ++timelineAsk;
  try {
    const data = await api("GET", `${sessionPath(session)}/timeline`);
    if (ask !== timelineAsk || session !== state.current) return;
    if (state.timeline?.session !== session) state.runZoom = null;
    if (state.timeline?.session !== session && state.runOpen === null) state.runOpen = data.turns.at(-1)?.n ?? null;
    if (state.runWant) {
      state.runOpen = data.turns.find((turn) => turn.turn_id === state.runWant)?.n ?? state.runOpen;
      state.runWant = null;
    }
    state.timeline = { session, data };
    if (state.pane === "run") settleRun(data);
  } catch (error) {
    if (ask !== timelineAsk || session !== state.current) return;
    state.timeline = { session, data: null, error: error.message };
  }
  renderHead();
  renderObserve();
}

// An address may name a turn or a model call the session does not hold: the
// Run view opens on its last turn instead, and says so. Where it settles --
// the last turn, when the address named none -- is written in the address's place.
function settleRun(data) {
  const turnOf = (n) => data.turns.find((turn) => turn.n === n);
  if (state.runOpen != null && !turnOf(state.runOpen)) {
    const last = data.turns.at(-1)?.n ?? null;
    toast(last == null ? t("This session has no turn {n}.", { n: state.runOpen })
      : t("This session has no turn {n}, so turn {last} is open.", { n: state.runOpen, last }));
    state.runOpen = last;
    state.drawer = null;
  }
  const d = state.drawer;
  if (d && !turnOf(d.turn)?.calls[d.call]) {
    toast(t("Turn {n} made no model call {call}.", { n: d.turn, call: d.call + 1 }));
    state.drawer = null;
  }
  writeLink("replace");
}

let timelineSoon = null;
function loadTimelineSoon(delay) {
  clearTimeout(timelineSoon);
  timelineSoon = setTimeout(loadTimeline, delay);
}

// The body a call sent, as its bytes: read once per call the inspector's
// "What was sent" shows, parsed once, and kept for copy and download.
let requestAsk = 0;
async function loadRequest(session, turnId, round) {
  const key = `${session} ${turnId} ${round}`;
  if (state.request?.key === key) return;
  const ask = ++requestAsk;
  state.request = { key, status: "loading" };
  let next;
  try {
    const response = round == null ? { status: 404 }
      : await reach(`${sessionPath(session)}/request?turn=${encodeURIComponent(turnId)}&round=${round}`);
    if (response.status === 404) next = { key, status: "missing" };
    else if (!response.ok) throw new Error(`${response.status}`);
    else {
      const bytes = new Uint8Array(await response.arrayBuffer());
      const text = new TextDecoder().decode(bytes);
      const shared = response.headers.get("x-shared-bytes");
      let value;
      try { value = JSON.parse(text); } catch { value = undefined; }
      next = { key, status: "ok", bytes, text, value, shared: shared == null ? null : Number(shared),
        divergedIn: response.headers.get("x-diverged-in"), previous: response.headers.get("x-previous")?.split(" ") };
    }
  } catch (error) {
    next = { key, status: "error", error: error.message };
  }
  if (ask !== requestAsk) return;
  state.request = next;
  renderObserve();
}

// Each read lands where it stands: the page paints on the first and fills as
// the rest arrive, a range switched meanwhile dropping the older answers.
let dashboardAsk = 0;
function loadDashboard() {
  const range = state.range;
  const ask = ++dashboardAsk;
  const read = async (key, path) => {
    try {
      const data = await api("GET", path);
      if (ask !== dashboardAsk) return;
      state[key] = data;
      if (key === "usage" && range === "7d") $("usage-reading").textContent = data.totals.cost_usd != null ? usd(data.totals.cost_usd) : "";
    } catch (error) {
      if (ask !== dashboardAsk) return;
      state[key] = { range, error: error.message };
    }
    if (state.pane === "dashboard") renderObserve();
  };
  read("usage", `/api/gateway/usage?range=${encodeURIComponent(range)}`);
  if (state.pane !== "dashboard") return;
  read("activity", `/api/gateway/activity?range=${encodeURIComponent(range)}`);
  read("health", "/api/gateway/health");
  read("identity", "/api/gateway/identity");
}

// The panes over every session, or over the organism, stand in for the
// session's own and take the box away.
const board = () => state.pane === "dashboard" || state.pane === "control";

// The strip names where a board is, once: Dashboard, Control, or Control / the
// tab open in it, Control there the way back on a phone; the page under it
// says how it stands.
function paintPlace() {
  const title = $("title");
  if (!board() || title.isContentEditable) return;
  title.title = state.pane === "control" ? t("this organism") : t("every session");
  const tab = state.pane === "control" ? control.label : null;
  if (!tab) {
    title.textContent = state.pane === "control" ? t("Settings") : t("Dashboard");
    return;
  }
  const back = document.createElement("button");
  back.type = "button";
  back.className = "crumb";
  back.append(icon("back", 16), el("span", "crumb-word", t("Settings")));
  back.addEventListener("click", () => control.back());
  const sep = document.createElement("span");
  sep.className = "sep";
  sep.textContent = " / ";
  const here = document.createElement("span");
  here.className = "here";
  here.textContent = tab;
  title.replaceChildren(back, sep, here);
}

// The wide window's list (app.css: 1024 px and up), beside the rail: what is
// in the place the rail has lit. A session's views keep the column's Sessions;
// the Dashboard lists its sections, the one in sight lit; Settings lists every
// section under its group, saying how it stands, the open one lit. The rail
// lights Sessions for every view of a session.
function paintList() {
  const list = $("place");
  const shown = WIDE.matches && board();
  if (shown) $("sessions-tab").removeAttribute("aria-current");
  else $("sessions-tab").setAttribute("aria-current", "page");
  list.hidden = !shown;
  if (!shown) { list.replaceChildren(); return; }
  const dash = state.pane === "dashboard";
  const head = el("div", "place-head");
  head.append(el("h2", "", dash ? t("Dashboard") : t("Settings")),
    el("span", "reading", dash ? RANGES.find(([id]) => id === state.range)?.[1] || "" : ""));
  const rows = el("div", "place-rows");
  const row = (label, data, { mark, words = "", tone = "", open = false } = {}) => {
    const go = el("button", "place-row");
    go.type = "button";
    Object.assign(go.dataset, data);
    if (open) go.setAttribute("aria-current", "page");
    if (mark) go.append(icon(mark, 15));
    const stands = el("span", "reading", words);
    if (tone) stands.dataset.tone = tone;
    go.append(el("span", "name", label), stands);
    return go;
  };
  if (dash) rows.append(row(t("Overview"), { jump: "top" }), ...JUMPS.map(([id, label]) => row(label, { jump: id })));
  else for (const [label, sections] of control.places()) {
    rows.append(el("h3", "micro", label), ...sections.map((s) => row(s.label, { tab: s.id }, s)));
  }
  list.replaceChildren(head, rows);
  if (dash) spyList();
}

// The Dashboard's section in sight: the nearest whose top has passed under the
// sticky bar, lit in the wide window's list; above the first, the overview.
function spyList() {
  if (state.pane !== "dashboard" || $("place").hidden) return;
  const line = $("observe").getBoundingClientRect().top + 80;
  let lit = "top";
  let best = -Infinity;
  for (const [id] of JUMPS) {
    const top = $(`dash-${id}`)?.getBoundingClientRect().top;
    if (top !== undefined && top <= line && top > best) { best = top; lit = id; }
  }
  for (const go of $("place").querySelectorAll("[data-jump]")) {
    if (go.dataset.jump === lit) go.setAttribute("aria-current", "page");
    else go.removeAttribute("aria-current");
  }
}

const control = makeControl({
  api, download, toast, openSession: select, isOpen: () => state.open, folder: hereFolder,
  redraw: () => { renderObserve(); paintLink(); paintPlace(); }, onTab: () => { writeLink(); paintPlace(); },
  openTab: (tab) => { setPop(null); setPane("control", tab); },
});
// HOME is defined below, with the strip: read when drawn, not now.
const filesView = makeFiles({ state, reach, save, toast, redraw: renderObserve, moved: () => writeLink(), home: (dir) => home(dir), openImage: openLightbox });

// A session's pane takes over from the board.
function toChat() {
  if (state.pane === "control") control.hide();
  state.pane = "chat";
}

// TAB: the Control tab to open, when PANE is Control; the one it was on when
// none, or on a phone the list of them (control.moves says whether it moves).
function setPane(pane, tab) {
  if (pane === state.pane) {
    if (pane === "control" && control.moves(tab)) {
      control.show(tab);
      renderObserve();
      writeLink();
    }
    return;
  }
  if (state.pane === "control") control.hide();
  leaving();
  state.pane = pane;
  arrived();
  state.drawer = null;
  app.classList.remove("column-open");
  if (pane === "run") loadTimeline();
  if (pane === "dashboard") loadDashboard();
  if (pane === "control") control.show(tab);
  if (pane === "files") filesView.show();
  $("observe").scrollTop = 0;
  placeComposer();
  renderHead();
  renderSessions();
  renderObserve();
  // Back from a board, the chip reads again: the default may have moved there.
  if (pane === "chat") { schedule(); loadTarget(); }
  writeLink();
}

function renderObserve() {
  const pane = state.pane;
  ledger.hidden = pane !== "chat";
  placeJump();
  paintList();
  $("observe").hidden = pane === "chat";
  $("observe").setAttribute("aria-label", pane === "control" ? t("Settings") : pane === "files" ? t("The session's folder") : t("Where the time and money went"));
  const lane = $("observe-lane");
  const timeline = state.timeline?.session === state.current ? state.timeline : null;
  if (pane === "run") {
    lane.replaceChildren(drawRun(timeline?.data, { zoom: state.runZoom, open: state.runOpen, all: state.runAll, error: timeline?.error,
      phone: PHONE.matches, paged: state.runPaged, phoneOpen: state.phoneOpen }));
  } else if (pane === "dashboard") {
    // A read landing under an open sheet redraws it where the reader was.
    const read = lane.querySelector(".dash-sheet-body")?.scrollTop || 0;
    lane.replaceChildren(drawDashboard(state, state.range, home));
    state.dashEnter = false;
    const body = lane.querySelector(".dash-sheet-body");
    if (body) body.scrollTop = read;
  } else if (pane === "control") {
    lane.replaceChildren(control.draw());
  } else if (pane === "files") {
    lane.replaceChildren(filesView.draw());
  }
  const drawer = $("drawer");
  const inspect = pane === "run" && state.drawer && timeline?.data;
  drawer.hidden = !inspect;
  if (!inspect) return;
  const { turn: turnN, call: index, tab, open } = state.drawer;
  const turn = timeline.data.turns.find((each) => each.n === turnN);
  if (tab === "request" && turn?.calls[index]) loadRequest(state.current, turn.turn_id, turn.calls[index].round);
  // A redraw of the same call and tab keeps its place: unfolding a line deep
  // in a request must not throw the reader back to the top.
  const place = `${turnN} ${index} ${tab}`;
  const scrolled = drawer.dataset.place === place ? drawer.querySelector(".insp-body")?.scrollTop || 0 : 0;
  drawer.replaceChildren(drawInspector(timeline.data, turnN, index, tab, state.request, open));
  drawer.dataset.place = place;
  const body = drawer.querySelector(".insp-body");
  if (body) body.scrollTop = scrolled;
}

function answerObserve(act) {
  const turn = Number(act.dataset.turn);
  switch (act.dataset.act) {
    // On a phone a turn opens as a page and goes back by its way back; wider, a row opens and closes in place.
    case "turn":
      if (PHONE.matches) { state.runOpen = turn; state.runPaged = true; state.phoneOpen.clear(); $("observe-lane").scrollTop = 0; break; }
      state.runOpen = state.runOpen === turn ? null : turn;
      break;
    case "run-back": state.runPaged = false; break;
    case "call":
      state.runOpen = turn;
      if (PHONE.matches) state.runPaged = true;
      state.drawer = { turn, call: Number(act.dataset.call), tab: state.drawer?.tab || "summary", open: new Set() };
      break;
    case "fold": {
      const open = state.drawer.open;
      if (!open.delete(act.dataset.path)) open.add(act.dataset.path);
      break;
    }
    case "copy-request":
      navigator.clipboard.writeText(state.request.text).then(() => toast(t("Copied what was sent")), () => toast(t("Couldn't copy: the browser refused the clipboard"), { error: true }));
      return;
    case "download-request": {
      const link = document.createElement("a");
      link.href = URL.createObjectURL(new Blob([state.request.bytes], { type: "application/json" }));
      link.download = `request-${state.request.key.split(" ").slice(1).join("-")}.json`;
      link.click();
      setTimeout(() => URL.revokeObjectURL(link.href), 1000);
      return;
    }
    case "all-flags": state.runAll = true; break;
    case "close-drawer": state.drawer = null; break;
    case "zoom": {
      const open = openTurn();
      if (act.dataset.zoom === "fit") state.runZoom = null;
      else if (open) zoomBy(open, act.dataset.zoom === "in" ? 1 / 2 : 2);
      break;
    }
    case "session": state.dashSheet = null; select(act.dataset.id); return;
    // A Dashboard row: its session on the Run view, the turn it names open.
    case "run":
      state.dashSheet = null;
      select(act.dataset.id);
      state.runWant = act.dataset.turn || null;
      state.runOpen = null;
      state.runPaged = !!act.dataset.turn && PHONE.matches;
      setPane("run");
      return;
  }
  renderObserve();
  writeLink();
}

// --- the Run view's clock: zoom and move along it ---------------------------------------
// The open turn's window of time is state.runZoom, its whole span when null.
// Ctrl or Cmd with the wheel, and a trackpad's pinch (the same wheel, with
// Ctrl), zoom around the pointer; Shift with the wheel, a sideways swipe or a
// drag move along it; a double click fits the window to a bar; + - 0 and the
// arrows do the same on a focused timeline. A gesture redraws the timeline
// alone, once a frame.

const SHORTEST_MS = 5; // the narrowest window: a few ticks a millisecond apart

function openTurn() {
  return state.timeline?.data?.turns.find((turn) => turn.n === state.runOpen);
}

// => [from, to]: the window TURN's clock shows.
function runWindow(turn) {
  return state.runZoom?.n === turn.n ? [state.runZoom.from, state.runZoom.to] : [0, turnSpan(turn)];
}

// The window moved to FROM..TO: kept inside the turn, never narrower than
// SHORTEST_MS, and no zoom at all once it holds the whole turn.
function setWindow(turn, from, to) {
  const whole = turnSpan(turn);
  const span = Math.min(whole, Math.max(to - from, SHORTEST_MS));
  const start = Math.min(Math.max(from, 0), whole - span);
  state.runZoom = span >= whole ? null : { n: turn.n, from: start, to: start + span };
}

// FACTOR times as wide, the point SHARE of the way across held still.
function zoomBy(turn, factor, share = 0.5) {
  const [from, to] = runWindow(turn);
  const at = from + share * (to - from);
  setWindow(turn, at - share * (to - from) * factor, at + (1 - share) * (to - from) * factor);
}

function panBy(turn, share) {
  const [from, to] = runWindow(turn);
  setWindow(turn, from + share * (to - from), to + share * (to - from));
}

let clockFrame = 0;
function redrawClock() {
  cancelAnimationFrame(clockFrame);
  clockFrame = requestAnimationFrame(() => {
    const box = $("observe").querySelector(".timeline");
    const turn = openTurn();
    if (box && turn) box.replaceChildren(...drawTimeline(turn, { zoom: state.runZoom }).childNodes);
  });
}

// => [share, width]: how far across the open timeline's tracks the pointer
// is, 0 to 1, and how wide they are.
function clockShare(event) {
  const rect = $("observe").querySelector(".timeline .ticks").getBoundingClientRect();
  return [Math.min(1, Math.max(0, (event.clientX - rect.left) / rect.width)), rect.width];
}

let drag = null; // { turn, x, width, window, moved } while the timeline is dragged
let dragged = false; // the click that ends a drag is not a click
let barClick = 0; // a call bar's click, held until it is not a double click

// --- the strip, the column -----------------------------------------------------------

// A room's line opens on its tag -- `who (operator) [m123 u456]: ...' -- and a
// thread's lane is titled by that line, cut before its words. The thread is
// about its first ask, a session of its own under it with a title of its
// own; with none listed, the tag's speaker and words stand in.
const ROOM_TAG = /\s*\[(?=[mur]\d)(?:m\d+)?\s*(?:u\d*)?\s*(?:r\d*)?\]?:?\s*/;
const ownTitle = (entry) => entry?.title || entry?.semantic_summary?.title || "";

function sessionName(entry) {
  const name = ownTitle(entry);
  if (!name) return t("untitled");
  if (!ROOM_TAG.test(name)) return name;
  const ask = (state.kids.get(entry.id) || []).find((kid) => ownTitle(kid) && !ROOM_TAG.test(ownTitle(kid)));
  if (ask) return ownTitle(ask);
  const [who, said] = name.split(ROOM_TAG);
  return said ? `${who}: ${said}` : who;
}

const home = (dir) => (dir || "").replace(/^\/(?:home|Users)\/[^/]+/, "~");

// What a live turn is doing, from its verb and ROWS. While a call runs, its
// card names what it runs, and the verb says only that it does: the head once
// said `RUNNING FOR I IN $(SEQ 1 14)', a command in capitals, and the bar under
// the card said it a third time. A retry's countdown stays whole.
function liveVerb(verb, rows) {
  const calling = rows.some((row) => row.kind === "tool_call" && (row.tool?.status || (row.live ? "running" : "")) === "running");
  return calling ? t((verb || "").split(" ")[0] || "running") : verb || t("running");
}

function renderHead() {
  const entry = state.sessionsById.get(state.current);
  const v = state.current ? state.views.get(state.current) : null;
  const turns = v ? turnsOf(v).turns.length : 0;
  // A name being typed is the operator's until it is saved or put back.
  if (!$("title").isContentEditable) $("title").textContent = state.current ? sessionName(entry) : t("New session");
  $("title").title = state.current
    ? [state.current, home(entry?.working_directory), turns ? plural(turns, t("{n} turn"), t("{n} turns")) : ""].filter(Boolean).join(" · ")
    : "";
  renderTitle();
  const active = !!v?.turn?.active;
  const usage = board();
  app.dataset.pane = state.pane;
  app.toggleAttribute("data-busy", active);
  renderContext(v);
  $("title").toggleAttribute("data-renameable", listed());
  $("markdown").hidden = state.pane !== "chat" || !listed();
  if (usage) {
    paintPlace();
    $("context").hidden = true;
  }
  paintList();
  $("views").hidden = !state.current || usage;
  for (const sw of $("views").children) sw.setAttribute("aria-checked", String(sw.dataset.view === state.pane));
  const meter = !usage && state.timeline?.session === state.current ? meterOf(state.timeline.data) : null;
  const flagged = !!meter?.flag && !active;
  for (const run of [$("views").querySelector('[data-view="run"]'), $("run-open")]) {
    run.title = meter && !active ? `${t("Run")} · ${meter.text}` : t("Run");
    run.toggleAttribute("data-flagged", flagged);
  }
  // Chat and Run are the switch, Files an act; on a phone all three are under
  // More, the one on screen lit, and More carries Run's flag while Run is off screen.
  for (const act of $("head-acts").querySelectorAll("[data-view]")) {
    act.hidden = !state.current || usage;
    if (act.dataset.view === state.pane) act.setAttribute("aria-current", "page");
    else act.removeAttribute("aria-current");
  }
  $("more").toggleAttribute("data-flagged", flagged && state.pane !== "run");
  $("redo").hidden = state.pane !== "chat" || !state.current || active || !state.redo.has(state.current);
  $("history-open").hidden = state.pane !== "chat" || !state.current || active || !turns;
  const execs = v?.execs?.length || 0;
  $("status").hidden = !active && !execs;
  $("verb").textContent = active ? liveVerb(v.turn.verb, [...v.rows.values()].filter((row) => row.turn_id === v.turn.turn_id))
    : execs === 1 ? t("{n} exec running", { n: execs }) : t("{n} execs running", { n: execs });
  $("stop").hidden = !active;
  $("queue").hidden = !active;
  renderComposer();
  // More holds the acts a phone has no room for, and is there while one is.
  $("more").hidden = [...$("head-acts").children].every((act) => act.hidden);
  if ($("more").hidden) setActs(false);
  const dir = state.current ? entry?.working_directory : state.folder;
  $("folder").textContent = folderName(dir);
  $("folder").title = dir || "";
}

// The tab's title: how many sessions have news, whether the one on screen runs,
// and its name.
function renderTitle() {
  const entry = state.sessionsById.get(state.current);
  const v = state.current ? state.views.get(state.current) : null;
  const news = state.sessions.filter(unseen).length;
  const name = state.current ? `${v?.turn?.active || v?.execs?.length ? "● " : ""}${sessionName(entry)} · nodecode` : "nodecode";
  document.title = news ? `(${news}) ${name}` : name;
  $("unfold").toggleAttribute("data-news", news > 0);
}

// A folder by its own name, the last part of its path: the box's foot keeps
// its room for the model and the reasoning, and the whole path is the title.
const folderName = (dir) => home(dir).split(/[\\/]/).filter(Boolean).pop() || home(dir);

// On a phone the strip has no room for the session's acts -- Redo, Context,
// History, Markdown -- and they fold under More: the same buttons, as a menu.
function setActs(open) {
  $("head-acts").toggleAttribute("data-open", open);
  $("more").setAttribute("aria-expanded", String(open));
}

// The head's name is edited where it is read: a click makes it a field, Enter
// or leaving it saves through the title route, Esc puts it back. An emptied
// name gives back the first prompt's line, which the route answers. A session
// standing by is in no store yet: it has no name to change and nothing to save.
function listed() {
  const entry = state.sessionsById.get(state.current);
  return !!entry && !entry.standby && !board();
}

async function rename(id, title) {
  try {
    const answer = await api("POST", `${sessionPath(id)}/title`, { title });
    const entry = state.sessionsById.get(id);
    if (entry) entry.title = answer.title;
    renderSessions();
  } catch (error) {
    toast(t("Couldn't rename the session: {why}", { why: error.message }), { error, retry: () => rename(id, title) });
  }
  renderHead();
}

function since(iso) {
  const then = Date.parse(iso);
  if (!then) return "";
  const minutes = Math.round((Date.now() - then) / 60000);
  if (minutes < 1) return t("now");
  if (minutes < 60) return `${minutes}m`;
  const hours = Math.round(minutes / 60);
  return hours < 48 ? `${hours}h` : `${Math.round(hours / 24)}d`;
}

// --- what happened while the operator was elsewhere ------------------------------------
// A session has news when its log moved on (the listing's updated_at) since
// this browser last had it on screen: a turn that finished or failed, a wake,
// a scheduled job's fire. What each browser has seen is its own (SEEN_KEY):
// SINCE is the newest stamp of the first listing it ever read, so what came
// before counts as seen, and AT each session's own stamp as last read. The
// work a session hands off -- a team seat, a reflection -- is read
// under the session that ran it, and is never news of its own.
const QUIET_SOURCES = new Set(["team", "experience"]);
const stamp = (iso) => Date.parse(iso || "") || 0;

function readSeen() {
  try {
    const kept = JSON.parse(localStorage.getItem(SEEN_KEY) || "null");
    if (kept && typeof kept.at === "object" && kept.at) return { since: typeof kept.since === "string" ? kept.since : null, at: kept.at };
  } catch {}
  return { since: null, at: {} };
}

// What another tab of this browser read since this one last looked counts here too.
function mergeSeen() {
  const kept = readSeen();
  state.seen.since ||= kept.since;
  for (const [id, at] of Object.entries(kept.at)) if (stamp(at) > stamp(state.seen.at[id])) state.seen.at[id] = at;
}

function writeSeen() {
  try { localStorage.setItem(SEEN_KEY, JSON.stringify(state.seen)); } catch {}
}

// The session on screen is being read: its chat, Run or Files view, in a tab in view.
const reading = (id) => !!id && id === state.current && !board() && document.visibilityState === "visible";

function unseen(entry) {
  return !!entry && !entry.standby && !QUIET_SOURCES.has(sourceOf(entry)) && !state.running.has(entry.id)
    && !reading(entry.id) && stamp(entry.updated_at) > stamp(state.seen.at[entry.id] || state.seen.since);
}

// ID seen as far as the listing knows it.
function markSeen(id) {
  const at = state.sessionsById.get(id)?.updated_at;
  if (at && stamp(at) > stamp(state.seen.at[id])) state.seen.at[id] = at;
}

// A listing read: the first this browser ever read sets what counts as old;
// the session being read, and the one just left standing idle, are seen as
// far as it goes; one it no longer holds is forgotten.
function noteSeen(list) {
  mergeSeen();
  state.seen.since ||= list.reduce((top, entry) => (stamp(entry.updated_at) > stamp(top) ? entry.updated_at : top), new Date(0).toISOString());
  if (reading(state.current)) markSeen(state.current);
  if (state.left && !state.running.has(state.left)) markSeen(state.left);
  state.left = null;
  const listed = new Set(list.map((entry) => entry.id));
  for (const id of Object.keys(state.seen.at)) if (!listed.has(id)) delete state.seen.at[id];
  writeSeen();
}

// Before the screen moves off the session being read: it is seen as far as
// the listing knows it and, standing idle, as far as the next listing does
// too -- what its turn wrote as it ended may not be listed yet.
function leaving() {
  const id = state.current;
  if (!reading(id)) return;
  mergeSeen();
  markSeen(id);
  writeSeen();
  if (!state.running.has(id)) {
    state.left = id;
    loadSessionsSoon();
  }
}

// After it moves: the session now on screen is seen.
function arrived() {
  if (!reading(state.current)) return;
  mergeSeen();
  markSeen(state.current);
  writeSeen();
}

// The mark a row with news carries: a dot beside its age.
function newsDot() {
  const dot = el("i", "news-dot");
  dot.setAttribute("role", "img");
  dot.setAttribute("aria-label", t("Something new since you last looked"));
  dot.title = t("Something new since you last looked");
  return dot;
}

// --- the list folds as the shell's /sessions picker does -------------------------------
// A session's forks and team seats hang under it, drawn only once the
// session is opened (▸ / ▾, or Right and Left on its row), so the list reads as
// the sessions a person began, each holding what came off it. A tree belongs
// to the folder it began in, and is listed whole or not at all.

// The session ENTRY hangs under: its parent, when the listing holds it.
const parentOf = (entry) => {
  const up = entry?.parent_id;
  return up && up !== entry.id && state.sessionsById.has(up) ? up : null;
};

// Every session ID hangs under, nearest first; a cycle ends at its first repeat.
function ancestors(id) {
  const out = [];
  for (let up = parentOf(state.sessionsById.get(id)); up && up !== id && !out.includes(up); up = parentOf(state.sessionsById.get(up))) out.push(up);
  return out;
}

const treeFolder = (id) => state.sessionsById.get(ancestors(id).at(-1) || id)?.working_directory;

// The listing in the gateway's tree order, with the sessions this tab minted
// and has not yet spoken in: at the top, or right under the session they fork.
function listOrder() {
  const minted = state.sessions.filter((entry) => entry.standby);
  // A fork of a fork not yet spoken in hangs under it as it does under a session.
  const under = (id) => minted.filter((each) => parentOf(each) === id).flatMap((each) => [each, ...under(each.id)]);
  const out = minted.filter((entry) => !parentOf(entry)).flatMap((entry) => [entry, ...under(entry.id)]);
  for (const entry of state.sessions) {
    if (!entry.standby) out.push(entry, ...under(entry.id));
  }
  return out;
}

// The folder "This folder" means: the open session's tree's, or the one a new
// session is about to open in.
function hereFolder() {
  return state.current ? treeFolder(state.current) : state.folder;
}

// --- rooms, and the list's three bands -------------------------------------------
// A channel's chat keeps a lane of its own, the root its threads and asks hang
// under: a room. Its threads are the operator's conversations, so the list
// unfolds it: each thread stands as a row of its own beside the room's own
// lane, and the room's id names its platform (the channel kit's `discord-<chat>').
const PLATFORMS = { discord: "Discord", telegram: "Telegram" };

// Who hangs under whom, oldest first, and which roots are rooms, from the listing.
function indexTree() {
  state.kids = new Map();
  state.rooms = new Set();
  const byAge = [...state.sessions].sort((a, b) => stamp(a.created_at) - stamp(b.created_at));
  for (const entry of byAge) {
    const up = parentOf(entry);
    if (!up) continue;
    if (!state.kids.has(up)) state.kids.set(up, []);
    state.kids.get(up).push(entry);
  }
  for (const entry of state.sessions) {
    const top = sourceOf(entry) === "channel" ? ancestors(entry.id).at(-1) : null;
    if (top) state.rooms.add(top);
  }
}

// The room ID's session hangs in, or null.
const roomOf = (id) => (state.rooms.has(id) ? id : ancestors(id).find((up) => state.rooms.has(up)) || null);

// What ID hangs under that the list folds: every session over it but a room.
const folds = (id) => ancestors(id).filter((up) => !state.rooms.has(up));

// Where a session's conversation is held, the line under its name.
function whereOf(entry) {
  const room = roomOf(entry.id);
  if (room) {
    const kind = room.split("-")[0];
    return PLATFORMS[kind] || kind;
  }
  const kind = sourceOf(entry);
  if (entry.standby || kind === "gateway") return entry.working_directory ? `${t("Shell")} · ${folderName(entry.working_directory)}` : t("Shell");
  return sourceWord(kind);
}

function renderSessions() {
  if (state.reveal && state.sessionsById.has(state.reveal)) {
    for (const up of ancestors(state.reveal)) state.branches.add(up);
    state.reveal = null;
  }
  indexTree();
  const here = hereFolder();
  const inHere = here ? state.sessions.filter((entry) => treeFolder(entry.id) === here) : [];
  const scope = here ? state.scope : "all";
  // How many sessions hang right under each, and the ones a running turn
  // anywhere under them keeps busy while they run none of their own. What the
  // machine keeps for itself (isNote) is neither counted nor nested here.
  const own = (list) => list.filter((entry) => !isNote(entry));
  const held = new Map();
  const holding = new Set();
  for (const entry of own(state.sessions)) {
    const up = parentOf(entry);
    if (up) held.set(up, (held.get(up) || 0) + 1);
  }
  for (const id of state.running) if (!isNote(state.sessionsById.get(id))) for (const up of folds(id)) holding.add(up);
  // What each session is doing, its ball (the operator, 2026-10-21: green
  // done, orange waiting on you, red running): "run" while a turn runs in it
  // or under it, "ask" while its answer waits on the person it answers (the
  // gateway's `waits': marked by its turn, or a newest turn that failed),
  // else "done". A session folded away carries what waits under it up.
  const doing = (entry) => (state.running.has(entry.id) || holding.has(entry.id) ? "run" : entry.waits ? "ask" : "done");
  const asksUnder = new Set();
  for (const entry of own(state.sessions)) if (doing(entry) === "ask") for (const up of folds(entry.id)) asksUnder.add(up);
  // Scoped, the head says so: how many of all it lists, and whose folder, with
  // Show all beside it; a scope that hides nothing reads as the whole count.
  const all = own(state.sessions).length;
  const narrowed = scope === "folder" && own(inHere).length < all;
  // Until the list is first read there is no count: an empty head, not a 0.
  $("count").textContent = state.listed ? String(all) : "";
  $("count").hidden = narrowed;
  $("filter").placeholder = state.listed ? t("Search {n}", { n: all.toLocaleString() }) : t("Search");
  $("scope-line").hidden = !narrowed;
  $("scope-said").textContent = narrowed ? t("{n} of {m} · {folder}", { n: own(inHere).length, m: all, folder: folderName(here) }) : "";
  $("scope-said").title = narrowed ? home(here) : "";
  // A session folded away under another carries its news up to it.
  const newsUnder = new Set();
  for (const entry of state.sessions) if (unseen(entry)) for (const up of folds(entry.id)) newsUnder.add(up);
  $("scope").disabled = !here;
  $("scope").setAttribute("aria-pressed", String(scope === "folder"));
  $("scope").title = here ? t("This folder only: {n} of {m}", { n: own(inHere).length, m: own(state.sessions).length }) : t("This folder only");
  // News the folder's scope keeps out of the list still shows, on the scope.
  const elsewhere = scope === "folder" ? state.sessions.filter((entry) => unseen(entry) && !inHere.includes(entry)).length : 0;
  $("scope").toggleAttribute("data-news", elsewhere > 0);
  if (elsewhere) $("scope").title += ` · ${t("{n} with news in other folders", { n: elsewhere })}`;
  for (const [id, pane] of [["usage-open", "dashboard"], ["control-open", "control"]]) {
    if (state.pane === pane) $(id).setAttribute("aria-current", "page");
    else $(id).removeAttribute("aria-current");
  }
  // A search or a source finds a session wherever it hangs: the fold stands
  // aside for it. A session this tab minted and has not spoken in yet is in
  // no store to have a source, and stays in sight. Every source but the
  // machine's notes and the scheduled jobs (Control's Scheduled jobs lists
  // those) is a conversation of the operator's; the notes are read under theirs.
  const scoped = scope === "folder" ? inHere : state.sessions;
  renderSources(scoped);
  const needle = state.filter.toLowerCase();
  const flat = !!needle || state.source !== null;
  const inScope = new Set(scoped.filter((entry) => entry.standby || (state.source === null
    ? (!isNote(entry) && sourceOf(entry) !== "cron") || entry.id === state.current : sourceOf(entry) === state.source)));
  const shown = listOrder()
    .filter((entry) => inScope.has(entry))
    .filter((entry) => needle
      ? [sessionName(entry), entry.id, entry.working_directory].some((field) => field && field.toLowerCase().includes(needle))
      : flat || folds(entry.id).every((up) => state.branches.has(up)));
  const row = (entry) => {
    const li = el("li");
    const button = el("button", "row");
    button.type = "button";
    button.dataset.id = entry.id;
    const kids = flat || state.rooms.has(entry.id) ? 0 : held.get(entry.id) || 0;
    const open = state.branches.has(entry.id);
    button.title = [entry.id, home(entry.working_directory), !entry.standby && sourceWord(sourceOf(entry)), kids && plural(kids, t("{n} node"), t("{n} nodes"))].filter(Boolean).join(" · ");
    if (entry.id === state.current && !board()) button.setAttribute("aria-current", "page");
    if (state.picking) pickable(button, entry);
    const depth = flat ? 0 : Math.min(folds(entry.id).length, 3);
    if (depth) {
      button.style.paddingLeft = `${16 + depth * 14}px`;
      button.append(el("span", "fork", "⌎"));
    }
    // The caret stands in the gutter ahead of the row, so every name at one
    // depth starts in one column.
    if (kids) {
      const caret = el("span", "caret");
      caret.style.left = `${depth * 14}px`;
      button.setAttribute("aria-expanded", String(open));
      button.append(caret);
    }
    const running = state.running.has(entry.id);
    const now = doing(entry);
    button.append(ball(now));
    if (now === "ask") {
      button.classList.add("asks");
      button.title += ` · ${waitsLine(entry.waits)}`;
    }
    // Two lines: its name, and where it is held -- answering now, while a turn
    // runs in it; what it waits on, while its answer waits on you.
    const words = el("span", "words");
    const meta = el("span", "meta", whereOf(entry));
    if (now === "run") meta.append(el("span", "ses-now", ` · ${t("answering now")}`));
    else if (now === "ask") meta.append(el("span", "ses-asks", ` · ${waitsLine(entry.waits)}`));
    words.append(el("span", "name", sessionName(entry)), meta);
    button.append(words);
    if (kids && !open) button.append(el("span", "n", String(kids)));
    if (unseen(entry) || (!open && !flat && newsUnder.has(entry.id))) {
      button.classList.add("with-news");
      button.append(newsDot());
    }
    button.append(el("span", "reading", running ? t("now") : since(entry.updated_at)));
    li.append(button);
    // Its own trash mark, beside the row and not in it (a button holds no
    // button): a session not yet spoken in is in no store to delete, and in
    // Select the ticks are the delete. What the mark asked stands under it.
    if (!state.picking && !entry.standby) {
      const mark = iconButton("trash", t("Delete this session, and every session under it"), "icon drop ses-del");
      mark.dataset.id = entry.id;
      li.append(mark);
      if (state.dropping?.id === entry.id) {
        mark.setAttribute("aria-expanded", "true");
        const panel = el("div", "confirm ses-drop");
        panel.append(...deleteAsk(state.dropping));
        li.append(panel);
      }
    }
    return li;
  };
  // Three bands, as the kanban reads (the operator's B): what needs you, what
  // runs now, then what is done -- each newest first, Done its newest
  // DONE_ROWS until Show all; a session this tab minted and has not spoken in
  // stands above them. A row carries what hangs open under it, and the news
  // and the turns under it.
  const newest = new Map();
  for (const entry of state.sessions) {
    for (const id of [entry.id, ...folds(entry.id)]) {
      if (stamp(entry.updated_at) > (newest.get(id) || 0)) newest.set(id, stamp(entry.updated_at));
    }
  }
  const shownIds = new Set(shown.map((entry) => entry.id));
  const blocks = [];
  for (const entry of shown) {
    if (flat || !blocks.length || !folds(entry.id).some((up) => shownIds.has(up))) blocks.push({ head: entry, rows: [entry] });
    else blocks.at(-1).rows.push(entry);
  }
  const band = ({ head }) => (head.standby ? "minted"
    : doing(head) === "run" ? "run"
    : doing(head) === "ask" || asksUnder.has(head.id) ? "ask" : "done");
  const bandHead = (key, label, n, more) => {
    const li = el("li", `ses-band ${DOING_WORDS[key] ? `is-${key}` : key}`);
    li.append(el("span", "", label));
    if (n) li.append(el("span", "n", String(n)));
    if (more) li.append(more);
    return li;
  };
  // Done is most of the list: its newest DONE_ROWS, and the session on
  // screen, until Show all lists the rest.
  const doneToggle = () => {
    const button = el("button", "word more", state.doneAll ? t("Show fewer") : t("Show all"));
    button.type = "button";
    button.addEventListener("click", (event) => {
      event.stopPropagation();
      state.doneAll = !state.doneAll;
      renderSessions();
    });
    return button;
  };
  const items = [];
  let asks = 0;
  if (needle) {
    // A thread named by its first ask is found once, as the thread.
    const found = shown.filter((entry) => !(parentOf(entry) && shownIds.has(parentOf(entry))
      && sessionName(state.sessionsById.get(parentOf(entry))) === sessionName(entry)));
    if (found.length) items.push(bandHead("titles", t("Titles"), found.length));
    items.push(...found.map(row));
  } else {
    items.push(...blocks.filter((block) => band(block) === "minted").flatMap((block) => block.rows.map(row)));
    const bands = [["ask", t("Needs you")], ["run", t("Running")], ["done", t("Done")]]
      .map(([key, label]) => [key, label, blocks.filter((block) => band(block) === key)
        .sort((a, b) => (newest.get(b.head.id) || 0) - (newest.get(a.head.id) || 0))]);
    asks = bands[0][2].length;
    renderDoing(Object.fromEntries(bands.map(([key, , mine]) => [key, mine.length])));
    for (const [key, label, mine] of bands) {
      // A chosen state lists that band alone.
      if (!mine.length || (state.doing && state.doing !== key)) continue;
      const folded = key === "done" && !state.doing && mine.length > DONE_ROWS;
      items.push(bandHead(key, label, mine.length, folded && doneToggle()));
      const listed = folded && !state.doneAll
        ? mine.filter((block, at) => at < DONE_ROWS || block.rows.some((entry) => entry.id === state.current))
        : mine;
      items.push(...listed.flatMap((block) => block.rows.map(row)));
    }
  }
  $("states").hidden = !!needle;
  $("sessions-badge").textContent = String(asks);
  $("sessions-badge").hidden = !asks;
  $("sessions-badge").title = asks ? t("{n} waiting on you", { n: asks }) : "";
  // What a note said is found only where the notes are read.
  const hits = state.found.filter((hit) => (state.source === "experience" || !isNote(state.sessionsById.get(hit.session_id)))
    && (scope !== "folder" || treeFolder(hit.session_id) === here));
  if (hits.length && !state.picking) {
    items.push(el("li", "found-head micro", t("In conversations")));
    for (const hit of hits) {
      const li = el("li");
      const button = el("button", "row found");
      button.type = "button";
      button.dataset.id = hit.session_id;
      const text = el("span", "name");
      text.append(el("span", "", hit.title || sessionName(state.sessionsById.get(hit.session_id))), el("span", "snippet", oneLine(hit.snippet)));
      button.append(text);
      li.append(button);
      items.push(li);
    }
  }
  // Scoped, an empty list says whose folder it is (Show all stands in the head).
  if (!items.length) items.push(el("li", "none", narrowed ? (needle || state.source !== null ? t("Nothing matches in {folder}.", { folder: folderName(here) })
    : t("No sessions in {folder} yet.", { folder: folderName(here) }))
    : (state.source === null ? own(state.sessions) : state.sessions).length ? t("Nothing matches.") : t("Nothing here yet.")));
  $("sessions").replaceChildren(...items);
  renderPicking();
  renderTitle();
}

// --- what the sessions are doing: the column's state choice ----------------------------
// One chip a state -- its ball, its word, how many the list holds in it
// (counted as the bands are) -- above the sources. A tap lists that state
// alone; a tap on the chosen one lists every state again.

function renderDoing(counts) {
  const box = $("states");
  // Rebuilt only when a count changed, as the sources are.
  const said = Object.keys(DOING_WORDS).map((key) => `${key}=${counts[key] || 0}`).join("|");
  if (box.dataset.said !== said) {
    box.replaceChildren(...Object.entries(DOING_WORDS).map(([key, word]) => {
      const chip = el("button", "");
      chip.type = "button";
      chip.setAttribute("role", "radio");
      chip.dataset.value = key;
      chip.append(ball(key), el("span", "", word), el("span", "n", String(counts[key] || 0)));
      return chip;
    }));
    box.dataset.said = said;
  }
  for (const chip of box.children) chip.setAttribute("aria-checked", String(chip.dataset.value === state.doing));
}

// --- the source that began each session: the column's source choice -------------------
// The gateway says it per row (`source': NLK:SESSION-SOURCE-KIND, the source
// kind of the request that began the session, its first turn's); the page
// only names it. The choice lists the sources the scope holds, each with its
// count, and the one chosen even when the scope holds none of it, so an empty
// list says why.

// Every source kind a session's first turn arrives with.
const SOURCE_WORDS = {
  channel: t("Channels"),
  gateway: t("Shell or web"),
  cron: t("Scheduled jobs"),
  team: t("Team seats"),
  checkpoint: t("Checkpoint forks"),
  in_process: t("In process"),
  experience: t("Reflections"),
};

// A session the log names no source for (one never spoken in) has the empty
// kind.
const sourceOf = (entry) => entry?.source || "";

// A session the machine keeps for itself -- a reflection's fork of a turn, a
// descent's pass -- is its note, not the operator's work: the lists that offer,
// count or nest sessions leave it out, and the Reflections source is where it
// is read. The one open stays in its list, under what it hangs from.
const isNote = (entry) => sourceOf(entry) === "experience";
const sourceWord = (kind) => (kind === "" ? t("Not recorded") : SOURCE_WORDS[kind] || kind);

function renderSources(scoped) {
  const counts = new Map();
  for (const entry of scoped) {
    if (!entry.standby) counts.set(sourceOf(entry), (counts.get(sourceOf(entry)) || 0) + 1);
  }
  if (state.source !== null && !counts.has(state.source)) counts.set(state.source, 0);
  // The conversations' sources first, in SOURCE_WORDS' order; one it does not
  // name after them, and the sessions none is recorded for last.
  const order = Object.keys(SOURCE_WORDS);
  const rank = (kind) => (kind === "" ? order.length + 1 : order.includes(kind) ? order.indexOf(kind) : order.length);
  const ranked = [...counts].sort(([a], [b]) => rank(a) - rank(b) || sourceWord(a).localeCompare(sourceWord(b)));
  const chips = [["all", t("All"), null], ...ranked.map(([kind, n]) => [`k:${kind}`, sourceWord(kind), n])];
  const box = $("sources");
  // Rebuilt only when what it offers changed: the list redraws on every
  // activity frame, and the row under the reader's thumb must not move.
  const said = chips.map((chip) => chip.join("=")).join("|");
  if (box.dataset.said !== said) {
    box.replaceChildren(...chips.map(([value, text, n]) => {
      const chip = el("button", "");
      chip.type = "button";
      chip.setAttribute("role", "radio");
      chip.dataset.value = value;
      chip.append(el("span", "", text));
      if (n !== null) chip.append(el("span", "n", String(n)));
      return chip;
    }));
    box.dataset.said = said;
  }
  const chosen = state.source === null ? "all" : `k:${state.source}`;
  for (const chip of box.children) chip.setAttribute("aria-checked", String(chip.dataset.value === chosen));
}

function chooseSource(value) {
  state.source = value === "all" ? null : value.slice(2);
  try {
    if (state.source === null) localStorage.removeItem(SOURCE_KEY);
    else localStorage.setItem(SOURCE_KEY, state.source);
  } catch {}
  renderSessions();
  if (state.prune) askPrune();
}

// --- prune: the sessions no one has touched in a while --------------------------------
// The gateway picks them (NLK:PRUNE-PICKS): last active before the age chosen,
// and so is every session under them, which go with them; begun by the
// source the column shows; nothing said in them when asked. The form asks it
// first, a dry run, and says what would go; the second press deletes what
// that answer named and nothing else. A session somebody is using stays, said
// why.

const PRUNE_AGES = [7, 30, 90];

// What the prune route is asked: the form's age and emptiness, and the
// source the column shows ("" the sessions none is recorded for).
function pruneAsk() {
  const p = state.prune;
  return { days: p.days, empty: p.empty, ...(state.source === null ? {} : { source: state.source }) };
}

function setPruning(on) {
  state.prune = on ? { days: 30, empty: false, answer: null, error: null, busy: false, failed: null } : null;
  if (on && state.picking) setPicking(false);
  renderPrune();
  if (on) askPrune();
}

let pruneAsked = 0;
async function askPrune() {
  const p = state.prune;
  if (!p) return;
  const asked = ++pruneAsked;
  p.answer = null;
  p.error = null;
  renderPrune();
  try {
    const answer = await api("POST", "/api/gateway/sessions/prune", { ...pruneAsk(), dry_run: true });
    if (asked === pruneAsked) p.answer = answer;
  } catch (error) {
    if (asked === pruneAsked) p.error = error.message;
  }
  if (asked === pruneAsked && state.prune === p) renderPrune();
}

const untitled = (title) => title || t("untitled");

// The dry run's answer as sentences: what goes, what it holds, what stays.
function pruneSaid(p) {
  if (p.error) return [el("p", "error", t("Couldn't count what would go: {why}", { why: p.error }))];
  const answer = p.answer;
  if (!answer) return [el("p", "files", t("Counting what would go…"))];
  const roots = answer.sessions.length;
  const nodes = [el("p", "title", roots ? deleteQuestion(roots, answer.count - roots) : t("No session matches."))];
  if (roots) {
    const names = answer.titles.map(untitled).join(t(", "));
    const more = roots - answer.titles.length;
    nodes.push(el("p", "files", plural(answer.turns, t("{n} turn in all."), t("{n} turns in all."))));
    nodes.push(el("p", "files", more ? t("Among them: {names}, and {n} more.", { names, n: more }) : t("Among them: {names}.", { names })));
  }
  for (const kept of answer.kept.slice(0, 3)) nodes.push(el("p", "error", t("{name} stays: {why}", { name: untitled(kept.title), why: kept.reason })));
  if (answer.kept.length > 3) nodes.push(el("p", "error", plural(answer.kept.length - 3, t("{n} more stays."), t("{n} more stay."))));
  if (roots) nodes.push(el("p", "files", t("This cannot be undone. Export first to keep a copy.")));
  if (p.failed) nodes.push(el("p", "error", t("Couldn't prune: {why}", { why: p.failed })));
  return nodes;
}

function renderPrune() {
  const p = state.prune;
  $("prune").setAttribute("aria-pressed", String(!!p));
  $("prune-form").hidden = !p;
  if (!p) return;
  const ages = el("div", "scope ses-ages");
  ages.setAttribute("role", "radiogroup");
  ages.setAttribute("aria-label", t("Not touched in"));
  for (const days of PRUNE_AGES) {
    const sw = el("button", "sw", t("{n} days", { n: days }));
    sw.type = "button";
    sw.setAttribute("role", "radio");
    sw.setAttribute("aria-checked", String(days === p.days));
    sw.dataset.days = String(days);
    sw.disabled = p.busy;
    ages.append(sw);
  }
  const empty = el("button", "ses-check");
  empty.type = "button";
  empty.dataset.act = "empty";
  empty.setAttribute("role", "checkbox");
  empty.setAttribute("aria-checked", String(p.empty));
  empty.disabled = p.busy;
  empty.append(el("i", "checkmark"), el("span", "", t("Only sessions nothing was said in")));
  const from = el("p", "files", state.source === null ? t("Every source, in every folder.")
    : t("{source} only, in every folder.", { source: sourceWord(state.source) }));
  const actions = el("div", "actions");
  const go = el("button", "solid danger", p.busy ? t("Deleting…") : t("Delete"));
  const cancel = el("button", "word", t("Cancel"));
  [[go, "prune"], [cancel, "cancel"]].forEach(([button, act]) => {
    button.type = "button";
    button.dataset.act = act;
    button.disabled = p.busy || (act === "prune" && !p.answer?.sessions.length);
  });
  actions.append(go, cancel);
  $("prune-form").replaceChildren(el("p", "micro", t("Not touched in")), ages, empty, from, ...pruneSaid(p), actions);
}

// The session on screen, if it goes, closes first, as a delete of it does.
async function prune() {
  const p = state.prune;
  const named = p?.answer?.sessions || [];
  if (!named.length || p.busy) return;
  p.busy = true;
  p.failed = null;
  renderPrune();
  const goes = (id, roots) => [id, ...ancestors(id)].some((up) => roots.includes(up));
  if (state.current && goes(state.current, named)) openNew();
  try {
    const answer = await api("POST", "/api/gateway/sessions/prune", { ...pruneAsk(), sessions: named });
    for (const entry of state.sessions.filter((each) => !each.standby && goes(each.id, answer.sessions))) {
      state.views.delete(entry.id);
      state.sessionsById.delete(entry.id);
    }
    state.sessions = state.sessions.filter((entry) => state.sessionsById.has(entry.id));
    const m = answer.kept.length;
    toast(m ? plural(answer.count, t("Deleted {n} session; {m} stayed", { m }), t("Deleted {n} sessions; {m} stayed", { m }))
      : plural(answer.count, t("Deleted {n} session"), t("Deleted {n} sessions")), { error: m > 0 });
    state.prune = null;
  } catch (error) {
    p.busy = false;
    p.failed = error.message;
  }
  renderPrune();
  loadSessions();
}

// --- a session that does not exist yet -------------------------------------------------
// The first message opens the session in the folder the opener names: the one
// here (the latest session's, or the one New session was pressed from), else
// the gateway's own, which it says (GET /api/gateway/folder). Change… asks for
// another in the page: what is typed is resolved and checked by the gateway
// as it opens a session, so a folder that is not there is refused, saying
// why, before anything is sent -- and the recent folders are offered.

function knownFolders() {
  const seen = [];
  const newest = [...state.sessions].sort((a, b) => Date.parse(b.updated_at || 0) - Date.parse(a.updated_at || 0));
  for (const entry of newest) {
    if (entry.working_directory && !seen.includes(entry.working_directory)) seen.push(entry.working_directory);
  }
  return seen;
}

async function loadGatewayFolder() {
  try {
    state.gatewayFolder = (await askFolder("")).path;
    if (!state.current) renderOpener();
  } catch {}
}

// The gateway's word on the folder DIR names ("" its own) => { path, exists, why }.
const askFolder = (dir) => api("GET", `/api/gateway/folder?path=${encodeURIComponent(dir)}`);

function renderOpener() {
  // The latest session's folder, once the list is read, else the gateway's own.
  if (!state.folder && state.listed) state.folder = knownFolders()[0] || state.gatewayFolder;
  const dir = state.folder;
  $("cwd-name").textContent = dir ? folderName(dir) : "…";
  $("cwd-name").title = home(dir);
  // Asked once each time the folder changes: a recent session's may have gone since.
  if (dir && state.folderCheck?.path !== dir) {
    state.folderCheck = { path: dir, exists: true, why: null };
    askFolder(dir).then((answer) => {
      if (state.folderCheck?.path !== dir) return;
      state.folderCheck = { path: dir, exists: answer.exists, why: answer.why };
      if (!state.current) renderOpener();
    }).catch(() => {});
  }
  const gone = state.folderCheck?.path === dir && !state.folderCheck.exists;
  $("cwd-missing").hidden = !gone;
  $("cwd-missing").textContent = gone ? sentence(state.folderCheck.why) : "";
  renderCwdPick();

  const recent = state.sessions
    .filter((entry) => entry.working_directory === state.folder && !entry.standby && !isNote(entry))
    .sort((a, b) => Date.parse(b.updated_at || 0) - Date.parse(a.updated_at || 0))
    .slice(0, 3);
  $("resume").hidden = !recent.length;
  $("resume-head").textContent = t("Or pick one up in {folder}", { folder: home(state.folder) });
  $("resume-list").replaceChildren(...recent.map((entry) => {
    const li = el("li");
    const button = el("button");
    button.type = "button";
    button.dataset.id = entry.id;
    if (state.running.has(entry.id)) button.append(lamp(true));
    button.append(el("span", "name", sessionName(entry)));
    if (unseen(entry)) {
      button.classList.add("with-news");
      button.append(newsDot());
    }
    button.append(el("span", "reading", since(entry.updated_at)));
    li.append(button);
    return li;
  }));
  renderHead();
}

// The folder field, open under the opener's line: what the gateway said of
// what is typed, and the folders sessions worked in, newest first, each with
// its count, the gateway's own among them.
function renderCwdPick() {
  const p = state.cwdPick;
  $("cwd-pick").hidden = !p;
  $("cwd-line").hidden = !!p;
  $("cwd-change").setAttribute("aria-expanded", String(!!p));
  if (!p) return;
  const a = p.answer;
  const said = $("cwd-said");
  said.classList.toggle("refused", !!a && !a.exists);
  said.textContent = !a ? (p.asking ? t("Checking…") : "")
    : a.exists ? t("The session will work in {folder}", { folder: home(a.path) }) : sentence(a.why);
  const bare = (dir) => dir.replace(/(.)\/+$/, "$1");
  const seen = new Set();
  const dirs = [...knownFolders(), ...(state.gatewayFolder ? [state.gatewayFolder] : [])]
    .filter((dir) => !seen.has(bare(dir)) && seen.add(bare(dir))).slice(0, 6);
  $("cwd-recent-head").hidden = !dirs.length;
  $("cwd-recent").replaceChildren(...dirs.map((dir) => {
    const li = el("li");
    const button = el("button");
    button.type = "button";
    button.append(el("span", "cwd-dir", home(dir)));
    button.dataset.folder = dir;
    button.title = dir;
    if (dir === state.folder) button.setAttribute("aria-current", "true");
    const count = state.sessions.filter((entry) => bare(entry.working_directory || "") === bare(dir) && !entry.standby && !isNote(entry)).length;
    button.append(el("span", "reading", dir === state.gatewayFolder && !count ? t("where the gateway runs") : plural(count, t("{n} session"), t("{n} sessions"))));
    li.append(button);
    return li;
  }));
}

function openCwdPick() {
  state.cwdPick = { text: state.folder || "", answer: null, asking: false };
  renderCwdPick();
  $("cwd-path").value = state.cwdPick.text;
  $("cwd-path").focus();
  $("cwd-path").select();
  checkCwdSoon();
}

function closeCwdPick() {
  state.cwdPick = null;
  renderCwdPick();
  $("cwd-change").focus();
}

// What is typed is asked a beat after the last key; the answer counts only
// while the field still says what was asked.
let cwdTimer = null;
function checkCwdSoon() {
  clearTimeout(cwdTimer);
  const p = state.cwdPick;
  if (!p) return;
  p.answer = null;
  p.asking = true;
  renderCwdPick();
  cwdTimer = setTimeout(() => checkCwd(p.text), 250);
}

async function checkCwd(text) {
  const p = state.cwdPick;
  let answer;
  try {
    answer = await askFolder(text.trim());
  } catch (error) {
    answer = { exists: false, why: t("Couldn't ask the gateway: {why}", { why: error.message }) };
  }
  if (state.cwdPick !== p || p.text !== text) return null;
  p.answer = answer;
  p.asking = false;
  renderCwdPick();
  return answer;
}

// The folder typed (or TEXT, a recent one picked) becomes the new session's
// once the gateway says it is there; a refusal stays in the field, saying why.
async function useCwd(text) {
  const p = state.cwdPick;
  if (!p) return;
  clearTimeout(cwdTimer);
  p.text = text;
  p.asking = true;
  renderCwdPick();
  const answer = await checkCwd(text);
  if (!answer?.exists) { $("cwd-path").focus(); return; }
  state.folder = answer.path;
  state.folderCheck = { path: answer.path, exists: true, why: null };
  state.cwdPick = null;
  renderOpener();
  renderSessions();
  promptEl.focus();
}

// The box stands in the middle of the window until the session exists, and at
// the foot of it after.
function placeComposer() {
  const target = state.current ? $("dock") : $("opener-box");
  if (composer.parentNode !== target) target.append(composer);
  $("dock").hidden = !state.current || board();
  promptEl.setAttribute("aria-label", state.current ? t("Message this session") : t("Open a session"));
  fit();
}

function select(id, carry) {
  app.classList.remove("column-open");
  if (id === state.current) {
    if (board()) setPane("chat");
    return;
  }
  // CARRY: the opener's draft becomes the session its first message opens.
  if (carry) forgetDraft();
  else leaveDraft();
  leaving();
  state.current = id;
  state.reveal = id;
  if (board()) toChat();
  arrived();
  state.timeline = null;
  state.runOpen = null;
  state.runAll = false;
  state.runPaged = false;
  state.drawer = null;
  state.filesPath = "";
  loadTimeline();
  state.recalled = null;
  closeModels();
  closeSlash();
  $("evict").hidden = true;
  loadTarget();
  state.rewind = null;
  state.history = null;
  renderHistory();
  writeLink();
  subscribe();
  placeComposer();
  if (carry) keepDraft();
  else takeDraft();
  renderSessions();
  renderHead();
  renderTranscript();
  renderObserve();
  schedule();
  promptEl.focus();
}

function openNew() {
  app.classList.remove("column-open");
  toChat();
  state.timeline = null;
  state.drawer = null;
  renderObserve();
  const here = hereFolder();
  if (here) state.folder = here;
  state.folderCheck = null; // asked again: it may have gone since
  leaveDraft();
  leaving();
  state.current = null;
  closeModels();
  closeSlash();
  loadTarget();
  writeLink();
  subscribe();
  placeComposer();
  takeDraft();
  renderTranscript();
  renderOpener();
  renderSessions();
  promptEl.focus();
}

async function newSession(cwd) {
  const id = `s-${Math.floor(Math.random() * 36 ** 8).toString(36).padStart(8, "0")}`;
  try {
    const entry = await api("POST", "/api/gateway/sessions", { session_id: id, ...(cwd ? { cwd } : {}) });
    const standby = { id, title: "", working_directory: entry?.working_directory || cwd || "", updated_at: new Date().toISOString(), standby: true };
    state.sessionsById.set(id, standby);
    state.sessions = [standby, ...state.sessions.filter((each) => each.id !== id)];
    select(id, true);
    return id;
  } catch (error) {
    // A refusal is the gateway's answer, and asking again changes nothing; the
    // folder it refused is asked again, so the opener says what is wrong with it.
    toast(t("Couldn't start a session: {why}", { why: error.message }), { error, retry: error.status ? undefined : () => newSession(cwd) });
    if (error.status && !state.current) {
      state.folderCheck = null;
      renderOpener();
    }
    return null;
  }
}

// --- sessions ticked: saved to a file, deleted, and a file brought back ----------------
// The gateway deletes and exports a session whole, every session under it (a
// fork, a seat) with it, so a row under a ticked one is ticked with it. The
// confirmation asks the gateway first (a dry run): what it names is what goes.

const underPicked = (id) => ancestors(id).some((up) => state.picking.ids.has(up));

// The ticked sessions no other ticked one already takes along.
const pickedRoots = () => [...state.picking.ids].filter((id) => !underPicked(id));

// In Select a row is a box to tick; a session not yet spoken in is not in the
// store to save or delete.
function pickable(button, entry) {
  const implied = underPicked(entry.id);
  button.setAttribute("role", "checkbox");
  button.setAttribute("aria-checked", String(implied || state.picking.ids.has(entry.id)));
  button.toggleAttribute("data-implied", implied);
  button.disabled = implied || !!entry.standby;
  if (implied) button.title = t("Goes with the session it hangs under");
  button.append(el("i", "checkmark"));
}

function setPicking(on) {
  state.picking = on ? { ids: new Set(), confirm: null } : null;
  if (on) state.dropping = null;
  if (on && state.prune) setPruning(false);
  renderSessions();
}

function tickable() {
  return [...$("sessions").querySelectorAll(".row[role=checkbox]:not(:disabled)")].map((row) => row.dataset.id);
}

function pick(ids, on) {
  for (const id of ids) {
    if (on) state.picking.ids.add(id); else state.picking.ids.delete(id);
  }
  state.picking.confirm = null;
  renderSessions();
}

function renderPicking() {
  const p = state.picking;
  $("pick").textContent = p ? t("Done") : t("Select");
  $("pick").setAttribute("aria-pressed", String(!!p));
  $("picking").hidden = !p;
  $("pick-confirm").hidden = !p?.confirm;
  if (!p) return;
  const roots = pickedRoots();
  const rows = tickable();
  $("picked").textContent = roots.length ? t("{n} ticked", { n: roots.length }) : t("none ticked");
  $("pick-all").setAttribute("aria-checked", String(rows.length > 0 && rows.every((id) => p.ids.has(id))));
  $("pick-export").disabled = $("pick-delete").disabled = !roots.length || !!p.confirm;
  if (p.confirm) $("pick-confirm").replaceChildren(...deleteAsk(p.confirm));
}

// "Delete 2 sessions and the 3 sessions under them?": ROOTS asked for, and
// UNDER going with them. A Select delete and a prune ask it alike.
function deleteQuestion(roots, under) {
  const one = roots === 1;
  const counts = { n: roots, m: under };
  return !under ? plural(roots, t("Delete {n} session?"), t("Delete {n} sessions?"))
    : under === 1 ? (one ? t("Delete {n} session and the {m} session under it?", counts) : t("Delete {n} sessions and the {m} session under them?", counts))
    : one ? t("Delete {n} session and the {m} sessions under it?", counts) : t("Delete {n} sessions and the {m} sessions under them?", counts);
}

function deleteAsk(ask) {
  if (!ask.answers) return [el("p", "files", t("Counting what would go…"))];
  const going = ask.answers.filter((answer) => answer.plan);
  const sessions = going.reduce((n, answer) => n + answer.plan.sessions.length, 0);
  const turns = going.reduce((n, answer) => n + answer.plan.turns, 0);
  const nodes = [el("p", "title", going.length ? deleteQuestion(going.length, sessions - going.length) : t("None of these can be deleted now."))];
  if (going.length) nodes.push(el("p", "files", plural(turns, t("{n} turn in all."), t("{n} turns in all."))));
  for (const answer of ask.answers.filter((each) => each.error)) {
    nodes.push(el("p", "error", t("{name} stays: {why}", { name: sessionName(state.sessionsById.get(answer.id)), why: answer.error })));
  }
  if (going.length) nodes.push(el("p", "files", t("This cannot be undone. Export first to keep a copy.")));
  const actions = el("div", "actions");
  const go = el("button", "solid danger", ask.busy ? t("Deleting…") : t("Delete"));
  const cancel = el("button", "word", t("Cancel"));
  [[go, "delete"], [cancel, "cancel"]].forEach(([button, act]) => {
    button.type = "button";
    button.dataset.act = act;
    button.disabled = !!ask.busy || (act === "delete" && !going.length);
  });
  actions.append(go, cancel);
  return [...nodes, actions];
}

async function askDelete() {
  const roots = pickedRoots();
  state.picking.confirm = {};
  renderPicking();
  const answers = await Promise.all(roots.map((id) => api("DELETE", `${sessionPath(id)}?dry_run=1`)
    .then((plan) => ({ id, plan }), (error) => ({ id, error: error.message }))));
  if (state.picking?.confirm) {
    state.picking.confirm = { answers };
    renderPicking();
  }
}

// The session on screen, if it goes, closes first: the tab lets go of it
// before the gateway takes it away.
async function deletePicked() {
  const ask = state.picking.confirm;
  state.picking.confirm = { ...ask, busy: true };
  renderPicking();
  await deleteAnswered(ask.answers, () => setPicking(false));
}

// One session's own delete, its row's trash mark (the operator, 2026-10-21:
// "add a delete knob or something for a conversation"): asked of the gateway
// first, as Select's delete is, the question under the row it was pressed on;
// the mark pressed again, Cancel or Escape lets it be.
async function askDrop(id) {
  if (state.picking) return;
  if (state.dropping?.id === id) { dropDone(); return; }
  state.dropping = { id, answers: null };
  renderSessions();
  const answers = [await api("DELETE", `${sessionPath(id)}?dry_run=1`)
    .then((plan) => ({ id, plan }), (error) => ({ id, error: error.message }))];
  if (state.dropping?.id !== id) return;
  state.dropping = { id, answers };
  renderSessions();
}

function dropDone() {
  const id = state.dropping?.id;
  state.dropping = null;
  renderSessions();
  if (id) $("sessions").querySelector(`.row[data-id="${CSS.escape(id)}"]`)?.focus();
}

async function drop() {
  const ask = state.dropping;
  if (!ask?.answers || ask.busy) return;
  state.dropping = { ...ask, busy: true };
  renderSessions();
  await deleteAnswered(ask.answers, () => { state.dropping = null; renderSessions(); });
}

// Each answered session deleted, the one on screen let go of first; what went
// forgotten here, AFTER run, and said. A Select delete and a row's alike.
async function deleteAnswered(answers, after) {
  const gone = new Set(answers.flatMap((answer) => answer.plan?.sessions || []));
  if (gone.has(state.current)) openNew();
  let deleted = 0;
  const kept = [];
  for (const { id } of answers.filter((answer) => answer.plan)) {
    try {
      deleted += (await api("DELETE", sessionPath(id))).sessions.length;
    } catch (error) {
      kept.push(`${sessionName(state.sessionsById.get(id))} (${error.message})`);
    }
  }
  for (const id of gone) {
    state.views.delete(id);
    state.sessionsById.delete(id);
  }
  state.sessions = state.sessions.filter((entry) => !gone.has(entry.id));
  after();
  toast(kept.length
    ? plural(deleted, t("Deleted {n} session; kept {kept}", { kept: kept.join(", ") }), t("Deleted {n} sessions; kept {kept}", { kept: kept.join(", ") }))
    : plural(deleted, t("Deleted {n} session"), t("Deleted {n} sessions")), { error: kept.length > 0 });
  loadSessions();
}

function save(blob, name) {
  const url = URL.createObjectURL(blob);
  const link = el("a");
  link.href = url;
  link.download = name;
  document.body.append(link);
  link.click();
  link.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

// A file a route answers, saved under the name the route gives it.
async function download(path, fallback) {
  const response = await reach(path);
  if (!response.ok) {
    const data = await response.json().catch(() => null);
    throw new Error(data?.error?.message || `${response.status}`);
  }
  save(await response.blob(), /filename="([^"]+)"/.exec(response.headers.get("Content-Disposition") || "")?.[1] || fallback);
}

// One file for every ticked session: the gateway's files run together are one.
async function exportPicked() {
  const roots = pickedRoots();
  const button = $("pick-export");
  button.disabled = true;
  button.setAttribute("aria-busy", "true");
  try {
    const parts = [];
    let name = null;
    for (const id of roots) {
      const response = await reach(`${sessionPath(id)}/export`);
      if (!response.ok) {
        const data = await response.json().catch(() => null);
        throw new Error(`${sessionName(state.sessionsById.get(id))}: ${data?.error?.message || response.status}`);
      }
      name = /filename="([^"]+)"/.exec(response.headers.get("Content-Disposition") || "")?.[1];
      parts.push(await response.blob());
    }
    const file = roots.length === 1 && name ? name : `nodecode-${roots.length}-sessions.jsonl`;
    save(new Blob(parts, { type: "application/x-ndjson" }), file);
    toast(t("Saved {file}", { file }));
  } catch (error) {
    toast(t("Couldn't export: {why}", { why: error.message }), { error });
  }
  button.removeAttribute("aria-busy");
  renderPicking();
}

async function importFile(file) {
  try {
    const answer = await api("POST", "/api/gateway/sessions/import", file, "application/x-ndjson");
    const counts = { n: answer.sessions.length, k: answer.kept.length };
    const one = counts.n === 1;
    toast(!counts.k ? plural(counts.n, t("Imported {n} session"), t("Imported {n} sessions"))
      : counts.k === 1 ? (one ? t("Imported {n} session; {k} session it builds on was here already", counts)
        : t("Imported {n} sessions; {k} session it builds on was here already", counts))
      : one ? t("Imported {n} session; {k} sessions it builds on were here already", counts)
      : t("Imported {n} sessions; {k} sessions it builds on were here already", counts));
    await loadSessions();
    if (answer.sessions[0]) select(answer.sessions[0]);
  } catch (error) {
    toast(t("Couldn't import {file}: {why}", { file: file.name, why: error.message }), { error, retry: () => importFile(file) });
  }
}

// --- the composer ----------------------------------------------------------------------
// A draft stays with the session it was typed in: a switch leaves it there,
// images and all, and its text outlives a reload in this browser.

function draftKey() {
  return DRAFT_KEY + (state.current || "");
}

function keepDraft() {
  try {
    if (promptEl.value) localStorage.setItem(draftKey(), promptEl.value);
    else localStorage.removeItem(draftKey());
  } catch {}
}

function forgetDraft() {
  try { localStorage.removeItem(draftKey()); } catch {}
}

function leaveDraft() {
  if (promptEl.value || state.images.length || state.files.length) {
    state.drafts.set(state.current || "", { text: promptEl.value, images: state.images, files: state.files });
  } else state.drafts.delete(state.current || "");
}

function takeDraft() {
  const draft = state.drafts.get(state.current || "");
  state.drafts.delete(state.current || "");
  promptEl.value = draft ? draft.text : safeGet(localStorage, draftKey()) || "";
  state.images = draft ? draft.images : [];
  state.files = draft ? draft.files : [];
  renderAttachments();
  fit();
}

// Whether the session on screen runs a turn, which the box's keys and words follow.
const busy = () => !!state.views.get(state.current)?.turn?.active;

function renderComposer() {
  $("send").disabled = !state.open || (!promptEl.value.trim() && !state.images.length && !state.files.length) || !attachedReady();
  if ($("attach-why")) $("send").setAttribute("aria-describedby", "attach-why");
  else $("send").removeAttribute("aria-describedby");
  $("queue").disabled = $("send").disabled;
  const running = busy();
  promptEl.placeholder = running ? t("Enter steers this turn · Tab queues for after it")
    : state.current ? t("Message this session") : t("The first message opens the session");
  $("send").title = running ? t("Steer this turn (Enter): it ends at its next step and this runs next") : t("Send (Enter)");
  $("send").setAttribute("aria-label", running ? t("Steer this turn") : t("Send"));
}

// The files ride the prompt as one line each under its text, naming the path
// each went up to.
async function submit(steer) {
  const text = promptEl.value;
  const { images, files } = state;
  if ((!text.trim() && !images.length && !files.length) || !state.open || !attachedReady()) return;
  if (!images.length && !files.length && slashCommand(text)) { runSlash(text.trim()); return; }
  const opening = !state.current;
  const session = state.current || (await newSession(state.folder));
  if (!session || (opening && !(await landAim(session)))) return;
  // Where each file lands is read again at the last moment: a name taken
  // since the chip read the folder holds the send for the chip to say so.
  if (files.some((entry) => entry.status === "waiting") && (await placeFiles(files, session, true))) return;
  // The files as Send found them: a chip removed on the way leaves the box's
  // list, not this one, and holds the send.
  const paths = await filesUp([...files], session);
  if (!paths) return;
  const said = [text.trim() ? text : "", paths.map((path) => t("Attached: {path}", { path })).join("\n")].filter(Boolean).join("\n\n");
  sendPrompt(session, said, images, steer);
  // Left for another session while the files went up, the box sent is that
  // session's draft now.
  if (state.files !== files) { state.drafts.delete(session); return; }
  clearAttachments();
  promptEl.value = "";
  keepDraft();
  fit();
}

// TEXT and IMAGES to SESSION as a turn: start_turn, or steer_turn when STEER
// and a turn runs. Whoever sends is looking for the answer, so the transcript
// goes to its end, and follows from there as it always does.
function sendPrompt(session, text, images, steer) {
  const turn = state.views.get(session)?.turn;
  const content = [
    ...(text.trim() ? [{ type: "text", text }] : []),
    ...images.map((image) => ({ type: "image", source: { type: "base64", media_type: image.media_type, data: image.data } })),
  ];
  const request = { session_id: session, content, generate_response: true };
  const steered = !!(steer && turn?.active);
  const id = steered
    ? command("steer_turn", { request: { ...request, expected_turn_id: turn.turn_id } })
    : command("start_turn", { request });
  state.sent.set(id, { text, session, ...(steered ? { steered, images } : {}) });
  state.redo.delete(session);
  if (session === state.current) toEnd();
}

// A failed turn's prompt, sent again as a turn of its own the way the box
// sends one, with the pictures it was sent with. The failed turn stays in the
// record rather than being rewound: what it did before it failed is not
// undone, and the model reads it. A picture no longer on disk is not sent
// short: the prompt waits for it to be attached again.
async function retry(button) {
  const session = state.current;
  const turn = turnsOf(view(session)).turns.find((each) => each.key === button.dataset.key);
  if (!turn?.prompt || !state.open) return;
  button.disabled = true;
  const { images, lost } = await promptPictures(session, turn.prompt);
  if (lost) {
    button.disabled = false;
    images.forEach((image) => URL.revokeObjectURL(image.url));
    toast(t("A picture sent with this prompt is no longer on disk: rewind the turn and attach it again"), { error: true });
    return;
  }
  sendPrompt(session, turn.prompt.text || "", images, false);
  images.forEach((image) => URL.revokeObjectURL(image.url));
}

// The pictures a prompt ROW of SESSION was sent with, as the box holds its
// own: their bytes fetched back by the ids the message shows them by.
// => { images, lost }, LOST the count no longer on disk.
async function promptPictures(session, row) {
  const facts = row?.metadata?.images || [];
  const images = [];
  for (const image of facts) {
    const blob = (await pictureHeld(session, image.id))?.blob;
    const data = blob && (await new Promise((done) => {
      const reader = new FileReader();
      reader.onload = () => done(String(reader.result).split(",")[1]);
      reader.onerror = () => done(null);
      reader.readAsDataURL(blob);
    }));
    if (data) images.push({ media_type: image.media_type, size: blob.size, url: URL.createObjectURL(blob), data });
  }
  return { images, lost: facts.length - images.length };
}

function commandResult(result) {
  const sent = state.sent.get(result.command_id);
  if (result.status === "accepted" || result.status === "rejected") state.sent.delete(result.command_id);
  if (sent?.navigate) { navigated(result, sent); return; }
  if (sent?.withdraw) {
    state.withdrawing.delete(sent.withdraw);
    if (result.status === "accepted") tookBack(sent.session, result.response || {});
    else if (result.status === "rejected") toast(result.code === "prompt_not_pending" ? t("It already ran") : result.message || result.code, { error: true });
    renderTranscript();
    return;
  }
  if (sent?.interrupt) {
    if (result.status === "rejected") {
      state.stopping.delete(sent.key);
      renderTranscript();
      toast(result.message || result.code, { error: true });
    }
    return;
  }
  // A steer that found its turn already over runs as the next turn instead.
  if (result.status === "rejected" && sent?.steered && result.code === "admission_failed") {
    sendPrompt(sent.session, sent.text, sent.images, false);
    return;
  }
  if (result.status === "rejected") {
    const why = result.message || result.code;
    toast(result.command ? t("The gateway refused {command}: {why}", { command: result.command, why }) : t("The gateway refused the command: {why}", { why }), { error: true });
    if (sent && !promptEl.value) { promptEl.value = sent.text; fit(); }
  }
}

// --- a queued prompt taken back ------------------------------------------------------
// The newest queued prompt of the session on screen, while its turn runs: what
// ↑ on an empty box takes back, as the shell's own ↑ does.
function newestQueued() {
  const v = state.views.get(state.current);
  if (!v?.turn?.active) return null;
  const queued = v.order.map((id) => v.rows.get(id))
    .filter((row) => row?.kind === "pending_input" && row.prompt_id && !state.withdrawing.has(row.prompt_id));
  return queued.at(-1)?.prompt_id || null;
}

function takeBack(promptId) {
  if (!promptId || state.withdrawing.has(promptId)) return;
  state.withdrawing.add(promptId);
  const id = command("withdraw_prompt", { session_id: state.current, prompt_id: promptId });
  state.sent.set(id, { withdraw: promptId, session: state.current });
  renderTranscript();
}

// What the gateway handed back, its words and pictures, goes into SESSION's
// box ahead of whatever was being typed there, which stays.
function tookBack(session, response) {
  const text = typeof response.content === "string" ? response.content : "";
  const images = (response.images || []).filter((item) => item?.source?.data).map(({ source }) => ({
    media_type: source.media_type, data: source.data, size: Math.floor((source.data.length * 3) / 4),
    url: `data:${source.media_type};base64,${source.data}`,
  }));
  const join = (typed) => [text, typed].filter((part) => part.trim()).join("\n\n");
  if (session !== state.current) {
    const draft = state.drafts.get(session) || { text: "", images: [], files: [] };
    state.drafts.set(session, { ...draft, text: join(draft.text), images: [...images, ...draft.images] });
    return;
  }
  promptEl.value = join(promptEl.value);
  state.images = [...images, ...state.images];
  renderAttachments();
  keepDraft();
  fit();
  promptEl.focus();
  promptEl.setSelectionRange(text.length, text.length);
}

function stop() {
  const turn = state.views.get(state.current)?.turn;
  if (turn?.active && turn.turn_id) command("cancel_turn", { session_id: state.current, turn_id: turn.turn_id });
}

function fit() {
  promptEl.style.height = "0px";
  promptEl.style.height = `${Math.min(promptEl.scrollHeight, 220)}px`;
  renderComposer();
}

// A notice. News leaves after five seconds, held while the pointer or focus is
// on it; an ERROR (true, or the error itself) stays until it is closed, and one
// that never reached the gateway (status 0) leaves when the link is back. RETRY
// rides it as a button. The same words again count up on the one on screen,
// which comes back to the top. A notice with a key stands: the gateway hands
// it to every tab that attaches ("discord: unconfigured ..." until it is set
// up). It toasts once in this browser for what it says, and again when the
// words under its KEY change -- in place of the one on screen that said the
// old words. The newest stands on top, and past TOASTS the oldest news leaves
// first, then the oldest error.
function freshNotice({ key, text }) {
  if (!key) return true;
  let seen = {};
  try { seen = JSON.parse(localStorage.getItem(NOTICES_KEY) || "{}") || {}; } catch {}
  if (seen[key] === text) return false;
  seen[key] = text;
  try { localStorage.setItem(NOTICES_KEY, JSON.stringify(seen)); } catch {}
  return true;
}

const TOASTS = 3;

function toast(text, { error, retry, key } = {}) {
  const box = $("toasts");
  const same = [...box.children].find((node) => node.dataset.text === text);
  if (same) {
    const n = Number(same.dataset.n) + 1;
    same.dataset.n = String(n);
    same.querySelector(".n").textContent = `×${n}`;
    same.leave?.();
    box.prepend(same);
    return;
  }
  if (key) [...box.children].find((node) => node.dataset.key === key)?.remove();
  const node = el("div", `toast${error ? " error" : ""}`);
  node.dataset.text = text;
  node.dataset.n = "1";
  if (key) node.dataset.key = key;
  if (error?.status === 0) node.dataset.link = "";
  const said = el("span", "said", text);
  said.append(el("span", "n"));
  node.append(said);
  if (retry) {
    const again = el("button", "word", t("Retry"));
    again.type = "button";
    again.addEventListener("click", () => { node.remove(); retry(); });
    node.append(again);
  }
  const shut = el("button", "shut");
  shut.type = "button";
  shut.setAttribute("aria-label", t("Close"));
  shut.innerHTML = `<svg viewBox="0 0 15 15" width="11" height="11" fill="none" aria-hidden="true"><path d="M3.5 3.5L11.5 11.5M11.5 3.5L3.5 11.5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>`;
  shut.addEventListener("click", () => node.remove());
  node.append(shut);
  box.prepend(node);
  while (box.children.length > TOASTS) {
    ([...box.children].findLast((each) => each !== node && !each.matches(".error, .link-ask")) || box.lastElementChild).remove();
  }
  if (error) return;
  let timer = null;
  const hold = () => clearTimeout(timer);
  node.leave = () => { hold(); timer = setTimeout(() => node.remove(), 5000); };
  node.addEventListener("pointerenter", hold);
  node.addEventListener("pointerleave", node.leave);
  node.addEventListener("focusin", hold);
  node.addEventListener("focusout", node.leave);
  node.leave();
}

// "the gateway isn't answering" as a sentence of its own, closed by the stop
// its script writes: a full-width one after Chinese.
function sentence(text) {
  if (!text) return "";
  const stop = /[.!?。！？]$/.test(text) ? "" : /[\u3000-\u9fff\uff00-\uffef]$/.test(text) ? "。" : ".";
  return `${text[0].toUpperCase()}${text.slice(1)}${stop}`;
}

async function copyCode(button) {
  const code = button.parentElement.querySelector("code")?.textContent ?? "";
  const say = (name, word) => {
    button.replaceChildren(icon(name));
    button.title = word;
    button.setAttribute("aria-label", word);
  };
  try {
    await navigator.clipboard.writeText(code);
    say("check", t("Copied"));
  } catch {
    say("close", t("Not copied"));
  }
  setTimeout(() => say("copy", t("Copy")), 1400);
}

// Up in a box that holds nothing -- or the prompt Up last put there -- walks
// back through this session's own prompts, newest first -- never a recorded
// pair's, which nobody typed here; Down walks forward and ends on an empty
// box. Anything typed ends the walk.
function recall(step) {
  const v = state.current && state.views.get(state.current);
  if (!v || (promptEl.value && state.recalled === null)) return false;
  const prompts = turnsOf(v).turns.map((turn) => (turn.prompt?.recorded ? "" : turn.prompt?.text || "")).filter(Boolean).reverse();
  const at = (state.recalled ?? -1) + step;
  if (at < -1 || at >= prompts.length || (step < 0 && state.recalled === null)) return false;
  promptEl.value = at < 0 ? "" : prompts[at];
  fit();
  state.recalled = at < 0 ? null : at;
  promptEl.setSelectionRange(promptEl.value.length, promptEl.value.length);
  return true;
}

// --- slash commands -------------------------------------------------------------------
// The gateway's own catalog, the one a channel publishes as Telegram's or
// Discord's command menu: `/' opens it above the box, a command that completes
// its argument asks the gateway as the argument is typed, and a line naming a
// command runs there -- no turn, nothing kept -- its answer standing in the
// same place until it is put away. A line naming no command is a prompt, as
// Discord sends one.

const slash = { commands: null, loaded: 0, items: [], at: 0, asked: 0 };
let slashTimer = null;

async function loadSlash() {
  slash.loaded = Date.now();
  try { slash.commands = (await api("GET", "/api/gateway/slash")).commands || []; return true; } catch { return false; }
}

function slashCommand(text) {
  const name = /^\/([A-Za-z]\S*)/.exec(text.trim())?.[1].toLowerCase();
  return name && slash.commands?.find((command) => command.name.toLowerCase() === name);
}

// A command's arguments as its usage spells them, without its own name.
function slashArgs(command) {
  const [first, ...rest] = (command.usage || "").split(/\s+/);
  return first.replace(/^\//, "").toLowerCase() === command.name.toLowerCase() ? rest.join(" ") : command.usage || "";
}

// What picking ITEM leaves in the box.
function slashCompletion(item) {
  if (item.choice) return `/${item.command.name} ${item.choice.value}`;
  return `/${item.command.name}${slashArgs(item.command) ? " " : ""}`;
}

function slashInput() {
  clearTimeout(slashTimer);
  const text = promptEl.value;
  const naming = /^\/(\S*)$/.exec(text);
  const arguing = !naming && /^\/(\S+) ([^\n]*)$/.exec(text);
  if (!naming && !arguing) { showSlash([]); return; }
  if (Date.now() - slash.loaded > 30000) loadSlash().then((ok) => { if (ok && promptEl.value === text) slashInput(); });
  if (naming) {
    const prefix = naming[1].toLowerCase();
    showSlash((slash.commands || []).filter((command) => command.name.toLowerCase().startsWith(prefix)).map((command) => ({ command })));
    return;
  }
  const command = slashCommand(text);
  if (!command?.autocomplete) { showSlash([]); return; }
  const asked = ++slash.asked;
  slashTimer = setTimeout(async () => {
    let choices = [];
    try { choices = (await api("POST", "/api/gateway/slash/completions", { name: command.name, text: arguing[2], ...(state.current ? { session_id: state.current } : {}) })).choices || []; } catch {}
    if (asked === slash.asked && promptEl.value === text) showSlash(choices.slice(0, 60).map((choice) => ({ command, choice })));
  }, 120);
}

// Over the box, as wide as it, wherever it stands: the dock or the opener.
function placeSlash() {
  const sheet = $("slash");
  const slab = document.querySelector(".slab").getBoundingClientRect();
  const box = composer.getBoundingClientRect();
  sheet.style.setProperty("--left", `${box.left - slab.left}px`);
  sheet.style.setProperty("--wide", `${box.width}px`);
  sheet.style.setProperty("--above", `${slab.bottom - box.top + 8}px`);
  sheet.style.setProperty("--room", `${box.top - slab.top - 16}px`);
}

function showSlash(items) {
  const same = items.length === slash.items.length && items.every((item, at) => slashCompletion(item) === slashCompletion(slash.items[at]));
  slash.items = items;
  if (!same) slash.at = 0;
  const list = $("slash-list");
  if (!items.length) {
    list.hidden = true;
    if ($("slash-reply").hidden) $("slash").hidden = true;
    return;
  }
  closeModels();
  list.replaceChildren(...items.map((item, at) => {
    const li = el("li");
    const pick = el("button", "pick");
    pick.type = "button";
    pick.tabIndex = -1;
    pick.dataset.index = at;
    pick.setAttribute("role", "option");
    pick.setAttribute("aria-selected", String(at === slash.at));
    if (item.choice) pick.append(el("span", "cmd", item.choice.name));
    else pick.append(el("span", "cmd", `/${item.command.name}`), el("span", "args", slashArgs(item.command)), el("span", "about", item.command.description || ""));
    li.append(pick);
    return li;
  }));
  list.hidden = false;
  $("slash-reply").hidden = true;
  $("slash").hidden = false;
  placeSlash();
  list.children[slash.at]?.scrollIntoView({ block: "nearest" });
}

function moveSlash(step) {
  slash.at = (slash.at + step + slash.items.length) % slash.items.length;
  $("slash-list").querySelectorAll(".pick").forEach((pick, at) => pick.setAttribute("aria-selected", String(at === slash.at)));
  $("slash-list").children[slash.at]?.scrollIntoView({ block: "nearest" });
}

// Tab, or a click: the pick goes in the box. Enter does the same, and runs the
// line once the box already says it.
function pickSlash(run) {
  const item = slash.items[slash.at];
  if (!item) return;
  const line = slashCompletion(item);
  if (run && line.trim() === promptEl.value.trim()) { showSlash([]); submit(false); return; }
  promptEl.value = line;
  fit();
  promptEl.setSelectionRange(line.length, line.length);
  slashInput();
}

function showReply(line, text, how, dialog) {
  $("slash-line").textContent = line;
  $("slash-text").textContent = text;
  $("slash-text").className = `reply-text${how ? ` ${how}` : ""}`;
  slashPanel(dialog);
  $("slash-list").hidden = true;
  $("slash-reply").hidden = false;
  $("slash").hidden = false;
  placeSlash();
}

function closeSlash() {
  clearTimeout(slashTimer);
  slash.items = [];
  $("slash").hidden = true;
  $("slash-list").hidden = true;
  $("slash-reply").hidden = true;
}

// The panel a command answered with, under its line: what it says of itself,
// its picture (a QR code a phone's camera reads), and its rows, one with a
// value putting it in the box and one with a line to run running it, as Enter
// on it does in a shell.
function slashPanel(dialog) {
  const panel = $("slash-panel");
  panel.hidden = !dialog;
  if (!dialog) { panel.replaceChildren(); return; }
  const said = el("div", "reply-said");
  if (dialog.context) said.append(el("p", "reply-context", dialog.context));
  const rows = (dialog.rows || []).map((row) => {
    const li = el("li");
    const run = row.run?.enter;
    const pick = el(run || row.value ? "button" : "div", "reply-row");
    if (run) { pick.type = "button"; pick.dataset.run = run; pick.title = run; }
    else if (row.value) { pick.type = "button"; pick.dataset.value = row.value; pick.title = row.value; }
    pick.append(el("span", `reply-mark${row.mark_tone === "accent" ? " lit" : ""}`, row.mark || ""),
      el("span", "reply-label", row.label), el("span", "reply-detail", row.detail));
    li.append(pick);
    return li;
  });
  if (rows.length) {
    const list = el("ol", "reply-rows");
    list.append(...rows);
    said.append(list);
  } else if (dialog.empty_label) said.append(el("p", "reply-context", dialog.empty_label));
  const nodes = [];
  if (dialog.picture?.rows?.length) {
    const picture = qrCode(dialog.picture.rows);
    picture.setAttribute("class", "reply-picture");
    picture.setAttribute("aria-label", dialog.picture.label || "");
    nodes.push(picture);
  }
  panel.replaceChildren(...nodes, said);
}

// A command opens the session the way a first message does: what it moves --
// the model, the reasoning -- is that session's. One its catalog entry says
// acts on no session (/link, /help) runs as it is, and the opener stays one.
async function runSlash(line) {
  promptEl.value = "";
  keepDraft();
  fit();
  closeSlash();
  const needs = slashCommand(line)?.session !== false;
  const session = state.current || (needs ? await newSession(state.folder) : null);
  if (needs && !session) return;
  showReply(line, "…", "waiting");
  try {
    const answer = await api("POST", "/api/gateway/slash", { line, ...(session ? { session_id: session } : {}) });
    showReply(line, [helpKeys(line), answer?.text || t("done")].filter(Boolean).join("\n\n"), "", answer?.dialog);
  } catch (error) {
    showReply(line, error.message, "error");
  }
  // What a command moved -- the model, the head, the list -- is read again.
  loadTarget();
  loadSessionsSoon();
}

// The keys the box answers, which /help lists above the gateway's commands:
// the gateway's words are every shell's, the keys are this page's own.
const KEYS = [
  ["Enter", t("Send; while a turn runs, steer: this runs at its next step")],
  ["Tab", t("While a turn runs, queue this for after it")],
  ["Shift+Enter, Alt+Enter", t("A new line")],
  ["↑", t("While a turn runs, take the last queued prompt back; else the prompt before")],
  ["↓", t("The prompt after")],
  ["Esc", t("Stop the running turn")],
];

function helpKeys(line) {
  if (line.trim().split(/\s+/)[0] !== "/help") return "";
  const width = Math.max(...KEYS.map(([key]) => key.length));
  return [t("Keys in this box"), ...KEYS.map(([key, what]) => `${key.padEnd(width)}  ${what}`)].join("\n");
}

// --- the column's fold ----------------------------------------------------------------

const narrow = matchMedia("(max-width: 1023px)");
// Across the phone's width the Run view changes how it draws: a list and pages, or rows that open in place.
PHONE.addEventListener("change", () => { if (state.pane === "run") renderObserve(); });

function setFolded(folded) {
  app.classList.toggle("folded", folded);
  try { localStorage.setItem(FOLD_KEY, folded ? "1" : "0"); } catch {}
}

// --- wiring ---------------------------------------------------------------------------

// The box answers the shell's own keys while a turn runs (tui/keys.lisp,
// surface/client.lisp): Enter, and Send, steer it -- the turn ends at its next
// step and this runs next; Tab, and Queue, wait for it to end; ↑ on an empty
// box takes the newest queued prompt back to edit; Alt+Enter is a new line.
composer.addEventListener("submit", (event) => { event.preventDefault(); submit(true); });
$("queue").addEventListener("click", () => { submit(false); promptEl.focus(); });
promptEl.addEventListener("keydown", (event) => {
  const menu = slash.items.length > 0;
  if (menu && (event.key === "ArrowDown" || event.key === "ArrowUp")) {
    event.preventDefault();
    moveSlash(event.key === "ArrowDown" ? 1 : -1);
  } else if (menu && (event.key === "Tab" || (event.key === "Enter" && !event.shiftKey && !event.isComposing))) {
    event.preventDefault();
    pickSlash(event.key === "Enter");
  } else if (event.key === "Escape" && !$("slash").hidden) {
    event.preventDefault();
    closeSlash();
  } else if (event.key === "Enter" && event.altKey && !event.isComposing) {
    event.preventDefault();
    promptEl.setRangeText("\n", promptEl.selectionStart, promptEl.selectionEnd, "end");
    promptEl.dispatchEvent(new Event("input"));
  } else if (event.key === "Enter" && !event.shiftKey && !event.isComposing) {
    event.preventDefault();
    submit(true);
  } else if (event.key === "Tab" && !event.shiftKey && busy() && !$("queue").disabled) {
    event.preventDefault();
    submit(false);
  } else if (event.key === "Escape") {
    stop();
  } else if (event.key === "ArrowUp" && !promptEl.value && newestQueued()) {
    event.preventDefault();
    takeBack(newestQueued());
  } else if ((event.key === "ArrowUp" || event.key === "ArrowDown") && recall(event.key === "ArrowUp" ? 1 : -1)) {
    event.preventDefault();
  }
});
promptEl.addEventListener("input", () => { state.recalled = null; });
promptEl.addEventListener("input", keepDraft);
promptEl.addEventListener("input", fit);
promptEl.addEventListener("input", slashInput);
$("slash-list").addEventListener("mousedown", (event) => event.preventDefault());
$("slash-list").addEventListener("click", (event) => {
  const pick = event.target.closest(".pick");
  if (!pick) return;
  slash.at = Number(pick.dataset.index);
  pickSlash(false);
});
$("slash-close").addEventListener("click", () => { closeSlash(); promptEl.focus(); });
$("slash-panel").addEventListener("click", (event) => {
  const pick = event.target.closest("button[data-value], button[data-run]");
  if (!pick) return;
  if (pick.dataset.run) { runSlash(pick.dataset.run); return; }
  closeSlash();
  promptEl.value = pick.dataset.value;
  fit();
  promptEl.focus();
  promptEl.setSelectionRange(promptEl.value.length, promptEl.value.length);
});
$("stop").addEventListener("click", stop);
// Execs that write nothing still age: their clocks tick in place.
setInterval(() => {
  for (const node of band.querySelectorAll(".elapsed")) node.textContent = elapsedText(Number(node.dataset.since));
}, 1000);
band.addEventListener("click", (event) => {
  const act = event.target.closest("[data-act]");
  if (act?.dataset.act === "interrupt") {
    const id = command("interrupt_exec", { session_id: state.current, exec_id: Number(act.dataset.exec) });
    state.sent.set(id, { interrupt: true, session: state.current, key: act.dataset.key });
    state.stopping.add(act.dataset.key);
    renderBand();
  } else if (act?.dataset.act === "bg-open") {
    state.toggled.set(act.dataset.key, !state.toggled.get(act.dataset.key));
    bandSig = null;
    renderBand();
  }
});
$("jump").addEventListener("click", toEnd);
ledger.addEventListener("scroll", () => {
  if (!ledger.hidden) following = ledger.scrollHeight - ledger.scrollTop - ledger.clientHeight < NEAR_END;
  placeJump();
}, { passive: true });
rowsEl.addEventListener("click", (event) => {
  const act = event.target.closest("[data-act]");
  if (act) {
    if (act.dataset.act === "copy") copyCode(act);
    else if (act.dataset.act === "fork") forkAt(act.dataset.turn);
    else if (act.dataset.act === "withdraw") takeBack(act.dataset.prompt);
    else if (act.dataset.act === "rewind") askRewind(act.dataset.key, act.dataset.turn, Number(act.dataset.number));
    else if (act.dataset.act === "retry") retry(act);
    else answerRewind(act.dataset.act);
    return;
  }
  const summary = event.target.closest("summary");
  const details = summary?.parentElement;
  if (details?.dataset.fold) state.toggled.set(details.dataset.fold, !details.open);
});
$("new-session").addEventListener("click", openNew);
// The page's own links (the wordmark, Dashboard, Control): a plain click moves
// the page in place; a middle, Ctrl, Cmd or Shift click is the browser's, the
// place opened in a tab or window of its own.
const inPlace = (event) => event.button === 0 && !event.metaKey && !event.ctrlKey && !event.shiftKey && !event.altKey;

// A tab of its own has no token: it lives in this tab alone, and a browser
// gives a tab a link opens none of this one's. So the link a middle or
// modified click follows hands it over the way `nodecode web' does, and goes
// back to naming the place alone once the browser has read it.
function handOver(link) {
  const place = link.getAttribute("href");
  if (!state.token || place.startsWith("#t=")) return;
  link.setAttribute("href", `#t=${encodeURIComponent(state.token)}${place.length > 1 ? `&${place.slice(1)}` : ""}`);
  setTimeout(() => link.setAttribute("href", place));
}

for (const [id, go] of [["home", openNew], ["strip-home", openNew], ["usage-open", () => setPane("dashboard")], ["control-open", () => setPane("control")], ["link-open", () => setPane("control", "link")]]) {
  const link = $(id);
  link.addEventListener("click", (event) => {
    if (!inPlace(event)) { handOver(link); return; }
    event.preventDefault();
    // A phone's tab leaves the Sessions page, even for the place it already shows.
    app.classList.remove("column-open");
    go();
  });
  link.addEventListener("auxclick", (event) => { if (event.button === 1) handOver(link); });
}
for (const host of [$("views"), $("head-acts")]) host.addEventListener("click", (event) => {
  const sw = event.target.closest("[data-view]");
  if (!sw) return;
  // Chat and Run are a toggle: a press on the lit one flips it.
  const flip = host === $("views") && sw.getAttribute("aria-checked") === "true";
  setPane(flip ? (sw.dataset.view === "chat" ? "run" : "chat") : sw.dataset.view);
});
// A phone's Dashboard: a row opens its section in a sheet; the veil, the
// close mark, Escape or a drag down by the grip closes it, the way it came.
let dashClosing = 0;
function openDashSheet(id) {
  dashClosing++;
  state.dashSheet = id;
  state.phoneOpen.clear();
  state.dashEnter = true;
  renderObserve();
}
function closeDashSheet() {
  if (!state.dashSheet) return;
  const ask = ++dashClosing;
  const gone = () => { if (ask !== dashClosing) return; state.dashSheet = null; renderObserve(); };
  const sheet = document.querySelector(".dash-sheet");
  if (!sheet?.getClientRects().length || matchMedia("(prefers-reduced-motion: reduce)").matches) { gone(); return; }
  sheet.dataset.leaving = "";
  document.querySelector(".dash-veil")?.setAttribute("data-leaving", "");
  setTimeout(gone, 220);
}
// A phone has no pointer to hover: a tap on a day's bar or an hour reads its
// tooltip out under the drawing.
function readDash(target) {
  const sheet = target.closest(".dash-sheet");
  for (const each of sheet.querySelectorAll(".read")) each.classList.remove("read");
  target.classList.add("read");
  const out = sheet.querySelector(".dash-readout");
  if (out) out.textContent = target.title;
}
let dashDrag = null;
$("observe").addEventListener("pointerdown", (event) => {
  const grip = event.target.closest("[data-dash-drag]");
  if (!grip || event.target.closest("button")) return;
  dashDrag = { y: event.clientY, dy: 0, sheet: grip.closest(".dash-sheet") };
  grip.setPointerCapture?.(event.pointerId);
});
$("observe").addEventListener("pointermove", (event) => {
  if (!dashDrag) return;
  dashDrag.dy = Math.max(0, event.clientY - dashDrag.y);
  dashDrag.sheet.style.transform = `translateY(${dashDrag.dy}px)`;
});
for (const kind of ["pointerup", "pointercancel"]) {
  $("observe").addEventListener(kind, () => {
    if (!dashDrag) return;
    const { dy, sheet } = dashDrag;
    dashDrag = null;
    if (dy > 90) closeDashSheet();
    else sheet.style.transform = "";
  });
}
document.addEventListener("keydown", (event) => {
  if (event.key !== "Escape" || !state.dashSheet || state.pane !== "dashboard" || !$("lightbox").hidden) return;
  closeDashSheet();
});
$("observe").addEventListener("click", (event) => {
  if (state.pane === "control") { control.click(event); return; }
  const door = event.target.closest("[data-dash-sheet]");
  if (door) { openDashSheet(door.dataset.dashSheet); return; }
  if (event.target.closest("[data-dash-close]")) { closeDashSheet(); return; }
  // A phone's row opens in place, a group shows all its rows, and a second tap closes either.
  const more = event.target.closest("[data-p-more]");
  if (more) {
    const key = more.dataset.pMore;
    if (!state.phoneOpen.delete(key)) state.phoneOpen.add(key);
    renderObserve();
    return;
  }
  const readable = event.target.closest(".dash-sheet :is(.day[title], .heat i[title])");
  if (readable) { readDash(readable); return; }
  const range = event.target.closest("[data-range]");
  if (range) {
    state.range = range.dataset.range;
    loadDashboard();
    renderObserve();
    writeLink();
    return;
  }
  const jump = event.target.closest("[data-jump]");
  if (jump) {
    $(`dash-${jump.dataset.jump}`)?.scrollIntoView({ behavior: "smooth", block: "start" });
    return;
  }
  if (dragged) return;
  const act = event.target.closest("[data-act]");
  if (!act) return;
  event.stopPropagation();
  // A call's bar also takes a double click, which fits the clock to it: its
  // own click waits that long, so the inspector it opens is not under the
  // second. A key's click (detail 0) opens it at once.
  if (event.detail && act.closest(".timeline .track")) {
    clearTimeout(barClick);
    if (event.detail === 1) barClick = setTimeout(() => answerObserve(act), 300);
    return;
  }
  answerObserve(act);
});
$("observe").addEventListener("dblclick", (event) => {
  clearTimeout(barClick);
  const bar = event.target.closest(".timeline [data-start]");
  const turn = bar && openTurn();
  if (!turn) return;
  const start = Number(bar.dataset.start);
  const end = Number(bar.dataset.end);
  const pad = Math.max((end - start) * 0.15, SHORTEST_MS);
  setWindow(turn, start - pad, end + pad);
  renderObserve();
});
$("observe").addEventListener("wheel", (event) => {
  const turn = event.target.closest(".timeline") && openTurn();
  if (!turn) return;
  const [share, width] = clockShare(event);
  const scale = event.deltaMode === 1 ? 16 : 1;
  if (event.ctrlKey || event.metaKey) zoomBy(turn, Math.exp(event.deltaY * scale * 0.002), share);
  else if (state.runZoom && (event.shiftKey || Math.abs(event.deltaX) > Math.abs(event.deltaY))) {
    panBy(turn, ((event.deltaX || event.deltaY) * scale) / width);
  } else return;
  event.preventDefault();
  redrawClock();
}, { passive: false });
$("observe").addEventListener("pointerdown", (event) => {
  const turn = event.button === 0 && state.runZoom && event.target.closest(".timeline .lanes") && openTurn();
  if (turn) drag = { turn, x: event.clientX, width: clockShare(event)[1], window: runWindow(turn), moved: false };
});
document.addEventListener("pointermove", (event) => {
  if (!drag || (!drag.moved && Math.abs(event.clientX - drag.x) < 4)) return;
  drag.moved = true;
  const [from, to] = drag.window;
  const by = ((drag.x - event.clientX) / drag.width) * (to - from);
  setWindow(drag.turn, from + by, to + by);
  redrawClock();
});
for (const name of ["pointerup", "pointercancel"]) {
  document.addEventListener(name, () => {
    if (drag?.moved) {
      dragged = true;
      setTimeout(() => { dragged = false; });
    }
    drag = null;
  });
}
$("observe").addEventListener("keydown", (event) => {
  const turn = event.target.classList.contains("timeline") && !event.ctrlKey && !event.metaKey && !event.altKey && openTurn();
  const keys = {
    "+": () => zoomBy(turn, 1 / 1.5), "=": () => zoomBy(turn, 1 / 1.5), "-": () => zoomBy(turn, 1.5),
    0: () => { state.runZoom = null; }, ArrowLeft: () => panBy(turn, -0.2), ArrowRight: () => panBy(turn, 0.2),
  };
  if (!turn || !keys[event.key]) return;
  event.preventDefault();
  keys[event.key]();
  redrawClock();
});
for (const type of ["input", "change"]) {
  $("observe").addEventListener(type, (event) => { if (state.pane === "control") control.input(event); });
}
$("observe").addEventListener("submit", (event) => { if (state.pane === "control") control.submit(event); });
$("drawer").addEventListener("click", (event) => {
  event.stopPropagation();
  const tab = event.target.closest("[data-tab]");
  if (tab && state.drawer) { state.drawer = { ...state.drawer, tab: tab.dataset.tab }; renderObserve(); writeLink(); return; }
  const act = event.target.closest("[data-act]");
  if (act && !act.disabled) answerObserve(act);
});
// Ahead of the prompt's Esc, which stops a turn: an open inspector closes first.
document.addEventListener("keydown", (event) => {
  if (event.key !== "Escape" || !state.drawer || state.pane !== "run" || !$("lightbox").hidden) return;
  event.preventDefault();
  event.stopPropagation();
  state.drawer = null;
  renderObserve();
  writeLink();
}, true);
$("title").addEventListener("click", () => {
  const title = $("title");
  if (title.isContentEditable || !listed()) return;
  title.dataset.session = state.current;
  title.contentEditable = "plaintext-only";
  title.focus();
  getSelection().selectAllChildren(title);
});
// Enter and Esc end the edit themselves: a window that never had focus never
// sends the blur a focus it did not take would have earned.
function endRename(save) {
  const title = $("title");
  if (!title.isContentEditable) return;
  const session = title.dataset.session;
  const name = title.textContent.replace(/\s+/g, " ").trim();
  title.contentEditable = "false";
  delete title.dataset.session;
  if (save && session && name !== sessionName(state.sessionsById.get(session))) rename(session, name);
  else renderHead();
}
$("title").addEventListener("keydown", (event) => {
  if (event.key !== "Enter" && event.key !== "Escape") return;
  event.preventDefault();
  event.stopPropagation();
  endRename(event.key === "Enter");
});
$("title").addEventListener("blur", () => endRename(true));
$("markdown").addEventListener("click", async () => {
  const id = state.current;
  try {
    await download(`${sessionPath(id)}/export?format=markdown`, `${id}.md`);
  } catch (error) {
    toast(t("Couldn't save the Markdown: {why}", { why: error.message }), { error });
  }
});
$("more").addEventListener("click", (event) => {
  event.stopPropagation();
  setActs(!$("head-acts").hasAttribute("data-open"));
});
// An act picked from the menu closes it, ahead of the act's own click.
$("head-acts").addEventListener("click", () => setActs(false), true);
$("head-acts").addEventListener("keydown", (event) => {
  if (event.key !== "Escape" || !$("head-acts").hasAttribute("data-open")) return;
  setActs(false);
  $("more").focus();
});
// A mark that holds choices -- the foot's link, look and language, the
// Sessions acts -- opens them on a click; one open closes the other, and a
// choice made, Esc or a click anywhere else closes it (the link's panel keeps
// its own clicks).
function setPop(id) {
  for (const mark of document.querySelectorAll("[data-pops]")) {
    const open = mark.dataset.pops === id;
    $(mark.dataset.pops).toggleAttribute("data-open", open);
    mark.setAttribute("aria-expanded", String(open));
  }
  if (id) $(id).querySelector('[aria-checked="true"]')?.focus();
}
document.addEventListener("click", (event) => {
  const mark = event.target.closest("[data-pops]");
  setPop(mark && mark.getAttribute("aria-expanded") !== "true" ? mark.dataset.pops : null);
});
document.addEventListener("keydown", (event) => {
  const open = document.querySelector('[data-pops][aria-expanded="true"]');
  if (event.key !== "Escape" || !open) return;
  setPop(null);
  open.focus();
});
// The link mark by the gateway line: a dot while the link is on, beating while
// it connects, ringed while a browser asks, hollow while its dials fail. Its
// panel (access.js) is read again as it opens; a click in it stays in it, and
// the hand's place in it is kept when a reading draws it again.
function paintLink() {
  const l = control.link.now;
  const pop = $("uplink");
  $("uplink-mark").dataset.state = l?.missing ? "missing" : l && !l.error ? l.state : "";
  $("uplink-mark").toggleAttribute("data-asking", !!l?.ask);
  paintAsk();
  if (!pop.hasAttribute("data-open")) return;
  const held = pop.contains(document.activeElement) && (document.activeElement.dataset.act || "");
  pop.replaceChildren(control.link.panel());
  if (held !== false) (held && pop.querySelector(`[data-act="${held}"]`) || pop.querySelector("button"))?.focus();
}
// A browser asking, outside the Link tab and its panel, is a notice that holds
// over the others (access.js askNotice) until it is answered or out of time.
// Its clock ticks every second, so it is kept out of the notices' live region:
// the cell's own notice of the ask is what is read aloud.
function paintAsk() {
  const note = control.link.askNotice();
  let held = $("toasts").querySelector(".link-ask");
  if (!note) { held?.remove(); return; }
  const hand = held?.contains(document.activeElement) && document.activeElement.dataset.act;
  if (!held) {
    held = el("div", "toast link-ask");
    held.setAttribute("role", "group");
    held.setAttribute("aria-live", "off");
    held.setAttribute("aria-label", t("A browser is asking"));
    held.addEventListener("click", (event) => control.click(event));
    $("toasts").prepend(held);
  }
  held.replaceChildren(note);
  if (hand) held.querySelector(`[data-act="${hand}"]`)?.focus();
}

// A notice may be the link's -- a browser asking, the line failing or back --
// so the link is read again, its own reading, and its mark and ask repainted.
let linkReadTimer = null;
function readLinkSoon() {
  clearTimeout(linkReadTimer);
  linkReadTimer = setTimeout(() => control.link.read().then(paintLink), 150);
}

$("uplink-mark").addEventListener("click", () => {
  if ($("uplink").hasAttribute("data-open")) return;
  $("uplink").replaceChildren(control.link.panel());
  control.link.read().then(paintLink);
});
$("uplink").addEventListener("click", (event) => {
  event.stopPropagation();
  control.click(event);
});
$("redo").addEventListener("click", () => navigate({ direction: "redo" }));
$("history-open").addEventListener("click", (event) => {
  event.stopPropagation();
  if (state.history) closeHistory(); else openHistory();
});
$("history-close").addEventListener("click", closeHistory);
// Ahead of the prompt's Esc, which stops a turn: an open sheet closes first,
// a turn picked in History going back to the list before the list goes.
document.addEventListener("keydown", (event) => {
  if (event.key !== "Escape" || !$("lightbox").hidden) return;
  if (state.history) {
    if (state.history.pick) answerHistory("cancel"); else closeHistory();
    $("history-open").focus();
  } else if (!$("evict").hidden) {
    $("evict").hidden = true;
    $("context").focus();
  } else return;
  event.preventDefault();
  event.stopPropagation();
}, true);
$("history").addEventListener("click", (event) => {
  event.stopPropagation();
  const act = event.target.closest("[data-act]");
  if (act) { answerHistory(act.dataset.act); return; }
  const pick = event.target.closest(".pick");
  if (!pick || pick.disabled || pick.parentElement.classList.contains("head") || !state.history) return;
  state.history = { ...state.history, pick: pick.dataset.turn, busy: false, error: null };
  renderHistory();
});
$("sessions").addEventListener("click", (event) => {
  const said = event.target.closest(".ses-drop [data-act]");
  if (said) { if (said.dataset.act === "delete") drop(); else dropDone(); return; }
  const mark = event.target.closest(".ses-del");
  if (mark) { askDrop(mark.dataset.id); return; }
  const item = event.target.closest(".row");
  if (!item) return;
  if (event.target.closest(".caret")) { branch(item.dataset.id, !state.branches.has(item.dataset.id)); return; }
  if (item.disabled) return;
  if (state.picking) pick([item.dataset.id], !state.picking.ids.has(item.dataset.id));
  else select(item.dataset.id);
});
// Right opens the row's session, Left closes it -- or, holding nothing open,
// closes the session it hangs under and moves there: the shell's own keys.
$("sessions").addEventListener("keydown", (event) => {
  if (event.key === "Escape" && state.dropping) { dropDone(); event.preventDefault(); return; }
  const item = event.target.closest(".row");
  // Delete on a row asks to delete its session, as its trash mark does.
  if (item && event.key === "Delete" && !state.picking && item.parentElement.querySelector(":scope > .ses-del")) {
    askDrop(item.dataset.id);
    event.preventDefault();
    return;
  }
  if (!item || state.filter || state.source !== null || !["ArrowLeft", "ArrowRight"].includes(event.key)) return;
  const id = item.dataset.id;
  const up = parentOf(state.sessionsById.get(id));
  if (event.key === "ArrowRight" && item.getAttribute("aria-expanded") === "false") branch(id, true);
  else if (event.key === "ArrowLeft" && state.branches.has(id)) branch(id, false);
  else if (event.key === "ArrowLeft" && up) branch(up, false);
  else return;
  event.preventDefault();
});

function branch(id, open) {
  if (open) state.branches.add(id); else state.branches.delete(id);
  const seat = $("sessions").contains(document.activeElement) && state.sessionsById.has(id) ? id : null;
  renderSessions();
  if (seat) $("sessions").querySelector(`.row[data-id="${CSS.escape(seat)}"]`)?.focus();
}
$("pick").addEventListener("click", () => setPicking(!state.picking));
$("pick-all").addEventListener("click", () => {
  const rows = tickable();
  pick(rows, !rows.every((id) => state.picking.ids.has(id)));
});
$("pick-export").addEventListener("click", exportPicked);
$("pick-done").addEventListener("click", () => setPicking(false));
$("pick-delete").addEventListener("click", askDelete);
$("pick-confirm").addEventListener("click", (event) => {
  const act = event.target.closest("[data-act]");
  if (act?.dataset.act === "delete") deletePicked();
  else if (act?.dataset.act === "cancel") { state.picking.confirm = null; renderPicking(); }
});
$("sessions-retry").addEventListener("click", loadSessions);
$("reconnect").addEventListener("click", reconnectNow);
$("offline-retry").addEventListener("click", reconnectNow);
$("offline-reload").addEventListener("click", () => location.reload());
$("offline-copy").addEventListener("click", copyStart);
for (const id of ["offline-scrim", "offline-close"]) {
  $(id).addEventListener("click", () => { state.offlineAway = state.broken; renderOffline(); });
}
// The device back online tries at once; gone offline, the sheet says so now.
window.addEventListener("online", reconnectNow);
window.addEventListener("offline", () => { if (!state.open) traceLine(); });
$("import").addEventListener("click", () => $("import-file").click());
$("import-file").addEventListener("change", (event) => {
  const [file] = event.target.files;
  event.target.value = "";
  if (file) importFile(file);
});
$("resume-list").addEventListener("click", (event) => {
  const item = event.target.closest("button");
  if (item) select(item.dataset.id);
});
$("cwd-change").addEventListener("click", openCwdPick);
$("cwd-cancel").addEventListener("click", closeCwdPick);
$("cwd-path").addEventListener("input", (event) => {
  state.cwdPick.text = event.target.value;
  checkCwdSoon();
});
$("cwd-path").addEventListener("keydown", (event) => {
  if (event.key !== "Escape") return;
  event.preventDefault();
  closeCwdPick();
});
$("cwd-pick").addEventListener("submit", (event) => {
  event.preventDefault();
  useCwd($("cwd-path").value);
});
$("cwd-recent").addEventListener("click", (event) => {
  const button = event.target.closest("[data-folder]");
  if (!button) return;
  $("cwd-path").value = button.dataset.folder;
  useCwd(button.dataset.folder);
});
function setScope(scope) {
  state.scope = scope;
  try { localStorage.setItem(SCOPE_KEY, scope); } catch {}
  renderSessions();
}
$("scope").addEventListener("click", () => setScope(state.scope === "folder" ? "all" : "folder"));
$("scope-all").addEventListener("click", () => setScope("all"));
$("filter").addEventListener("input", (event) => { state.filter = event.target.value; renderSessions(); searchSoon(); });
$("sources").addEventListener("click", (event) => {
  const chip = event.target.closest("[data-value]");
  if (chip) chooseSource(chip.dataset.value);
});
$("states").addEventListener("click", (event) => {
  const chip = event.target.closest("[data-value]");
  if (!chip) return;
  state.doing = state.doing === chip.dataset.value ? null : chip.dataset.value;
  renderSessions();
});
// A phone's Sessions tab is the page it stands on: a tap goes back to its top.
// A phone's Sessions tab: from Dashboard or Control it opens the Sessions page;
// on it, the list goes back to its top.
$("sessions-tab").addEventListener("click", () => {
  // A wide window's rail: Sessions brings the list back beside the session it
  // left (or a new one); on it, the list goes back to its top.
  if (WIDE.matches) {
    if (app.classList.contains("folded")) setFolded(false);
    if (board()) setPane("chat");
    else $("sessions").scrollTop = 0;
    return;
  }
  if (app.classList.contains("column-open")) $("sessions").scrollTop = 0;
  else app.classList.add("column-open");
});
$("place").addEventListener("click", (event) => {
  const go = event.target.closest(".place-row");
  if (!go) return;
  if (go.dataset.tab) setPane("control", go.dataset.tab);
  else if (go.dataset.jump === "top") $("observe").scrollTo({ top: 0, behavior: "smooth" });
  else if (go.dataset.jump) $(`dash-${go.dataset.jump}`)?.scrollIntoView({ behavior: "smooth", block: "start" });
});
let spying = false;
$("observe").addEventListener("scroll", () => {
  if (spying || state.pane !== "dashboard") return;
  spying = true;
  requestAnimationFrame(() => { spying = false; spyList(); });
}, { passive: true });
// Across 1024 px the list beside the rail comes or goes.
WIDE.addEventListener("change", paintList);
$("prune").addEventListener("click", () => setPruning(!state.prune));
$("prune-form").addEventListener("click", (event) => {
  const p = state.prune;
  if (!p || p.busy) return;
  const age = event.target.closest("[data-days]");
  const act = event.target.closest("[data-act]")?.dataset.act;
  if (age) p.days = Number(age.dataset.days);
  else if (act === "empty") p.empty = !p.empty;
  else if (act === "prune") { prune(); return; }
  else if (act === "cancel") { setPruning(false); return; }
  else return;
  askPrune();
});
$("model").addEventListener("click", (event) => {
  event.stopPropagation();
  if ($("models").hidden) openModels(); else closeModels();
});
$("lightbox-mask").addEventListener("click", closeLightbox);
$("lightbox-close").addEventListener("click", closeLightbox);
// Ahead of every other Esc (the prompt's stops a turn): the viewer is on top.
document.addEventListener("keydown", (event) => {
  if (event.key !== "Escape" || $("lightbox").hidden) return;
  event.preventDefault();
  event.stopPropagation();
  closeLightbox();
}, true);
$("models-filter").addEventListener("input", renderModels);
$("models").addEventListener("click", (event) => {
  event.stopPropagation();
  const to = event.target.closest("[data-to]");
  if (to) { showPane(to.dataset.to); return; }
  if (event.target.closest("#pickscope-go")) { closeModels(); setPop(null); setPane("control", "settings"); return; }
  const effort = event.target.closest("[data-effort]");
  if (effort) { chooseEffort(effort.dataset.effort); return; }
  const pick = event.target.closest("[data-model]");
  if (pick) chooseModel(pick.dataset.provider, pick.dataset.model);
});
$("models").addEventListener("keydown", (event) => {
  const sheet = $("models");
  if (event.key === "Escape") {
    event.preventDefault();
    if (sheet.dataset.pane === "root") closeModels(true); else showPane("root");
  } else if (event.key === "ArrowDown" || event.key === "ArrowUp") {
    event.preventDefault();
    const items = [...sheet.querySelectorAll(".pane:not([hidden]) :is(.cell:not([hidden]), .pick)")];
    const at = items.indexOf(document.activeElement);
    items[(at + (event.key === "ArrowDown" ? 1 : items.length - 1 + (at < 0 ? 1 : 0))) % items.length]?.focus();
  } else if (event.key === "Enter" && event.target.id === "models-filter") {
    event.preventDefault();
    $("models-list").querySelector("[data-model]")?.click();
  }
});
$("context").addEventListener("click", (event) => {
  event.stopPropagation();
  if (!$("evict").hidden) { $("evict").hidden = true; return; }
  if (state.history) closeHistory();
  openEvict();
});
$("evict").addEventListener("click", (event) => {
  event.stopPropagation();
  const act = event.target.closest("[data-act]");
  if (act?.dataset.act === "evict") evict();
  else if (act?.dataset.act === "cancel") $("evict").hidden = true;
});
$("attachments").addEventListener("click", (event) => {
  const view = event.target.closest(".view");
  if (view) { openLightbox(state.images[Number(view.dataset.index)]?.url); return; }
  const again = event.target.closest(".attach-retry");
  if (again) {
    const entry = state.files[Number(again.dataset.file)];
    if (entry && state.current) putUp(entry, state.current);
    promptEl.focus();
    return;
  }
  const drop = event.target.closest(".remove");
  if (!drop) return;
  // A file on its way up is stopped, and a torn upload leaves nothing (the
  // route writes through one rename); one a held send already put up stays.
  if (drop.dataset.file) {
    const [entry] = state.files.splice(Number(drop.dataset.file), 1);
    if (entry) entry.gone = true;
    if (entry?.status === "going") {
      entry.stop();
      toast(t("Stopped uploading {name}", { name: entry.file.name }));
    } else if (entry?.status === "up") {
      toast(t("{name} is already in {folder} and stays there", { name: entry.name, folder: folderName(entry.folder) }));
    }
  } else {
    const [image] = state.images.splice(Number(drop.dataset.index), 1);
    if (image) URL.revokeObjectURL(image.url);
  }
  renderAttachments();
});
$("attach").addEventListener("click", () => $("attach-input").click());
$("attach-input").addEventListener("change", (event) => {
  const files = [...event.target.files];
  event.target.value = "";
  attach(files);
  promptEl.focus();
});
promptEl.addEventListener("paste", (event) => {
  const files = [...(event.clipboardData?.files || [])];
  if (files.length) { event.preventDefault(); attach(files); }
});
composer.addEventListener("dragover", (event) => {
  if ([...(event.dataTransfer?.items || [])].some((item) => item.kind === "file")) event.preventDefault();
});
// A folder dropped is no file to send: it is said, and the files beside it come.
composer.addEventListener("drop", (event) => {
  const items = [...(event.dataTransfer?.items || [])].filter((item) => item.kind === "file");
  if (!items.length) return;
  event.preventDefault();
  const files = [];
  for (const item of items) {
    const file = item.getAsFile();
    if (item.webkitGetAsEntry()?.isDirectory) toast(t("{name} is a folder: attach the files in it", { name: file.name }), { error: true });
    else if (file) files.push(file);
  }
  attach(files);
});
// On a phone the column is a page over the chat, and its mark closes it.
$("fold").addEventListener("click", () => {
  if (narrow.matches) app.classList.remove("column-open");
  else setFolded(true);
});
$("unfold").addEventListener("click", (event) => {
  event.stopPropagation();
  if (narrow.matches) app.classList.toggle("column-open");
  else setFolded(false);
});
document.querySelector(".slab").addEventListener("click", (event) => {
  if (!event.target.closest("#composer, #slash")) closeSlash();
  app.classList.remove("column-open");
  if (state.history) closeHistory();
  closeModels();
  setActs(false);
  $("evict").hidden = true;
});
window.addEventListener("popstate", arrive);
// A tab brought back to view is reading its session again, and a line that is
// down tries again at once; another tab of this browser that read one clears
// its mark here too.
document.addEventListener("visibilitychange", () => {
  arrived();
  renderSessions();
  if (!document.hidden) reconnectNow();
});
window.addEventListener("storage", (event) => {
  if (event.key !== SEEN_KEY) return;
  mergeSeen();
  renderSessions();
});
window.addEventListener("hashchange", arrive);

translatePage();
readLink();
state.reveal = state.current;
state.source = safeGet(localStorage, SOURCE_KEY);
if (safeGet(localStorage, SCOPE_KEY) === "all") state.scope = "all";
state.seen = readSeen();
app.classList.toggle("folded", safeGet(localStorage, FOLD_KEY) === "1");
const host = $("host");
host.textContent = location.hostname;
if (location.port) host.append(el("span", "", `:${location.port}`));
placeComposer();
takeDraft();
renderTranscript();
renderHead();
renderSessions();
if (!state.current) renderOpener();
connect();
