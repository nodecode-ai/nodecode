// md.js -- the markdown a model writes, as HTML: fences, headings, lists
// (nested, loose, numbered from where they start, task boxes), quotes, tables,
// rules, and the inline marks. Everything is escaped first; a link survives
// only as http, https or mailto, and an image only as the place its caller
// puts a picture it holds a fact for. An unclosed fence -- an answer still
// streaming -- is code to the end.

const escapeHtml = (text) =>
  text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");

// A link to HREF (escaped, as the text around it is) named LABEL, or null
// when HREF is not a scheme the page follows.
const link = (href, label, schemes = /^(https?:|mailto:)/i) => {
  const url = href.replace(/&amp;/g, "&");
  return schemes.test(url) ? `<a href="${escapeHtml(url)}" target="_blank" rel="noopener noreferrer">${label}</a>` : null;
};

// The image targets the render under way may draw: markdown's second argument.
let pictures = [];
const unescapeHtml = (html) =>
  html.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&amp;/g, "&");

// A link's or an image's target, escaped as the text around it is: in angle
// brackets, which may hold spaces, the first group and the target what they
// hold; else bare, the second. The engine reads an image's target the same
// way (exec.lisp MARKDOWN-IMAGE-TARGETS).
const TARGET = String.raw`\((?:&lt;((?:(?!&[lg]t;)[^\u0000])+)&gt;|((?!&lt;)[^)\s\u0000]+))\)`;
const IMAGE = new RegExp(String.raw`!\[([^\]]*)\]${TARGET}`, "g");
const LINK = new RegExp(String.raw`\[([^\]]+)\]${TARGET}`, "g");

// The hex-only foreground contract is shared with the TUI and locked by
// src/src/tui/inline-colors.json in Nodecode. Nothing else is raw HTML.
const COLOR_OPEN = /^<span style="color: *(#[0-9a-fA-F]{6})">/;
function colorRun(text, start) {
  const open = text.slice(start).match(COLOR_OPEN);
  if (!open) return null;
  const body = start + open[0].length;
  let at = body, depth = 1;
  while (at < text.length) {
    if (text[at] === "\\") { at += 2; continue; }
    if (text[at] === "`") {
      const close = text.indexOf("`", at + 1);
      at = close < 0 ? at + 1 : close + 1;
      continue;
    }
    if (text.startsWith("</span>", at)) {
      if (--depth === 0) return { body, close: at, end: at + 7, color: open[1] };
      at += 7;
      continue;
    }
    const nested = text.slice(at).match(COLOR_OPEN);
    if (nested) {
      if (++depth > 16) return null;
      at += nested[0].length;
    } else at++;
  }
  return null;
}

function colored(text, hold, depth, held) {
  if (depth >= 16) return text;
  let out = "", from = 0, at = 0;
  while (at < text.length) {
    if (text[at] === "\\") { at += 2; continue; }
    if (text[at] === "`") {
      const close = text.indexOf("`", at + 1);
      at = close < 0 ? at + 1 : close + 1;
      continue;
    }
    const run = text[at] === "<" && colorRun(text, at);
    if (run) {
      out += text.slice(from, at) + hold(`<span class="inline-color" data-color="${run.color}">${inline(text.slice(run.body, run.close), depth + 1, held)}</span>`);
      at = from = run.end;
    } else at++;
  }
  return out + text.slice(from);
}

function inline(text, depth = 0, held = []) {
  // Backslash escapes and code spans first, held aside so nothing in them is
  // read as a mark: an escaped character is itself, and a backslash inside a
  // code span is only a backslash.
  if (depth === 0) text = text.replace(/\u0000/g, "␀");
  const hold = (html) => `\u0000${held.push(html) - 1}\u0000`;
  let out = text.replace(/\\([!-/:-@[-`{-~])|(`+)([^`]|[^`][\s\S]*?[^`])\2(?!`)/g,
    (_, escaped, _ticks, code) => hold(escaped ? escapeHtml(escaped) : `<code>${escapeHtml(code.trim())}</code>`));
  out = escapeHtml(colored(out, hold, depth, held))
    // The page loads no picture from elsewhere (its img-src is its own), so an
    // image is a link to it, named by its alt text -- but for a target among
    // PICTURES, which leaves a mark where the caller puts the picture.
    .replace(IMAGE, (whole, alt, bracketed, bare) => {
      const href = bracketed ?? bare;
      return pictures.includes(unescapeHtml(href))
        ? hold(`<span class="picture" data-target="${href}">${alt}</span>`)
        : link(href, alt || href, /^https?:/i) ?? whole;
    })
    .replace(LINK, (whole, label, bracketed, bare) => link(bracketed ?? bare, label) ?? whole)
    .replace(/(^|[\s(])(https?:\/\/[^\s<)\u0000]+[^\s<).,;:!?'"\u0000])/g, (_, lead, url) => `${lead}${link(url, url)}`)
    .replace(/\*\*(?=\S)([\s\S]*?\S)\*\*/g, "<strong>$1</strong>")
    .replace(/__(?=\S)([\s\S]*?\S)__/g, "<strong>$1</strong>")
    .replace(/(^|[^*\w])\*(?=\S)([^*]*?\S)\*(?![*\w])/g, "$1<em>$2</em>")
    .replace(/(^|[^_\w])_(?=\S)([^_]*?\S)_(?![_\w])/g, "$1<em>$2</em>")
    .replace(/~~(?=\S)([\s\S]*?\S)~~/g, "<del>$1</del>");
  // What was held comes back, and what it held in turn: a picture's alt text
  // may carry a code span.
  const restore = (html) => html.replace(/\u0000(\d+)\u0000/g, (_, index) => restore(held[Number(index)]));
  return restore(out);
}

// A table row's cells. A pipe inside a code span is the code's, and \| is a
// pipe that does not split.
function cells(line) {
  const row = [""];
  for (const [piece] of line.trim().replace(/^\|/, "").matchAll(/(`+)[\s\S]*?\1|\\\||[\s\S]/g)) {
    if (piece === "|") row.push("");
    else row[row.length - 1] += piece.replace(/\\\|/g, "|");
  }
  if (row.length > 1 && !row[row.length - 1].trim()) row.pop();
  return row.map((cell) => cell.trim());
}

// The row under a table's head: a dash or more per column, a colon at either
// end saying where the column stands -- :-- left, --: right, :-: center.
const DELIMITER = /^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)*\|?\s*$/;
const alignments = (line) =>
  cells(line).map((cell) => (/^:.*:$/.test(cell) ? "center" : /:$/.test(cell) ? "right" : /^:/.test(cell) ? "left" : ""));

const cell = (tag, text, align) => `<${tag}${align ? ` data-align="${align}"` : ""}>${inline(text)}</${tag}>`;

// A diff's lines, each marked by what it does: added, removed, or a hunk's
// head (and the file names above the first hunk).
const diffLines = (text) =>
  text.replace(/\n$/, "").split("\n").map((line) => {
    const kind = /^(@@|\+\+\+ |--- )/.test(line) ? "diff-hunk" : line[0] === "+" ? "diff-add" : line[0] === "-" ? "diff-del" : "";
    return kind ? `<span class="${kind}">${escapeHtml(line)}</span>` : escapeHtml(line);
  }).join("\n");

// A diff as the page draws one: every line escaped, the added ones green,
// the removed ones red, a hunk's head dim.
export function diffHtml(text) {
  return `<pre class="diff"><code>${diffLines(text)}</code></pre>`;
}

const FENCE = /^\s*(```+|~~~+)\s*([\w+.#-]*)/;
const ITEM = /^(\s*)([-*+]|\d{1,9}[.)])\s+(.*)$/;
const indentOf = (line) => line.match(/^\s*/)[0].length;
const ordered = (marker) => /\d/.test(marker);
const filled = (lines, i) => {
  while (i < lines.length && !lines[i].trim()) i++;
  return i;
};

// A list, from its first item at lines[start]: each item takes the lines
// indented under it -- blank lines between them, further paragraphs, a
// fence, a list nested inside -- and is read as markdown of its own. Items
// apart by blank lines are still one list, so a loose list keeps counting.
function list(lines, start) {
  const first = lines[start].match(ITEM);
  const items = [];
  let i = start;
  for (;;) {
    const item = lines[i].match(ITEM);
    const indent = item[1].length;
    // A line under the item loses the indent up to where the item's text starts.
    const outdent = new RegExp(`^\\s{0,${indent + item[2].length + 1}}`);
    const under = (line) => line.replace(outdent, "");
    const body = [item[3]];
    for (i++; i < lines.length; i++) {
      // The item holds each line indented under it, and a blank one when the
      // next line with anything on it is.
      const next = filled(lines, i);
      if (next === lines.length || indentOf(lines[next]) <= indent) break;
      body.push(under(lines[i]));
      // A fence inside the item runs to its close, however its lines are indented.
      const fence = lines[i].match(FENCE);
      if (fence) for (i++; i < lines.length; i++) {
        body.push(under(lines[i]));
        if (lines[i].trim().startsWith(fence[1])) break;
      }
    }
    items.push(body);
    const next = filled(lines, i);
    const sibling = next < lines.length && lines[next].match(ITEM);
    if (!sibling || ordered(sibling[2]) !== ordered(first[2])) break;
    i = next;
  }
  const tag = ordered(first[2]) ? "ol" : "ul";
  const from = parseInt(first[2], 10);
  const html = items.map((body) => {
    const task = body[0].match(/^\[([ xX])\](?:\s+(.*))?$/);
    if (task) body[0] = task[2] ?? "";
    const inner = blocks(body);
    // A single paragraph is the item's text; anything more keeps its blocks.
    const content = inner.length === 1 && inner[0].startsWith("<p>") ? inner[0].slice(3, -4) : inner.join("");
    return task
      ? `<li class="task"><input type="checkbox" disabled${task[1] === " " ? "" : " checked"}>${content}</li>`
      : `<li>${content}</li>`;
  });
  return { html: `<${tag}${tag === "ol" && from !== 1 ? ` start="${from}"` : ""}>${html.join("")}</${tag}>`, end: i };
}

function blocks(lines) {
  const out = [];
  let paragraph = [];
  const flush = () => {
    if (paragraph.length) out.push(`<p>${paragraph.map(inline).join("<br>")}</p>`);
    paragraph = [];
  };

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const fence = line.match(FENCE);
    if (fence) {
      flush();
      const body = [];
      for (i++; i < lines.length && !lines[i].trim().startsWith(fence[1]); i++) body.push(lines[i]);
      const lang = fence[2] ? ` data-lang="${escapeHtml(fence[2])}"` : "";
      out.push(fence[2] === "diff"
        ? `<pre class="diff"${lang}><code>${diffLines(body.join("\n"))}</code></pre>`
        : `<pre${lang}><code>${escapeHtml(body.join("\n"))}</code></pre>`);
      continue;
    }
    if (!line.trim()) { flush(); continue; }
    const heading = line.match(/^(#{1,6})\s+(.*?)\s*#*\s*$/);
    if (heading) {
      flush();
      const level = heading[1].length;
      out.push(`<h${level}>${inline(heading[2])}</h${level}>`);
      continue;
    }
    if (/^\s*([-*_])(\s*\1){2,}\s*$/.test(line)) { flush(); out.push("<hr>"); continue; }
    if (/^\s*>/.test(line)) {
      flush();
      const quoted = [];
      for (; i < lines.length && /^\s*>/.test(lines[i]); i++) quoted.push(lines[i].replace(/^\s*>\s?/, ""));
      i--;
      out.push(`<blockquote>${blocks(quoted).join("")}</blockquote>`);
      continue;
    }
    if (line.includes("|") && i + 1 < lines.length && lines[i + 1].includes("|") && DELIMITER.test(lines[i + 1])) {
      flush();
      const head = cells(line);
      const align = alignments(lines[i + 1]);
      const body = [];
      for (i += 2; i < lines.length && lines[i].includes("|") && lines[i].trim(); i++) body.push(cells(lines[i]));
      i--;
      // Its own box scrolls sideways, so a wide table never widens the page.
      out.push(`<div class="table-scroll"><table><thead><tr>${head.map((text, at) => cell("th", text, align[at])).join("")}</tr></thead><tbody>${
        body.map((row) => `<tr>${row.map((text, at) => cell("td", text, align[at])).join("")}</tr>`).join("")
      }</tbody></table></div>`);
      continue;
    }
    if (ITEM.test(line)) {
      flush();
      const { html, end } = list(lines, i);
      out.push(html);
      i = end - 1;
      continue;
    }
    paragraph.push(line);
  }
  flush();
  return out;
}

// SOURCE as HTML. An image whose target is one of NAMED is a
// span.picture[data-target] holding its alt text, for the caller to put the
// picture in place of; every other image is a link, or the text it was.
export function markdown(source, named = []) {
  pictures = named;
  try {
    return blocks(source.replace(/\r\n?/g, "\n").split("\n")).join("");
  } finally {
    pictures = [];
  }
}
