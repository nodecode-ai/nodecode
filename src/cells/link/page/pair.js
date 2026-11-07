// pair.js -- a browser the machine has not let in asks to be, and waits.
// The machine answers POST /_link/ask with a code its operator's terminal shows
// too; GET /_link/ask?id= waits until the operator allows or denies it there,
// and an allowed answer carries the cookie this browser keeps for the machine.

const $ = (id) => document.getElementById(id);
const machine = location.hostname.split(".")[0];
$("machine").textContent = machine;

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

function say(text) {
  $("said").textContent = text;
}

function finish(text) {
  say(text);
  $("code").hidden = true;
  $("wait").hidden = true;
  $("ask").hidden = false;
  $("ask").disabled = false;
  $("ask").textContent = "Ask again";
}

let ticker = null;
function countDown(until) {
  clearInterval(ticker);
  const tick = () => {
    const left = Math.max(0, Math.ceil((until - Date.now()) / 1000));
    $("wait").textContent = `Waiting for ${machine} · ${Math.floor(left / 60)}:${String(left % 60).padStart(2, "0")}`;
  };
  tick();
  ticker = setInterval(tick, 1000);
}

async function ask() {
  $("ask").disabled = true;
  let answer;
  try {
    const response = await fetch("/_link/ask", { method: "POST" });
    answer = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(answer?.error?.message || `the machine answered ${response.status}`);
  } catch (error) {
    finish(`Couldn't ask: ${error.message}.`);
    return;
  }
  if (answer.state === "allowed") return location.replace("/web/");
  $("ask").hidden = true;
  $("code").textContent = answer.code;
  $("code").hidden = false;
  $("wait").hidden = false;
  say(`Check that ${answer.machine || machine} shows this same code, in /link or on its page's Link tab, then allow it there.`);
  countDown(Date.now() + answer.seconds * 1000);
  for (;;) {
    let state;
    try {
      const response = await fetch(`/_link/ask?id=${encodeURIComponent(answer.id)}`);
      state = (await response.json()).state;
    } catch {
      await sleep(2000);
      continue;
    }
    if (state === "allowed") {
      clearInterval(ticker);
      return location.replace("/web/");
    }
    if (state === "denied") return finish("The machine said no.");
    if (state === "expired") return finish("The code expired before it was allowed.");
  }
}

$("ask").addEventListener("click", ask);
