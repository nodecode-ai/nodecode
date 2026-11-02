// Both message renderers lock to the core-owned golden color contract.
import fs from "node:fs";
const { markdown } = await import("data:text/javascript;base64," + Buffer.from(fs.readFileSync(process.argv[2], "utf8")).toString("base64"));
const fixture = JSON.parse(fs.readFileSync(process.argv[3], "utf8"));
let failed = 0;
for (const c of fixture.cases) {
  const actual = markdown(c.source);
  if (actual !== c.html) { console.error("FAIL", c.name, JSON.stringify(actual), "expected", JSON.stringify(c.html)); failed++; }
  else console.log("pass", c.name);
}
if (failed) process.exit(1);
console.log("WEB-COLOR-GOLDENS", fixture.cases.length, "passed");
