// Every suite in turn, or those named: node run.js kit narrow
const path = require("path");
const { spawnSync } = require("child_process");

const SUITES = ["kit", "herd-chips", "herd-forms", "herd-relay", "herd-handoff", "herd-primary", "herd-changes", "herd-foot", "herd-copy", "herd-meter", "herd-order", "herd-subs", "herd-subview", "herd-earlier", "herd-log", "herd-chat", "herd-login", "herd-sleep", "herd-phone", "herd-scrollbar", "herd-memory", "herd-source", "apps-phone", "pr-local", "pr-review", "pr-range", "pr-pr-review", "pr-editors", "pr-reuse", "narrow", "cu-edit"];
const names = process.argv.length > 2 ? process.argv.slice(2) : SUITES;
const failed = [];
for (const name of names) {
  console.log("\n== " + name);
  const r = spawnSync(process.execPath, [path.join(__dirname, name.replace(/\.js$/, "") + ".js")], { stdio: "inherit" });
  if (r.status !== 0) failed.push(name);
}
console.log(failed.length ? "\nfailed: " + failed.join(", ") : "\nall " + names.length + " suites passed");
process.exit(failed.length ? 1 : 0);
