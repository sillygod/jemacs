// Every suite in turn, or those named: node run.js kit narrow
const path = require("path");
const { spawnSync } = require("child_process");

const SUITES = ["kit", "herd-chips", "apps-phone", "narrow", "cu-edit"];
const names = process.argv.length > 2 ? process.argv.slice(2) : SUITES;
const failed = [];
for (const name of names) {
  console.log("\n== " + name);
  const r = spawnSync(process.execPath, [path.join(__dirname, name.replace(/\.js$/, "") + ".js")], { stdio: "inherit" });
  if (r.status !== 0) failed.push(name);
}
console.log(failed.length ? "\nfailed: " + failed.join(", ") : "\nall " + names.length + " suites passed");
process.exit(failed.length ? 1 : 0);
