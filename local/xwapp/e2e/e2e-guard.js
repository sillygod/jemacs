// The e2e's Emacs keeps off your own state (e2e-guard.el, appslib's
// guardEnv): its home is the run's, so ~/.ghostherd and its like are the
// run's; what it still reaches of yours -- here asked for on purpose, by
// your home's own path, only whether it is there -- is noted, and a run
// that did fails; the checkout under test, in a worktree under
// ~/.ghostherd/worktrees maybe, is not yours to keep it off.
const fs = require("fs");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");
const { guardEnv, realTouched } = require("./appslib");
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const dir = fs.mkdtempSync(path.join(os.tmpdir(), "e2e-guard-"));
const real = os.homedir();
const el = `(progn
  (princ (expand-file-name "~/.ghostherd/handoff-leads.eld")) (terpri)
  (princ (expand-file-name "x" user-emacs-directory)) (terpri)
  (princ enable-dir-local-variables) (terpri)
  (file-exists-p "~/.ghostherd/handoff-leads.eld")
  (file-exists-p ${JSON.stringify(path.join(real, ".ghostherd", "e2e-guard-probe-none"))})
  (file-attributes ${JSON.stringify(path.join(real, ".emacs.d", "emacs-home", "e2e-guard-probe-none"))})
  (file-exists-p ${JSON.stringify(path.join(LOCAL, "xwapp", "xwapp.el"))})
  (file-symlink-p ${JSON.stringify(path.dirname(path.dirname(LOCAL)))})
  (file-readable-p ${JSON.stringify(path.join(real, ".emacs.d", "e2e-guard-probe-none"))})
  (file-exists-p ${JSON.stringify(path.join(real, ".ghostherd-not-it"))}))`;
const out = execFileSync(process.env.EMACS || "emacs", ["-Q", "--batch", "-l", path.join(__dirname, "e2e-guard.el"), "--eval", el],
  { encoding: "utf8", env: Object.assign({}, process.env, guardEnv(dir), { E2E_DIR: dir, E2E_LOCAL: LOCAL }) }).split("\n");
check("~/.ghostherd is the run's, under its own home", out[0].startsWith(path.join(dir, "home")), out[0]);
check("so is user-emacs-directory", out[1].startsWith(path.join(dir, "home")), out[1]);
const touched = realTouched(dir);
// Your ~/.emacs.d noted too unless it is the checkout under test.
const inEmacsD = LOCAL.startsWith(path.join(real, ".emacs.d") + path.sep);
check("what reached your own ~/.ghostherd, ~/.emacs.d/emacs-home and ~/.emacs.d is noted, each once",
  touched.length === (inEmacsD ? 2 : 3) && /^file-exists-p .*\/\.ghostherd\/e2e-guard-probe-none$/.test(touched[0]) &&
  /^file-attributes .*\/\.emacs\.d\/emacs-home\/e2e-guard-probe-none$/.test(touched[1]) &&
  (inEmacsD || /^file-readable-p .*\/\.emacs\.d\/e2e-guard-probe-none$/.test(touched[2])), touched);
check("not the checkout under test, a directory above it, nor a name that only begins the same",
  !touched.some((t) => t.includes("xwapp.el") || t.includes("ghostherd-not-it") || /^file-symlink-p /.test(t)));
check("no .dir-locals.el looked for", out[2] === "nil", out[2]);
fs.rmSync(dir, { recursive: true, force: true });
const failed = results.filter((r) => !r).length;
console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
process.exit(failed ? 1 : 0);
