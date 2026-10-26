"""The herd's checks: a fact of the shared memory as a rule run before a shell call.

Some facts guard against real damage -- the live session index opened
twice, a CPU pegged by a regex over a CLI binary, the user's Emacs frozen
by a probe -- yet live only as prose an agent may skip when its context is
long.  A check is such a fact made into a rule: before each shell call the
agent makes, the CLI's PreToolUse hook runs `herd lint', and a call a rule
matches is denied with the fact's name and a hint saying what to do
instead.  (ask bd7efaa8)

What a check may do, and no more:

- Deny, only.  It never allows: a call it does not match goes on to
  whatever the CLI or the guard would do with it.  For a guarded agy agent
  it runs inside `herd guard', where it can turn an allow into a deny and
  nothing else.
- Fail open.  An unreadable rules file, a rule that does not compile, a
  command it cannot read, its own error, or a match past its time budget:
  no decision -- the call goes on as if there were no check.  A broken
  check must never stop work.
- Shell calls only: claude's Bash, grok's run_terminal_command, agy's
  run_command.
- Read here, from files: never the sidecar, before the answer.  The rules
  are the herd's own store, outside any repo -- `<lint dir>/user.json' for
  every agent, `<lint dir>/<project slug>.json' for a project's -- and only
  the herd writes them, when the user approves a rule on the page.  A
  denial is told to the herd afterwards, for the log and the rule's count.

A rule, one per fact (the fact is its name):

  {"fact": "grep-cli-binaries-cheaply", "status": "on",
   "hint": "Run `strings FILE | grep -F ...' instead ...",
   "check": {"command": {"all": [RE...], "none": [RE...]},
             "code":    {"all": [...], "none": [...]},
             "line":    {"all": [...], "none": [...]}},
   "catches": [COMMAND...], "passes": [COMMAND...]}

`check' looks at the command three ways, and every way it names must
match: `command', one simple command of the line at a time -- the one
whose text matches every `all' and no `none' (its leading VAR=value words
and wrappers -- env, timeout, nice, nohup, time, command, exec, rtk -- cut
off, and one that reads a pipe written `| grep ...'); `code', a piece of
code the line hands an interpreter (python -c, `python - <<EOF', a script
it writes and runs in the same line, node -e, bash -c ...); `line', the
whole line.  What a line hands a program as text -- a heredoc to cat, to
`herd reply', to `git commit -F -' -- is in none of them: a commit message
or an answer that names a pattern is not a call that runs it.  Patterns
are Python regular expressions, `.' matching a newline too.  `catches' and
`passes' are the rule's own examples, checked when it is proposed.

A check catches a mistake, not an agent working round it: what a script
file run by name does, or a command built in a variable, it cannot see.

Python 3.8, stdlib only: it runs under whatever python3 the agent's shell
finds, before every shell call -- so what it imports is little, and what
it reads is two small files.
"""

from __future__ import annotations

import json
import os
import re
import sys

LINT_DIR = "~/.ghostherd/lint"
USER_KEY = "user"
VIEWS = ("command", "code", "line")
# What a check may spend on one call, past which it makes no decision.
BUDGET_SECONDS = 1.0
# Shell tools, by CLI (grok's hook may name it by claude's name too).
SHELL_TOOLS = {"Bash", "run_terminal_command", "run_command"}
HINT_MAX = 600
EXAMPLES_MAX = 20


class Rule:
    "One fact's check, compiled."

    def __init__(self, data):
        self.fact = str(data["fact"])
        self.hint = str(data.get("hint") or "").strip()
        self.views = {}
        check = data.get("check") or {}
        for view in VIEWS:
            spec = check.get(view)
            if spec is None:
                continue
            self.views[view] = ([re.compile(p, re.S) for p in spec.get("all") or []],
                                [re.compile(p, re.S) for p in spec.get("none") or []])

    def _fits(self, view, text):
        need, refuse = self.views[view]
        return all(p.search(text) for p in need) and not any(p.search(text) for p in refuse)

    def matches(self, seen):
        "Whether SEEN, a command's `Views', is what this rule denies."
        if not self.views:
            return False
        for view in self.views:
            items = [seen.line] if view == "line" else getattr(seen, view)
            if not any(self._fits(view, t) for t in items):
                return False
        return True


# ---- Reading a shell line ----------------------------------------------------
#
# Enough of a shell parser to tell what runs from what is text: simple
# commands split at ; & && || | and newlines, $( ) and backticks read as
# commands of their own, and each heredoc's body given to the command
# that owns it.  Not a shell: what it gets wrong, it gets wrong quietly,
# and a check is a net for mistakes, not a guard.

_INTERPRETER = re.compile(r"(python[\d.]*|pypy[\d.]*|node|nodejs|deno|bun|ruby|perl|php|osascript)\Z")
_SHELLS = re.compile(r"(sh|bash|zsh|dash|ksh|fish)\Z")
_CODE_FLAGS = ("-c", "-e", "--eval", "-E")
_WRAPPER = re.compile(
    r"""\A\s*(?:
          [A-Za-z_][A-Za-z0-9_]*=(?:'[^']*'|"(?:\\.|[^"\\])*"|[^\s;&|]*)\s+   # VAR=value
        | (?:do|then|else|elif|if|while|until|!|\{)\s+                       # a shell keyword
        | (?:env|nohup|time|command|exec|builtin)\s+(?:-\S+\s+)*
        | nice\s+(?:-n\s*)?(?:-?\d+\s+)?
        | timeout\s+(?:-\S+\s+)*\S+\s+
        | rtk\s+(?:proxy\s+)?
        )+""", re.X)
_HEREDOC = re.compile(r"""<<(-?)\s*(?:'([^']*)'|"([^"]*)"|\\?([^\s;&|<>()'"]+))""")
_WRITE_TARGET = re.compile(r"""(?:^|\s)(?:>|>>)\s*(?:'([^']+)'|"([^"]+)"|([^\s;&|<>]+))""")


class Views:
    """A command line, three ways: COMMAND, its simple commands (text, the
    wrappers cut off; one that reads a pipe as `| ...'); CODE, what it
    hands an interpreter; LINE, the whole of it but the text it hands
    other programs."""

    def __init__(self, command):
        self.command = []
        self.code = []
        self._raw = []
        self._written = {}
        self._scan(command, 0)
        self.line = "\n".join(self._raw)

    def _scan(self, text, depth):
        if depth > 8:
            return
        segs = _split(text)
        for raw, piped, bodies in segs:
            for body in _substitutions(raw):
                self._scan(body, depth + 1)
            self._raw.append(raw)
            words = _words(raw)
            norm = _WRAPPER.sub("", raw, count=1).strip()
            self.command.append(("| " if piped else "") + norm)
            prog, args = _program(words)
            name = os.path.basename(prog)
            if _SHELLS.match(name):
                for arg in _code_args(args):
                    self._scan(arg, depth + 1)
                for body in bodies:
                    self._scan(body, depth + 1)
                for path in _plain_args(args)[:1]:
                    self._run_written(path, depth, shell=True)
            elif _INTERPRETER.match(name):
                self.code.extend(_code_args(args))
                self.code.extend(bodies)
                for path in _plain_args(args)[:1]:
                    self._run_written(path, depth, shell=False)
            elif bodies and name in ("cat", "tee"):
                # A heredoc written to a file is kept: the same line may run it.
                target = _WRITE_TARGET.search(raw)
                files = ([next(g for g in target.groups() if g)] if target
                         else _plain_args(args)[:1] if name == "tee" else [])
                for path in files:
                    self._written[_unquote(path)] = "\n".join(bodies)

    def _run_written(self, path, depth, shell):
        body = self._written.get(_unquote(path))
        if body is None:
            return
        if shell:
            self._scan(body, depth + 1)
        else:
            self.code.append(body)


def _split(text):
    """[(raw, piped, heredoc bodies)]: TEXT's simple commands, in order."""
    out = []
    i, n = 0, len(text)
    start = 0
    piped = False
    pending = []          # this line's heredocs: [(delim, strip, owner's index)]

    def close(end, next_piped):
        nonlocal start, piped
        raw = text[start:end].strip()
        if raw:
            out.append([raw, piped, []])
            idx = len(out) - 1
            for k, (_d, _s, owner) in enumerate(pending):
                if owner is None:
                    pending[k] = (_d, _s, idx)
        piped = next_piped
        start = end

    while i < n:
        c = text[i]
        if c == "\\":
            i += 2
            continue
        if c == "'":
            j = text.find("'", i + 1)
            i = n if j < 0 else j + 1
            continue
        if c == '"':
            i = _skip_double(text, i + 1)
            continue
        if c == "`":
            j = i + 1
            while j < n and text[j] != "`":
                j += 2 if text[j] == "\\" else 1
            i = j + 1
            continue
        if c == "$" and text.startswith("$(", i):
            i = _skip_parens(text, i + 2)
            continue
        if c == "#" and (i == start or text[i - 1] in " \t\n;&|("):
            j = text.find("\n", i)
            # A comment: gone from the command's text.
            text = text[:i] + ("" if j < 0 else text[j:])
            n = len(text)
            continue
        if c == "<" and text.startswith("<<", i) and not text.startswith("<<<", i):
            m = _HEREDOC.match(text, i)
            if m:
                delim = next(g for g in m.groups()[1:] if g is not None)
                pending.append((delim, m.group(1) == "-", None))
                i = m.end()
                continue
        if c in ";&|\n()":
            two = text[i:i + 2]
            if c == "&" and (i > 0 and text[i - 1] in "<>" or two == "&>"):
                i += 1
                continue
            if c == "|" and two == "|&":
                close(i, True)
                i += 2
                start = i
                continue
            if two in ("&&", "||", ";;"):
                close(i, False)
                i += 2
                start = i
                continue
            close(i, c == "|")
            i += 1
            start = i
            if c == "\n" and pending:
                i = _bodies(text, i, pending, out)
                start = i
                pending = []
            continue
        i += 1
    close(n, False)   # a heredoc with no line after it has no body
    return [(raw, piped, bodies) for raw, piped, bodies in out]


def _bodies(text, i, pending, out):
    """Read the bodies of PENDING heredocs from TEXT at I; give each to its
    command.  Return where the line after the last one starts."""
    n = len(text)
    for delim, strip, owner in pending:
        lines = []
        while i < n:
            j = text.find("\n", i)
            end = n if j < 0 else j
            line = text[i:end]
            i = n if j < 0 else j + 1
            if (line.lstrip("\t") if strip else line).rstrip() == delim:
                break
            lines.append(line)
        if owner is not None:
            out[owner][2].append("\n".join(lines))
    return i


def _skip_double(text, i):
    n = len(text)
    while i < n:
        c = text[i]
        if c == "\\":
            i += 2
            continue
        if c == '"':
            return i + 1
        if c == "$" and text.startswith("$(", i):
            i = _skip_parens(text, i + 2)
            continue
        i += 1
    return n


def _skip_parens(text, i):
    "Past the `)' closing a `$(' whose inside starts at I."
    depth, n = 1, len(text)
    while i < n:
        c = text[i]
        if c == "\\":
            i += 2
            continue
        if c == "'":
            j = text.find("'", i + 1)
            i = n if j < 0 else j + 1
            continue
        if c == '"':
            i = _skip_double(text, i + 1)
            continue
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return i + 1
        i += 1
    return n


def _substitutions(raw):
    "The commands inside RAW's $( ) and backticks (not in single quotes)."
    out = []
    i, n = 0, len(raw)
    while i < n:
        c = raw[i]
        if c == "\\":
            i += 2
            continue
        if c == "'":
            j = raw.find("'", i + 1)
            i = n if j < 0 else j + 1
            continue
        if c == "$" and raw.startswith("$(", i):
            end = _skip_parens(raw, i + 2)
            out.append(raw[i + 2:end - 1])
            i = end
            continue
        if c == "`":
            j = raw.find("`", i + 1)
            if j < 0:
                break
            out.append(raw[i + 1:j])
            i = j + 1
            continue
        i += 1
    return out


def _words(raw):
    try:
        import shlex
        return shlex.split(raw, comments=False, posix=True)
    except ValueError:
        return raw.split()


_ASSIGN = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")
_RUNNERS = {"env", "nohup", "time", "command", "exec", "builtin", "nice", "timeout", "xargs",
            "sudo", "doas"}
_KEYWORDS = {"do", "then", "else", "elif", "if", "while", "until", "!", "{"}


def _program(words):
    "(program, its args): past assignments and wrappers, and `uv run' and the like."
    w = list(words)
    while w:
        head = w[0]
        if _ASSIGN.match(head) or head in _KEYWORDS:
            w.pop(0)
        elif head in ("rtk",):
            w.pop(0)
            if w and w[0] == "proxy":
                w.pop(0)
        elif head in _RUNNERS:
            w.pop(0)
            while w and w[0].startswith("-"):
                w.pop(0)
            if head == "timeout" and w:
                w.pop(0)
            if head == "nice" and w and re.match(r"-?\d+\Z", w[0]):
                w.pop(0)
        elif head in ("uv", "poetry", "pipenv", "pdm", "hatch", "rye") and "run" in w[1:3]:
            rest = w[w.index("run") + 1:]
            k = next((k for k, x in enumerate(rest)
                      if _INTERPRETER.match(os.path.basename(x)) or _SHELLS.match(os.path.basename(x))), None)
            if k is None:
                return head, w[1:]
            w = rest[k:]
            break
        else:
            break
    return (w[0], w[1:]) if w else ("", [])


def _code_args(args):
    "The code an interpreter's ARGS hand it: the word after -c, -e..."
    out = []
    for k, a in enumerate(args[:-1]):
        if a in _CODE_FLAGS or (len(a) > 2 and a[0] == "-" and a[1] != "-" and a[-1] in "ce"
                                and a[1:].isalpha()):
            out.append(args[k + 1])
    return out


def _plain_args(args):
    "ARGS that are not options: a script's path comes first."
    out, skip = [], False
    for a in args:
        if skip:
            skip = False
            continue
        if a in _CODE_FLAGS:
            return []   # code given inline: no script
        if a in ("-m",):
            return []
        if a.startswith("-"):
            continue
        out.append(a)
    return out


def _unquote(s):
    s = s.strip()
    if len(s) >= 2 and s[0] == s[-1] and s[0] in "'\"":
        s = s[1:-1]
    return s


# ---- The rules -------------------------------------------------------------------


def lint_dir():
    "Where the rules live: GHOSTHERD_LINT_DIR, or ~/.ghostherd/lint."
    return os.path.expanduser(os.environ.get("GHOSTHERD_LINT_DIR") or LINT_DIR)


def rules_path(key, root=None):
    if not re.match(r"[A-Za-z0-9_.-]+\Z", key or "") or key in (".", ".."):
        raise ValueError("not a store key: %r" % key)
    return os.path.join(root or lint_dir(), key + ".json")


def read_rules(key, root=None):
    "Store KEY's rules as written, [] when it has none; raises on a bad file."
    try:
        with open(rules_path(key, root), encoding="utf-8") as f:
            data = json.load(f)
    except FileNotFoundError:
        return []
    rules = data.get("rules") if isinstance(data, dict) else None
    if not isinstance(rules, list):
        raise ValueError("%s: no rules list" % rules_path(key, root))
    return [r for r in rules if isinstance(r, dict)]


def write_rules(key, rules, root=None):
    "Store KEY's RULES, whole, in one step: the hook never reads half a file."
    path = rules_path(key, root)
    os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
    tmp = "%s.%d.tmp" % (path, os.getpid())
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump({"rules": rules}, f, indent=2, ensure_ascii=False)
        f.write("\n")
    os.replace(tmp, path)


def active(keys, root=None):
    """The rules that are on, of every store in KEYS, compiled.  A file that
    cannot be read, or a rule that does not compile, is left out -- it
    makes no decision."""
    out = []
    for key in keys:
        try:
            rules = read_rules(key, root)
        except (OSError, ValueError):
            continue
        for data in rules:
            if data.get("status") != "on":
                continue
            try:
                out.append((key, Rule(data)))
            except (KeyError, TypeError, ValueError, re.error):
                continue
    return out


def first_match(rules, command):
    "The first of RULES ((key, Rule)...) that denies COMMAND, or None."
    if not rules or not command.strip():
        return None
    seen = Views(command)
    for key, rule in rules:
        if rule.matches(seen):
            return key, rule
    return None


def validate(data):
    """DATA as a rule may be stored -- its fact, hint, check, catches and
    passes -- or ValueError saying what is wrong.  Every catch must be
    denied by the rule and every pass let through: a proposal shows what
    it does, and is held to it."""
    if not isinstance(data, dict):
        raise ValueError("a rule is a JSON object")
    fact = str(data.get("fact") or "").strip()
    if not fact:
        raise ValueError("a rule names its fact")
    hint = str(data.get("hint") or "").strip()
    if not hint:
        raise ValueError("a rule has a hint: what to do instead")
    if len(hint) > HINT_MAX:
        raise ValueError("the hint is %d characters; keep it under %d" % (len(hint), HINT_MAX))
    check = data.get("check")
    if not isinstance(check, dict) or not check:
        raise ValueError("a rule has a check: {command|code|line: {all: [...], none: [...]}}")
    clean = {}
    for view, spec in check.items():
        if view not in VIEWS:
            raise ValueError("no view %r: a check looks at %s" % (view, ", ".join(VIEWS)))
        if not isinstance(spec, dict) or set(spec) - {"all", "none"}:
            raise ValueError("%s: {all: [...], none: [...]}" % view)
        pats = {}
        for part in ("all", "none"):
            got = spec.get(part) or []
            if not isinstance(got, list) or not all(isinstance(p, str) and p for p in got):
                raise ValueError("%s.%s: a list of patterns" % (view, part))
            for p in got:
                try:
                    re.compile(p, re.S)
                except re.error as exc:
                    raise ValueError("%s.%s: %r does not compile: %s" % (view, part, p, exc))
            if got:
                pats[part] = got
        if not pats.get("all"):
            raise ValueError("%s: at least one pattern in `all'" % view)
        clean[view] = pats
    examples = {}
    for part in ("catches", "passes"):
        got = data.get(part) or []
        if not isinstance(got, list) or not all(isinstance(c, str) and c.strip() for c in got):
            raise ValueError("%s: a list of commands" % part)
        if not got:
            raise ValueError("a rule shows at least one command it %s"
                             % ("denies (catches)" if part == "catches" else "lets through (passes)"))
        if len(got) > EXAMPLES_MAX:
            raise ValueError("%s: %d at most" % (part, EXAMPLES_MAX))
        examples[part] = got
    rule = Rule({"fact": fact, "hint": hint, "check": clean})
    for c in examples["catches"]:
        if not rule.matches(Views(c)):
            raise ValueError("it does not catch its own example: %s" % c)
    for c in examples["passes"]:
        if rule.matches(Views(c)):
            raise ValueError("it denies its own pass example: %s" % c)
    return {"fact": fact, "hint": hint, "check": clean,
            "catches": examples["catches"], "passes": examples["passes"]}


# ---- Which stores, for this call ----------------------------------------------


def slug(path):
    "PATH as a store key: shared_memory.slug_for_path."
    return re.sub(r"[/.]", "-", os.path.abspath(os.path.expanduser(path)))


def project_of(cwd):
    """The project CWD is in: its git checkout -- a worktree's main
    checkout -- or CWD itself when it is in none."""
    d = os.path.abspath(cwd)
    while True:
        dot = os.path.join(d, ".git")
        if os.path.isdir(dot):
            return d
        if os.path.isfile(dot):
            try:
                with open(dot, encoding="utf-8") as f:
                    head = f.read(4096)
            except OSError:
                return d
            m = re.match(r"gitdir:\s*(.+?)\s*$", head, re.M)
            if m:
                gitdir = os.path.normpath(os.path.join(d, m.group(1)))
                # <main>/.git/worktrees/<name>: the main checkout's store.
                parts = gitdir.split(os.sep)
                if len(parts) >= 3 and parts[-2] == "worktrees" and parts[-3] == ".git":
                    return os.sep.join(parts[:-3]) or os.sep
            return d
        up = os.path.dirname(d)
        if up == d:
            return os.path.abspath(cwd)
        d = up


def keys_for(cwd):
    "The stores whose rules apply in CWD: the user's, and its project's."
    keys = [USER_KEY]
    if cwd:
        keys.append(slug(project_of(cwd)))
    return keys


# ---- The hook ---------------------------------------------------------------------


def shell_call(payload):
    """(cli, command, cwd) for a shell call's PreToolUse PAYLOAD, or None for
    any other tool: claude's Bash, grok's run_terminal_command, agy's
    run_command (its guard's payload)."""
    if not isinstance(payload, dict):
        return None
    call = payload.get("toolCall")
    if isinstance(call, dict):
        args = call.get("args") if isinstance(call.get("args"), dict) else {}
        if call.get("name") in SHELL_TOOLS and isinstance(args.get("CommandLine"), str):
            return "agy", args["CommandLine"], args.get("Cwd") or ""
        return None
    if "toolName" in payload:
        cli, tool, given = "grok", payload.get("toolName"), payload.get("toolInput")
    else:
        cli, tool, given = "claude", payload.get("tool_name"), payload.get("tool_input")
    if tool not in SHELL_TOOLS or not isinstance(given, dict) or not isinstance(given.get("command"), str):
        return None
    return cli, given["command"], payload.get("cwd") or payload.get("workspaceRoot") or ""


def reason(rule):
    "What the agent is told, the call denied."
    return ("Denied by the herd's check for the fact `%s': %s  (`herd recall %s' has the "
            "whole fact.)  Do not look for another way round it.  If the check is wrong for "
            "this call, say so in your answer -- the user can turn it off on the page."
            % (rule.fact, rule.hint, rule.fact))


def decide(payload, cwd=None, root=None):
    """(cli, key, rule, command) when a check denies PAYLOAD's call, else None.
    Within BUDGET_SECONDS, or None."""
    call = shell_call(payload)
    if call is None:
        return None
    cli, command, where = call
    rules = active(keys_for(where or cwd or os.getcwd()), root)
    if not rules:
        return None
    hit = _timed(first_match, rules, command)
    if hit is None:
        return None
    key, rule = hit
    return cli, key, rule, command


class _Late(Exception):
    pass


def _timed(fn, *args):
    "FN(*ARGS), or None past BUDGET_SECONDS (where a timer can say so)."
    try:
        import signal
        if not hasattr(signal, "setitimer"):
            return fn(*args)

        def late(_sig, _frame):
            raise _Late()
        old = signal.signal(signal.SIGALRM, late)
    except (ImportError, ValueError):   # not the main thread
        return fn(*args)
    try:
        signal.setitimer(signal.ITIMER_REAL, BUDGET_SECONDS)
        return fn(*args)
    except _Late:
        return None
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0)
        signal.signal(signal.SIGALRM, old)


def answer(cli, rule):
    "The deny CLI reads."
    why = reason(rule)
    if cli == "agy":
        return {"decision": "deny", "reason": why}
    return {"hookSpecificOutput": {"hookEventName": "PreToolUse",
                                   "permissionDecision": "deny",
                                   "permissionDecisionReason": why}}


def report(cli, key, rule, command, timeout=2):
    """Tell the herd of a denial, for the log and the rule's count.  After
    the answer is decided; a sidecar away changes nothing but the telling."""
    params = {"session": os.environ.get("GHOSTHERD_SESSION") or "", "key": key,
              "fact": rule.fact, "cli": cli, "command": command[:2000]}
    import urllib.request
    body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "herd_lint_denied",
                       "params": params}).encode("utf-8")
    urls = [os.environ.get("GHOSTHERD_RPC", "").strip()]
    try:
        with open(os.environ.get("GHOSTHERD_RPC_FILE", "") or os.devnull, encoding="utf-8") as f:
            urls.append(f.read().strip())
    except OSError:
        pass
    for url in dict.fromkeys(u for u in urls if u):
        try:
            req = urllib.request.Request(url, data=body, headers={"Content-Type": "application/json"})
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                if not json.loads(resp.read().decode("utf-8")).get("error"):
                    return True
        except Exception:  # noqa: BLE001 -- the telling never fails the hook
            continue
    return False


def hook(stdin=None, stdout=None):
    """`herd lint': claude's and grok's PreToolUse hook on a shell call.  A
    deny, or nothing at all (no decision); always exit 0."""
    stdin = stdin or sys.stdin
    stdout = stdout or sys.stdout
    try:
        payload = json.loads(stdin.read() or "{}")
        hit = decide(payload)
    except Exception:  # noqa: BLE001 -- fail open: a broken check stops nothing
        return 0
    if hit is None:
        return 0
    cli, key, rule, command = hit
    stdout.write(json.dumps(answer(cli, rule), ensure_ascii=False) + "\n")
    stdout.flush()
    try:
        report(cli, key, rule, command)
    except Exception:  # noqa: BLE001
        pass
    return 0


def check(command, cwd=None, root=None, rule=None):
    """For `herd lint check': the rule that denies COMMAND in CWD -- among
    the active ones, or RULE (a candidate, as proposed) alone -- or None."""
    if rule is not None:
        data = validate(rule) if "catches" in rule else rule
        r = Rule(data)
        return ("candidate", r) if r.matches(Views(command)) else None
    return first_match(active(keys_for(cwd or os.getcwd()), root), command)


def _main(argv):
    """`python3 herd_lint.py validate': a proposed rule on stdin, checked
    against its own examples.  The sidecar runs it in a process of its own,
    so a pattern that backtracks without end costs that process, not one
    of its workers."""
    if argv[1:] != ["validate"]:
        print("usage: herd_lint.py validate < RULE.json", file=sys.stderr)
        return 2
    try:
        out = {"rule": validate(json.loads(sys.stdin.read()))}
    except ValueError as exc:
        out = {"error": str(exc)}
    print(json.dumps(out, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(_main(sys.argv))
