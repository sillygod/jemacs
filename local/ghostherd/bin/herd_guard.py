"""The herd's guard for an agy agent: one PreToolUse decision per tool call.

agy runs a guarded agent with --dangerously-skip-permissions, so this is the
only gate (ask 75362f01; measured on agy 1.4.5, ask 134ac578): its `allow'
runs the call, its `deny' blocks it and the agent carries on with the
reason.  `ask' and `force_ask' would run under that flag, so this never
answers them: what wants the user becomes a denial the user can approve once,
on the agent's card.  It answers allow or deny only -- unknown tools and its
own failure are denied -- and it may rewrite what it allows (`overwrite',
which agy honours under the flag): a recursive search so it skips secret
files, the agent's git so it runs no hooks, and every command so it runs in
the workspace it was checked in (agy runs commands in its first workspace,
the guard's own --add-dir).

The policy (`auto', the default; `read-only' is jnotes' rules):

- allow: reads, and in auto writes, inside the workspace; read-only commands
  (grep, rg, ls, cat, head, tail, wc, find without -exec/-delete..., and a
  few filters) whose files are in it; git status/diff/log/show/branch, and in
  auto the local writes agents make (add, commit, switch); the herd's own
  commands; the project's own commands (`allow_commands', auto only).
- deny, the user may approve the call once ("ask"): reads or writes outside
  the workspace, git push, package installs, the network, chains and
  redirects, any program or tool it does not know.
- deny, the user may approve the call once ("secret"): anything that could
  reveal a secret -- env, printenv, set, export -p, declare -p/-x, any $VAR or
  `...` expansion, ps e, launchctl getenv, security find-*-password, gh auth
  token, gcloud auth print-*, git config credential.*, git credential,
  interpreters with inline code (python -c, node -e...) -- and secret files,
  read or searched, inside the workspace too (.env, *.pem, id_rsa*, ...,
  .git/config, ~/.ssh, ~/.aws, ~/.claude, ~/.gemini, the herd's data),
  however a command names them (an option's -fFILE, --opt=FILE too).
- deny, the user may approve the call once ("ask"), as code that would run
  outside the guard: writes into .git and the workspace's .agents, .gemini,
  .claude, .grok; awk -f and awk that writes; sort --compress-program, rg
  --hostname-bin, git grep -O, git --output.  And a walk that would follow
  a symlink out of the workspace (grep -R, rg -L, find -L, ls -RL, tree -l)
  or read every file below (diff -r).
- deny, not approvable ("never"): rm -rf, sudo, git push --force, git reset
  --hard, git clean -f, and the guard's own directory.

agy's tool names and arguments (1.4.x): read_file/view_file/list_dir (Path),
write_file/edit_file/write_to_file... (Path, TargetFile), run_command
(CommandLine); the payload's keys are camelCase.  Python 3.8, stdlib only:
it runs under whatever python3 the agent's shell finds.
"""

from __future__ import annotations

import fnmatch
import glob
import hashlib
import json
import os
import re
import shlex
import tempfile
import time

# ---- What it knows of agy's tools ------------------------------------------

READ_TOOLS = frozenset({"read_file", "view_file", "view_file_outline", "view_code_item",
                        "list_dir", "find_by_name", "grep_search", "search_in_file"})
SEARCH_TOOLS = frozenset({"grep_search"})
WRITE_TOOLS = frozenset({"write_file", "write_to_file", "create_file", "edit_file",
                         "replace_file_content", "multi_replace_file_content",
                         "sed_file", "delete_file", "notebook_edit"})
COMMAND_TOOLS = frozenset({"run_command"})
# No side effect outside the agent itself.  A web search sends words the
# model already has; reading a URL fetches, and asks.
HARMLESS_TOOLS = frozenset({"finish", "wait", "wait_5_seconds", "manage_task", "command_status",
                            "list_permissions", "ask_question", "search_web", "task_boundary",
                            "notify_user", "send_command_input", "read_terminal"})

# Argument keys that hold a path (Path, TargetFile, DirectoryPath, SearchPath,
# TargetDirectories, ...), as jnotes' guard found them.
_PATH_KEY = re.compile(r"(Path|Paths|Dir|Directory|Directories|File|Files|Uri|Folder)$")

# ---- Secrets ----------------------------------------------------------------

# File names, matched case-insensitively against a path's last part.
SECRET_NAMES = (".env", ".env.*", ".envrc", "*.pem", "*.key", "*.p12", "*.pfx",
                "id_rsa*", "id_ed25519*", "id_ecdsa*", "id_dsa*", ".netrc", ".npmrc",
                ".pypirc", ".git-credentials", ".pgpass", "credentials*", "*secret*", "*token*")
# Directories whose contents are secret, wherever they are.
SECRET_DIR_NAMES = (".ssh", ".aws", ".gnupg")
# Directories in the workspace holding what runs, or says what runs, outside
# the guard: git's hooks and config, the agent CLIs' hooks and settings.
# Writing there asks.
RUNS_CODE_DIRS = (".git", ".agents", ".gemini", ".claude", ".grok")
# What a recursive search leaves out besides secret files: .git, whose config
# can hold a token in a remote URL (rg --hidden searches it).
SEARCH_SKIP_DIRS = SECRET_DIR_NAMES + (".git",)
# A git config that holds a credential: a URL's user part, an extra header,
# a password or token setting.
_GIT_CREDENTIAL = re.compile(r"://[^/\s@]+@|extraheader|password|token", re.I)
# Directories and files under the home directory, and the herd's own data
# (rpc.token, rpc.url) -- from the policy.
SECRET_HOME = ("~/.ssh", "~/.aws", "~/.gnupg", "~/.kube", "~/.docker", "~/.config/gh",
               "~/.config/gcloud", "~/.azure", "~/.password-store", "~/.claude", "~/.gemini",
               "~/.grok", "~/.netrc", "~/.git-credentials")

# ---- Commands -----------------------------------------------------------------

# Read-only programs; no option of theirs below writes or runs anything.
READ_PROGRAMS = frozenset({"grep", "egrep", "fgrep", "rg", "ls", "cat", "head", "tail", "wc",
                           "find", "sort", "uniq", "cut", "nl", "diff", "stat", "file", "tree",
                           "column", "comm", "cmp", "md5", "md5sum", "shasum", "sha256sum",
                           "awk", "gawk", "mawk", "jq", "date"})
# Programs that read their program (a pattern, a filter) first, then files.
PATTERN_FIRST = frozenset({"grep", "egrep", "fgrep", "rg", "awk", "gawk", "mawk", "jq"})
# Programs that read no file and print nothing secret.
QUIET_PROGRAMS = frozenset({"pwd", "echo", "true", "false", "basename", "dirname", "realpath",
                            "which", "tr", "seq", "printf", "sleep", "test", "["})
UNSAFE_OPTIONS = {
    "find": ("-exec", "-execdir", "-ok", "-okdir", "-delete", "-fprint", "-fprint0",
             "-fprintf", "-fls"),
    "rg": ("--pre", "--pre-glob", "--search-zip", "-z", "--hostname-bin"),
    "sort": ("-o", "--output", "--compress-program", "--files0-from"),
    "wc": ("--files0-from",),
    "tree": ("-o",),
    "file": ("-C", "--compile"),
}
# Short options that take a value, per program: a cluster of flags (-nR)
# ends at one, the rest its value (-eRESULT is a pattern, not -e -R ...).
_VALUE_LETTERS = {"grep": "efmABCdD", "rg": "efgtTmABCjMrE", "diff": "CUDFILSWxX", "ls": "wIT",
                  "tree": "LIPHT"}
# awk's ways to run a program from a file or load code: refused.
AWK_CODE_OPTIONS = ("-f", "--file", "-i", "--include", "-l", "--load", "-E", "--exec")
# Options taking a value, per program (the next word, when not attached).
VALUE_OPTIONS = {
    "grep": ("-e", "-f", "-m", "-A", "-B", "-C", "-d", "-D", "--regexp", "--file", "--max-count",
             "--after-context", "--before-context", "--context", "--include", "--exclude",
             "--exclude-dir", "--label", "--directories", "--devices"),
    "rg": ("-e", "-f", "-g", "-t", "-T", "-m", "-A", "-B", "-C", "-j", "-M", "-r", "-E",
           "--regexp", "--file", "--glob", "--iglob", "--type", "--type-not", "--max-count",
           "--max-depth", "--after-context", "--before-context", "--context", "--threads",
           "--max-columns", "--replace", "--encoding", "--sort", "--sortr", "--type-add"),
    "head": ("-n", "-c"), "tail": ("-n", "-c"), "cut": ("-d", "-f", "-c", "-b"),
    "sort": ("-k", "-t", "-T", "-S"), "uniq": ("-f", "-s", "-w"), "nl": ("-b", "-s", "-w", "-v", "-i"),
    "tree": ("-L", "-I", "-P"), "stat": ("-f", "-c", "-t"), "column": ("-s", "-t", "-c"),
    "diff": ("-U", "-C", "--label"),
}
GREP_FAMILY = frozenset({"grep", "egrep", "fgrep"})
# Programs whose words are not files: their checks are only the parser's.
NO_FILES = QUIET_PROGRAMS
# git that only reads; and, in auto, the local writes agents make.
GIT_READ = frozenset({"status", "diff", "log", "show", "branch", "rev-parse", "ls-files",
                      "blame", "describe", "shortlog", "reflog", "tag", "grep"})
# git that shows files' contents: kept off secret files by pathspec.
GIT_CONTENT = frozenset({"diff", "show", "log", "grep"})
GIT_WRITE = frozenset({"add", "commit", "switch", "stash", "restore", "mv"})
# Package installs.
INSTALLS = {"npm": ("install", "i", "add", "ci", "update"), "yarn": ("add", "install"),
            "pnpm": ("add", "install", "i"), "pip": ("install",), "pip3": ("install",),
            "uv": ("add", "pip"), "brew": ("install", "upgrade"), "cargo": ("install", "add"),
            "gem": ("install",), "go": ("install", "get"), "apt": ("install",),
            "apt-get": ("install",), "poetry": ("add", "install")}
NETWORK = frozenset({"curl", "wget", "http", "https", "nc", "ncat", "telnet", "ssh", "scp",
                     "sftp", "rsync", "ftp"})
INTERPRETERS = {
    "python": ("-c",), "python2": ("-c",), "python3": ("-c",), "pypy": ("-c",), "pypy3": ("-c",),
    "node": ("-e", "--eval", "-p", "--print"), "nodejs": ("-e", "--eval", "-p", "--print"),
    "deno": ("eval",), "bun": ("-e", "--eval", "eval"), "ruby": ("-e",), "perl": ("-e", "-E"),
    "php": ("-r",), "lua": ("-e",), "Rscript": ("-e",), "osascript": ("-e",),
    "sh": ("-c",), "bash": ("-c",), "zsh": ("-c",), "dash": ("-c",), "ksh": ("-c",),
    "fish": ("-c",),
}
# What in an awk program or a jq filter reaches the environment or runs a program.
_AWK_JQ_REVEAL = re.compile(r"ENVIRON|system|getline|\$ENV|\benv\b|\|")
# The herd's own commands an agent may run: not its hooks or this guard.
HERD_SUBCOMMANDS = frozenset({"ask", "reply", "status", "cancel", "list", "asks", "recall",
                              "remember", "forget", "handoff", "start", "stop", "relay",
                              "promote", "screens", "search", "cost", "--help", "-h"})

# `2>/dev/null' and `2>&1' (agents add them everywhere) are the redirections let through.
_STDERR = re.compile(r"\s+2>\s*(?:/dev/null|&1)(?=\s|$)")
# `cd TARGET &&': TARGET is one \S+ word (a path with a space never matches, so
# the whole line is then read as a chain and refused).  The captured TARGET is
# screened like any other word before it is trusted -- see `_cd_target'.
_CD = re.compile(r"\A\s*cd\s+(\S+)\s*&&\s*")
# A plain path: only characters the shell leaves alone, so nothing in a cd
# target can run code or re-parse.  `~' is kept -- it names a directory, not
# code, and `check_path' resolves it and refuses one that leaves the workspace.
_PLAIN_PATH = re.compile(r"\A[A-Za-z0-9/._~@%+,:=-]+\Z")
_HERD_WORD = re.compile(r"""\A("\$\{?GHOSTHERD_HERD\}?"|\$\{?GHOSTHERD_HERD\}?)(?=\s|$)""")
_HEREDOC = re.compile(r"\s<<-?\s*(['\"])([A-Za-z_][A-Za-z0-9_]*)\1[ \t]*\n(?:.*\n)?\2[ \t]*\n?\Z", re.S)

# Arguments of a call that do not say what it does.
_VOLATILE = frozenset({"WaitMsBeforeAsync", "toolSummary", "toolAction", "toolDescription",
                       "SafeToAutoRun", "Blocking"})

# How long an approval waits to be used.
APPROVAL_SECONDS = 1800


class Verdict:
    "What the guard says of one call, and why."

    def __init__(self, decision, reason="", kind="allow", rule="", overwrite=None):
        self.decision = decision      # "allow" or "deny"
        self.reason = reason
        self.kind = kind              # allow, ask, secret, never
        self.rule = rule
        self.overwrite = overwrite

    @property
    def approvable(self):
        return self.decision == "deny" and self.kind in ("ask", "secret")

    def answer(self):
        out = {"decision": self.decision}
        if self.reason:
            out["reason"] = self.reason
        if self.overwrite:
            out["overwrite"] = self.overwrite
        return out


def _allow(rule="", overwrite=None):
    return Verdict("allow", kind="allow", rule=rule, overwrite=overwrite)


def _ask(reason, rule):
    return Verdict("deny", reason, "ask", rule)


def _secret(reason, rule):
    return Verdict("deny", reason, "secret", rule)


def _never(reason, rule):
    return Verdict("deny", reason, "never", rule)


# ---- The policy -----------------------------------------------------------------


def _real(path):
    return os.path.realpath(os.path.expanduser(str(path)))


def _within(path, root):
    return path == root or path.startswith(root.rstrip(os.sep) + os.sep)


class Policy:
    """A guarded agent's rules, from policy.json in its guard directory, which
    the herd writes as it starts the agent."""

    def __init__(self, guard_dir, data=None):
        self.guard_dir = _real(guard_dir)
        if data is None:
            with open(os.path.join(self.guard_dir, "policy.json"), encoding="utf-8") as f:
                data = json.load(f)
        self.agent = str(data.get("agent") or "")
        self.mode = data.get("mode") or "auto"
        if self.mode not in ("auto", "read-only"):
            raise ValueError("unknown guard mode %r" % self.mode)
        self.workspace = _real(data["workspace"])
        self.read_roots = [self.workspace] + [_real(r) for r in data.get("read_roots") or []]
        self.allow_commands = [shlex.split(c) for c in data.get("allow_commands") or [] if c.strip()]
        self.herd = [_real(h) for h in data.get("herd") or []]
        self.guard_root = _real(data.get("guard_root") or os.path.dirname(self.guard_dir))
        self.secret_names = tuple(n.lower() for n in (data.get("secret_names") or SECRET_NAMES))
        self.secret_dirs = [_real(d) for d in list(SECRET_HOME) + list(data.get("secret_dirs") or [])]
        self.secret_files = [_real(f) for f in data.get("secret_files") or []]
        # The herd's own token: no approval hands it to an agent.
        self.never_files = [_real(f) for f in data.get("never_files") or []]

    # -- Paths

    def secret_path(self, path, given=None):
        """Why PATH (real) is secret, or None.  GIVEN is the path as written:
        a link's own name counts as well as where it leads."""
        names = [os.path.basename(path)]
        if given:
            names.append(os.path.basename(given.rstrip("/")))
        for n in names:
            low = n.lower()
            if any(fnmatch.fnmatchcase(low, p) for p in self.secret_names):
                return "%s is a secret file" % n
        parts = path.split(os.sep)
        hit = next((d for d in SECRET_DIR_NAMES if d in parts), None)
        if hit:
            return "it is inside %s" % hit
        if ".git" in parts[:-1] and parts[-1].lower() == "config":
            return "it is git's config, which can hold credentials (a token in a remote URL)"
        for d in self.secret_dirs:
            if _within(path, d):
                return "%s holds secrets" % d.replace(os.path.expanduser("~"), "~", 1)
        if path in self.secret_files:
            return "%s holds the herd's secrets" % os.path.basename(path)
        return None

    def guarded_path(self, path):
        return _within(path, self.guard_root) or _within(path, self.guard_dir)

    def inside(self, path, write=False):
        return _within(path, self.workspace) if write else any(_within(path, r) for r in self.read_roots)

    def check_path(self, given, base=None, write=False, quiet_secret=False):
        """None when GIVEN may be read (or, with WRITE, written); else the
        verdict that refuses it.  Relative paths are from BASE."""
        p = given.strip()
        if p.startswith("file://"):
            p = p[len("file://"):]
        if not os.path.isabs(os.path.expanduser(p)):
            p = os.path.join(base or self.workspace, p)
        real = _real(p)
        if self.guarded_path(real):
            return _never("%s holds the herd's guard, which no agent reads or changes." % given, "guard-dir")
        if real in self.never_files:
            return _never("%s is the herd's own token, which no agent reads -- not even approved."
                          % given, "herd-token")
        why = None if quiet_secret else self.secret_path(real, given)
        if why:
            return _secret("%s: %s.  Reading it needs the user's approval -- they can approve this "
                           "exact call once on your card." % (given, why), "secret-file")
        if not self.inside(real, write):
            what = "changed" if write else "read"
            return _ask("%s is outside your workspace (%s); only files inside it are %s without the "
                        "user's approval -- they can approve this exact call once on your card."
                        % (given, self.workspace, what), "outside-workspace")
        if write:
            parts = os.path.relpath(real, self.workspace).split(os.sep)
            hit = next((d for d in RUNS_CODE_DIRS if d in parts), None)
            if hit:
                return _ask("%s is in %s, which holds what git or an agent CLI runs (hooks, settings); "
                            "changing it could run code outside the guard -- the user can approve this "
                            "exact change once on your card." % (given, hit), "runs-code")
        return None


# ---- Reading a command line ------------------------------------------------------


def _split(command):
    """COMMAND's pipeline stages as lists of (word, globbed) -- GLOBBED when
    the word has an unquoted * ? or [ -- or why it cannot be read: it chains,
    redirects, runs in the background or expands something ($, backticks),
    as sh would outside single quotes (and, for $ and `, inside double
    quotes too).  The kind of refusal goes with it: ("secret" | "ask", why)."""
    stages, words, word, globbed, quote, has = [], [], [], False, None, False
    i = 0
    while i < len(command):
        ch = command[i]
        if quote == "'":
            if ch == "'":
                quote = None
            else:
                word.append(ch)
        elif ch in "$`":
            return None, ("secret", "it expands a variable or a command ($, ${...} or backticks), "
                                    "which could reveal secrets -- quote a literal $ with single quotes")
        elif quote == '"':
            if ch == '"':
                quote = None
            elif ch == "\\" and i + 1 < len(command) and command[i + 1] in '"\\$`':
                i += 1
                word.append(command[i])
            else:
                word.append(ch)
        elif ch in "'\"":
            quote, has = ch, True
        elif ch == "\\":
            i += 1
            if i < len(command):
                word.append(command[i])
                has = True
        elif ch in ";&<>\n":
            return None, ("ask", "it chains, redirects or backgrounds commands; run one command at a "
                                 "time (a pipe | is fine)")
        elif ch == "|":
            if command[i + 1:i + 2] == "|":
                return None, ("ask", "it chains commands with ||; run one at a time")
            if word or has:
                words.append(("".join(word), globbed))
            stages.append(words)
            words, word, globbed, has = [], [], False, False
        elif ch.isspace():
            if word or has:
                words.append(("".join(word), globbed))
            word, globbed, has = [], False, False
        else:
            if ch in "*?[":
                globbed = True
            word.append(ch)
        i += 1
    if quote:
        return None, ("ask", "its quotes do not close")
    if word or has:
        words.append(("".join(word), globbed))
    stages.append(words)
    if any(not s for s in stages):
        return None, ("ask", "it has an empty stage")
    return stages, None


def _split_or(command):
    """COMMAND's parts around a top-level `||', quotes respected as sh would
    (a `||' quoted or escaped is a word); one part when it has none."""
    parts, start, quote, i = [], 0, None, 0
    while i < len(command):
        ch = command[i]
        if quote == "'":
            if ch == "'":
                quote = None
        elif quote == '"':
            if ch == '"':
                quote = None
            elif ch == "\\":
                i += 1
        elif ch in "'\"":
            quote = ch
        elif ch == "\\":
            i += 1
        elif ch == "|" and command[i + 1:i + 2] == "|":
            parts.append(command[start:i])
            i += 2
            start = i
            continue
        i += 1
    parts.append(command[start:])
    return parts


def _option_value(program, word):
    "Whether WORD, an option of PROGRAM, takes the next word as its value."
    opts = VALUE_OPTIONS.get(program, ())
    if word in opts:
        return True
    # -e PATTERN glued into a cluster (-ie, -rne) takes the next word too.
    if program in GREP_FAMILY | {"rg"} and re.match(r"\A-[a-zA-Z]*[efmABCdD]\Z", word) and not word.startswith("--"):
        return word[-1] in "efmABCdD" and len(word) > 1
    return False


def _paths_of(program, words):
    """The words of PROGRAM's arguments that name files, and its options.
    The grep family and rg take a pattern first, unless -e or -f gives it."""
    files, options, i = [], [], 0
    pattern_given = False
    while i < len(words):
        w, globbed = words[i]
        if w == "--":
            files.extend(words[i + 1:])
            break
        if w.startswith("-") and w != "-":
            options.append(w)
            if program in PATTERN_FIRST and (w in ("-e", "-f", "--regexp", "--file")
                                                     or w.startswith(("--regexp=", "--file="))
                                                     or re.match(r"\A-[a-zA-Z]*[ef]\Z", w)):
                pattern_given = True
                if w in ("-f", "--file") or w.startswith("--file=") or (re.match(r"\A-[a-zA-Z]*f\Z", w)):
                    # A file of patterns is a file read.
                    if "=" in w:
                        files.append((w.split("=", 1)[1], False))
                    elif i + 1 < len(words):
                        files.append(words[i + 1])
            if "=" not in w and _option_value(program, w):
                i += 2
                continue
            # A value glued on: --file=/x, -f/x (a pattern file, read), -eX (a pattern).
            if "=" in w:
                v = w.split("=", 1)[1]
                if v.startswith(("/", "~", ".")) and program not in ("rg",) and not w.startswith("--regexp="):
                    files.append((v, False))
            elif program in PATTERN_FIRST and re.match(r"\A-[ef].", w):
                pattern_given = True
                if w[1] == "f":
                    files.append((w[2:], False))
            elif re.match(r"\A-[a-zA-Z][/~.]", w):
                files.append((w[2:], False))
            i += 1
            continue
        files.append((w, globbed))
        i += 1
    if program in PATTERN_FIRST and not pattern_given and files:
        files = files[1:]
    return files, options


def _recursive(program, options):
    if program == "rg":
        return True
    if program in GREP_FAMILY:
        for o in options:
            if o in ("--recursive", "--dereference-recursive", "--directories=recurse", "-d"):
                return True
            if re.match(r"\A-[a-zA-Z]+\Z", o) and ("r" in o or "R" in o):
                return True
    return False


def _flags(program, words):
    """The one-letter flags PROGRAM's WORDS set, clusters undone, and its long
    options (without values): ({"n", "R"}, {"--follow"}) for -nR --follow."""
    takes = _VALUE_LETTERS.get("grep" if program in GREP_FAMILY else program, "")
    letters, longs = set(), set()
    for w in words:
        if w == "--":
            break
        if w.startswith("--"):
            longs.add(w.split("=", 1)[0])
        elif w.startswith("-") and len(w) > 1:
            for ch in w[1:]:
                letters.add(ch)
                if ch in takes:
                    break
    return letters, longs


def _follows_links(program, words):
    """Why PROGRAM's walk would follow a symbolic link out of the workspace
    -- past the guard, which judges a path by where it leads only when it
    is named -- or None.  grep -r, rg and find do not follow; these do."""
    letters, longs = _flags(program, words)
    if program in GREP_FAMILY:
        recursive = _recursive(program, [w for w in words if w.startswith("-")])
        if ("R" in letters or "--dereference-recursive" in longs or any("dereference" in w for w in words)
                or (recursive and ("S" in letters or "--dereference-files" in longs))):
            return "%s -R follows symbolic links, out of your workspace; use %s -r, which does not" % (
                program, program)
    elif program == "rg" and ("L" in letters or "--follow" in longs):
        return "rg -L follows symbolic links, out of your workspace; rg alone does not"
    elif program == "find" and any(w == "-follow" or re.match(r"\A-[HLP]*L[HLP]*\Z", w) for w in words):
        return "find -L follows symbolic links, out of your workspace; find alone does not"
    elif program == "ls" and "L" in letters and ("R" in letters or "--recursive" in longs):
        return "ls -RL follows symbolic links, out of your workspace; ls -R does not"
    elif program == "tree" and "l" in letters:
        return "tree -l follows symbolic links, out of your workspace; tree alone does not"
    elif program == "diff" and ("r" in letters or "--recursive" in longs):
        return ("diff -r reads every file below, secret ones and links out of your workspace too; "
                "compare the files you mean one by one")
    return None


def _excludes(program, policy):
    "Options that keep a recursive search off secret files."
    names = list(policy.secret_names)
    if program == "rg":
        return ["--glob=!%s" % n for n in names] + ["--glob=!%s" % d for d in SEARCH_SKIP_DIRS]
    return ["--exclude=%s" % n for n in names] + ["--exclude-dir=%s" % d for d in SEARCH_SKIP_DIRS]


def _git_excludes(plain, policy):
    """git PLAIN (diff, show, log, grep), its pathspec keeping it off secret
    files: their contents would otherwise show, tracked or in history."""
    specs = [":(exclude,icase,glob)**/%s" % n for n in policy.secret_names] + \
            [":(exclude,glob)**/%s/**" % d for d in SECRET_DIR_NAMES]
    if "--" in plain:
        at = plain.index("--")
        paths = plain[at + 1:] or ["."]
        return plain[:at + 1] + paths + specs
    return plain + ["--", "."] + specs


def _first(words, prefix):
    return [w for w, _ in words[:len(prefix)]] == prefix


class Guard:
    def __init__(self, policy, cwd=None):
        """CWD is where agy runs the agent's tools, when it says (its first
        workspace); None when it does not."""
        self.p = policy
        self.cwd = _real(cwd) if cwd else None

    # -- One command line

    def _cd_target(self, target, cwd):
        """The verdict refusing a `cd TARGET', for its TARGET, or None.

        TARGET rides into the shell at the front of the rewritten line, so it
        is screened first -- by the same quoting-aware scanner as any word,
        and a plain-path allowlist.  A $, backtick, ${...}, $(...), brace,
        glob, or a ;, |, &, <, > in it would run code or re-parse the line, so
        such a target is denied and cannot be approved: there is no safe
        reading of it.  A plain path is then `check_path'-ed -- a `~' it
        resolves, a path outside the workspace it refuses (approvably)."""
        stages, _refusal = _split(target)
        if (not _PLAIN_PATH.match(target) or stages is None
                or len(stages) != 1 or len(stages[0]) != 1 or stages[0][0][1]):
            return _never(
                "This command cannot run as written: its `cd' target %r is not a plain path.  A $, "
                "backtick, ${...}, $(...), brace, glob or other shell character in a cd target would "
                "run code or leave your workspace, so it is refused -- and cannot be approved.  cd to a "
                "plain directory inside your workspace." % target, "cd-target")
        return self.p.check_path(target, cwd)

    def command(self, line, cwd=None):
        """The verdict on run_command LINE, run in CWD (the workspace by
        default) -- with the line rewritten when a search must skip secrets."""
        if cwd:
            v = self.p.check_path(cwd)
            if v:
                return v
        cwd = _real(cwd) if cwd else self.p.workspace
        line = line.strip()
        if not line:
            return _ask("empty command.", "empty")
        prefix = ""
        m = _CD.match(line)
        if m:
            target = m.group(1)
            v = self._cd_target(target, cwd)
            if v:
                return v
            cwd = _real(os.path.join(cwd, os.path.expanduser(target)))
            # Rebuilt from the screened target, shlex-quoted, never the raw
            # match: what is prepended to the rewritten line cannot re-parse.
            prefix, line = "cd " + shlex.quote(target) + " && ", line[m.end():]
        herd = self._herd(line)
        if herd:
            return herd
        parts = _split_or(line)
        if len(parts) > 1:
            # A fallback, A || B: each one judged as if run alone, in the same
            # place.  Grouped, so a failed cd runs neither -- B is never run
            # where it was not checked.
            lines, changed = [], False
            for part in parts:
                v = self._line(part, cwd)
                if v.decision != "allow":
                    return v
                new = (v.overwrite or {}).get("CommandLine")
                changed = changed or bool(new)
                lines.append(new or part.strip())
            return _allow("search-excludes-secrets" if changed else "command",
                          {"CommandLine": prefix + "(" + " || ".join(lines) + ")"})
        v = self._line(line, cwd)
        if v.overwrite and prefix:
            v.overwrite = dict(v.overwrite, CommandLine=prefix + v.overwrite["CommandLine"])
        return v

    def _line(self, line, cwd):
        """The verdict on LINE, one pipeline run in CWD -- rewritten, when a
        search must skip secrets, without its cd."""
        line = line.strip()
        if not line:
            return _ask("empty command.", "empty")
        stripped = _STDERR.sub("", " " + line).strip()
        tail = " 2>/dev/null" if stripped != line and "2>/dev/null" in line else ""
        stages, refusal = _split(stripped)
        if stages is None:
            kind, why = refusal
            make = _secret if kind == "secret" else _ask
            return make("This command cannot run as written: %s.  The user can approve this exact "
                        "call once on your card." % why, "parse-" + kind)
        rewritten, changed = [], False
        for words in stages:
            v, new = self._stage(words, cwd)
            if v is not None:
                return v
            rewritten.append(new)
            changed = changed or new != [w for w, _ in words]
        if changed:
            return _allow("search-excludes-secrets", {"CommandLine": " | ".join(
                " ".join(shlex.quote(w) for w in ws) for ws in rewritten) + tail})
        return _allow("command")

    def _herd(self, line):
        """A herd command -- "$GHOSTHERD_HERD" ... or bin/herd by its path,
        perhaps with a quoted heredoc -- allowed by its subcommand; else None."""
        m = _HERD_WORD.match(line)
        rest = None
        if m:
            rest = line[m.end():]
        else:
            first = line.split(None, 1)
            if first and _real(first[0].strip("'\"")) in self.p.herd:
                rest = line[len(first[0]):]
        if rest is None:
            return None
        body = _HEREDOC.search(rest)
        args = rest[:body.start()] if body else rest
        stages, refusal = _split(args.strip())
        if stages is None or len(stages) != 1:
            return _ask("This herd command cannot run as written: %s." % (refusal[1] if refusal else "a pipe"),
                        "herd-parse")
        words = [w for w, _ in stages[0]]
        if not words or words[0] not in HERD_SUBCOMMANDS:
            return _ask("herd %s is not one an agent runs; the user can approve this exact call once "
                        "on your card." % (words[0] if words else ""), "herd-subcommand")
        return _allow("herd")

    def _stage(self, words, cwd):
        """One stage of a pipeline: (verdict, None) when refused, else (None,
        its words, rewritten to skip secrets when it searches)."""
        program = os.path.basename(words[0][0])
        args = words[1:]
        plain = [w for w, _ in words]
        # Never: what no approval should run for an agent.
        if program in ("sudo", "doas", "su"):
            return _never("%s is never run by an agent; the user runs it themselves if they mean it."
                          % program, "sudo"), None
        if program == "rm":
            flags = "".join(w[1:] for w, _ in args if w.startswith("-") and not w.startswith("--"))
            longs = {w for w, _ in args if w.startswith("--")}
            if ("r" in flags or "R" in flags or "--recursive" in longs) and ("f" in flags or "--force" in longs):
                return _never("rm -rf is never run by an agent; delete with care, file by file, or ask "
                              "the user.", "rm-rf"), None
        if program == "git":
            # git -C DIR ...: the rest as if run in DIR -- itself a path
            # checked like any other, and a write's to be in the workspace.
            dirs = []
            while args and args[0][0] == "-C":
                if len(args) < 2:
                    return _ask("git -C needs a directory.", "git-other"), None
                v = self.p.check_path(args[1][0], cwd)
                if v is not None:
                    return v, None
                cwd = _real(os.path.join(cwd, os.path.expanduser(args[1][0])))
                dirs.append(args[1][0])
                args = args[2:]
            if dirs and args and args[0][0] in GIT_WRITE:
                v = self.p.check_path(cwd, write=True)
                if v is not None:
                    return v, None
            v = self._git(args, cwd)
            if v is not None:
                return v, None
            sub = args[0][0] if args else ""
            named = args
            if sub == "grep":  # its pattern is no file; its paths come after --
                ws = [w for w, _ in args]
                named = [a for a in args if a[0].startswith("-")] + (args[ws.index("--") + 1:] if "--" in ws else [])
            v = self._named_files(named, cwd, words=True)
            if v is not None:
                return v, None
            if sub in GIT_CONTENT:
                return None, _git_excludes(plain, self.p)
            if sub in GIT_WRITE:
                # Hooks are code the agent can change (a tracked .husky/, a
                # pre-commit config): its git runs none.
                return None, [plain[0], "-c", "core.hooksPath=/dev/null"] + plain[1:]
            return None, plain
        # Secrets: what could print the environment or a credential.
        v = self._reveals(program, args)
        if v is not None:
            return v, None
        if program in INSTALLS and args and args[0][0] in INSTALLS[program]:
            return _ask("%s installs packages; the user can approve this exact call once on your card."
                        % " ".join(plain[:2]), "install"), None
        if program in NETWORK:
            return _ask("%s reaches the network; the user can approve this exact call once on your card."
                        % program, "network"), None
        # The project's own commands (auto): checked for files all the same.
        if self.p.mode == "auto" and any(_first(words, c) for c in self.p.allow_commands):
            for w, globbed in args:
                v = self._file_word(w, globbed, cwd, lenient=True)
                if v is not None:
                    return v, None
            return None, plain
        if program in NO_FILES:
            return None, plain
        if program not in READ_PROGRAMS:
            return _ask("%s is not a command the herd lets an agent run on its own; the user can "
                        "approve this exact call once on your card." % program, "unknown-command"), None
        for w, _ in args:
            if any(w == o or w.startswith(o + "=") for o in UNSAFE_OPTIONS.get(program, ())):
                return _ask("%s %s could write files or run programs; the user can approve this exact "
                            "call once on your card." % (program, w), "unsafe-option"), None
        why = _follows_links(program, [w for w, _ in args])
        if why:
            return _ask("%s -- or the user can approve this exact call once on your card." % why,
                        "follows-links"), None
        if program in ("awk", "gawk", "mawk"):
            v = self._awk(args)
            if v is not None:
                return v, None
        if program == "uniq" and len([w for w, _ in args if not w.startswith("-")]) > 1:
            return _ask("uniq writes its second file; the user can approve this exact call once on your "
                        "card.", "unsafe-option"), None
        v = self._named_files(args, cwd)
        if v is not None:
            return v, None
        files, options = _paths_of(program, args)
        if program == "find":
            files = self._find_paths(args)
        for w, globbed in files:
            v = self._file_word(w, globbed, cwd)
            if v is not None:
                return v, None
        if _recursive(program, options):
            return None, [plain[0]] + _excludes(program, self.p) + plain[1:]
        return None, plain

    def _awk(self, args):
        """The verdict refusing an awk that runs code from a file, or whose
        program could write one (print > FILE), or None."""
        for w, _ in args:
            if any(w == o or w.startswith(o + "=") or (len(o) == 2 and w.startswith(o) and len(w) > 2)
                   for o in AWK_CODE_OPTIONS):
                return _ask("awk %s runs code from a file; the user can approve this exact call once on "
                            "your card." % w, "awk-code")
        program = next((w for w, _ in args if not w.startswith("-")), "")
        if ">" in program or "@" in program:
            return _ask("this awk program could write a file (> or @include): compare with < or <= "
                        "instead (a > b is b < a), or the user can approve this exact call once on your "
                        "card.", "awk-write")
        return None

    def _named_files(self, args, cwd, words=False):
        """The verdict refusing a file an option names -- glued on, -fFILE or
        --opt=FILE -- and, with WORDS (git's), any other word: whatever names
        a file that is there is checked as a read.  A pattern (-e, --regexp)
        is not a file.  The strongest refusal, or None."""
        refusals = []
        for w, _ in args:
            if w.startswith(("-e", "--regexp=")):
                continue
            if w.startswith("--") and "=" in w:
                value = w.split("=", 1)[1]
            elif w.startswith("-") and not w.startswith("--") and len(w) > 2:
                value = w[2:]
            elif words and not w.startswith("-"):
                value = w
            else:
                continue
            if not value or value.startswith("-") or value == os.devnull:
                continue
            full = value if os.path.isabs(os.path.expanduser(value)) else os.path.join(cwd, value)
            if os.path.lexists(os.path.expanduser(full)):
                v = self.p.check_path(value, cwd)
                if v is not None:
                    refusals.append(v)
        order = {"never": 0, "secret": 1}
        return min(refusals, key=lambda v: order.get(v.kind, 2)) if refusals else None

    def _find_paths(self, args):
        "find's start points: its words before the first expression."
        out = []
        for w, globbed in args:
            if w.startswith(("-", "(", "!", ")")):
                break
            out.append((w, globbed))
        return out

    def _file_word(self, word, globbed, cwd, lenient=False):
        """The verdict refusing a word that names a file, or None.  A glob is
        what the shell would make of it.  LENIENT (a project command's
        arguments): only what looks like a path is a path."""
        if lenient and not (globbed or word.startswith(("/", "~", ".")) or "/" in word):
            return None
        if globbed:
            pattern = word if os.path.isabs(os.path.expanduser(word)) else os.path.join(cwd, word)
            matches = glob.glob(os.path.expanduser(pattern))
            for m in matches:
                v = self.p.check_path(m, cwd)
                if v is not None:
                    return v
            if matches:
                return None
        return self.p.check_path(word, cwd)

    def _git(self, args, cwd):
        sub = args[0][0] if args else ""
        rest = [w for w, _ in args[1:]]
        if sub in ("credential", "credential-store", "credential-cache", "credential-osxkeychain"):
            return _secret("git %s handles credentials; the user can approve this exact call once on "
                           "your card." % sub, "git-credential")
        if sub == "config":
            if any("credential" in w.lower() or w in ("-l", "--list", "--get-regexp") for w in rest):
                return _secret("git config here could print credentials; the user can approve this "
                               "exact call once on your card.", "git-config-credential")
            return _ask("git config changes or reads settings; the user can approve this exact call "
                        "once on your card.", "git-config")
        if sub == "push":
            if any(w in ("-f", "--force", "--mirror", "--force-with-lease") or w.startswith(("--force-with-lease=", "+"))
                   for w in rest):
                return _never("git push --force is never run by an agent; the user does it themselves "
                              "if they mean it.", "git-push-force")
            return _ask("git push publishes the work; the user can approve this exact call once on your "
                        "card.", "git-push")
        if sub == "remote" and (not rest or any(w in ("-v", "--verbose", "get-url", "show") for w in rest)):
            return _secret("git remote %s prints remote URLs, which can carry tokens; the user can approve "
                           "this exact call once on your card." % " ".join(rest), "git-remote-urls")
        if sub == "grep" and any(w in ("-O", "--open-files-in-pager") or w.startswith(("-O", "--open-files-in-pager="))
                                 for w in rest):
            return _ask("git grep -O runs a program on the files; the user can approve this exact call once "
                        "on your card.", "git-pager")
        if any(w in ("--output", "--output-directory", "-o") or w.startswith(("--output=", "--output-directory="))
               for w in rest) and sub not in ("ls-files", "grep"):
            return _ask("git %s --output writes a file; the user can approve this exact call once on your "
                        "card." % sub, "git-output")
        if sub == "reset" and "--hard" in rest:
            return _never("git reset --hard throws away work; never run by an agent.", "git-reset-hard")
        if sub == "clean" and any(w.startswith("-") and "f" in w for w in rest):
            return _never("git clean -f deletes untracked files; never run by an agent.", "git-clean")
        if sub == "show":
            for w in rest:
                if ":" in w and not w.startswith("-"):
                    v = self.p.check_path(w.split(":", 1)[1] or ".", cwd)
                    if v is not None and v.kind == "secret":
                        return v
        if sub == "branch" and any(w in ("-d", "-D", "-m", "-M", "--delete", "--move", "-f", "--force")
                                   for w in rest):
            return _ask("git branch with -d/-m/-f changes branches; the user can approve this exact call "
                        "once on your card.", "git-branch-write")
        if sub in GIT_READ:
            return None
        if self.p.mode == "auto" and sub in GIT_WRITE:
            return None
        return _ask("git %s is not one the herd lets an agent run on its own; the user can approve this "
                    "exact call once on your card." % (sub or "(nothing)"), "git-other")

    def _reveals(self, program, args):
        "The verdict refusing a command that could print a secret, or None."
        words = [w for w, _ in args]
        why = None
        if program in ("env", "printenv"):
            why = "%s prints the environment" % program
        elif program == "set" or program in ("compgen",):
            why = "%s can print every variable" % program
        elif program in ("export", "declare", "typeset"):
            if not words or any(w in ("-p", "-x") or (w.startswith("-") and ("p" in w or "x" in w)) for w in words):
                why = "%s -p prints the environment" % program
            else:
                return _ask("%s changes the shell's environment, for nothing a command after it keeps; the "
                            "user can approve this exact call once on your card." % program, "env-change")
        elif program == "ps":
            if any(("e" in w and not w.startswith("-")) or w in ("-E", "-e", "eww", "-ww") for w in words):
                why = "ps with e shows processes' environments"
        elif program == "launchctl" and words[:1] in (["getenv"], ["print"], ["export"], ["managername"]):
            why = "launchctl %s prints the environment" % words[0]
        elif program == "security" and words and (words[0].startswith("find-") or words[0].startswith("dump-")):
            why = "security %s reads the keychain" % words[0]
        elif program == "gh" and words[:2] == ["auth", "token"] or (program == "gh" and words[:2] == ["auth", "status"]
                                                                    and any(w in ("-t", "--show-token") for w in words)):
            why = "gh auth token prints a token"
        elif program == "gcloud" and len(words) > 1 and words[0] == "auth" and words[1].startswith("print-"):
            why = "gcloud auth %s prints a token" % words[1]
        elif program == "gcloud" and words[:2] == ["config", "config-helper"]:
            why = "gcloud config config-helper prints a token"
        elif program == "aws" and (words[:2] == ["configure", "get"] or "export-credentials" in words
                                   or words[:2] == ["sts", "get-session-token"]):
            why = "aws %s prints credentials" % " ".join(words[:2])
        elif program in INTERPRETERS:
            flags = INTERPRETERS[program]
            if any(w in flags or any(w.startswith(f) and len(f) == 2 and f.startswith("-") and w != f
                                     for f in flags) for w in words):
                why = "%s with inline code can read the environment or any file" % program
        elif program in ("awk", "gawk", "mawk", "jq") and any(_AWK_JQ_REVEAL.search(w) for w in words):
            why = "this %s program reads the environment or runs a command" % program
        if any("/environ" in w for w in words):
            why = "it reads a process's environment"
        if why:
            return _secret("%s, so it could reveal secrets; it needs the user's approval -- they can "
                           "approve this exact call once on your card." % why, "reveals-secrets")
        return None

    # -- One tool call

    def decide(self, call):
        "The verdict on one tool call, {name, args}."
        if not isinstance(call, dict):
            return _never("unreadable tool call.", "unreadable")
        name = str(call.get("name") or "")
        args = call.get("args") if isinstance(call.get("args"), dict) else {}
        if name in HARMLESS_TOOLS:
            return _allow("harmless")
        if name in COMMAND_TOOLS:
            return self.command(str(args.get("CommandLine") or ""), args.get("Cwd") or None)
        relative = self._relative(_call_paths(args)) if name in READ_TOOLS | WRITE_TOOLS else None
        if relative:
            return relative
        if name in READ_TOOLS:
            paths = _call_paths(args)
            for p in paths or [self.p.workspace]:
                v = self.p.check_path(p)
                if v is not None:
                    return v
            if name in SEARCH_TOOLS:
                for p in paths or [self.p.workspace]:
                    hit = self._secret_inside(p)
                    if hit:
                        return _secret("Searching %s would read %s; search with run_command (grep -r, "
                                       "rg), which the herd runs with secret files left out -- or the user "
                                       "can approve this exact call once on your card." % (p, hit),
                                       "search-sweeps-secrets")
            return _allow("read")
        if name in WRITE_TOOLS:
            paths = _call_paths(args)
            if not paths:
                return _ask("could not tell which file this would change; the user can approve this "
                            "exact call once on your card.", "write-unknown-path")
            for p in paths:
                v = self.p.check_path(p, write=True)
                if v is not None:
                    return v
            if self.p.mode == "read-only":
                return _ask("this agent reads but does not change files; the user can approve this exact "
                            "change once on your card.", "read-only")
            return _allow("write")
        return _ask("%s is not a tool the herd lets an agent use on its own (only reading, searching and "
                    "changing files in its workspace, and running read-only commands); the user can "
                    "approve this exact call once on your card." % (name or "this tool"), "unknown-tool")

    def _relative(self, paths):
        """A refusal for a relative path in PATHS, unless agy says its tools
        run in the workspace: else the guard would check one file and agy
        read another."""
        if self.cwd == self.p.workspace:
            return None
        for p in paths:
            q = p[len("file://"):] if p.startswith("file://") else p
            if not os.path.isabs(os.path.expanduser(q)):
                return _never("%s is a relative path, and your tools do not run in your workspace; give "
                              "its full path, %s." % (p, os.path.join(self.p.workspace, q)), "relative-path")
        return None

    def _secret_inside(self, path, most=20000):
        """What a search of PATH (a file or directory) must not read -- a
        secret file, git's config holding a credential, a link out of the
        workspace or to a secret -- said for the agent, or None.  Looks at
        MOST entries."""
        real = _real(path if os.path.isabs(os.path.expanduser(path)) else os.path.join(self.p.workspace, path))
        if os.path.isfile(real):
            return "%s, a secret file" % os.path.basename(real) if self.p.secret_path(real) else None
        seen = 0
        for root, dirs, files in os.walk(real):
            if ".git" in dirs:
                config = os.path.join(root, ".git", "config")
                try:
                    with open(config, encoding="utf-8", errors="replace") as f:
                        if _GIT_CREDENTIAL.search(f.read(200000)):
                            return "%s, git's config, which holds a credential" % os.path.relpath(config, real)
                except OSError:
                    pass
            dirs[:] = [d for d in dirs if d != ".git"]
            for f in files + dirs:
                seen += 1
                full = os.path.join(root, f)
                if self.p.secret_path(full):
                    return "%s, a secret file" % os.path.relpath(full, real)
                if os.path.islink(full) and self.p.check_path(full) is not None:
                    return "%s, a link to %s" % (os.path.relpath(full, real), os.path.realpath(full))
                if seen >= most:
                    return "(more than %d files, not all looked at)" % most
        return None


def _call_paths(args):
    out = []
    for key, value in args.items():
        if not _PATH_KEY.search(key):
            continue
        for v in value if isinstance(value, list) else [value]:
            if isinstance(v, str) and v.strip():
                out.append(v.strip())
    return out


# ---- Approvals ------------------------------------------------------------------


def fingerprint(call):
    "One tool call, exactly, as the user approves it: its tool and its arguments."
    name = str((call or {}).get("name") or "")
    args = (call or {}).get("args") if isinstance((call or {}).get("args"), dict) else {}
    keep = {k: v for k, v in args.items() if k not in _VOLATILE}
    return hashlib.sha256(json.dumps([name, keep], sort_keys=True, ensure_ascii=False)
                          .encode("utf-8")).hexdigest()[:32]


def summary(call, width=200):
    "One line saying what the call does, for the card and the log."
    name = str((call or {}).get("name") or "?")
    args = (call or {}).get("args") if isinstance((call or {}).get("args"), dict) else {}
    what = args.get("CommandLine") or ", ".join(_call_paths(args)) or json.dumps(
        {k: v for k, v in args.items() if k not in _VOLATILE}, ensure_ascii=False)
    text = "%s: %s" % (name, " ".join(str(what).split()))
    return text if len(text) <= width else text[:width - 1] + "…"


def approvals_path(guard_dir):
    return os.path.join(guard_dir, "approved.json")


def take_approval(guard_dir, fp, now=None):
    """The approval of call FP, used up: its id, or None.  Approvals the user
    gave on the card wait APPROVAL_SECONDS, and each runs one call once."""
    path = approvals_path(guard_dir)
    now = time.time() if now is None else now
    try:
        with open(path, encoding="utf-8") as f:
            entries = json.load(f).get("approvals") or []
    except (OSError, ValueError):
        return None
    live = [e for e in entries if isinstance(e, dict) and float(e.get("expires") or 0) > now]
    hit = next((e for e in live if e.get("fp") == fp), None)
    if hit is None and len(live) == len(entries):
        return None
    rest = [e for e in live if e is not hit]
    fd, tmp = tempfile.mkstemp(dir=guard_dir, prefix=".approved.")
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        json.dump({"approvals": rest}, f)
    os.replace(tmp, path)
    return hit.get("id") if hit else None


def add_approval(guard_dir, fp, approval_id, now=None):
    "Let call FP run once, for APPROVAL_SECONDS (the herd, when the user approves it)."
    path = approvals_path(guard_dir)
    now = time.time() if now is None else now
    try:
        with open(path, encoding="utf-8") as f:
            entries = json.load(f).get("approvals") or []
    except (OSError, ValueError):
        entries = []
    entries = [e for e in entries if float(e.get("expires") or 0) > now and e.get("fp") != fp]
    entries.append({"id": approval_id, "fp": fp, "expires": now + APPROVAL_SECONDS})
    fd, tmp = tempfile.mkstemp(dir=guard_dir, prefix=".approved.")
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        json.dump({"approvals": entries}, f)
    os.replace(tmp, path)


# ---- The hook -------------------------------------------------------------------


def in_workspace(verdict, call, policy, agy_cwd):
    """VERDICT, an allowed run_command, run where the guard checked it: its
    Cwd, or the workspace.  agy runs commands in its first workspace, and an
    --add-dir comes before the workspace itself -- so in the guard's own
    directory (measured on 1.4.5: `pwd' there, even with the workspace added
    first).  Unless agy says (workspacePaths) it runs there, the line goes
    there first."""
    if verdict.decision != "allow" or (call or {}).get("name") not in COMMAND_TOOLS:
        return verdict
    args = (call or {}).get("args") or {}
    base = _real(args["Cwd"]) if args.get("Cwd") else policy.workspace
    if agy_cwd and _real(agy_cwd) == base:
        return verdict
    line = (verdict.overwrite or {}).get("CommandLine") or str(args.get("CommandLine") or "")
    verdict.overwrite = dict(verdict.overwrite or {}, CommandLine="cd %s && %s" % (shlex.quote(base), line))
    return verdict


def judge(guard_dir, payload):
    """(answer, verdict, call) for one PreToolUse payload: what agy reads, why,
    and the call.  A call the user approved once runs, that once."""
    policy = Policy(guard_dir)
    call = payload.get("toolCall") if isinstance(payload, dict) else None
    paths = payload.get("workspacePaths") if isinstance(payload, dict) else None
    agy_cwd = paths[0] if isinstance(paths, list) and paths and isinstance(paths[0], str) else None
    verdict = Guard(policy, agy_cwd).decide(call)
    if verdict.approvable:
        used = take_approval(policy.guard_dir, fingerprint(call))
        if used:
            verdict = Verdict("allow", kind="approved", rule="approved:" + str(used))
    verdict = in_workspace(verdict, call, policy, agy_cwd)
    return verdict.answer(), verdict, call
