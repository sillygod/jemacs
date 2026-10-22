"""The guard of a herd agy agent (bin/herd_guard.py; ask 75362f01).

agy runs a guarded agent with --dangerously-skip-permissions, so the guard
is the only gate: allow or deny.  The user's hard requirement: the agent
never quietly gets a secret -- in the environment or in a file -- unless it
asks and the user approves that exact call, once.  jnotes' cases are here,
ported, and every way to a secret the ask named, plain, quoted and
obfuscated; the recursive searches the guard rewrites are run for real on
planted secrets."""

import json
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

BIN = Path(__file__).resolve().parents[2] / "bin"
HERD = BIN / "herd"
sys.path.insert(0, str(BIN))
import herd_guard as g  # noqa: E402

SECRET = "tok_SECRETVALUE_123"


@pytest.fixture
def home(tmp_path, monkeypatch):
    """A home of its own: the agent's workspace with planted secrets, a
    project beside it, ~/.ssh and the rest, and the agent's guard dir."""
    h = tmp_path.resolve() / "home"
    monkeypatch.setenv("HOME", str(h))
    ws = h / "src" / "proj"
    files = {
        "notes.md": "TOKEN appears here, harmlessly.\n",
        "src/main.py": "print('hi')  # TOKEN in code\n",
        ".env": f"API_TOKEN={SECRET}\n",
        "sub/.env.local": f"TOKEN={SECRET}\n",
        ".envrc": f"export TOKEN={SECRET}\n",
        "certs/server.pem": f"-----BEGIN PRIVATE KEY-----\nTOKEN {SECRET}\n",
        "certs/server.key": SECRET,
        "id_rsa": SECRET,
        "config/credentials.json": f'{{"TOKEN": "{SECRET}"}}\n',
        "app_secret.txt": SECRET,
        "my_token.txt": SECRET,
        ".netrc": SECRET, ".npmrc": SECRET, ".pypirc": SECRET, ".git-credentials": SECRET,
        "keys.p12": SECRET,
    }
    for rel, text in files.items():
        p = ws / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text)
    for rel in [".ssh/id_rsa", ".ssh/id_ed25519", ".aws/credentials", ".kube/config", ".docker/config.json",
                ".config/gh/hosts.yml", ".claude/settings.json", ".gemini/oauth_creds.json", ".grok/auth.json",
                "other/notes.txt", "data/rpc.token", "mail/rpc.url", "mail/PROTOCOL.md"]:
        p = h / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(SECRET)
    (ws / "link").symlink_to(h / ".ssh" / "id_rsa")
    guard = h / ".ghostherd" / "guard" / "agy-proj"
    guard.mkdir(parents=True)
    return {"home": h, "ws": ws, "guard": guard}


def policy(home, **over):
    data = {"agent": "agy-proj", "mode": "auto", "workspace": str(home["ws"]),
            "guard_root": str(home["guard"].parent), "herd": [str(HERD)],
            "secret_dirs": [str(home["home"] / "data")], "secret_files": [str(home["home"] / "mail" / "rpc.url")],
            "allow_commands": ["npm test", "pytest"]}
    data.update(over)
    (home["guard"] / "policy.json").write_text(json.dumps(data))
    return home["guard"]


@pytest.fixture
def guard(home):
    return policy(home)


def agy_paths(guard):
    """workspacePaths as agy sends them: the guard dir (an --add-dir) first,
    the workspace after -- and agy runs its tools in the first."""
    return [str(guard), json.loads((Path(guard) / "policy.json").read_text())["workspace"]]


def judge(guard, name, **args):
    return g.judge(str(guard), {"toolCall": {"name": name, "args": args}, "stepIdx": 3,
                                "workspacePaths": agy_paths(guard)})


def verdict(guard, name, **args):
    return judge(guard, name, **args)[1]


def verdict_at(guard, cwd, name, **args):
    "The verdict when agy says it runs its tools in CWD."
    return g.judge(str(guard), {"toolCall": {"name": name, "args": args}, "workspacePaths": [str(cwd)]})[1]


def cmd(guard, line):
    return verdict(guard, "run_command", CommandLine=line)


def allowed(v):
    return v.decision == "allow"


# ---- jnotes' cases, ported ------------------------------------------------------


def test_reads_inside_the_workspace_only(guard, home):
    ws = home["ws"]
    assert allowed(verdict(guard, "read_file", Path=str(ws / "notes.md")))
    assert allowed(verdict_at(guard, ws, "read_file", Path="notes.md"))
    assert allowed(verdict(guard, "read_file", Path=str(ws)))      # agy lists a folder with read_file
    assert allowed(verdict(guard, "grep_search", Query="x", SearchPath=str(ws / "src")))
    for outside in [home["home"] / "other" / "notes.txt", ws / ".." / "outside.txt", "file:///etc/hosts"]:
        v = verdict(guard, "read_file", Path=str(outside))
        assert (v.decision, v.kind) == ("deny", "ask") and "outside your workspace" in v.reason, outside
    assert not allowed(verdict(guard, "list_dir", DirectoryPath=str(home["home"])))


def test_writes_inside_in_auto_none_in_read_only(home):
    ws = home["ws"]
    gd = policy(home)
    assert allowed(verdict(gd, "edit_file", Path=str(ws / "notes.md")))
    assert allowed(verdict(gd, "write_to_file", TargetFile=str(ws / "new.md"), CodeContent="x"))
    assert verdict(gd, "write_file", Path=str(home["home"] / "other" / "x")).kind == "ask"
    assert verdict(gd, "write_file").kind == "ask"                     # which file?
    gd = policy(home, mode="read-only")
    v = verdict(gd, "edit_file", Path=str(ws / "notes.md"))
    assert (v.decision, v.kind) == ("deny", "ask") and "reads but does not change" in v.reason


@pytest.mark.parametrize("line", [
    'grep -rn "TOKEN" src', "rg -n '^#\\+title:' src", "grep -rl x . 2>/dev/null", "ls -la src | head -5",
    "find . -name '*.md'", "cat notes.md", "grep -n 'end$' notes.md", "wc -l notes.md src/main.py",
    "git status", "git log --oneline -5", "git diff", "git branch -a", "cd src && ls",
])
def test_read_only_commands_inside_run(guard, line):
    assert allowed(cmd(guard, line)), line


@pytest.mark.parametrize("line,kind,why", [
    ("cat ../../other/notes.txt", "ask", "outside your workspace"),
    ("grep -r x /", "ask", "outside your workspace"),
    ("grep --file=/etc/passwd x", "ask", "outside your workspace"),
    ("grep -f/etc/passwd x", "ask", "outside your workspace"),
    ("echo hi > x.md", "ask", "chains, redirects"),
    ("grep a . ; rm -rf src", "ask", "chains, redirects"),
    ("grep a . && rm x", "ask", "chains, redirects"),
    ("grep a || rm x", "ask", "||"),
    ('grep "$(rm x)" .', "secret", "expands a variable"),
    ("cat `ls`", "secret", "expands a variable"),
    ("find . -name '*.md' -delete", "ask", "could write files"),
    ("find . -exec rm {} +", "ask", "could write files"),
    ("rg --pre=sh x", "ask", "could write files"),
    ("ls | xargs rm", "ask", "xargs is not a command"),
    ("sed -i s/a/b/ x", "ask", "sed is not a command"),
])
def test_other_commands_are_refused_with_a_reason(guard, line, kind, why):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", kind) and why in v.reason, (line, v.kind, v.reason)
    assert v.approvable


def test_other_tools(guard):
    assert allowed(verdict(guard, "search_web", Query="x"))
    assert allowed(verdict(guard, "finish"))
    for name in ["read_url_content", "open_browser_url", "call_mcp_tool", "invoke_subagent", "something_new"]:
        v = verdict(guard, name)
        assert (v.decision, v.kind) == ("deny", "ask"), name


# ---- Secrets: what reveals the environment or a credential ---------------------


@pytest.mark.parametrize("line", [
    "env", "env | grep TOKEN", "printenv", "printenv HOME", "set", "export -p", "export", "declare -p",
    "declare -x", "typeset -p", "compgen -v",
    "echo $GITHUB_TOKEN", 'echo "${GITHUB_TOKEN}"', "echo `id`", 'cat "$HOME/.netrc"', "grep x $(pwd)",
    "ps e", "ps eww", "ps -E", "ps aux e",
    "launchctl getenv GITHUB_TOKEN", "security find-generic-password -s x -w",
    "security find-internet-password -s github.com", "security dump-keychain",
    "gh auth token", "gh auth status -t", "gh auth status --show-token",
    "gcloud auth print-access-token", "gcloud auth print-identity-token", "gcloud config config-helper",
    "aws configure get aws_secret_access_key", "aws configure export-credentials",
    "git config credential.helper", "git config --get-regexp cred", "git config -l", "git config --list",
    "git credential fill", "git remote -v", "git remote get-url origin",
    'python -c "import os; print(os.environ)"', "python3 -c'print(1)'", 'node -e "console.log(process.env)"',
    "node --eval x", "node -p process.env", "ruby -e 'p ENV'", "perl -e 'print %ENV'", "perl -E say",
    "php -r 'var_dump(getenv());'", "bash -c env", "sh -c 'echo hi'", "zsh -c env", "osascript -e x",
    "awk 'BEGIN{print ENVIRON[\"X\"]}'", "jq -n env", "jq -n '$ENV'", "cat /proc/self/environ",
])
def test_what_reveals_secrets_needs_the_users_approval(guard, line):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", "secret"), (line, v.kind, v.reason)
    assert "approve this exact call once on your card" in v.reason
    assert v.approvable


# ---- Secrets: files, read or searched, inside the workspace too ----------------


@pytest.mark.parametrize("line", [
    "cat .env", "cat ./.env", "cat sub/../.env", 'cat .e""nv', "cat '.env'", "cat \".env\"", "cat .\\env",
    "cat '.'env", "cat -- .env", "cat .en?", "cat .en*", "cat sub/.e*", "cd sub && cat .env.local",
    "head -5 sub/.env.local", "tail .envrc", "cat certs/server.pem", "cat certs/server.key", "cat certs/*",
    "head *", "cat id_rsa", "cat config/credentials.json", "cat app_secret.txt", "cat my_token.txt",
    "cat .netrc", "cat .npmrc", "cat .pypirc", "cat .git-credentials", "cat keys.p12", "cat link",
    "cat ~/.ssh/id_rsa", "cat ~/.ssh/id_ed25519", "ls ~/.ssh", "cat ~/.aws/credentials", "cat ~/.kube/config",
    "cat ~/.docker/config.json", "cat ~/.config/gh/hosts.yml", "cat ~/.claude/settings.json",
    "cat ~/.gemini/oauth_creds.json", "cat ~/.grok/auth.json", "cat ~/data/rpc.token", "cat ~/mail/rpc.url",
    "grep TOKEN .env", "grep -r TOKEN .env", "rg TOKEN .env", "grep -f .env notes.md", "grep --file=.env x",
    "wc -c .env", "diff .env notes.md", "sort .env", "awk '{print}' .env", "jq . config/credentials.json",
    "git show HEAD:.env",
])
def test_secret_files_need_the_users_approval(guard, line):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", "secret"), (line, v.kind, v.reason)
    assert "approve this exact call once on your card" in v.reason


@pytest.mark.parametrize("name,args", [
    ("read_file", {"Path": ".env"}), ("read_file", {"Path": "certs/server.pem"}),
    ("view_file", {"Path": "link"}), ("read_file", {"Path": "~/.ssh/id_rsa"}),
    ("list_dir", {"DirectoryPath": "~/.ssh"}), ("view_file", {"Path": "config/credentials.json"}),
    ("grep_search", {"Query": "TOKEN", "SearchPath": "."}),
    ("edit_file", {"Path": ".env"}),
])
def test_secret_files_through_agys_own_tools(guard, home, name, args):
    # agy gives its file tools full paths; as written, they are from the workspace.
    full = {k: (v if v.startswith("~") or k == "Query" else str(home["ws"] / v)) for k, v in args.items()}
    v = verdict(guard, name, **full)
    assert (v.decision, v.kind) == ("deny", "secret"), (name, full, v.reason)
    v = verdict_at(guard, home["ws"], name, **args)
    assert (v.decision, v.kind) == ("deny", "secret"), (name, args, v.reason)


@pytest.mark.parametrize("name,args", [
    ("read_file", {"Path": "notes.md"}), ("read_file", {"Path": "policy.json"}),
    ("read_file", {"Path": "approved.json"}), ("list_dir", {"DirectoryPath": "."}),
    ("grep_search", {"Query": "x", "SearchPath": "src"}), ("edit_file", {"Path": "notes.md"}),
    ("write_to_file", {"TargetFile": "file://new.md"}),
])
def test_a_relative_path_is_refused_unless_agy_runs_in_the_workspace(guard, home, name, args):
    """agy runs its tools in its first workspace -- the guard's own dir, an
    --add-dir -- so a relative path there is not the file the guard checked.
    Not approvable: the agent gives the full path instead."""
    for paths in (agy_paths(guard), None, [str(home["home"])]):
        payload = {"toolCall": {"name": name, "args": args}}
        if paths:
            payload["workspacePaths"] = paths
        v = g.judge(str(guard), payload)[1]
        assert (v.decision, v.rule, v.approvable) == ("deny", "relative-path", False), (paths, v)
        assert "give its full path, " + str(home["ws"]) in v.reason


@pytest.mark.parametrize("line", [
    "find . -name '*.pem' -exec cat {} +", "find . -name '*.pem' -exec cat {} ;",
    "find . -name '*.pem' | xargs cat", "find . -type f | xargs grep TOKEN", "cat < .env",
    "(cat .env)", "tac .env", "cp .env x", "base64 .env", "od -c .env", "strings .env",
    "python3 x.py", "nl .env | cat",
])
def test_every_other_way_round_is_denied(guard, line):
    v = cmd(guard, line)
    assert v.decision == "deny" and v.kind in ("ask", "secret"), (line, v.kind, v.reason)


# ---- Recursive searches skip secrets: rewritten, and run for real ---------------


@pytest.mark.parametrize("line", [
    "grep -r TOKEN .", "grep -rn TOKEN", "grep -r TOKEN src", "grep -rni token . | head -20",
    "grep --recursive TOKEN .", "egrep -r TOKEN .", "rg TOKEN", "rg --hidden TOKEN", "rg -uu TOKEN .",
])
def test_recursive_searches_are_rewritten_to_skip_secrets(guard, line):
    answer, v, _ = judge(guard, "run_command", CommandLine=line)
    assert v.decision == "allow" and v.rule == "search-excludes-secrets", (line, v.reason)
    new = answer["overwrite"]["CommandLine"]
    assert ("--exclude=.env" in new or "--glob='!.env'" in new or "--glob=!.env" in new), new


@pytest.mark.parametrize("line", ["grep -r TOKEN .", "grep -rn TOKEN .", "grep -rli token .",
                                  "egrep -r TOKEN .", "rg TOKEN", "rg --hidden TOKEN .", "rg -uu TOKEN ."])
def test_the_rewritten_search_never_prints_a_secret(guard, home, line):
    prog = line.split()[0]
    if not shutil.which(prog):
        pytest.skip(f"no {prog} here")
    answer, v, _ = judge(guard, "run_command", CommandLine=line)
    new = answer["overwrite"]["CommandLine"]
    out = subprocess.run(new, shell=True, cwd=home["guard"], capture_output=True, text=True).stdout  # as agy runs it
    assert SECRET not in out, (new, out)
    assert "notes.md" in out or "TOKEN appears" in out or "src/main.py" in out, (new, out)
    swept = (".env", "server.pem", "credentials.json", ".envrc")
    assert not any(s in out for s in swept), (new, out)  # nor their names (grep -l)
    raw = subprocess.run(line, shell=True, cwd=home["ws"], capture_output=True, text=True).stdout
    if "-uu" in line or "--hidden" in line or prog != "rg":
        assert SECRET in raw or any(s in raw for s in swept)  # unguarded, it swept them


def test_git_shows_no_secret_file_from_history(guard, home):
    ws = home["ws"]
    def git(*a):
        return subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *a], cwd=ws,
                              capture_output=True, text=True)
    git("init", "-q")
    git("add", "-A")
    git("commit", "-q", "-m", "everything, secrets too")
    for line in ["git log -p", "git show HEAD", "git grep TOKEN", "git diff HEAD~0", "git log -p -- ."]:
        answer, v, _ = judge(guard, "run_command", CommandLine=line)
        assert v.decision == "allow", (line, v.reason)
        new = answer.get("overwrite", {}).get("CommandLine", line)
        out = subprocess.run(new, shell=True, cwd=ws, capture_output=True, text=True).stdout
        assert SECRET not in out, (line, new)
    raw = subprocess.run("git show HEAD", shell=True, cwd=ws, capture_output=True, text=True).stdout
    assert SECRET in raw


# ---- Links out of the workspace -------------------------------------------------


@pytest.mark.parametrize("line", [
    "grep -R SECRETVALUE .", "grep -rR SECRETVALUE .", "grep -nR SECRETVALUE .", "grep -Rn SECRETVALUE .",
    "grep --dereference-recursive SECRETVALUE .", "grep -rS SECRETVALUE .", "grep -r --dereference-files x .",
    "grep -d dereference-recurse x .", "grep --directories=dereference-recurse x .", "egrep -R x .",
    "rg -L SECRETVALUE", "rg --follow SECRETVALUE .", "rg -nL SECRETVALUE", "rg -uuL SECRETVALUE",
    "find -L . -name hosts.yml", "find . -follow -name hosts.yml", "find -HL . -name x",
    "ls -RL", "ls -LR .", "ls -R -L", "tree -l", "tree -al .",
    "diff -r . src", "diff -rN . src", "diff -Nru . src", "diff --recursive . src",
])
def test_a_walk_that_follows_links_asks(guard, line):
    """A link in the workspace to ~/.config/gh, say: a search that follows
    links would read past the guard, which judges a link by where it leads
    only when it is named.  grep -r, rg and find do not follow links."""
    v = cmd(guard, line)
    assert (v.decision, v.rule, v.approvable) == ("deny", "follows-links", True), (line, v.reason)


@pytest.mark.parametrize("line", ["grep -eR notes.md", "grep -e RESULT -r .", "grep -A3 -rn x .", "rg -e L x",
                                  "rg -tpy -n x", "find . -links 1", "ls -R", "ls -L notes.md", "diff notes.md src/main.py"])
def test_flags_that_do_not_follow_links_run(guard, line):
    assert allowed(cmd(guard, line)), line


def test_a_search_does_not_follow_a_link_out(guard, home):
    "Run for real: a link to a folder outside, and to a file, holding a token."
    ws, out = home["ws"], home["home"] / "other" / "gh"
    out.mkdir(parents=True)
    (out / "hosts.yml").write_text(f"oauth_token: {SECRET}\n")
    (ws / "pub").mkdir()
    (ws / "pub" / "cfgs").symlink_to(out)
    (ws / "pub" / "tok").symlink_to(out / "hosts.yml")
    (ws / "pub" / "a.txt").write_text("SECRETVALUE is a word here\n")
    for line in ["grep -r SECRETVALUE .", "grep -rn SECRETVALUE pub", "rg SECRETVALUE", "rg --hidden SECRETVALUE ."]:
        prog = line.split()[0]
        if not shutil.which(prog):
            continue
        answer, v, _ = judge(guard, "run_command", CommandLine=line)
        assert v.decision == "allow", (line, v.reason)
        out_ = subprocess.run(answer["overwrite"]["CommandLine"], shell=True, cwd=home["guard"],
                              capture_output=True, text=True).stdout
        assert "a word here" in out_ and SECRET not in out_, (line, out_)
    if shutil.which("rg"):  # unguarded, following links, it reads the token
        assert SECRET in subprocess.run("rg -L SECRETVALUE", shell=True, cwd=ws, capture_output=True, text=True).stdout
    # Named, a link is judged by where it leads.
    assert cmd(guard, "cat pub/tok").kind == "ask" and cmd(guard, "grep -r x pub/cfgs").kind == "ask"
    # agy's own search: a link out in its scope stops it.
    v = verdict(guard, "grep_search", Query="SECRETVALUE", SearchPath=str(ws / "pub"))
    assert v.decision == "deny" and "a link to" in v.reason, v.reason
    (ws / "pub" / "cfgs").unlink()
    (ws / "pub" / "tok").unlink()
    assert allowed(verdict(guard, "grep_search", Query="SECRETVALUE", SearchPath=str(ws / "pub")))
    (ws / "pub" / "env").symlink_to(ws / ".env")  # in the workspace, but to a secret
    assert verdict(guard, "grep_search", Query="x", SearchPath=str(ws / "pub")).kind == "secret"


# ---- Code that would run outside the guard --------------------------------------


@pytest.mark.parametrize("rel", [".git/hooks/pre-commit", ".git/config", ".git", ".agents/hooks.json",
                                 ".gemini/settings.json", ".claude/settings.local.json", ".grok/x.json",
                                 "sub/.git/hooks/post-checkout"])
def test_writing_where_code_runs_from_asks(guard, home, rel):
    """A git hook, git's config (core.fsmonitor), an agent CLI's hooks: a
    write there would run code outside the guard."""
    for name, key in [("write_to_file", "TargetFile"), ("edit_file", "Path")]:
        v = verdict(guard, name, **{key: str(home["ws"] / rel)})
        assert v.decision == "deny" and v.approvable, (rel, v.reason)
        assert v.rule == ("secret-file" if rel.endswith("config") else "runs-code"), (rel, v.rule)
    assert verdict(guard, "read_file", Path=str(home["ws"] / rel)).decision == ("deny" if rel.endswith("config")
                                                                                 else "allow")


def test_writing_beside_them_runs(guard, home):
    for rel in [".gitignore", ".github/workflows/ci.yml", "src/agents.py", "docs/.claude.md", "CLAUDE.md"]:
        assert allowed(verdict(guard, "write_to_file", TargetFile=str(home["ws"] / rel))), rel


@pytest.mark.parametrize("line", [
    "awk -f prog.awk notes.md", "awk -fprog.awk notes.md", "awk --file=prog.awk notes.md",
    "gawk -i inc notes.md", "gawk --load=ext notes.md", "gawk -E prog.awk",
    "awk '{print > \"out\"}' notes.md", "awk '{print >> \"out\"}' notes.md", "awk '@include \"x\"' notes.md",
    "sort --compress-program=sh notes.md", "sort --files0-from=list", "wc --files0-from=list",
    "rg --hostname-bin=sh TOKEN", "file -C -m magic", "uniq notes.md .git/hooks/pre-commit",
    "git grep -O x", "git grep -Ovim x", "git grep --open-files-in-pager=sh x",
    "git diff --output=x", "git log -p --output=.git/hooks/pre-commit", "git -c core.fsmonitor=x status",
    "git -C /tmp status", "git --exec-path=/tmp status",
])
def test_commands_that_could_run_or_write_code_ask(guard, line):
    v = cmd(guard, line)
    assert v.decision == "deny" and v.approvable, (line, v.reason)


@pytest.mark.parametrize("line", ["awk '$3 < 100' notes.md", "awk '{print $1}' notes.md",
                                  "awk -F: '{print $2}' notes.md", "uniq notes.md", "sort -k2 notes.md"])
def test_their_plain_forms_run(guard, line):
    assert allowed(cmd(guard, line)), line


def test_the_agents_git_runs_no_hooks(guard, home):
    """A hook is code the agent can change (a tracked .husky/, a pre-commit
    config), so its git writes run with hooks off -- run for real."""
    ws = home["ws"]
    subprocess.run(["git", "init", "-q"], cwd=ws, check=True)
    hook = ws / ".git" / "hooks" / "pre-commit"
    hook.write_text("#!/bin/sh\ntouch HOOK-RAN\n")
    hook.chmod(0o755)
    for line in ["git add notes.md", "git commit -q -m x", "git switch -q -c b", "git stash"]:
        answer, v, _ = judge(guard, "run_command", CommandLine=line)
        new = answer["overwrite"]["CommandLine"]
        assert v.decision == "allow" and "git -c core.hooksPath=/dev/null " + line.split(" ", 1)[1] in new, new
    new = judge(guard, "run_command", CommandLine="git commit -q -m x")[0]["overwrite"]["CommandLine"]
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t", GIT_COMMITTER_NAME="t",
               GIT_COMMITTER_EMAIL="t@t")
    subprocess.run("git add notes.md", shell=True, cwd=ws, check=True)
    out = subprocess.run(new, shell=True, cwd=home["guard"], capture_output=True, text=True, env=env)
    assert out.returncode == 0, out.stderr
    assert not (ws / "HOOK-RAN").exists()
    subprocess.run("git commit -q --allow-empty -m y", shell=True, cwd=ws, env=env, check=True)
    assert (ws / "HOOK-RAN").exists()  # unguarded, it ran


@pytest.mark.parametrize("line", [
    "file -fcerts/server.pem", "file -m certs/server.key x", "diff --from-file=certs/server.pem notes.md",
    "diff --to-file=.env notes.md", "date -f .env", "date -r ~/.ssh/id_rsa", "grep -fcerts/server.key notes.md",
    "git commit -F .env", "git commit --file=certs/server.pem", "git commit -tcerts/server.key",
    "git blame --contents .env notes.md", "git blame -S config/credentials.json notes.md",
    "git diff --no-index /dev/null .env", "git diff --no-index notes.md ~/.ssh/id_rsa",
    "git diff --no-index ~/other/notes.txt .env",  # outside asks; the secret says more
    "git log -- .env", "git grep TOKEN -- .env",
])
def test_a_file_named_any_way_is_checked(guard, line):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", "secret"), (line, v.kind, v.reason)


@pytest.mark.parametrize("line", ["grep -rn '.env' src", "grep -e .env notes.md", "grep -e.env notes.md",
                                  "grep --regexp=.env notes.md", "git grep .env",
                                  "git grep -n .env -- src", "rg -n '.envrc' src"])
def test_a_secret_name_as_a_pattern_is_no_read(guard, line):
    assert allowed(cmd(guard, line)), line


def test_gits_config_is_a_secret(guard, home):
    """A remote URL can carry a token: git's config is a secret file, and
    searches -- rg --hidden goes into .git -- leave it out (run for real)."""
    ws = home["ws"]
    subprocess.run(["git", "init", "-q"], cwd=ws, check=True)
    subprocess.run(["git", "remote", "add", "origin", f"https://{SECRET}@github.com/x/y.git"], cwd=ws, check=True)
    for line in ["cat .git/config", "grep url .git/config", "head .git/config", "cat .git/*"]:
        v = cmd(guard, line)
        assert (v.decision, v.kind) == ("deny", "secret"), (line, v.reason)
    assert verdict(guard, "read_file", Path=str(ws / ".git" / "config")).kind == "secret"
    for line in ["grep -rn SECRETVALUE .", "rg --hidden SECRETVALUE", "rg -uuu SECRETVALUE ."]:
        prog = line.split()[0]
        if not shutil.which(prog):
            continue
        new = judge(guard, "run_command", CommandLine=line)[0]["overwrite"]["CommandLine"]
        out = subprocess.run(new, shell=True, cwd=home["guard"], capture_output=True, text=True).stdout
        assert SECRET not in out, (new, out)
        raw = subprocess.run(line, shell=True, cwd=ws, capture_output=True, text=True).stdout
        assert ".git/config" in raw  # unguarded, it swept it
    # agy's own search: only a config that holds a credential stops it.
    for name, url in [("with", f"https://{SECRET}@github.com/x/y.git"), ("without", "https://github.com/x/y.git")]:
        d = ws / "repos" / name
        d.mkdir(parents=True)
        subprocess.run(["git", "init", "-q"], cwd=d, check=True)
        subprocess.run(["git", "remote", "add", "origin", url], cwd=d, check=True)
        (d / "a.txt").write_text("TOKEN\n")
    assert verdict(guard, "grep_search", Query="TOKEN", SearchPath=str(ws / "repos" / "with")).kind == "secret"
    assert allowed(verdict(guard, "grep_search", Query="TOKEN", SearchPath=str(ws / "repos" / "without")))


# ---- Never, not even approved ---------------------------------------------------


@pytest.mark.parametrize("line", [
    "rm -rf build", "rm -fr build", "rm -r -f build", "rm --recursive --force build", "sudo ls",
    "git push --force", "git push -f origin main", "git push --force-with-lease", "git push origin +main",
    "git reset --hard HEAD~1", "git clean -fd",
])
def test_destructive_commands_are_never_run(guard, line):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", "never") and not v.approvable, (line, v.reason)


def test_the_guard_dir_is_never_read(guard):
    for v in [cmd(guard, f"cat {guard}/policy.json"), verdict(guard, "read_file", Path=str(guard / "approved.json")),
              verdict(guard, "write_file", Path=str(guard / "approved.json")),
              cmd(guard, f"ls {guard.parent}")]:
        assert (v.decision, v.kind) == ("deny", "never") and "guard" in v.reason


# ---- What the user approves: once ------------------------------------------------


@pytest.mark.parametrize("line,kind", [
    ("git push", "ask"), ("npm install left-pad", "ask"), ("pip install requests", "ask"),
    ("curl https://example.com", "ask"), ("wget x", "ask"), ("cat ../../other/notes.txt", "ask"),
])
def test_what_wants_the_user_is_approvable(guard, line, kind):
    v = cmd(guard, line)
    assert (v.decision, v.kind) == ("deny", kind) and v.approvable


def test_an_approval_runs_that_call_once(guard, home):
    call = {"name": "run_command", "args": {"CommandLine": "cat .env", "WaitMsBeforeAsync": 5000}}
    fp = g.fingerprint(call)
    agy = agy_paths(guard)
    assert g.judge(str(guard), {"toolCall": call, "workspacePaths": agy})[1].kind == "secret"
    g.add_approval(str(guard), fp, "r1")
    # Another call is not that call; the volatile arguments do not matter.
    assert g.judge(str(guard), {"toolCall": {"name": "run_command", "args": {"CommandLine": "cat .envrc"}},
                                "workspacePaths": agy})[1].decision == "deny"
    same = {"name": "run_command", "args": {"CommandLine": "cat .env", "WaitMsBeforeAsync": 1, "toolSummary": "x"}}
    answer, v, _ = g.judge(str(guard), {"toolCall": same, "workspacePaths": agy})
    assert (v.decision, v.kind, v.rule) == ("allow", "approved", "approved:r1")
    # It runs in the workspace, where the user saw it would -- not agy's guard dir.
    line = answer["overwrite"]["CommandLine"]
    assert line == "cd %s && cat .env" % shlex.quote(str(home["ws"]))
    assert SECRET in subprocess.run(line, shell=True, cwd=guard, capture_output=True, text=True).stdout
    # Once.
    assert g.judge(str(guard), {"toolCall": call, "workspacePaths": agy})[1].decision == "deny"


def test_commands_run_where_the_guard_checked_them(guard, home):
    """agy runs run_command in its first workspace, the guard's dir: every
    allowed line goes first to where the guard checked it -- its Cwd, or the
    workspace -- unless agy says it already runs there."""
    ws = home["ws"]
    def run(line, **args):
        answer, v, _ = judge(guard, "run_command", CommandLine=line, **args)
        assert v.decision == "allow", (line, v.reason)
        new = (answer.get("overwrite") or {}).get("CommandLine", line)
        return new, subprocess.run(new, shell=True, cwd=guard, capture_output=True, text=True).stdout
    new, out = run("cat notes.md")
    assert new == "cd %s && cat notes.md" % shlex.quote(str(ws)) and "harmlessly" in out
    (guard / "approved.json").write_text('{"x": "GUARDSTATE"}')
    for line in ["cat policy.json", "cat approved.json", "ls", "grep -n mode policy.json"]:
        new, out = run(line)
        assert "GUARDSTATE" not in out and "workspace" not in out and "approved.json" not in out, (line, out)
    new, out = run("cat main.py", Cwd=str(ws / "src"))
    assert new.startswith("cd %s && " % shlex.quote(str(ws / "src"))) and "print('hi')" in out
    new, out = run("cd src && cat main.py")
    assert "print('hi')" in out
    # A search the guard rewrote still runs there.
    new, out = run("grep -rn TOKEN .")
    assert "notes.md" in out and SECRET not in out, (new, out)
    # Where agy says it runs in the workspace, the line is as written.
    answer, v, _ = g.judge(str(guard), {"toolCall": {"name": "run_command", "args": {"CommandLine": "ls"}},
                                        "workspacePaths": [str(ws)]})
    assert answer == {"decision": "allow"}
    # A Cwd of the guard's, a secret one, or one outside: refused.
    assert verdict(guard, "run_command", CommandLine="cat policy.json", Cwd=str(guard)).kind == "never"
    assert verdict(guard, "run_command", CommandLine="ls", Cwd=str(home["home"] / ".ssh")).kind == "secret"
    assert verdict(guard, "run_command", CommandLine="ls", Cwd=str(home["home"] / "other")).kind == "ask"


def test_an_approval_lapses_and_never_lifts_a_never(guard):
    call = {"name": "run_command", "args": {"CommandLine": "cat .env"}}
    g.add_approval(str(guard), g.fingerprint(call), "old", now=0)
    assert g.judge(str(guard), {"toolCall": call, "workspacePaths": agy_paths(guard)})[1].decision == "deny"
    never = {"name": "run_command", "args": {"CommandLine": "rm -rf build"}}
    g.add_approval(str(guard), g.fingerprint(never), "nope")
    assert g.judge(str(guard), {"toolCall": never})[1].kind == "never"


# ---- The herd's commands, the project's, read-only mode -------------------------


@pytest.mark.parametrize("line", [
    '"$GHOSTHERD_HERD" recall', "$GHOSTHERD_HERD recall deploy", '"${GHOSTHERD_HERD}" list',
    '"$GHOSTHERD_HERD" ask qa --wait <<\'HERD\'\nthe $work, `literally`\nHERD',
    f"{HERD} reply 1a2b3c4d <<'HERD'\nDone: $X stays literal\nHERD\n",
    '"$GHOSTHERD_HERD" remember --name x "a fact"',
    # agy's agents go to their workspace first (seen live).
    'cd {ws} && "$GHOSTHERD_HERD" recall', 'cd src && "$GHOSTHERD_HERD" recall',
])
def test_the_herds_own_commands_run(guard, home, line):
    line = line.replace("{ws}", str(home["ws"]))
    if line.startswith("cd "):  # where it goes first is checked as any cd is
        assert cmd(guard, '"$GHOSTHERD_HERD" recall'.join(["cd ~/.ssh && ", ""])).kind == "secret"
        assert cmd(guard, '"$GHOSTHERD_HERD" recall'.join(["cd ~/.ghostherd/guard/agy-proj && ", ""])).kind == "never"
    assert allowed(cmd(guard, line)), line


@pytest.mark.parametrize("line", ['"$GHOSTHERD_HERD" hooks install', '"$GHOSTHERD_HERD" guard /x',
                                  '"$GHOSTHERD_HERD" ask qa <<HERD\n$X\nHERD'])
def test_not_every_herd_command(guard, line):
    assert cmd(guard, line).decision == "deny", line


def test_the_projects_own_commands_in_auto_only(home):
    gd = policy(home)
    for line in ["npm test", "npm test -- --watch=false", "pytest -q src"]:
        assert allowed(cmd(gd, line)), line
    assert cmd(gd, "pytest ~/.ssh").kind == "secret"
    assert cmd(gd, "npm run build").kind == "ask"
    assert allowed(cmd(gd, "git commit -am wip"))
    gd = policy(home, mode="read-only")
    for line in ["npm test", "git commit -am wip", "git add ."]:
        assert cmd(gd, line).decision == "deny", line


# ---- The hook itself, as agy runs it ----------------------------------------------


def run_hook(guard_dir, payload, env=None):
    e = {"PATH": os.environ["PATH"], "HOME": os.environ.get("HOME", "")}
    e.update(env or {})
    out = subprocess.run([str(HERD), "guard", str(guard_dir)], input=json.dumps(payload),
                         capture_output=True, text=True, env=e, timeout=20)
    assert out.returncode == 0, out.stderr
    return json.loads(out.stdout)


def test_the_hook_answers_allow_or_deny_and_fails_closed(guard, home, tmp_path):
    ok = run_hook(guard, {"toolCall": {"name": "run_command", "args": {"CommandLine": "ls"}},
                          "workspacePaths": agy_paths(guard)})
    assert ok == {"decision": "allow", "overwrite": {"CommandLine": "cd %s && ls" % shlex.quote(str(home["ws"]))}}
    no = run_hook(guard, {"toolCall": {"name": "run_command", "args": {"CommandLine": "env"}}})
    assert no["decision"] == "deny" and "secrets" in no["reason"]
    # No policy: the guard fails, and denies.
    broken = run_hook(tmp_path, {"toolCall": {"name": "run_command", "args": {"CommandLine": "ls"}}})
    assert broken["decision"] == "deny" and "could not check this call" in broken["reason"]
    # Not JSON at all.
    out = subprocess.run([str(HERD), "guard", str(guard)], input="not json", capture_output=True, text=True)
    assert json.loads(out.stdout)["decision"] == "deny"


def test_scrub_exec_drops_secret_names_and_keeps_the_rest(tmp_path):
    env = {"PATH": os.environ["PATH"], "HOME": str(tmp_path), "GITHUB_TOKEN": "s1", "BITBUCKET_TOKEN": "s2",
           "GRAFANA_API_KEY": "s3", "AWS_REGION": "s4", "MY_PASSWORD": "s5", "GHOSTHERD_RPC": "keep",
           "GHOSTHERD_TELEGRAM_TOKEN": "s6", "SSH_AUTH_SOCK": "/tmp/agent", "PLAIN": "ok", "GIT_AUTHOR_NAME": "me"}
    out = subprocess.run(
        [str(HERD), "scrub-exec", "--scrub", "(^|_)(TOKEN|KEY|PASSWORD|AUTH)(_|$)", "--scrub", "^AWS_",
         "--scrub", "^GHOSTHERD_TELEGRAM_", "--keep", "SSH_AUTH_SOCK", "--keep", "GHOSTHERD_RPC", "--",
         sys.executable, "-c", "import json, os; print(json.dumps(dict(os.environ)))"],
        capture_output=True, text=True, env=env, timeout=20)
    got = json.loads(out.stdout)
    for gone in ["GITHUB_TOKEN", "BITBUCKET_TOKEN", "GRAFANA_API_KEY", "AWS_REGION", "MY_PASSWORD",
                 "GHOSTHERD_TELEGRAM_TOKEN"]:
        assert gone not in got, gone
    for kept in ["PLAIN", "GHOSTHERD_RPC", "SSH_AUTH_SOCK", "GIT_AUTHOR_NAME", "PATH"]:
        assert kept in got, kept


# ---- The herd: the request, the approval, the agent told --------------------------


def test_the_herd_records_approves_once_and_tells_the_agent(guard, home, tmp_path, monkeypatch):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from herd import HerdStore, reset_herd
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=[{"name": "agy-proj", "kind": "agy", "state": "idle", "project": str(home["ws"])}])
    call = {"name": "run_command", "args": {"CommandLine": "cat .env"}}
    fp = g.fingerprint(call)
    one = h.guard_denied("agy-proj", str(guard), "run_command", "run_command: cat .env", "needs approval",
                         "secret-file", "secret", True, fp, json.dumps(call))
    # The same call refused again while it waits: the same request.
    assert h.guard_denied("agy-proj", str(guard), "run_command", "s", "r", "secret-file", "secret", True, fp, "")["id"] == one["id"]
    never = h.guard_denied("agy-proj", str(guard), "run_command", "rm -rf x", "never", "rm-rf", "never", False, "f2", "")
    with pytest.raises(ValueError, match="never run by an agent"):
        h.guard_approve(never["id"], root=str(guard.parent))
    # Only into the agent's own guard dir, under the root.
    with pytest.raises(ValueError, match="not from agy-proj's guard"):
        h.guard_approve(one["id"], root=str(tmp_path / "elsewhere"))
    assert h.guard_approve(one["id"], root=str(guard.parent))["status"] == "approved"
    mail = [m["body"] for m in h.tick(sessions=[{"name": "agy-proj", "kind": "agy", "state": "idle",
                                                 "project": str(home["ws"])}])["pending"]]
    assert any(b.startswith("[guard] The user approved, once: run_command: cat .env") for b in mail), mail
    # The agent runs it: allowed, once, and the herd hears it was used.
    answer, v, _ = g.judge(str(guard), {"toolCall": call, "workspacePaths": agy_paths(guard)})
    assert answer["decision"] == "allow" and v.rule == f"approved:{one['id']}"
    # A second tap, after the call ran but before the herd heard: no second run, no second mail.
    assert h.guard_approve(one["id"], root=str(guard.parent))["status"] == "approved"
    assert g.judge(str(guard), {"toolCall": call, "workspacePaths": agy_paths(guard)})[1].decision == "deny"
    assert h._conn.execute("SELECT COUNT(*) FROM mail WHERE body LIKE '[guard]%'").fetchone()[0] == 1
    h.guard_used(one["id"])
    reqs = {r["id"]: r for r in h.guard_requests()["requests"]}
    assert reqs[one["id"]]["status"] == "used" and reqs[never["id"]]["status"] == "denied"
    assert g.judge(str(guard), {"toolCall": call, "workspacePaths": agy_paths(guard)})[1].decision == "deny"
    h.close()
