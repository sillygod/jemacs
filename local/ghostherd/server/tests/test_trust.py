"""bin/herd_trust.py: the trust the herd records for the worktrees it
creates, in each CLI's own store (ask 2befbbd6).  Stores are temp files:
never the user's ~/.grok or agy's settings."""

import fcntl
import importlib.machinery
import importlib.util
import json
import os
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "bin" / "herd_trust.py"


def _load():
    loader = importlib.machinery.SourceFileLoader("herd_trust", str(SCRIPT))
    spec = importlib.util.spec_from_loader("herd_trust", loader)
    mod = importlib.util.module_from_spec(spec)
    was, sys.dont_write_bytecode = sys.dont_write_bytecode, True
    try:
        loader.exec_module(mod)
    finally:
        sys.dont_write_bytecode = was
    return mod


ht = _load()

GROK = (
    '[folders."/Users/u/.emacs.d"]\ntrusted = true\ndecided_at = 1792824400\n\n'
    '[folders."/Users/u/src/my \\"odd\\" app"]\ntrusted = true\ndecided_at = 1786282329\n\n'
    '[folders."/Users/u/src/declined"]\ntrusted = false\ndecided_at = 1786282330\n'
)

AGY = json.dumps({
    "altScreenMode": "always",
    "model": "Gemini 4 Argon (High)",
    "permissions": {"allow": ["command(git status)"]},
    "trustedWorkspaces": ["/Users/u/src/jrevba", "/Users/u/.emacs.d/", "/Users/u/src/中文"],
}, indent=2, ensure_ascii=False) + "\n"


def _store(tmp_path, name, text):
    p = tmp_path / name
    p.write_text(text, encoding="utf-8")
    os.chmod(p, 0o600)
    return str(p)


def _run(*args):
    return ht.main([str(a) for a in args])


@pytest.mark.parametrize("cli,name,text", [("grok", "trusted_folders.toml", GROK),
                                          ("agy", "settings.json", AGY)])
def test_an_entry_comes_and_goes_and_the_store_is_as_it_was(tmp_path, capsys, cli, name, text):
    store = _store(tmp_path, name, text)
    wt = "/Users/u/.ghostherd/worktrees/.emacs.d/rd one"
    assert _run("add", cli, store, wt) == 0 and capsys.readouterr().out == "added\n"
    assert _run("trusts", cli, store, wt) == 0
    assert _run("add", cli, store, wt) == 0 and capsys.readouterr().out == "present\n"
    assert oct(os.stat(store).st_mode & 0o777) == "0o600"
    assert _run("remove", cli, store, wt) == 0 and capsys.readouterr().out == "removed\n"
    assert Path(store).read_text(encoding="utf-8") == text
    assert _run("remove", cli, store, wt) == 0 and capsys.readouterr().out == "absent\n"
    assert sorted(os.listdir(tmp_path)) == sorted([name] + ([name + ".lock"] if cli == "grok" else []))


def test_grok_writes_its_table_as_grok_does(tmp_path):
    store = _store(tmp_path, "trusted_folders.toml", GROK)
    assert _run("add", "grok", store, "/w/rd") == 0
    tail = Path(store).read_text().split("\n\n")[-1]
    assert tail.startswith('[folders."/w/rd"]\ntrusted = true\ndecided_at = ')
    assert tail.endswith("\n") and tail.count("\n") == 3


def test_a_cli_trusts_what_its_own_record_says(tmp_path):
    grok = _store(tmp_path, "trusted_folders.toml", GROK)
    agy = _store(tmp_path, "settings.json", AGY)
    # grok: its table, exactly, and trusted = true.
    assert _run("trusts", "grok", grok, "/Users/u/.emacs.d") == 0
    assert _run("trusts", "grok", grok, '/Users/u/src/my "odd" app') == 0
    for no in ("/Users/u/.emacs.d/", "/Users/u/.emacs.d/sub", "/Users/u", "/Users/u/src/declined"):
        assert _run("trusts", "grok", grok, no) == 1, no
    # agy: in its list, with or without the slash it was written with.
    for yes in ("/Users/u/src/jrevba", "/Users/u/.emacs.d", "/Users/u/src/中文"):
        assert _run("trusts", "agy", agy, yes) == 0, yes
    for no in ("/Users/u/src", "/Users/u/src/jrevba/sub"):
        assert _run("trusts", "agy", agy, no) == 1, no
    # No store yet: nothing is trusted.
    assert _run("trusts", "grok", tmp_path / "none.toml", "/Users/u/.emacs.d") == 1


@pytest.mark.parametrize("cli,name,bad", [
    ("grok", "trusted_folders.toml", '[folders."/a"\ntrusted = '),
    ("grok", "trusted_folders.toml", 'folders = 3\n'),
    ("agy", "settings.json", '{"trustedWorkspaces": ['),
    ("agy", "settings.json", '{"trustedWorkspaces": "/a"}\n'),
    ("agy", "settings.json", '[1, 2]\n'),
])
def test_a_store_not_as_its_cli_writes_it_is_left_be(tmp_path, cli, name, bad):
    store = _store(tmp_path, name, bad)
    assert _run("add", cli, store, "/w/rd") == 3
    assert _run("trusts", cli, store, "/w/rd") in (1, 3)
    assert Path(store).read_text() == bad


def test_grok_is_waited_for_while_it_saves(tmp_path, monkeypatch):
    """grok saves under trusted_folders.toml.lock: held, the herd waits,
    then gives up and writes nothing; free, it writes."""
    store = _store(tmp_path, "trusted_folders.toml", GROK)
    monkeypatch.setattr(ht, "LOCK_WAIT", 0.2)
    fd = os.open(store + ".lock", os.O_RDWR | os.O_CREAT)
    fcntl.flock(fd, fcntl.LOCK_EX)
    try:
        assert _run("add", "grok", store, "/w/rd") == 3
        assert Path(store).read_text() == GROK
    finally:
        os.close(fd)
    assert _run("add", "grok", store, "/w/rd") == 0


def test_a_store_written_meanwhile_is_read_again(tmp_path, monkeypatch):
    """A save of the CLI's between the herd's read and its rename: the herd
    reads again and adds to that -- the CLI's save is not lost."""
    store = _store(tmp_path, "settings.json", AGY)
    real = ht._same
    calls = []

    def theirs_in_between(path, st):
        if not calls:
            calls.append(1)
            data = json.loads(Path(path).read_text())
            data["model"] = "Claude Opus 5.5"
            Path(path).write_text(json.dumps(data, indent=2) + "\n")
        return real(path, st)

    monkeypatch.setattr(ht, "_same", theirs_in_between)
    assert _run("add", "agy", store, "/w/rd") == 0
    data = json.loads(Path(store).read_text())
    assert data["model"] == "Claude Opus 5.5" and "/w/rd" in data["trustedWorkspaces"]
    monkeypatch.setattr(ht, "_same", lambda path, st: False)
    assert _run("add", "agy", store, "/w/other") == 3


def test_a_linked_store_is_written_through(tmp_path):
    real = Path(_store(tmp_path, "real.toml", GROK))
    link = tmp_path / "trusted_folders.toml"
    link.symlink_to(real)
    assert _run("add", "grok", link, "/w/rd") == 0
    assert link.is_symlink() and '"/w/rd"' in real.read_text()


def test_a_new_store_is_made_private(tmp_path):
    store = tmp_path / "new" / "settings.json"
    assert _run("add", "agy", store, "/w/rd") == 0
    assert json.loads(store.read_text()) == {"trustedWorkspaces": ["/w/rd"]}
    assert oct(os.stat(store).st_mode & 0o777) == "0o600"


def test_no_folder_too_broad_to_be_its_own(tmp_path):
    store = _store(tmp_path, "settings.json", AGY)
    for path in ("/", os.path.expanduser("~"), "relative/dir"):
        assert _run("add", "agy", store, path) == 2
    assert _run("frob", "agy", store, "/w") == 2
    assert _run("add", "claude", store, "/w") == 2
    assert Path(store).read_text() == AGY
