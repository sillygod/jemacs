import json
import sqlite3
from pathlib import Path
from urllib.parse import quote

from importers.agy import iter_agy
from importers.claude import iter_claude
from importers.grok import iter_grok
from importers.textutil import grok_user_text, salvage_blob


def test_claude_skips_mode_and_keeps_user(tmp_path: Path):
    root = tmp_path / "projects" / "-tmp-demo"
    root.mkdir(parents=True)
    path = root / "sess-1.jsonl"
    lines = [
        {"type": "mode", "mode": "default", "sessionId": "sess-1"},
        {"type": "permission-mode", "sessionId": "sess-1"},
        {
            "type": "user",
            "sessionId": "sess-1",
            "cwd": "/tmp/demo",
            "timestamp": "2026-01-01T00:00:00Z",
            "message": {"role": "user", "content": "how does the overlay work"},
        },
        {
            "type": "assistant",
            "sessionId": "sess-1",
            "cwd": "/tmp/demo",
            "message": {
                "role": "assistant",
                "content": [
                    {"type": "thinking", "thinking": "secret"},
                    {"type": "text", "text": "it uses posframe when available"},
                    {
                        "type": "tool_use",
                        "name": "Bash",
                        "input": {"command": "ls", "description": "list"},
                    },
                ],
            },
        },
    ]
    path.write_text("\n".join(json.dumps(x) for x in lines), encoding="utf-8")
    (root / "memory").mkdir()
    (root / "memory" / "MEMORY.md").write_text(
        "# memory\nprefer reasoned design\n", encoding="utf-8"
    )

    sources = list(iter_claude(tmp_path / "projects"))
    texts = [t.text for _, turns in sources for t in turns]
    roles = [t.role for _, turns in sources for t in turns]
    assert any("overlay" in t for t in texts)
    assert any("posframe" in t for t in texts)
    assert any("[tool Bash]" in t for t in texts)
    assert all("secret" not in t for t in texts)
    assert "memory" in roles
    assert all(t.project == "/tmp/demo" or t.role == "memory" for _, turns in sources for t in turns)


def test_grok_skips_recap_updates_system_compaction(tmp_path: Path):
    proj = quote("/tmp/demo-project", safe="")
    sess = tmp_path / proj / "abc-session"
    sess.mkdir(parents=True)
    (sess / "recap_requests").mkdir()
    (sess / "recap_requests" / "dump.json").write_text(
        json.dumps({"should": "not be imported"}), encoding="utf-8"
    )
    (sess / "updates.jsonl").write_text(
        json.dumps({"type": "assistant", "content": "ACP stream junk"}) + "\n",
        encoding="utf-8",
    )
    chat = [
        {"type": "system", "content": "You are Grok"},
        {
            "type": "user",
            "synthetic_reason": "compaction_meta",
            "content": [{"type": "text", "text": "<user_query>old dump</user_query>"}],
        },
        {
            "type": "user",
            "content": [
                {
                    "type": "text",
                    "text": "<user_info>noise</user_info>\n<user_query>posframe overlay size</user_query>",
                }
            ],
        },
        {"type": "reasoning", "encrypted_content": "nope"},
        {"type": "assistant", "content": "make it 0.6 of the frame"},
        {
            "type": "tool_result",
            "content": "x" * 5000,
        },
    ]
    (sess / "chat_history.jsonl").write_text(
        "\n".join(json.dumps(x) for x in chat), encoding="utf-8"
    )
    (sess / "summary.json").write_text(
        json.dumps(
            {
                "generated_title": "Ghostherd overlay",
                "session_summary": "posframe instead of sidebar",
                "git_root_dir": "/tmp/demo-project",
            }
        ),
        encoding="utf-8",
    )

    sources = list(iter_grok(tmp_path))
    paths = [meta.path for meta, _ in sources]
    assert not any("recap_requests" in p for p in paths)
    assert not any(p.endswith("updates.jsonl") for p in paths)
    texts = [t.text for _, turns in sources for t in turns]
    roles = [t.role for _, turns in sources for t in turns]
    assert "You are Grok" not in " ".join(texts)
    assert "old dump" not in " ".join(texts)
    assert any("posframe overlay size" in t for t in texts)
    assert any("0.6" in t for t in texts)
    assert "memory" in roles
    assert all(len(t) < 2000 for t in texts if t.startswith("x") or "xxx" in t[:10])


def test_grok_user_query_extract():
    wrapped = "<user_info>os</user_info>\n<user_query>hello 中文</user_query>"
    assert grok_user_text(wrapped) == "hello 中文"


def test_agy_salvage_and_summaries(tmp_path: Path):
    conv = tmp_path / "conversations"
    conv.mkdir()
    db = conv / "conv-1.db"
    conn = sqlite3.connect(db)
    conn.execute(
        "CREATE TABLE steps (idx INTEGER, step_payload BLOB)"
    )
    blob = b"\x00\x01garbage  " + "agy-ghost-commit should not be blocked 中文".encode() + b"\x02"
    conn.execute("INSERT INTO steps VALUES (0, ?)", (blob,))
    conn.commit()
    conn.close()

    summaries = tmp_path / "conversation_summaries.db"
    conn = sqlite3.connect(summaries)
    conn.execute(
        """
        CREATE TABLE conversation_summaries (
            conversation_id TEXT,
            title TEXT,
            preview TEXT,
            workspace_uris TEXT,
            last_modified_time TEXT
        )
        """
    )
    conn.execute(
        "INSERT INTO conversation_summaries VALUES (?,?,?,?,?)",
        (
            "conv-1",
            "ghost commit",
            "agy-ghost-commit",
            json.dumps(["file:///tmp/demo-project"]),
            "2026-01-01",
        ),
    )
    conn.commit()
    conn.close()

    sources = list(iter_agy(tmp_path))
    texts = [t.text for _, turns in sources for t in turns]
    assert any("agy-ghost-commit" in t for t in texts)
    projects = {t.project for _, turns in sources for t in turns}
    assert "/tmp/demo-project" in projects


def test_claude_skips_huge_jsonl_lines(tmp_path: Path):
    root = tmp_path / "projects" / "p"
    root.mkdir(parents=True)
    huge = json.dumps(
        {
            "type": "user",
            "cwd": "/tmp/p",
            "message": {"role": "user", "content": "x" * 3000},
        }
    )
    small = json.dumps(
        {
            "type": "user",
            "cwd": "/tmp/p",
            "message": {"role": "user", "content": "keep this overlay question"},
        }
    )
    (root / "s.jsonl").write_text(huge + "\n" + small + "\n", encoding="utf-8")
    sources = list(iter_claude(tmp_path / "projects", max_line=200))
    texts = [t.text for _, turns in sources for t in turns]
    assert any("overlay" in t for t in texts)
    assert all("xxx" not in t for t in texts)


def test_salvage_drops_binary_noise():
    assert salvage_blob(b"\x00\x01\x02\xff") == ""
    assert "hello world this is prose" in salvage_blob(
        b"\x00hello world this is prose\x01"
    )
