from chunk import Turn, chunk_turns, split_text


def test_short_text_is_one_chunk():
    assert split_text("hello world") == ["hello world"]


def test_empty_is_nothing():
    assert split_text("  ") == []


def test_splits_on_paragraphs():
    body = ("para one.\n\n" * 20) + ("x" * 50)
    parts = split_text(body, max_chars=80, overlap=10)
    assert len(parts) > 1
    assert all(len(p) <= 90 for p in parts)


def test_chunk_turns_assigns_indices_and_drops_tiny():
    turns = [
        Turn(agent="claude", role="user", text="ok", source_path="/a"),
        Turn(
            agent="claude",
            role="user",
            text="this is a real question about posframe overlays",
            source_path="/a",
        ),
    ]
    out = chunk_turns(turns, min_chars=10)
    assert len(out) == 1
    assert out[0].chunk_index == 0
    assert "posframe" in out[0].text
