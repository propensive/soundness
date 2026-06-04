"""Shared helpers for soundness local-CI attestation tooling.

Two responsibilities:

  1. Parse `.dockerignore` (reused as the canonical CI input-set definition)
     and decide which tracked files are part of the CI input set, then compute
     a deterministic SHA-256 digest over them. The algorithm only uses git, so
     GitHub Actions can verify the digest without docker.

  2. Canonicalise the in-toto statement JSON for signing/verification.

This module is imported by the bash entry points in the same directory.
"""

from __future__ import annotations

import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path
from typing import Iterable


# ---- .dockerignore parsing ----------------------------------------------------

def _parse_dockerignore(path: Path) -> list[tuple[bool, str]]:
    if not path.exists():
        return []
    out: list[tuple[bool, str]] = []
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        negate = line.startswith("!")
        if negate:
            line = line[1:].lstrip()
        if line.startswith("./"):
            line = line[2:]
        elif line.startswith("/"):
            line = line[1:]
        # Strip trailing slash (docker treats `foo/` like `foo` for our purposes:
        # both exclude the directory and everything underneath).
        if line.endswith("/"):
            line = line[:-1]
        if not line:
            continue
        out.append((negate, line))
    return out


def _pattern_to_regex(pat: str) -> re.Pattern[str]:
    r"""Translate a docker-style ignore pattern to a regex matching the full path.

    Supported:
      *      matches any sequence of non-slash characters
      ?      matches a single non-slash character
      **/    matches zero or more leading directory components (".*/")
      /**    matches zero or more trailing path components
      **     in other positions matches any sequence including slashes
      All other characters are literal.
    """
    chars: list[str] = ["^"]
    i = 0
    n = len(pat)
    while i < n:
        # Three-char lookahead for `**/`
        if pat[i:i + 3] == "**/":
            chars.append("(?:.*/)?")
            i += 3
            continue
        # Two-char lookahead for `/**`
        if pat[i:i + 3] == "/**" and (i + 3 == n or pat[i + 3] == "/"):
            # `foo/**` matches `foo`, `foo/x`, `foo/x/y` …
            chars.append("(?:/.*)?")
            i += 3
            continue
        if pat[i:i + 2] == "**":
            chars.append(".*")
            i += 2
            continue
        c = pat[i]
        if c == "*":
            chars.append("[^/]*")
            i += 1
        elif c == "?":
            chars.append("[^/]")
            i += 1
        else:
            chars.append(re.escape(c))
            i += 1
    chars.append("$")
    return re.compile("".join(chars))


def _matches(path: str, regex: re.Pattern[str]) -> bool:
    """A pattern matches `path` if it matches the path itself or any ancestor
    directory of it (so excluding a directory excludes its contents)."""
    if regex.match(path):
        return True
    parts = path.split("/")
    for i in range(1, len(parts)):
        if regex.match("/".join(parts[:i])):
            return True
    return False


def is_excluded(path: str, patterns: list[tuple[bool, str]]) -> bool:
    excluded = False
    for negate, pat in patterns:
        regex = _pattern_to_regex(pat)
        if _matches(path, regex):
            excluded = not negate
    return excluded


# ---- git plumbing -------------------------------------------------------------

def _load_dockerignore(root: Path, commit: str) -> list[tuple[bool, str]]:
    """Read `.dockerignore` from `commit` (preferred) or fall back to the
    working tree if the file is not yet tracked there. The latter is needed
    only when bootstrapping — once `.dockerignore` is committed, every digest
    computation is a pure function of the commit."""
    try:
        data = subprocess.check_output(
            ["git", "show", f"{commit}:.dockerignore"],
            stderr=subprocess.DEVNULL,
        )
        tmp = root / ".soundness-ci-dockerignore.tmp"
        tmp.write_bytes(data)
        patterns = _parse_dockerignore(tmp)
        tmp.unlink()
        return patterns
    except subprocess.CalledProcessError:
        fallback = root / ".dockerignore"
        if fallback.exists():
            return _parse_dockerignore(fallback)
        return []


def _repo_root() -> Path:
    return Path(
        subprocess.check_output(["git", "rev-parse", "--show-toplevel"]).decode().strip()
    )


def _ls_tree(commit: str = "HEAD") -> Iterable[tuple[str, str]]:
    """Yield (blob_sha, path) for every tracked file at `commit`. NUL-separated
    so paths with spaces or special chars are safe."""
    raw = subprocess.check_output(["git", "ls-tree", "-r", "-z", commit])
    for entry in raw.split(b"\0"):
        if not entry:
            continue
        header, path_bytes = entry.split(b"\t", 1)
        mode, type_, sha = header.split(b" ")
        if type_ != b"blob":
            continue
        yield sha.decode("ascii"), path_bytes.decode("utf-8")


def _batch_sha256(blob_shas: list[str]) -> list[str]:
    """Pass `blob_shas` through a single `git cat-file --batch` process and
    return the SHA-256 of each blob's content. Order is preserved.

    Writing all SHAs up front and then reading every response is unsafe:
    for a tree of any real size, git's blob output (many MB) fills its
    stdout pipe and blocks git, while the parent is still writing to git's
    stdin (also pipe-bounded), so neither side makes progress. We feed
    stdin from a background thread so the main thread can drain stdout
    concurrently."""
    if not blob_shas:
        return []
    import threading

    proc = subprocess.Popen(
        ["git", "cat-file", "--batch"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    assert proc.stdin is not None and proc.stdout is not None

    write_error: list[BaseException] = []

    def write_input() -> None:
        try:
            for sha in blob_shas:
                proc.stdin.write((sha + "\n").encode("ascii"))
            proc.stdin.close()
        except BrokenPipeError:
            pass
        except BaseException as e:  # noqa: BLE001
            write_error.append(e)

    writer = threading.Thread(target=write_input, daemon=True)
    writer.start()

    out: list[str] = []
    for _ in blob_shas:
        header = proc.stdout.readline().decode().rstrip("\n")
        # Format: "<sha> <type> <size>" — type must be "blob".
        parts = header.split()
        if len(parts) != 3 or parts[1] != "blob":
            raise RuntimeError(f"unexpected git cat-file output: {header!r}")
        size = int(parts[2])
        content = proc.stdout.read(size)
        # Consume the trailing newline emitted after content.
        proc.stdout.read(1)
        out.append(hashlib.sha256(content).hexdigest())

    writer.join()
    if write_error:
        raise write_error[0]
    proc.wait()
    if proc.returncode != 0:
        raise RuntimeError(f"git cat-file exited with status {proc.returncode}")
    return out


# ---- public API ---------------------------------------------------------------

def compute_input_digest(commit: str = "HEAD") -> str:
    """Return a hex SHA-256 over the CI input set at `commit`.

    Algorithm:
      1. `git ls-tree -r <commit>` → tracked paths + blob SHAs.
      2. Filter through `.dockerignore` (loaded from `<commit>` *not* working tree).
      3. Sort surviving paths lexicographically (LC_ALL=C / Python default).
      4. For each, emit "<sha256-of-blob-content>  <path>\\n" (two-space separator).
      5. SHA-256 the concatenation. Return hex.
    """
    root = _repo_root()
    patterns = _load_dockerignore(root, commit)

    entries: list[tuple[str, str]] = []  # (path, blob_sha)
    for blob_sha, path in _ls_tree(commit):
        if is_excluded(path, patterns):
            continue
        entries.append((path, blob_sha))
    entries.sort(key=lambda x: x[0])

    content_shas = _batch_sha256([blob_sha for _, blob_sha in entries])
    h = hashlib.sha256()
    for (path, _), content_sha in zip(entries, content_shas):
        h.update(f"{content_sha}  {path}\n".encode("utf-8"))
    return h.hexdigest()


def canonical_json(obj) -> bytes:
    """Stable JSON encoding for signing/verification.

    Sorted keys, no insignificant whitespace, UTF-8, ensure_ascii=False so the
    encoding is well-defined for non-ASCII content."""
    return json.dumps(
        obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ).encode("utf-8")


# ---- CLI dispatch (used by bash wrappers) -------------------------------------

def _cli() -> int:
    if len(sys.argv) < 2:
        print("usage: _lib.py {input-digest|canonicalize}", file=sys.stderr)
        return 2
    cmd = sys.argv[1]
    if cmd == "input-digest":
        commit = sys.argv[2] if len(sys.argv) > 2 else "HEAD"
        print(compute_input_digest(commit))
        return 0
    if cmd == "canonicalize":
        obj = json.load(sys.stdin)
        sys.stdout.buffer.write(canonical_json(obj))
        return 0
    if cmd == "list-inputs":
        # Diagnostic: print the surviving input paths (for tuning .dockerignore).
        commit = sys.argv[2] if len(sys.argv) > 2 else "HEAD"
        root = _repo_root()
        patterns = _load_dockerignore(root, commit)
        for _, path in _ls_tree(commit):
            if not is_excluded(path, patterns):
                print(path)
        return 0
    print(f"unknown command: {cmd}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(_cli())
