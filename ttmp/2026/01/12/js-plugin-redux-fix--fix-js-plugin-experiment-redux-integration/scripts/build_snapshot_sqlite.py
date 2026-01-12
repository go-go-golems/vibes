#!/usr/bin/env python3
"""Build a sqlite snapshot DB for two codebases."""

import argparse
import hashlib
import re
import sqlite3
from pathlib import Path

TEXT_EXTS = {".ts", ".tsx", ".js", ".jsx", ".md", ".json"}
CODE_EXTS = {".ts", ".tsx", ".js", ".jsx"}
REGEXES = [
    ("export_function", re.compile(r"^\s*export\s+function\s+([A-Za-z0-9_]+)")),
    ("function", re.compile(r"^\s*function\s+([A-Za-z0-9_]+)")),
    ("export_class", re.compile(r"^\s*export\s+class\s+([A-Za-z0-9_]+)")),
    ("class", re.compile(r"^\s*class\s+([A-Za-z0-9_]+)")),
    ("export_const_arrow", re.compile(r"^\s*export\s+const\s+([A-Za-z0-9_]+)\s*=\s*\(")),
    ("const_arrow", re.compile(r"^\s*const\s+([A-Za-z0-9_]+)\s*=\s*\(")),
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--current", required=True, help="Current workspace root")
    parser.add_argument("--bundle", required=True, help="Bundle snapshot root")
    parser.add_argument("--out", required=True, help="Output sqlite file")
    return parser.parse_args()


def write_snapshot(cur: sqlite3.Cursor, label: str, root: Path) -> None:
    for path in root.rglob("*"):
        if path.is_dir():
            continue
        if path.suffix not in TEXT_EXTS:
            continue

        rel = path.relative_to(root).as_posix()
        try:
            data = path.read_bytes()
        except Exception:
            continue

        sha256 = hashlib.sha256(data).hexdigest()
        cur.execute(
            "INSERT INTO files (snapshot, path, size, sha256) VALUES (?, ?, ?, ?)",
            (label, rel, len(data), sha256),
        )

        if path.suffix in CODE_EXTS:
            try:
                text = data.decode("utf-8")
            except UnicodeDecodeError:
                text = data.decode("utf-8", errors="replace")

            for line_no, line in enumerate(text.splitlines(), 1):
                for kind, rgx in REGEXES:
                    match = rgx.match(line)
                    if match:
                        cur.execute(
                            "INSERT INTO defs (snapshot, path, line, kind, name, text) "
                            "VALUES (?, ?, ?, ?, ?, ?)",
                            (label, rel, line_no, kind, match.group(1), line.strip()),
                        )


def main() -> None:
    args = parse_args()
    out_db = Path(args.out)
    if out_db.exists():
        out_db.unlink()

    conn = sqlite3.connect(out_db)
    cur = conn.cursor()
    cur.execute(
        """
        CREATE TABLE files (
            snapshot TEXT NOT NULL,
            path TEXT NOT NULL,
            size INTEGER NOT NULL,
            sha256 TEXT NOT NULL,
            PRIMARY KEY (snapshot, path)
        )
        """
    )
    cur.execute(
        """
        CREATE TABLE defs (
            snapshot TEXT NOT NULL,
            path TEXT NOT NULL,
            line INTEGER NOT NULL,
            kind TEXT NOT NULL,
            name TEXT,
            text TEXT NOT NULL
        )
        """
    )

    write_snapshot(cur, "current", Path(args.current))
    write_snapshot(cur, "bundle", Path(args.bundle))

    conn.commit()
    conn.close()


if __name__ == "__main__":
    main()
