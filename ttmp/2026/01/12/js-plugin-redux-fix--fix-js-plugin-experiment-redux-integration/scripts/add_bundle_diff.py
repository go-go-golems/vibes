#!/usr/bin/env python3
"""Append git diff metadata to the snapshot sqlite DB."""

import argparse
import sqlite3
import subprocess
from pathlib import Path
from typing import Dict, Tuple


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", required=True, help="Path to git repo")
    parser.add_argument("--db", required=True, help="SQLite db path")
    parser.add_argument("--from", dest="commit_from", required=True, help="Base commit")
    parser.add_argument("--to", dest="commit_to", required=True, help="Target commit")
    return parser.parse_args()


def parse_numstat(repo: Path, commit_from: str, commit_to: str) -> Dict[str, Tuple[int, int]]:
    output = subprocess.check_output(
        ["git", "-C", str(repo), "diff", "--numstat", f"{commit_from}..{commit_to}"]
    ).decode("utf-8")
    stats: Dict[str, Tuple[int, int]] = {}
    for line in output.strip().splitlines():
        parts = line.split("\t")
        if len(parts) < 3:
            continue
        added, removed, path = parts[0], parts[1], parts[2]
        try:
            a = int(added)
            r = int(removed)
        except ValueError:
            a = -1
            r = -1
        stats[path] = (a, r)
    return stats


def main() -> None:
    args = parse_args()
    repo = Path(args.repo)
    db = Path(args.db)

    conn = sqlite3.connect(db)
    cur = conn.cursor()
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS commit_diffs (
            commit_from TEXT NOT NULL,
            commit_to TEXT NOT NULL,
            path TEXT NOT NULL,
            status TEXT NOT NULL,
            added INTEGER,
            removed INTEGER,
            PRIMARY KEY (commit_from, commit_to, path)
        )
        """
    )

    numstat = parse_numstat(repo, args.commit_from, args.commit_to)

    name_status = subprocess.check_output(
        ["git", "-C", str(repo), "diff", "--name-status", f"{args.commit_from}..{args.commit_to}"]
    ).decode("utf-8")

    for line in name_status.strip().splitlines():
        if not line:
            continue
        parts = line.split("\t")
        status = parts[0]
        path = parts[-1]
        added, removed = numstat.get(path, (-1, -1))
        cur.execute(
            "INSERT OR REPLACE INTO commit_diffs (commit_from, commit_to, path, status, added, removed) "
            "VALUES (?, ?, ?, ?, ?, ?)",
            (args.commit_from, args.commit_to, path, status, added, removed),
        )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    main()
