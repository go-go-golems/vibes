#!/usr/bin/env bash
set -u
for s in ssh cron docker; do
  st=$(systemctl is-active "$s" 2>/dev/null || true)
  echo "$s: ${st:-unknown}"
done
