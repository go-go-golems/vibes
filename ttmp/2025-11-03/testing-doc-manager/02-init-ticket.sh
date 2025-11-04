#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-/tmp/docmgr-scenario}"
REPO="${ROOT_DIR}/acme-chat-app"
cd "${REPO}"

DOCMGR="${DOCMGR_PATH:-docmgr}"

# Seed vocabulary (topics/docTypes/intent)
${DOCMGR} vocab add --category topics --slug chat --description "Chat backend and frontend surfaces" || true
${DOCMGR} vocab add --category topics --slug backend --description "Backend services" || true
${DOCMGR} vocab add --category topics --slug websocket --description "WebSocket lifecycle & events" || true
${DOCMGR} vocab add --category docTypes --slug design-doc --description "Structured rationale and architecture notes" || true
${DOCMGR} vocab add --category docTypes --slug reference --description "Prompt packs or API contracts" || true
${DOCMGR} vocab add --category docTypes --slug playbook --description "Operational cURL/test sequences" || true
${DOCMGR} vocab add --category docTypes --slug index --description "Ticket landing page" || true
${DOCMGR} vocab add --category intent --slug long-term --description "Likely to persist" || true

# Create ticket workspace (RFC-aligned under ttmp/)
${DOCMGR} init --ticket MEN-4242 --title "Normalize chat API paths and WebSocket lifecycle" --topics chat,backend,websocket --root ttmp

# Verify scaffolding
${DOCMGR} list tickets --root ttmp
