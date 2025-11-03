#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-/tmp/docmgr-scenario}"
REPO="${ROOT_DIR}/acme-chat-app"
cd "${REPO}"

DOCMGR="${DOCMGR_PATH:-docmgr}"

# Add documents
${DOCMGR} add --ticket MEN-4242 --doc-type design-doc --title "Path Normalization Strategy" --root ttmp
${DOCMGR} add --ticket MEN-4242 --doc-type reference --title "Chat WebSocket Lifecycle" --root ttmp
${DOCMGR} add --ticket MEN-4242 --doc-type playbook --title "Smoke Tests for Chat" --root ttmp

# Show guidelines for design-doc
${DOCMGR} guidelines --doc-type design-doc --output markdown || true

# Enrich metadata on index.md
INDEX_MD="ttmp/MEN-4242-normalize-chat-api-paths-and-websocket-lifecycle/index.md"
${DOCMGR} meta update --doc "${INDEX_MD}" --field Owners --value "manuel,alex"
${DOCMGR} meta update --doc "${INDEX_MD}" --field Summary --value "Unify chat HTTP paths and stabilize WebSocket flows."
${DOCMGR} meta update --doc "${INDEX_MD}" --field ExternalSources --value "https://example.com/rfc/chat-api,https://example.com/ws-lifecycle"

# List docs and tickets
${DOCMGR} list tickets --root ttmp --ticket MEN-4242
${DOCMGR} list docs --root ttmp --ticket MEN-4242
