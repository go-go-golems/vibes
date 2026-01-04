ui = true
disable_mlock = true

storage "file" {
  path = "/home/ubuntu/vault-envrc-project/vault-config/data"
}

listener "tcp" {
  address     = "0.0.0.0:8200"
  tls_disable = 1
}

api_addr = "http://127.0.0.1:8200"
cluster_addr = "https://127.0.0.1:8201"

# Enable audit logging (will be configured after initialization)
# audit "file" {
#   file_path = "/home/ubuntu/vault-envrc-project/audit-logs/vault_audit.log"
# }

log_level = "Info"

