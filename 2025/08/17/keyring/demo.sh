#!/bin/bash
echo "=== Keyring CLI Demo ==="
echo
echo "1. Show help:"
./keyring --help
echo
echo "2. Store some secrets:"
./keyring put --path openai/api_key --value sk-demo123456789
./keyring put --path aws/access_key --value AKIADEMO123
./keyring put --path aws/secret_key --value secretdemo456
echo
echo "3. List all top-level paths:"
./keyring list
echo
echo "4. List AWS secrets:"
./keyring list --prefix aws/
echo
echo "5. Retrieve a secret:"
./keyring get --path openai/api_key
echo
echo "6. Get secret in JSON format:"
./keyring get --path aws/access_key --output json
echo
echo "7. Delete a secret:"
./keyring delete --path aws/secret_key
echo
echo "8. Verify deletion:"
./keyring get --path aws/secret_key || echo "Secret successfully deleted!"
echo
echo "=== Demo Complete ==="
