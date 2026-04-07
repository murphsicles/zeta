#!/bin/bash
echo "Testing WSL access..."
cd /mnt/c/Users/mummy/.openclaw/workspace
echo "Current directory: $(pwd)"
echo "Files in workspace:"
ls -la | head -20