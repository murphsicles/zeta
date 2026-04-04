@echo off
echo === Testing Zeta Compiler in WSL ===
echo.

REM Run test in WSL docker-desktop distribution
wsl -d docker-desktop bash -c "
cd /mnt/c/Users/mummy/.openclaw/workspace
echo 'Current directory:'
pwd
echo ''
echo 'Checking for compiler...'
if [ -f target/release/zetac ]; then
    echo 'Compiler exists! Testing while loop fix...'
    ./target/release/zetac test_while_final_correct.z 2>&1 | grep -i 'result\|error\|panic'
else
    echo 'Compiler not found. Building in WSL...'
    echo 'Installing Rust...'
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    export PATH=\"/root/.cargo/bin:\$PATH\"
    echo 'Building Zeta compiler...'
    cargo build --release 2>&1 | tail -5
    echo ''
    echo 'Testing while loop fix...'
    if [ -f target/release/zetac ]; then
        timeout 10 ./target/release/zetac test_while_final_correct.z 2>&1 | grep -i 'result\|error\|panic'
    fi
fi
"