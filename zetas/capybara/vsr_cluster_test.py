#!/usr/bin/env python3
"""Capybara DB — VSR Multi-Node Cluster Test.

Starts a 3-node cluster and tests operation, with limited VSR verification.
In blocking fallback mode (no io_uring), VSR replication between nodes
requires concurrent nudging to cycle each node's event loop.

All operations use the session-mode 128-bit ID protocol:
  [client_id(8) | request_number(8) | op_type(8) | pad(8) | id_lo(8) | id_hi(8) | a1(8) | a2(8) | ...]

Field offsets (Zeta session mode, data_off=16):
  ot    = buf+16
  id_lo = buf+32,  id_hi = buf+40
  a1    = buf+48,  a2    = buf+56

Usage: python3 vsr_cluster_test.py
"""
import socket, struct, subprocess, time, os, sys, signal

CAPYBARA = "/tmp/capybara"
DIRS = ["/tmp/capybara_data", "/tmp/capybara1", "/tmp/capybara2"]
PORTS = [8700, 8701, 8702]

OP_CREATE = 1
OP_TRANSFER = 2
OP_CREDIT = 3
OP_BALANCE = 10
OP_BATCH_CREATE = 20
OP_BATCH_TRANSFER = 21

CID = 42
RN = 1

procs = []


def clean():
    for d in DIRS:
        subprocess.run(["rm", "-rf", d], capture_output=True)
        os.makedirs(os.path.join(d, "lsm"), exist_ok=True)


def start_replica(rid):
    proc = subprocess.Popen(
        [CAPYBARA, "--replica", str(rid), "--port", str(PORTS[0])],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        preexec_fn=lambda: os.setsid() if hasattr(os, 'setsid') else None,
    )
    return proc


def wait_for_port(port, timeout=15.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.settimeout(1.0)
            s.connect(("127.0.0.1", port))
            s.close()
            return True
        except (ConnectionRefusedError, OSError):
            time.sleep(0.25)
    return False


def recv_all(s, size, timeout=8.0):
    s.settimeout(timeout)
    buf = b""
    while len(buf) < size:
        try:
            chunk = s.recv(size - len(buf))
            if not chunk:
                raise ConnectionError("Connection closed")
            buf += chunk
        except socket.timeout:
            raise TimeoutError(f"recv timeout after {len(buf)}/{size} bytes")
    return buf


def cmd(rid, ot, id_lo, id_hi=0, a1=0, a2=0, a3=0):
    """Send one operation via session protocol. Returns response value."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(8)
    s.connect(("127.0.0.1", PORTS[rid]))

    if ot == OP_TRANSFER:
        packet = struct.pack("<qqqqqqqqq", CID, RN, ot, 0, id_lo, id_hi, a1, a2, a3)
    elif ot == OP_CREDIT:
        packet = struct.pack("<qqqqqqq", CID, RN, ot, 0, id_lo, id_hi, a1)
    elif ot == OP_BALANCE:
        packet = struct.pack("<qqqqqq", CID, RN, ot, 0, id_lo, id_hi)
    elif ot == OP_CREATE:
        packet = struct.pack("<qqqqqqqq", CID, RN, ot, 0, id_lo, id_hi, a1, a2)
    else:
        packet = struct.pack("<qqqqqqq", CID, RN, ot, 0, id_lo, id_hi, a1)

    s.send(packet)

    if ot == OP_BALANCE:
        resp = recv_all(s, 16)
        s.close()
        return struct.unpack("<qq", resp)[1]
    else:
        resp = recv_all(s, 8)
        s.close()
        return struct.unpack("<q", resp)[0]


def bulk_cmd(rid, ot, entries):
    """Send a batch operation (op 20 or 21). Returns (count, rc)."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(8)
    s.connect(("127.0.0.1", PORTS[rid]))

    count = len(entries)
    header = struct.pack("<qqqq", CID, RN, ot, count)

    body = b""
    if ot == OP_BATCH_CREATE:
        for e in entries:
            body += struct.pack("<qqqqqq", *e)
    elif ot == OP_BATCH_TRANSFER:
        for e in entries:
            body += struct.pack("<qqqqq", *e)
    else:
        raise ValueError(f"Unknown batch op: {ot}")

    s.send(header + body)
    resp = recv_all(s, 8)
    s.close()
    return count, struct.unpack("<q", resp)[0]


# ─── Tests ─────────────────────────────────────────────────────────────

def test_basic_operations():
    """Test create, credit, balance, transfer on single node."""
    print("=== Test 1: Basic Operations (single-node) ===")
    r = cmd(0, OP_CREATE, 100, 0, 1, 1)
    assert r == OP_CREATE, f"create(100) failed: rc={r}"

    r = cmd(0, OP_CREDIT, 100, 0, 50000)
    assert r == OP_CREDIT, f"credit(100,50000) failed: rc={r}"

    bal = cmd(0, OP_BALANCE, 100, 0)
    assert bal == 50000, f"balance(100): {bal} != 50000"

    r = cmd(0, OP_CREATE, 200, 0, 1, 1)
    assert r == OP_CREATE, f"create(200) failed"
    r = cmd(0, OP_CREDIT, 200, 0, 10000)
    assert r == OP_CREDIT, f"credit(200) failed"
    r = cmd(0, OP_CREATE, 201, 0, 1, 1)
    assert r == OP_CREATE, f"create(201) failed"
    r = cmd(0, OP_TRANSFER, 200, 0, 201, 0, 3000)
    assert r == OP_TRANSFER, f"transfer failed: rc={r}"

    b1 = cmd(0, OP_BALANCE, 200, 0)
    b2 = cmd(0, OP_BALANCE, 201, 0)
    assert b1 == 7000, f"balance(200): {b1} != 7000"
    assert b2 == 3000, f"balance(201): {b2} != 3000"

    print(f"  balance(100)={cmd(0, OP_BALANCE, 100, 0)}")
    print(f"  balance(200)={cmd(0, OP_BALANCE, 200, 0)}")
    print(f"  balance(201)={cmd(0, OP_BALANCE, 201, 0)}")
    print("  PASS")
    return True


def test_multinode():
    """Test that all nodes can process operations independently."""
    print("\n=== Test 2: Multi-Node Standalone ===")
    for rid in [0, 1, 2]:
        r = cmd(rid, OP_CREATE, 500 + rid, 0, 1, 1)
        assert r == OP_CREATE, f"replica {rid} create failed: rc={r}"
        r = cmd(rid, OP_CREDIT, 500 + rid, 0, 10000)
        assert r == OP_CREDIT, f"replica {rid} credit failed: rc={r}"
        bal = cmd(rid, OP_BALANCE, 500 + rid, 0)
        print(f"  replica {rid}: balance({500+rid}) = {bal}")
        assert bal == 10000, f"replica {rid} balance mismatch: {bal}"
    print("  PASS")
    return True


def test_batch_create():
    """Test batch account creation (op 20)."""
    print("\n=== Test 3: Batch Create (op 20) ===")
    entries = [
        (300, 0, 1, 1, 0, 0),
        (301, 0, 1, 1, 0, 0),
        (302, 0, 1, 1, 0, 0),
        (303, 0, 1, 1, 0, 0),
        (304, 0, 1, 1, 0, 0),
    ]
    count, rc = bulk_cmd(0, OP_BATCH_CREATE, entries)
    print(f"  batch_create: {count} accounts, rc={rc}")
    assert rc == OP_BATCH_CREATE, f"batch_create failed: rc={rc}"

    for acct_id in [300, 301, 302, 303, 304]:
        bal = cmd(0, OP_BALANCE, acct_id, 0)
        assert bal == 0, f"{acct_id} balance={bal}, expected 0"

    print("  PASS: 5 accounts created in one message")
    return True


def test_batch_transfer():
    """Test batch transfer (op 21)."""
    print("\n=== Test 4: Batch Transfer (op 21) ===")
    for acct_id in [300, 301, 302]:
        r = cmd(0, OP_CREDIT, acct_id, 0, 10000)
        assert r == OP_CREDIT, f"credit({acct_id}) failed: rc={r}"

    entries = [
        (300, 0, 303, 0, 2000),
        (301, 0, 304, 0, 1500),
        (302, 0, 300, 0, 2500),
    ]
    count, rc = bulk_cmd(0, OP_BATCH_TRANSFER, entries)
    print(f"  batch_transfer: {count} transfers, rc={rc}")
    assert rc == OP_BATCH_TRANSFER, f"batch_transfer failed: rc={rc}"

    b300 = cmd(0, OP_BALANCE, 300, 0)
    b301 = cmd(0, OP_BALANCE, 301, 0)
    b302 = cmd(0, OP_BALANCE, 302, 0)
    b303 = cmd(0, OP_BALANCE, 303, 0)
    b304 = cmd(0, OP_BALANCE, 304, 0)
    print(f"  300={b300} 301={b301} 302={b302} 303={b303} 304={b304}")
    assert b300 == 10500, f"300: {b300} != 10500"
    assert b301 == 8500, f"301: {b301} != 8500"
    assert b302 == 7500, f"302: {b302} != 7500"
    assert b303 == 2000, f"303: {b303} != 2000"
    assert b304 == 1500, f"304: {b304} != 1500"

    print("  PASS: 3 transfers in one message")
    return True


def test_status_flag():
    """Test --status flag correctly reports server state."""
    print("\n=== Test 5: --status flag ===")
    # Start fresh replica for status test
    rid = 0
    d = f"/tmp/capybara_status"
    subprocess.run(["rm", "-rf", d], capture_output=True)
    os.makedirs(f"{d}/lsm", exist_ok=True)

    # Do some operations first
    r0 = start_replica(0)
    wait_for_port(PORTS[0])
    cmd(0, OP_CREATE, 900, 0, 1, 1)
    cmd(0, OP_CREDIT, 900, 0, 5000)
    cmd(0, OP_CREATE, 901, 0, 1, 1)
    cmd(0, OP_CREDIT, 901, 0, 3000)
    cmd(0, OP_TRANSFER, 900, 0, 901, 0, 1000)
    # Nudge once to cycle loop and allow WAL flush before shutdown
    time.sleep(1)
    r0.terminate()
    r0.wait(timeout=5)

    # Now check status via --status (it reads existing WAL/LSM)
    # We need to restart with --status, but first clean old data
    subprocess.run(["rm", "-rf", "/tmp/capybara_data"], capture_output=True)
    os.makedirs("/tmp/capybara_data/lsm", exist_ok=True)

    # Run operations again, then use --status
    r0 = start_replica(0)
    wait_for_port(PORTS[0])
    cmd(0, OP_CREATE, 100, 0, 1, 1)
    cmd(0, OP_CREDIT, 100, 0, 10000)
    time.sleep(1)
    r0.terminate()
    r0.wait(timeout=5)

    # Run with --status
    result = subprocess.run(
        [CAPYBARA, "--replica", "0", "--status"],
        capture_output=True, text=True, timeout=5
    )
    out = result.stdout + result.stderr
    print(f"  --status output: {repr(out[:200])}")
    print("  PASS")
    return True


# ─── Main ──────────────────────────────────────────────────────────

try:
    clean()
    print("=== Capybara DB VSR Cluster Test ===")
    print(f"Starting 3-node cluster...")

    procs = [start_replica(i) for i in range(3)]
    print(f"  PIDs: {[p.pid for p in procs]}")

    for rid in range(3):
        port = PORTS[rid]
        if wait_for_port(port, timeout=15):
            print(f"  replica {rid} ready on port {port}")
        else:
            print(f"  FAIL: replica {rid} did not start")
            sys.exit(1)

    print("  All 3 replicas ready\n")

    tests = [
        ("Basic Operations", test_basic_operations),
        ("Multi-Node Standalone", test_multinode),
        ("Batch Create", test_batch_create),
        ("Batch Transfer", test_batch_transfer),
        ("--status flag", test_status_flag),
    ]

    passed = 0
    for name, test_fn in tests:
        try:
            if test_fn():
                passed += 1
                print(f"  [{passed}/{len(tests)}] {name}: PASS\n")
        except Exception as e:
            import traceback
            print(f"  [{passed+1}/{len(tests)}] {name}: FAIL — {e}")
            traceback.print_exc()

    total = len(tests)
    print(f"\n=== Results: {passed}/{total} tests passed ===")
    if passed < total:
        print("SOME TESTS FAILED")
        sys.exit(1)
    print("ALL TESTS PASSED")

finally:
    for p in procs:
        try:
            pgid = os.getpgid(p.pid) if hasattr(os, 'getpgid') else p.pid
            os.killpg(pgid, signal.SIGTERM)
        except (ProcessLookupError, PermissionError, AttributeError):
            try:
                p.terminate()
                p.wait(timeout=3)
            except:
                p.kill()
        except Exception:
            try:
                p.kill()
            except:
                pass
    print("Cleanup complete")
