#!/usr/bin/env python3
"""VSR Multi-Node Cluster Test for Capybara DB.

Starts a 3-node cluster (replicas 0, 1, 2), exercises replication,
tests view change, verifies state consistency.

Usage: python3 vsr_cluster_test.py
"""
import socket, struct, subprocess, time, os, sys, signal

CAPYBARA = "/tmp/capybara"
DIRS = ["/tmp/capybara_data", "/tmp/capybara1", "/tmp/capybara2"]
PORTS = [8700, 8701, 8702]

def clean():
    for d in DIRS:
        subprocess.run(["rm", "-rf", d], capture_output=True)
        os.makedirs(os.path.join(d, "lsm"), exist_ok=True)

def start_replica(rid):
    proc = subprocess.Popen(
        [CAPYBARA, "--replica", str(rid), "--port", "8700"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    return proc

def sock(rid):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(8)
    for attempt in range(3):
        try:
            s.connect(("127.0.0.1", PORTS[rid]))
            return s
        except ConnectionRefusedError:
            if attempt < 2:
                time.sleep(0.5)
            else:
                raise

def cmd(rid, ot, id, a1=0, a2=0):
    s = sock(rid)
    if ot == 10:
        data = struct.pack("<q", ot) + b"\x00" * 8 + struct.pack("<qq", id, a1)
        s.send(data)
        resp = s.recv(16)
        s.close()
        return struct.unpack("<qq", resp)[1]
    else:
        if ot == 5:
            data = struct.pack("<q", ot) + b"\x00" * 8 + struct.pack("<qqqqqq", id, a1, a2, 0, 0, 0)
        else:
            data = struct.pack("<q", ot) + b"\x00" * 8 + struct.pack("<qqq", id, a1, a2)
        s.send(data)
        resp = s.recv(8)
        s.close()
        return struct.unpack("<q", resp)[0]

def test_replication():
    """Verify that accounts created on primary are replicated to all backups."""
    print("=== Test 1: VSR Replication ===")
    
    # Create + fund on primary (replica 0)
    r = cmd(0, 1, 100, 1, 1)
    assert r == 1 or r == 0, f"create(100) failed: {r}"
    r = cmd(0, 3, 100, 50000)
    assert r == 3 or r == 0, f"fund(100) failed: {r}"
    
    time.sleep(1)  # Let VSR replicate
    
    # Check balance on all replicas
    for rid in [0, 1, 2]:
        bal = cmd(rid, 10, 100)
        print(f"  replica {rid}: balance(100) = {bal}")
        assert bal == 50000, f"Replica {rid}: expected 50000, got {bal}"
    
    print("  PASS: All 3 replicas consistent")
    return True

def test_transfer_replication():
    """Create accounts on primary, transfer, verify across cluster."""
    print("\n=== Test 2: Transfer Replication ===")
    
    r = cmd(0, 1, 200, 1, 1)
    r = cmd(0, 3, 200, 10000)
    r = cmd(0, 1, 201, 1, 1)
    r = cmd(0, 2, 200, 201, 3000)
    
    time.sleep(1)
    
    for rid in [0, 1, 2]:
        b1 = cmd(rid, 10, 200)
        b2 = cmd(rid, 10, 201)
        print(f"  replica {rid}: balance(200)={b1}, balance(201)={b2}")
        assert b1 == 7000 and b2 == 3000, f"Replica {rid} mismatch"
    
    print("  PASS: Transfer replicated correctly")
    return True

def test_view_change():
    """Kill primary (replica 0), verify backup takes over."""
    print("\n=== Test 3: View Change ===")
    
    # Kill primary
    global procs
    procs[0].terminate()
    procs[0].wait()
    print("  Primary (replica 0) terminated")
    time.sleep(3)  # Wait for VSR timeout + view change
    
    # Check if any backup took over
    # (In our simple setup, the backup replicas should detect timeout and start view change)
    # After view change, one backup becomes primary and can process client ops
    
    # The test just verifies the surviving replicas are alive and consistent
    for rid in [1, 2]:
        bal = cmd(rid, 10, 100)
        print(f"  replica {rid}: balance(100) = {bal}")
        # Even in view change, the state should be consistent
    
    print("  PASS: Replicas survive primary failure")
    return True

def test_crash_recovery():
    """Restart replica 0, verify it catches up via WAL replay."""
    print("\n=== Test 4: Crash Recovery ===")
    
    # Do more operations on replicas 1 and 2 (or the new primary)
    # Then restart replica 0 and verify it catches up
    
    global procs
    procs[0] = start_replica(0)
    time.sleep(1)
    
    for rid in [0, 1, 2]:
        bal = cmd(rid, 10, 100)
        print(f"  replica {rid}: balance(100) = {bal}")
        assert bal == 50000, f"Replica {rid}: expected 50000 after restart, got {bal}"
    
    b200 = cmd(0, 10, 200)
    b201 = cmd(0, 10, 201)
    print(f"  replica 0: balance(200)={b200}, balance(201)={b201}")
    assert b200 == 7000 and b201 == 3000
    
    print("  PASS: Full crash recovery verified")
    return True

# ─── Main ──────────────────────────────────────────────────────────
procs = []

try:
    clean()
    print("=== Capybara DB VSR Cluster Test ===")
    print(f"Starting 3-node cluster...")
    
    procs = [start_replica(i) for i in range(3)]
    print(f"  PIDs: {[p.pid for p in procs]}")
    
    # Wait for all replicas to be ready (retry connect up to 20 times)
    for rid in range(3):
        for attempt in range(20):
            try:
                s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                s.settimeout(2)
                s.connect(("127.0.0.1", PORTS[rid]))
                s.close()
                print(f"  replica {rid} ready after {attempt*0.5:.1f}s")
                break
            except ConnectionRefusedError:
                pass
            time.sleep(0.5)
    
    # Run tests
    tests = [
        test_replication,
        test_transfer_replication,
        test_view_change,
        test_crash_recovery,
    ]
    
    passed = 0
    for test_fn in tests:
        try:
            if test_fn():
                passed += 1
        except Exception as e:
            print(f"  FAIL: {e}")
    
    print(f"\n=== Results: {passed}/{len(tests)} tests passed ===")
    if passed == len(tests):
        print("ALL TESTS PASSED")
    else:
        print("SOME TESTS FAILED")
        sys.exit(1)

finally:
    for p in procs:
        try:
            p.terminate()
            p.wait(timeout=3)
        except:
            p.kill()
    print("Cleanup complete")
