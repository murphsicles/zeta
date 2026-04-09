//! Consensus algorithm tests for Zeta v0.3.47
//! Tests Raft, Paxos, and Byzantine Fault Tolerance implementations

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

/// Raft consensus node state
#[derive(Debug, Clone, PartialEq)]
enum RaftState {
    Follower,
    Candidate,
    Leader,
}

/// Raft log entry
#[derive(Debug, Clone)]
struct LogEntry {
    term: u64,
    index: u64,
    command: Vec<u8>,
}

/// Raft node implementation
struct RaftNode {
    id: u64,
    current_term: u64,
    voted_for: Option<u64>,
    log: Vec<LogEntry>,
    state: RaftState,
    commit_index: u64,
    last_applied: u64,
    next_index: HashMap<u64, u64>,
    match_index: HashMap<u64, u64>,
}

impl RaftNode {
    fn new(id: u64) -> Self {
        Self {
            id,
            current_term: 0,
            voted_for: None,
            log: Vec::new(),
            state: RaftState::Follower,
            commit_index: 0,
            last_applied: 0,
            next_index: HashMap::new(),
            match_index: HashMap::new(),
        }
    }

    fn become_candidate(&mut self) {
        self.state = RaftState::Candidate;
        self.current_term += 1;
        self.voted_for = Some(self.id);
    }

    fn become_leader(&mut self) {
        self.state = RaftState::Leader;
        // Initialize next_index and match_index for all followers
    }

    fn become_follower(&mut self, term: u64) {
        self.state = RaftState::Follower;
        self.current_term = term;
        self.voted_for = None;
    }

    fn append_entries(&mut self, term: u64, leader_id: u64, prev_log_index: u64, 
                     prev_log_term: u64, entries: Vec<LogEntry>, leader_commit: u64) -> bool {
        if term < self.current_term {
            return false;
        }
        
        if self.log.len() as u64 <= prev_log_index || 
           (prev_log_index > 0 && self.log[prev_log_index as usize - 1].term != prev_log_term) {
            return false;
        }

        // Append new entries
        let mut i = prev_log_index as usize;
        for entry in entries {
            if i < self.log.len() && self.log[i].term != entry.term {
                self.log.truncate(i);
            }
            if i >= self.log.len() {
                self.log.push(entry.clone());
            }
            i += 1;
        }

        if leader_commit > self.commit_index {
            self.commit_index = std::cmp::min(leader_commit, self.log.len() as u64);
        }

        true
    }

    fn request_vote(&mut self, term: u64, candidate_id: u64, last_log_index: u64, last_log_term: u64) -> bool {
        if term < self.current_term {
            return false;
        }

        let last_log = self.log.last();
        let my_last_log_term = last_log.map(|e| e.term).unwrap_or(0);
        let my_last_log_index = self.log.len() as u64;

        let log_ok = last_log_term > my_last_log_term || 
                    (last_log_term == my_last_log_term && last_log_index >= my_last_log_index);

        if term > self.current_term {
            self.become_follower(term);
        }

        if self.voted_for.is_none() || self.voted_for == Some(candidate_id) {
            if log_ok {
                self.voted_for = Some(candidate_id);
                return true;
            }
        }

        false
    }
}

/// Paxos proposer
struct PaxosProposer {
    id: u64,
    proposal_number: u64,
    accepted_value: Option<Vec<u8>>,
}

/// Paxos acceptor
struct PaxosAcceptor {
    id: u64,
    promised_proposal: u64,
    accepted_proposal: u64,
    accepted_value: Option<Vec<u8>>,
}

/// Byzantine Fault Tolerance node
struct BFTNode {
    id: u64,
    is_byzantine: bool,
    view: u64,
    sequence_number: u64,
    prepared: bool,
    committed: bool,
}

impl BFTNode {
    fn new(id: u64, is_byzantine: bool) -> Self {
        Self {
            id,
            is_byzantine,
            view: 0,
            sequence_number: 0,
            prepared: false,
            committed: false,
        }
    }

    fn pre_prepare(&mut self, view: u64, sequence: u64, request: &[u8]) -> bool {
        if self.is_byzantine {
            // Byzantine node may behave arbitrarily
            return rand::random();
        }

        if view == self.view && sequence == self.sequence_number + 1 {
            self.sequence_number = sequence;
            true
        } else {
            false
        }
    }

    fn prepare(&mut self, view: u64, sequence: u64, request_digest: &[u8]) -> bool {
        if self.is_byzantine {
            return rand::random();
        }

        if view == self.view && sequence == self.sequence_number {
            self.prepared = true;
            true
        } else {
            false
        }
    }

    fn commit(&mut self, view: u64, sequence: u64, request_digest: &[u8]) -> bool {
        if self.is_byzantine {
            return rand::random();
        }

        if view == self.view && sequence == self.sequence_number && self.prepared {
            self.committed = true;
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::thread;

    #[test]
    fn test_raft_leader_election() {
        let mut nodes: Vec<RaftNode> = (0..5).map(RaftNode::new).collect();
        
        // Node 0 becomes candidate
        nodes[0].become_candidate();
        assert_eq!(nodes[0].state, RaftState::Candidate);
        assert_eq!(nodes[0].current_term, 1);
        assert_eq!(nodes[0].voted_for, Some(0));

        // Node 0 requests votes
        let mut votes = 1; // votes for itself
        for i in 1..5 {
            if nodes[i].request_vote(1, 0, 0, 0) {
                votes += 1;
            }
        }

        // Should get majority (3/5)
        assert!(votes >= 3);
        
        // Node 0 becomes leader
        nodes[0].become_leader();
        assert_eq!(nodes[0].state, RaftState::Leader);
    }

    #[test]
    fn test_raft_log_replication() {
        let mut leader = RaftNode::new(0);
        leader.become_leader();
        
        let mut follower = RaftNode::new(1);
        
        // Leader appends entry
        let entry = LogEntry {
            term: 1,
            index: 1,
            command: b"SET key value".to_vec(),
        };
        
        // Replicate to follower
        let success = follower.append_entries(
            1, 0, 0, 0, vec![entry.clone()], 0
        );
        
        assert!(success);
        assert_eq!(follower.log.len(), 1);
        assert_eq!(follower.log[0].command, b"SET key value");
    }

    #[test]
    fn test_paxos_consensus() {
        let proposer = PaxosProposer {
            id: 0,
            proposal_number: 1,
            accepted_value: Some(b"consensus value".to_vec()),
        };
        
        let mut acceptors = vec![
            PaxosAcceptor {
                id: 0,
                promised_proposal: 0,
                accepted_proposal: 0,
                accepted_value: None,
            },
            PaxosAcceptor {
                id: 1,
                promised_proposal: 0,
                accepted_proposal: 0,
                accepted_value: None,
            },
            PaxosAcceptor {
                id: 2,
                promised_proposal: 0,
                accepted_proposal: 0,
                accepted_value: None,
            },
        ];
        
        // Simulate Phase 1: Prepare
        let mut promises = 0;
        for acceptor in &mut acceptors {
            if acceptor.promised_proposal < proposer.proposal_number {
                acceptor.promised_proposal = proposer.proposal_number;
                promises += 1;
            }
        }
        
        // Need majority
        assert!(promises >= 2);
        
        // Simulate Phase 2: Accept
        let mut accepts = 0;
        for acceptor in &mut acceptors {
            if acceptor.promised_proposal == proposer.proposal_number {
                acceptor.accepted_proposal = proposer.proposal_number;
                acceptor.accepted_value = proposer.accepted_value.clone();
                accepts += 1;
            }
        }
        
        // Should reach consensus
        assert!(accepts >= 2);
        for acceptor in &acceptors {
            if acceptor.accepted_proposal == proposer.proposal_number {
                assert_eq!(acceptor.accepted_value, proposer.accepted_value);
            }
        }
    }

    #[test]
    fn test_byzantine_fault_tolerance() {
        // Create 4 nodes, 1 Byzantine
        let mut nodes: Vec<BFTNode> = vec![
            BFTNode::new(0, false),
            BFTNode::new(1, false),
            BFTNode::new(2, false),
            BFTNode::new(3, true), // Byzantine node
        ];
        
        let request = b"important operation";
        let request_digest = b"digest";
        
        // Pre-prepare phase
        let mut pre_prepare_ok = 0;
        for node in &mut nodes {
            if node.pre_prepare(0, 1, request) {
                pre_prepare_ok += 1;
            }
        }
        
        // With 1 Byzantine, we should still get 3/4 honest responses
        assert!(pre_prepare_ok >= 3);
        
        // Prepare phase
        let mut prepare_ok = 0;
        for node in &mut nodes {
            if node.prepare(0, 1, request_digest) {
                prepare_ok += 1;
            }
        }
        
        // Need 2f+1 prepare messages (f=1, so need 3)
        assert!(prepare_ok >= 3);
        
        // Commit phase
        let mut commit_ok = 0;
        for node in &mut nodes {
            if node.commit(0, 1, request_digest) {
                commit_ok += 1;
            }
        }
        
        // Need 2f+1 commit messages
        assert!(commit_ok >= 3);
    }

    #[test]
    fn test_consensus_with_network_partitions() {
        // Simulate network partition where 2 nodes are isolated
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();
        
        let handle = thread::spawn(move || {
            while running_clone.load(Ordering::Relaxed) {
                thread::sleep(Duration::from_millis(10));
            }
        });
        
        // Create nodes
        let mut nodes: Vec<RaftNode> = (0..5).map(RaftNode::new).collect();
        
        // Partition: nodes 0-2 can communicate, nodes 3-4 are isolated
        nodes[0].become_candidate();
        
        // Only nodes 1 and 2 can vote (nodes 3-4 are partitioned)
        let mut votes = 1; // votes for itself
        for i in 1..3 {
            if nodes[i].request_vote(1, 0, 0, 0) {
                votes += 1;
            }
        }
        
        // Should still get majority (3/3 in partition)
        assert_eq!(votes, 3);
        
        running.store(false, Ordering::Relaxed);
        handle.join().unwrap();
    }

    #[test]
    fn test_log_compaction_and_snapshotting() {
        let mut node = RaftNode::new(0);
        
        // Add many log entries
        for i in 1..=100 {
            node.log.push(LogEntry {
                term: 1,
                index: i,
                command: format!("entry {}", i).into_bytes(),
            });
        }
        
        node.commit_index = 50;
        node.last_applied = 50;
        
        // Simulate snapshot at index 50
        let snapshot_index = 50;
        let snapshot_term = node.log[snapshot_index as usize - 1].term;
        
        // Compact log
        if snapshot_index > 0 {
            node.log.drain(0..snapshot_index as usize);
            // Adjust indices
            for entry in &mut node.log {
                entry.index -= snapshot_index;
            }
            node.commit_index -= snapshot_index;
            node.last_applied -= snapshot_index;
        }
        
        assert_eq!(node.log.len(), 50);
        assert_eq!(node.log[0].index, 1); // After compaction, indices restart from 1
        assert_eq!(node.commit_index, 0); // Commit index adjusted
    }
}