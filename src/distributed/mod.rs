//! Distributed systems native support for Zeta
//!
//! Built-in distribution, not bolted-on. Provides actor model, CRDTs,
//! distributed transactions, and cloud-native architecture.

pub mod actor;
pub mod cluster;
pub mod crdt;
pub mod transaction;
pub mod transport;

/// Re-exports for convenient access
pub use actor::DistributedActorSystem;
pub use cluster::{
    ClusterConfig, ClusterManager, LoadBalancer, NodeId, NodeInfo, ServiceDiscovery,
};
pub use crdt::{GCounter, GSet, LWWRegister, ORSet, PNCounter, SharedCRDT, TwoPSet, VectorClock};
pub use transaction::DistributedTransactionManager;
pub use transport::NetworkTransport;

/// Initialize distributed runtime
pub fn init() -> Result<(), String> {
    // Initialize cluster discovery
    cluster::discovery::init()?;

    // Initialize network transport
    transport::init()?;

    // Initialize distributed actor system
    actor::init()?;

    Ok(())
}

/// Shutdown distributed runtime
pub fn shutdown() -> Result<(), String> {
    actor::shutdown()?;
    transport::shutdown()?;
    cluster::discovery::shutdown()?;
    Ok(())
}
