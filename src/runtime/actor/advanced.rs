// src/runtime/actor/advanced.rs
// Advanced actor system features for v0.3.42

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use tokio::sync::mpsc;
use tokio::task;

/// Message type for actor communication
#[derive(Debug, Clone)]
pub enum ActorMessage {
    Data(Vec<u8>),
    Text(String),
    Number(i64),
    Shutdown,
    Error(String),
}

/// Actor state
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ActorState {
    Created,
    Running,
    Paused,
    Stopped,
    Error,
}

/// Actor trait for user-defined actors
pub trait Actor: Send + Sync + 'static {
    fn on_start(&mut self, context: ActorContext);
    fn on_message(&mut self, context: ActorContext, message: ActorMessage);
    fn on_stop(&mut self, context: ActorContext);
    fn on_error(&mut self, context: ActorContext, error: String);
}

/// Actor context for message sending and actor management
#[derive(Clone)]
pub struct ActorContext {
    id: ActorId,
    system: Arc<ActorSystem>,
    sender: mpsc::UnboundedSender<ActorMessage>,
}

impl ActorContext {
    /// Send a message to another actor
    pub fn send(&self, target: ActorId, message: ActorMessage) -> Result<(), String> {
        self.system.send(self.id, target, message)
    }
    
    /// Spawn a child actor
    pub fn spawn<A: Actor>(&self, actor: A) -> Result<ActorId, String> {
        self.system.spawn_child(self.id, actor)
    }
    
    /// Get actor ID
    pub fn id(&self) -> ActorId {
        self.id
    }
    
    /// Stop this actor
    pub fn stop(&self) {
        self.system.stop_actor(self.id);
    }
}

/// Actor identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ActorId(u64);

impl ActorId {
    fn new() -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(1);
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Actor handle for external control
#[derive(Clone)]
pub struct ActorHandle {
    id: ActorId,
    system: Arc<ActorSystem>,
    sender: mpsc::UnboundedSender<ActorMessage>,
}

impl ActorHandle {
    /// Send a message to the actor
    pub fn send(&self, message: ActorMessage) -> Result<(), String> {
        self.system.send(ActorId(0), self.id, message)
    }
    
    /// Stop the actor
    pub fn stop(&self) {
        self.system.stop_actor(self.id);
    }
    
    /// Get actor ID
    pub fn id(&self) -> ActorId {
        self.id
    }
}

/// Actor system for managing actors
pub struct ActorSystem {
    actors: RwLock<HashMap<ActorId, ActorHandle>>,
    children: RwLock<HashMap<ActorId, Vec<ActorId>>>,
    states: RwLock<HashMap<ActorId, ActorState>>,
    running: AtomicBool,
}

impl ActorSystem {
    /// Create a new actor system
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            actors: RwLock::new(HashMap::new()),
            children: RwLock::new(HashMap::new()),
            states: RwLock::new(HashMap::new()),
            running: AtomicBool::new(true),
        })
    }
    
    /// Spawn a new actor
    pub fn spawn<A: Actor>(self: &Arc<Self>, actor: A) -> Result<ActorHandle, String> {
        let id = ActorId::new();
        let (sender, mut receiver) = mpsc::unbounded_channel();
        
        let system = Arc::clone(self);
        let context = ActorContext {
            id,
            system: Arc::clone(&system),
            sender: sender.clone(),
        };
        
        // Store actor handle
        let handle = ActorHandle {
            id,
            system: Arc::clone(&system),
            sender: sender.clone(),
        };
        
        {
            let mut actors = self.actors.write().unwrap();
            actors.insert(id, handle.clone());
            
            let mut states = self.states.write().unwrap();
            states.insert(id, ActorState::Created);
        }
        
        // Spawn actor task
        let system_clone = Arc::clone(&system);
        task::spawn(async move {
            // Create mutable actor
            let mut actor_instance = actor;
            
            // Call on_start
            actor_instance.on_start(context.clone());
            
            {
                let mut states = system_clone.states.write().unwrap();
                states.insert(id, ActorState::Running);
            }
            
            // Process messages
            while let Some(message) = receiver.recv().await {
                if !system_clone.running.load(Ordering::Relaxed) {
                    break;
                }
                
                match message {
                    ActorMessage::Shutdown => {
                        actor_instance.on_stop(context.clone());
                        break;
                    }
                    ActorMessage::Error(err) => {
                        actor_instance.on_error(context.clone(), err);
                    }
                    _ => {
                        actor_instance.on_message(context.clone(), message);
                    }
                }
            }
            
            // Update state
            let mut states = system_clone.states.write().unwrap();
            states.insert(id, ActorState::Stopped);
        });
        
        Ok(handle)
    }
    
    /// Spawn a child actor
    pub fn spawn_child<A: Actor>(self: &Arc<Self>, parent: ActorId, actor: A) -> Result<ActorId, String> {
        let id = ActorId::new();
        let (sender, mut receiver) = mpsc::unbounded_channel();
        
        let system = Arc::clone(self);
        let context = ActorContext {
            id,
            system: Arc::clone(&system),
            sender: sender.clone(),
        };
        
        // Store actor handle
        let handle = ActorHandle {
            id,
            system: Arc::clone(&system),
            sender: sender.clone(),
        };
        
        {
            let mut actors = self.actors.write().unwrap();
            actors.insert(id, handle.clone());
            
            let mut states = self.states.write().unwrap();
            states.insert(id, ActorState::Created);
        }
        
        // Record parent-child relationship
        let mut children = self.children.write().unwrap();
        children.entry(parent).or_insert_with(Vec::new).push(id);
        
        // Spawn actor task
        let system_clone = Arc::clone(&system);
        task::spawn(async move {
            // Create mutable actor
            let mut actor_instance = actor;
            
            // Call on_start
            actor_instance.on_start(context.clone());
            
            {
                let mut states = system_clone.states.write().unwrap();
                states.insert(id, ActorState::Running);
            }
            
            // Process messages
            while let Some(message) = receiver.recv().await {
                if !system_clone.running.load(Ordering::Relaxed) {
                    break;
                }
                
                match message {
                    ActorMessage::Shutdown => {
                        actor_instance.on_stop(context.clone());
                        break;
                    }
                    ActorMessage::Error(err) => {
                        actor_instance.on_error(context.clone(), err);
                    }
                    _ => {
                        actor_instance.on_message(context.clone(), message);
                    }
                }
            }
            
            // Update state
            let mut states = system_clone.states.write().unwrap();
            states.insert(id, ActorState::Stopped);
        });
        
        Ok(id)
    }
    
    /// Send a message between actors
    pub fn send(&self, from: ActorId, to: ActorId, message: ActorMessage) -> Result<(), String> {
        let actors = self.actors.read().unwrap();
        if let Some(handle) = actors.get(&to) {
            handle.sender.send(message)
                .map_err(|e| format!("Failed to send message: {}", e))?;
            Ok(())
        } else {
            Err(format!("Actor {} not found", to.0))
        }
    }
    
    /// Stop an actor
    pub fn stop_actor(&self, id: ActorId) {
        let actors = self.actors.read().unwrap();
        if let Some(handle) = actors.get(&id) {
            let _ = handle.sender.send(ActorMessage::Shutdown);
        }
    }
    
    /// Stop all actors
    pub fn stop_all(&self) {
        self.running.store(false, Ordering::Relaxed);
        
        let actors: Vec<ActorId> = {
            let actors = self.actors.read().unwrap();
            actors.keys().cloned().collect()
        };
        
        for actor_id in actors {
            self.stop_actor(actor_id);
        }
    }
    
    /// Get actor state
    pub fn get_state(&self, id: ActorId) -> Option<ActorState> {
        let states = self.states.read().unwrap();
        states.get(&id).copied()
    }
    
    /// Get children of an actor
    pub fn get_children(&self, id: ActorId) -> Vec<ActorId> {
        let children = self.children.read().unwrap();
        children.get(&id).cloned().unwrap_or_default()
    }
}

/// Supervisor strategy for handling actor failures
pub enum SupervisorStrategy {
    /// Restart the actor
    Restart,
    /// Stop the actor
    Stop,
    /// Escalate to parent supervisor
    Escalate,
    /// Resume the actor (ignore error)
    Resume,
}

/// Supervisor trait for managing actor lifecycles
pub trait Supervisor: Actor {
    fn supervisor_strategy(&self, child: ActorId, error: String) -> SupervisorStrategy;
}

/// Simple actor implementation for testing
pub struct EchoActor {
    received: usize,
}

impl EchoActor {
    pub fn new() -> Self {
        Self { received: 0 }
    }
}

impl Actor for EchoActor {
    fn on_start(&mut self, context: ActorContext) {
        println!("EchoActor {} started", context.id().0);
    }
    
    fn on_message(&mut self, context: ActorContext, message: ActorMessage) {
        self.received += 1;
        println!("EchoActor {} received message {}: {:?}", 
                 context.id().0, self.received, message);
        
        // Echo back if it's a text message
        if let ActorMessage::Text(text) = message {
            let echo_message = ActorMessage::Text(format!("Echo: {}", text));
            let _ = context.send(context.id(), echo_message);
        }
    }
    
    fn on_stop(&mut self, context: ActorContext) {
        println!("EchoActor {} stopped after {} messages", 
                 context.id().0, self.received);
    }
    
    fn on_error(&mut self, context: ActorContext, error: String) {
        println!("EchoActor {} error: {}", context.id().0, error);
    }
}

/// Worker actor for processing tasks
pub struct WorkerActor {
    processed: usize,
}

impl WorkerActor {
    pub fn new() -> Self {
        Self { processed: 0 }
    }
}

impl Actor for WorkerActor {
    fn on_start(&mut self, context: ActorContext) {
        println!("WorkerActor {} started", context.id().0);
    }
    
    fn on_message(&mut self, context: ActorContext, message: ActorMessage) {
        self.processed += 1;
        
        match message {
            ActorMessage::Number(n) => {
                println!("WorkerActor {} processing number: {}", context.id().0, n);
                // Simulate work
                let result = n * 2;
                let _ = context.send(context.id(), ActorMessage::Number(result));
            }
            ActorMessage::Text(text) => {
                println!("WorkerActor {} processing text: {}", context.id().0, text);
                let result = text.to_uppercase();
                let _ = context.send(context.id(), ActorMessage::Text(result));
            }
            _ => {
                println!("WorkerActor {} received unsupported message type", context.id().0);
            }
        }
    }
    
    fn on_stop(&mut self, context: ActorContext) {
        println!("WorkerActor {} stopped after processing {} tasks", 
                 context.id().0, self.processed);
    }
    
    fn on_error(&mut self, context: ActorContext, error: String) {
        println!("WorkerActor {} error: {}", context.id().0, error);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::time::{sleep, Duration};
    
    #[tokio::test]
    async fn test_actor_system() {
        let system = ActorSystem::new();
        
        // Spawn echo actor
        let echo_actor = EchoActor::new();
        let echo_handle = system.spawn(echo_actor).unwrap();
        
        // Spawn worker actor
        let worker_actor = WorkerActor::new();
        let worker_handle = system.spawn(worker_actor).unwrap();
        
        // Send messages
        echo_handle.send(ActorMessage::Text("Hello".to_string())).unwrap();
        worker_handle.send(ActorMessage::Number(42)).unwrap();
        worker_handle.send(ActorMessage::Text("test".to_string())).unwrap();
        
        // Wait for processing
        sleep(Duration::from_millis(100)).await;
        
        // Stop actors
        echo_handle.stop();
        worker_handle.stop();
        
        // Wait for shutdown
        sleep(Duration::from_millis(50)).await;
        
        system.stop_all();
        
        println!("Actor system test completed");
    }
}