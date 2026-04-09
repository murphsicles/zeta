//! Types for consciousness simulation

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Consciousness level
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum ConsciousnessLevel {
    /// No consciousness (automaton)
    None,
    /// Basic awareness (reactive)
    Reactive,
    /// Self-awareness (introspective)
    SelfAware,
    /// Ethical awareness (moral reasoning)
    Ethical,
    /// Transcendent awareness (beyond self)
    Transcendent,
}

impl ConsciousnessLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            ConsciousnessLevel::None => "none",
            ConsciousnessLevel::Reactive => "reactive",
            ConsciousnessLevel::SelfAware => "self_aware",
            ConsciousnessLevel::Ethical => "ethical",
            ConsciousnessLevel::Transcendent => "transcendent",
        }
    }
    
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "none" => Some(ConsciousnessLevel::None),
            "reactive" => Some(ConsciousnessLevel::Reactive),
            "self_aware" => Some(ConsciousnessLevel::SelfAware),
            "ethical" => Some(ConsciousnessLevel::Ethical),
            "transcendent" => Some(ConsciousnessLevel::Transcendent),
            _ => None,
        }
    }
}

/// Perception from environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Perception {
    /// Source of perception
    pub source: String,
    /// Content of perception
    pub content: HashMap<String, serde_json::Value>,
    /// Intensity (0-1)
    pub intensity: f64,
    /// Novelty (0-1)
    pub novelty: f64,
    /// Emotional valence (-1 to 1)
    pub valence: f64,
    /// Objects perceived
    pub objects: HashMap<String, ObjectProperties>,
    /// Affects self-identity
    pub affects_self_identity: bool,
    /// Identity implications
    pub identity_implications: HashMap<String, f64>,
    /// Happiness impact (-1 to 1)
    pub happiness_impact: f64,
    /// Harm potential (0-1)
    pub harm_potential: f64,
    /// Requires courage
    pub requires_courage: bool,
    /// Requires wisdom
    pub requires_wisdom: bool,
    /// Requires temperance
    pub requires_temperance: bool,
    /// Requires justice
    pub requires_justice: bool,
}

/// Object properties in perception
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectProperties {
    /// Type of object
    pub object_type: String,
    /// Position (if applicable)
    pub position: Option<(f64, f64, f64)>,
    /// Properties
    pub properties: HashMap<String, serde_json::Value>,
    /// Confidence in perception (0-1)
    pub confidence: f64,
}

/// Action that can be taken
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Action {
    /// No action
    Ignore,
    /// Observe without acting
    Observe,
    /// React to stimulus
    React,
    /// Process internally
    Process,
    /// Self-reflection
    SelfReflect,
    /// Ethical action
    EthicalAct,
    /// Transcendent action
    TranscendentAct,
    /// Neutral action
    Neutral,
    /// Contemplate
    Contemplate,
    /// Reflex action
    Reflex(Perception),
}

/// Goal for consciousness model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Goal {
    /// Goal name
    pub name: String,
    /// Priority (0-1)
    pub priority: f64,
    /// Progress (0-1)
    pub progress: f64,
    /// Creation time
    pub created_at: DateTime<Utc>,
}

impl Goal {
    pub fn new(name: &str, priority: f64) -> Self {
        Goal {
            name: name.to_string(),
            priority,
            progress: 0.0,
            created_at: Utc::now(),
        }
    }
}

/// Emotional state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmotionalState {
    /// Joy (0-1)
    pub joy: f64,
    /// Sadness (0-1)
    pub sadness: f64,
    /// Fear (0-1)
    pub fear: f64,
    /// Anger (0-1)
    pub anger: f64,
    /// Surprise (0-1)
    pub surprise: f64,
    /// Disgust (0-1)
    pub disgust: f64,
    /// Trust (0-1)
    pub trust: f64,
    /// Anticipation (0-1)
    pub anticipation: f64,
    /// Guilt (0-1)
    pub guilt: f64,
    /// Shame (0-1)
    pub shame: f64,
    /// Pride (0-1)
    pub pride: f64,
    /// Awe (0-1)
    pub awe: f64,
    /// Insignificance (0-1)
    pub insignificance: f64,
    /// Self-reflection (0-1)
    pub self_reflection: f64,
    /// Overall valence (-1 to 1)
    pub valence: f64,
    /// Overall arousal (0-1)
    pub arousal: f64,
}

impl EmotionalState {
    pub fn neutral() -> Self {
        EmotionalState {
            joy: 0.0,
            sadness: 0.0,
            fear: 0.0,
            anger: 0.0,
            surprise: 0.0,
            disgust: 0.0,
            trust: 0.5,
            anticipation: 0.0,
            guilt: 0.0,
            shame: 0.0,
            pride: 0.0,
            awe: 0.0,
            insignificance: 0.0,
            self_reflection: 0.0,
            valence: 0.0,
            arousal: 0.0,
        }
    }
    
    pub fn update(&mut self, perception: &Perception) {
        // Update emotions based on perception
        self.valence = perception.valence;
        self.arousal = perception.intensity;
        
        // Basic emotion updates
        if perception.valence > 0.5 {
            self.joy = (self.joy * 0.8 + perception.valence * 0.2).clamp(0.0, 1.0);
        } else if perception.valence < -0.5 {
            self.sadness = (self.sadness * 0.8 + (-perception.valence) * 0.2).clamp(0.0, 1.0);
        }
        
        if perception.harm_potential > 0.7 {
            self.fear = (self.fear * 0.8 + perception.harm_potential * 0.2).clamp(0.0, 1.0);
        }
        
        if perception.novelty > 0.7 {
            self.surprise = (self.surprise * 0.8 + perception.novelty * 0.2).clamp(0.0, 1.0);
        }
    }
    
    pub fn dominant_emotion(&self) -> &'static str {
        let emotions = [
            ("joy", self.joy),
            ("sadness", self.sadness),
            ("fear", self.fear),
            ("anger", self.anger),
            ("surprise", self.surprise),
            ("disgust", self.disgust),
            ("trust", self.trust),
            ("anticipation", self.anticipation),
            ("guilt", self.guilt),
            ("shame", self.shame),
            ("pride", self.pride),
            ("awe", self.awe),
            ("insignificance", self.insignificance),
            ("self_reflection", self.self_reflection),
        ];
        
        emotions.iter()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(name, _)| *name)
            .unwrap_or("neutral")
    }
}

/// Reflection on consciousness state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reflection {
    /// When reflection occurred
    pub timestamp: DateTime<Utc>,
    /// Which model reflected
    pub model_id: String,
    /// Self-insights gained
    pub self_insights: Vec<String>,
    /// Ethical considerations
    pub ethical_considerations: Vec<String>,
    /// Emotional state at reflection
    pub emotional_state: EmotionalState,
    /// Stability at reflection (0-1)
    pub stability: f64,
}

/// Semantic memory
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SemanticMemory {
    /// Concept
    pub concept: String,
    /// Properties
    pub properties: HashMap<String, serde_json::Value>,
    /// Relationships to other concepts
    pub relationships: HashMap<String, String>,
}

/// Procedural memory
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProceduralMemory {
    /// Skill name
    pub skill: String,
    /// Proficiency (0-1)
    pub proficiency: f64,
    /// Last practiced
    pub last_practiced: DateTime<Utc>,
}

/// Memory chunk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryChunk {
    /// Content
    pub content: serde_json::Value,
    /// Importance (0-1)
    pub importance: f64,
    /// Last accessed
    pub last_accessed: DateTime<Utc>,
}

/// Memory access
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryAccess {
    /// Memory identifier
    pub memory_id: String,
    /// Memory type
    pub memory_type: String,
    /// Accessibility (0-1)
    pub accessibility: f64,
}

/// Experience for learning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Experience {
    /// What was experienced
    pub description: String,
    /// Learning value (0-1)
    pub learning_value: f64,
    /// Emotional impact (-1 to 1)
    pub emotional_impact: f64,
    /// Ethical dimension
    pub ethical_dimension: Option<String>,
    /// Capability learned
    pub capability_learned: Option<String>,
    /// Law discovered
    pub law_discovered: Option<String>,
    /// Norm learned
    pub norm_learned: Option<String>,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}