//! XAI API client for Zeta AI integration.
//! Provides blocking HTTP client for Grok-beta chat completions.
//! Used for MLGO hooks, CTFE eval, and dynamic specialization queries.
//! Requires XAI_API_KEY env var.
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use serde_json;

/// Request struct for chat completions API.
#[derive(Serialize)]
struct ChatRequest {
    /// Model name (e.g., "grok-beta").
    model: String,
    /// Array of message objects.
    messages: Vec<Message>,
}

/// Message struct for chat role/content.
#[derive(Deserialize, Serialize, Debug)]
struct Message {
    /// Role: "user", "system", etc.
    role: String,
    /// Message text.
    content: String,
}

/// Response wrapper for chat completions.
#[derive(Deserialize, Debug)]
struct ChatResponse {
    /// Array of choice objects.
    choices: Vec<Choice>,
}

/// Individual choice from response.
#[derive(Deserialize, Debug)]
struct Choice {
    /// Message in choice.
    message: Message,
}

/// Thread-safe blocking client for XAI API.
pub struct XAIClient {
    /// HTTP client instance.
    client: Client,
    /// API key from env.
    api_key: String,
    /// Base API URL.
    base_url: String,
}

impl XAIClient {
    /// Creates a new client, loading API key from XAI_API_KEY env.
    /// Returns error if key missing.
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let api_key = std::env::var("XAI_API_KEY")?;
        let base_url = "https://api.x.ai/v1".to_string();
        let client = Client::new();
        Ok(Self {
            client,
            api_key,
            base_url,
        })
    }

    /// Queries Grok-beta with prompt, returns response content.
    /// Handles JSON serialization/deserialization and auth.
    pub fn query(&self, prompt: &str) -> Result<String, Box<dyn std::error::Error>> {
        let request = ChatRequest {
            model: "grok-beta".to_string(),
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
        };
        let req_body = serde_json::to_string(&request)?;
        let res = self
            .client
            .post(&format!("{}/chat/completions", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .json(&req_body)
            .send()?;
        let response: ChatResponse = res.json()?;
        Ok(response.choices[0].message.content.clone())
    }

    /// MLGO hook: Queries Grok for LLVM pass optimization recommendations.
    /// Prompt includes MIR stats; returns JSON pass order/params.
    pub fn mlgo_optimize(&self, mir_stats: &str) -> Result<String, Box<dyn std::error::Error>> {
        let prompt = format!(
            "Given LLVM MIR stats: {}. Recommend optimized pass pipeline for vectorization/branch-pred/MLGO. Output JSON: {{ \"passes\": [\"pass1\", \"pass2\"], \"params\": {{ \"vec-threshold\": 128 }} }}",
            mir_stats
        );
        self.query(&prompt)
    }
}
