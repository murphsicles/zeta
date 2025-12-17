// src/xai.rs
//! Client for xAI API integration.
//! Supports blocking requests to Grok-beta for chat completions.
//! Requires XAI_API_KEY environment variable.
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use serde_json;
/// Request structure for chat completions.
#[derive(Serialize)]
struct ChatRequest {
    /// Model identifier.
    model: String,
    /// Sequence of messages.
    messages: Vec<Message>,
}
/// Message structure with role and content.
#[derive(Deserialize, Serialize, Debug)]
struct Message {
    /// Message role.
    role: String,
    /// Message content.
    content: String,
}
/// Response structure for chat completions.
#[derive(Deserialize, Debug)]
struct ChatResponse {
    /// List of response choices.
    choices: Vec<Choice>,
}
/// Single choice in response.
#[derive(Deserialize, Debug)]
struct Choice {
    /// Message within choice.
    message: Message,
}
/// Blocking client for xAI API.
pub struct XAIClient {
    /// HTTP client.
    client: Client,
    /// API key.
    api_key: String,
    /// Base URL.
    base_url: String,
}
impl XAIClient {
    /// Creates client using XAI_API_KEY from environment.
    /// Returns error if key absent.
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
    /// Sends prompt to Grok-beta and returns response content.
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
            .post(format!("{}/chat/completions", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .json(&req_body)
            .send()?;
        let response: ChatResponse = res.json()?;
        Ok(response.choices[0].message.content.clone())
    }
    /// Requests LLVM pass recommendations based on MIR statistics.
    /// Returns JSON with passes and parameters.
    pub fn mlgo_optimize(&self, mir_stats: &str) -> Result<String, Box<dyn std::error::Error>> {
        let prompt = format!(
            "Given LLVM MIR stats: {}. Recommend optimized pass pipeline for vectorization/branch-pred/MLGO. Output JSON: {{ \"passes\": [\"pass1\", \"pass2\"], \"params\": {{ \"vec-threshold\": 128 }} }}",
            mir_stats
        );
        self.query(&prompt)
    }
}
