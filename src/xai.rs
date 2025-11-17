// src/xai.rs: xAI API integration for Zeta
// Queries Grok API for AI-opt suggestions (e.g., MLGO hints, codegen transforms).
// Auth: API key from env (ZETA_XAI_API_KEY). Quotas: SuperGrok/Premium+ for Grok-4.

use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Serialize)]
struct GrokRequest {
    model: String, // "grok-4" or "grok-3"
    messages: Vec<Message>,
    stream: bool,
    temperature: f32,
}

#[derive(Serialize)]
struct Message {
    role: String,
    content: String,
}

#[derive(Deserialize)]
struct GrokResponse {
    choices: Vec<Choice>,
}

#[derive(Deserialize)]
struct Choice {
    message: Message,
}

pub struct XAIClient {
    client: Client,
    api_key: String,
    base_url: String,
}

impl XAIClient {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let api_key = env::var("ZETA_XAI_API_KEY")?;
        Ok(Self {
            client: Client::new(),
            api_key,
            base_url: "https://api.x.ai/v1".to_string(),
        })
    }

    pub fn query_opt(
        &self,
        prompt: &str,
        model: &str,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let req = GrokRequest {
            model: model.to_string(),
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            stream: false,
            temperature: 0.1,
        };

        let res = self
            .client
            .post(&format!("{}/chat/completions", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .json(&req)
            .send()?
            .json::<GrokResponse>()?;

        Ok(res.choices[0].message.content.clone())
    }

    pub fn suggest_mlgo(&self, mir_str: &str) -> Result<String, Box<dyn std::error::Error>> {
        let prompt = format!("Suggest MLGO metadata for this MIR: {}", mir_str);
        self.query_opt(&prompt, "grok-4")
    }

    pub fn optimize_codegen(&self, ast_str: &str) -> Result<String, Box<dyn std::error::Error>> {
        let prompt = format!("Optimize Zeta AST for perf: {}", ast_str);
        self.query_opt(&prompt, "grok-3")
    }
}

pub fn check_quota(model: &str) -> bool {
    // Stub: Query usage (SuperGrok for grok-4)
    if model == "grok-4" {
        // Assume Premium+ check
        true
    } else {
        true
    }
}
