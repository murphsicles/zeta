// src/xai.rs
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize)]
struct ChatRequest {
    model: String,
    messages: Vec<Message>,
}

#[derive(Deserialize, Serialize, Debug)]
struct Message {
    role: String,
    content: String,
}

#[derive(Deserialize, Debug)]
struct ChatResponse {
    choices: Vec<Choice>,
}

#[derive(Deserialize, Debug)]
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
        let api_key = std::env::var("XAI_API_KEY")?;
        let base_url = "https://api.x.ai/v1".to_string();
        let client = Client::new();
        Ok(Self {
            client,
            api_key,
            base_url,
        })
    }

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
}
