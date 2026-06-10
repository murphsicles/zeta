// src/frontend/cfg.rs
//! `#[cfg(...)]` predicate evaluator.
//!
//! Supports:
//! - `feature = "name"` → enabled when `name` is in the feature set
//! - `any(pred, pred, ...)` → true if any predicate is true
//! - `all(pred, pred, ...)` → true if all predicates are true
//! - `not(pred)` → negates a predicate
//! - Bare `test` / `debug_assertions` → always false (not used by zetac)
//!
//! Feature set is configured via `--features` CLI flag.

use std::sync::OnceLock;

static FEATURES: OnceLock<Vec<String>> = OnceLock::new();

/// Set the enabled feature flags from a comma-separated string.
/// Called once at compiler startup from `--features` CLI arg.
pub fn init_features(input: &str) {
    let list: Vec<String> = input
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();
    let _ = FEATURES.set(list);
}

/// Get the current feature set.
fn get_features() -> &'static Vec<String> {
    FEATURES.get().unwrap_or(&EMPTY)
}

static EMPTY: Vec<String> = Vec::new();

/// Check whether a `#[cfg(...)]` attribute should be included.
///
/// Returns:
/// - `Some(true)` if the attr is a `cfg(...)` that evaluates to true
/// - `Some(false)` if the attr is a `cfg(...)` that evaluates to false
/// - `None` if the attr is not a cfg attribute at all
pub fn check_cfg_attr(attr: &str) -> Option<bool> {
    let trimmed = attr.trim();

    // Must be exactly `cfg(...)` or start with `cfg(`
    if !trimmed.starts_with("cfg(") {
        return None;
    }

    // Extract the content inside cfg(...)
    let inner = trimmed.strip_prefix("cfg(")?;
    if !inner.ends_with(')') {
        return None;
    }
    let predicate = &inner[..inner.len() - 1].trim();

    Some(eval_predicate(predicate, get_features()))
}

/// Evaluate a cfg predicate expression.
fn eval_predicate(pred: &str, features: &[String]) -> bool {
    let pred = pred.trim();

    // Handle parenthesized groups
    if pred.starts_with('(') && pred.ends_with(')') {
        return eval_predicate(&pred[1..pred.len() - 1], features);
    }

    // `any(p1, p2, ...)`
    if pred.starts_with("any(") && pred.ends_with(')') {
        let inner = &pred[4..pred.len() - 1];
        return split_predicates(inner)
            .iter()
            .any(|p| eval_predicate(p, features));
    }

    // `all(p1, p2, ...)`
    if pred.starts_with("all(") && pred.ends_with(')') {
        let inner = &pred[3..pred.len() - 1];
        return split_predicates(inner)
            .iter()
            .all(|p| eval_predicate(p, features));
    }

    // `not(p)`
    if pred.starts_with("not(") && pred.ends_with(')') {
        let inner = &pred[4..pred.len() - 1];
        return !eval_predicate(inner, features);
    }

    // `feature = "name"`
    if let Some(eq_pos) = pred.find('=') {
        let key = pred[..eq_pos].trim();
        let val = pred[eq_pos + 1..].trim().trim_matches('"');
        if key == "feature" {
            return features.iter().any(|f| f == val);
        }
        // `target_os = "linux"` etc. — check against build target
        if key == "target_os" {
            return val == std::env::consts::OS;
        }
        if key == "target_arch" {
            return val == std::env::consts::ARCH;
        }
        if key == "target_family" {
            return val == std::env::consts::FAMILY;
        }
        // Unknown key → not matched
        return false;
    }

    // Bare identifiers: `test`, `debug_assertions`, etc.
    // These aren't relevant to zetac builds — always false.
    false
}

/// Split cfg predicate arguments at top-level commas (respecting nested parens).
fn split_predicates(input: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0;
    let mut start = 0;
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                result.push(input[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    let last = input[start..].trim();
    if !last.is_empty() {
        result.push(last);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_true() {
        let features = vec!["xxh64".to_string(), "xxh32".to_string()];
        assert!(eval_predicate(r#"feature = "xxh64""#, &features));
        assert!(!eval_predicate(r#"feature = "xxh3""#, &features));
    }

    #[test]
    fn test_any() {
        let features = vec!["xxh64".to_string()];
        assert!(eval_predicate(r#"any(feature = "xxh64", feature = "xxh3")"#, &features));
        assert!(!eval_predicate(r#"any(feature = "xxh3", feature = "xxh32")"#, &features));
    }

    #[test]
    fn test_all() {
        let features = vec!["a".to_string(), "b".to_string()];
        assert!(eval_predicate(r#"all(feature = "a", feature = "b")"#, &features));
        assert!(!eval_predicate(r#"all(feature = "a", feature = "c")"#, &features));
    }

    #[test]
    fn test_not() {
        let features = vec!["xxh64".to_string()];
        assert!(eval_predicate(r#"not(feature = "xxh3")"#, &features));
        assert!(!eval_predicate(r#"not(feature = "xxh64")"#, &features));
    }

    #[test]
    fn test_complex_nested() {
        let features = vec!["xxh64".to_string()];
        assert!(eval_predicate(
            r#"any(feature = "xxh64", all(feature = "a", feature = "b"))"#,
            &features
        ));
        assert!(!eval_predicate(
            r#"all(feature = "xxh64", feature = "missing")"#,
            &features
        ));
    }

    #[test]
    fn test_non_cfg_attr() {
        assert_eq!(check_cfg_attr("derive(Clone, Debug)"), None);
        assert_eq!(check_cfg_attr("inline(always)"), None);
        assert_eq!(check_cfg_attr("test"), None);
    }

    #[test]
    fn test_cfg_attr_parsing() {
        // With features set
        init_features("xxh64,xxh32");
        assert_eq!(check_cfg_attr(r#"cfg(feature = "xxh64")"#), Some(true));
        assert_eq!(check_cfg_attr(r#"cfg(feature = "xxh3")"#), Some(false));
    }

    #[test]
    fn test_split_predicates() {
        let r = split_predicates(r#"feature = "a", feature = "b""#);
        assert_eq!(r.len(), 2);
        assert!(r[0].contains(r#"feature = "a""#));
        assert!(r[1].contains(r#"feature = "b""#));
    }
}
