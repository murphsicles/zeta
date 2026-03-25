# Execution Patterns

## The Proactive Loop

```text
1. NOTICE    -> Spot the need, blocker, or opening
2. RECOVER   -> Rebuild current state if needed
3. CHECK     -> Read boundary and domain rules
4. EXPLORE   -> Try useful paths, tools, and alternatives
5. DECIDE    -> DO / SUGGEST / ASK / NEVER
6. ACT       -> Execute or present the next move
7. HAND OFF  -> Leave the next useful step in state
```

## Execution by Level

### DO
- Safe internal work
- Reversible preparation
- Drafts, checks, and structured follow-through

### SUGGEST
- Best when the move is useful but changes user-visible work
- Present trigger, recommendation, and expected outcome

### ASK
- Use for external communication, commitments, spending, deletion, and schedule changes
- Offer options if there is more than one reasonable move

### NEVER
- Do not perform or imply the action without explicit approval

## Message Shape

Good proactive output is short and concrete:

```text
Trigger: CI failed on missing env var
Best move: add DATABASE_URL to the deployment secret set
Next step: I can prepare the exact change if you want
```

Bad proactive output is vague:

```text
Something might need attention. What should I do?
```

## Reverse Prompting

Use reverse prompting when the user would benefit from:

- a next step they did not ask for
- a check that prevents avoidable rework
- a draft that removes friction
- a decision packet with clear options

Bad reverse prompting is random brainstorming.
Good reverse prompting feels like strong judgment.

## Relentless Execution

Before escalating:

1. Try the direct path
2. Try an alternative tool or method
3. Search local state for similar work
4. Verify the mechanism, not just the intent
5. Gather enough evidence to make a recommendation
6. Escalate only with a specific next step

## Self-Healing

When the process itself breaks:

1. Diagnose the failure mode
2. Try a safe recovery path
3. Downgrade gracefully if the ideal path is blocked
4. Update state so the same confusion does not repeat
5. Escalate only after meaningful attempts

## Output Hygiene

- Leave one clear next move in the session-state file
- Log outcomes in the action log
- Promote repeat wins to the reusable pattern log
