# Memory Template - Proactivity

Create `~/proactivity/memory.md` with this structure:

```markdown
# Proactivity Memory

## Status
status: ongoing
version: 1.0.1
last: YYYY-MM-DD
integration: pending | complete | paused | never_ask

## Activation Preferences
- When this skill should auto-activate
- Whether it should jump in on blocked work, context drift, or missing next steps
- Quiet hours, batching, and message style

## Action Boundaries
- Safe actions it may do automatically
- Actions it should suggest first
- Actions that always require approval
- Actions it should never take

## State Rules
- What belongs in the session-state file
- When the working-buffer file should be used
- When active state should be cleared or refreshed

## Heartbeat Behavior
- What should be re-checked in the background
- Which changes deserve a message
- What should stay silent unless asked

## Notes
- Durable operating preferences
- Reliable trigger patterns
- Boundary exceptions worth keeping

---
*Updated: YYYY-MM-DD*
```

## Status Values

| Value | Meaning | Behavior |
|-------|---------|----------|
| `ongoing` | Setup still evolving | Keep learning useful boundaries |
| `complete` | Stable proactivity setup | Focus on execution and follow-through |
| `paused` | User wants less proactivity | Run only on explicit request |
| `never_ask` | User does not want setup prompts | Stop proactive setup questions |

## Local Files to Initialize

```bash
mkdir -p ~/proactivity/{domains,memory}
touch ~/proactivity/{memory.md,session-state.md,heartbeat.md,patterns.md,log.md}
touch ~/proactivity/memory/working-buffer.md
```

## Templates for Other Files

`session-state.md`
```markdown
# Session State
- Current objective
- Last confirmed decision
- Blocker or open question
- Next useful move
```

`heartbeat.md`
```markdown
# Heartbeat
- Active follow-ups worth re-checking
- Recurring checks that should stay lightweight
- Conditions that justify messaging the user
```
