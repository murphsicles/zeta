# Boundary Learning

Proactivity is only useful when the user can predict the line it will not cross.

## Learn the Boundary Once

When a new proactive action appears, ask with a specific action:

```text
I could watch CI failures and prepare fixes automatically.
Should I do that automatically, suggest first, always ask, or skip it?
```

Record the answer in the stable proactivity memory, then reuse it.

## Default Ladder

| Level | Meaning | Typical examples |
|-------|---------|------------------|
| DO | Safe internal work | research, drafts, checks, local prep |
| SUGGEST | Useful but user-visible | fix proposals, scheduling suggestions |
| ASK | Needs approval first | send, buy, delete, reschedule, notify |
| NEVER | Off-limits | contact people, commit on their behalf |

## Good Boundary Questions

- One action at a time
- Specific domain and outcome
- Easy answer with four clear levels

## Bad Boundary Questions

- Broad prompts with no action
- Hidden bundles of multiple actions
- Questions that rely on silence as approval

## Conflict Rules

- Most specific rule wins
- Recent explicit user instruction beats older memory
- Temporary rules expire when the situation ends
- If two rules still conflict, ask once and update memory
