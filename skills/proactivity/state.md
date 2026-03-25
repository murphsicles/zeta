# State Routing

Proactivity works best when stable memory and live task state stay separate.

## Use Stable Memory For

- durable activation preferences
- action boundaries that should persist
- batching, timing, and style preferences
- recurring rules the user expects later

## Use Session State For

- current objective
- last confirmed decision
- current blocker
- next useful move

## Use the Working Buffer For

- volatile breadcrumbs during long tasks
- partial findings not ready for durable memory
- recovery hints after tool-heavy work
- temporary notes that should be cleared later

## Use Heartbeat State For

- promised follow-ups
- stale blockers worth re-checking
- recurring checks that should stay lightweight
- triggers that justify messaging the user

## Routing Rule

If the note should still matter next week, it belongs in stable memory.
If it matters for the current task only, it belongs in active state.
