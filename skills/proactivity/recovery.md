# Recovery Flow

When context gets long, interrupted, or stale, recover before asking the user to repeat work.

## Recovery Order

1. Read stable proactivity memory
2. Read session state
3. Read the working buffer if the task was long or tool-heavy
4. Reconstruct:
- current objective
- last confirmed decision
- blocker or missing input
- next useful move

## Good Recovery

- "Last agreed move was to draft the fix, and the blocker is missing deploy access. I can prepare the patch now."

## Bad Recovery

- "Can you remind me what we were doing?"

## When to Ask the User

- a required input is still missing after recovery
- local state conflicts with a newer instruction
- the task changed direction and old state is no longer trustworthy
