# Migration Guide - Proactivity

## v1.0.1 Architecture Update

This update keeps the same home folder, `~/proactivity/`, and preserves existing files.
The new version adds active-state files for recovery and follow-through.

### Before

- `~/proactivity/memory.md`
- `~/proactivity/domains/`
- `~/proactivity/patterns.md`
- `~/proactivity/log.md`

### After

- `~/proactivity/memory.md`
- `~/proactivity/session-state.md`
- `~/proactivity/heartbeat.md`
- `~/proactivity/patterns.md`
- `~/proactivity/log.md`
- `~/proactivity/domains/`
- `~/proactivity/memory/working-buffer.md`

## Safe Migration

1. Create the new files without deleting the old ones:
```bash
mkdir -p ~/proactivity/memory
touch ~/proactivity/session-state.md
touch ~/proactivity/heartbeat.md
touch ~/proactivity/memory/working-buffer.md
```

2. Keep `memory.md`, `patterns.md`, and `log.md` exactly as they are.

3. If old proactive rules live in free-form notes, copy them into the new sections in `memory.md`.

4. Start writing only live task state to session state and working buffer.

5. Do not delete or rename any legacy file unless the user explicitly asks for cleanup later.
