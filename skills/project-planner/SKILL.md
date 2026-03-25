---
name: project-planner
description: >
  Triage ideas, problems, and feature requests into the right format: proposal doc,
  feature issue, or bug report. Repo-aware — discovers templates and docs structure
  from the current repository. Use when: (1) the user describes an idea, feature,
  or problem they want to track, (2) the user says "file a bug", "I have an idea",
  "let's plan this feature", or similar, (3) the user wants to break down a large
  feature into phases with GitHub issues. NOT for: actually implementing code (use
  coding-agent), reviewing PRs, or general questions about the codebase.
---

# Project Planner

## Prerequisites

- git
- gh (GitHub CLI, authenticated via `gh auth login`)

Triage user input into the right project artifact: a **proposal** (big idea with phases),
a **feature issue** (small enhancement), or a **bug report** (something's broken).

## Repo Discovery

Before doing anything, discover the current repo's configuration:

1. Run `git rev-parse --show-toplevel` to find the repo root
2. Check for `.project-planner.yml` at the repo root — if it exists, read it and
   use its values for all paths, labels, and conventions
3. If no config file, fall back to auto-discovery:
   - Proposal template: look for `docs/proposals/TEMPLATE.md`
   - Issue templates: look in `.github/ISSUE_TEMPLATE/`
   - Docs directory: look for `docs/`, `mkdocs.yml`
   - If nothing found, use the fallback formats bundled with the skill
4. If the repo has `CLAUDE.md` or `CONTRIBUTING.md`, read for conventions
5. Run `gh repo view --json name,owner` to confirm the repo for issue creation

### Config File: `.project-planner.yml`

Optional config file at repo root. All fields are optional — auto-discovery fills gaps.
See `project-planner.yml` in the skill directory for a copy-paste starter.

```yaml
project: MyProject                    # project name (for issue titles)
repo: owner/repo                      # GitHub repo (usually auto-detected)

proposals:
  dir: docs/proposals                 # where proposal docs live
  template: docs/proposals/TEMPLATE.md # proposal template to follow
  index: docs/proposals/index.md      # index file to update with new proposals
  mkdocs_nav: true                    # update mkdocs.yml nav when creating proposals

issues:
  labels:
    feature: enhancement              # label for feature issues
    bug: bug                          # label for bug issues
  # branch_prefix: feature/           # branch naming prefix

# conventions:
#   docs: docs                        # where project docs live
```

## Triage Rules

Determine the type by asking: **does this need design work or multiple phases?**

- Needs design decisions, multiple phases, or architectural thought → **Proposal**
- Single, obvious change — no design needed → **Feature issue**
- Something is broken or behaving wrong → **Bug report**

If unclear, ask the user: "Is this a quick fix or does it need a design doc?"

## Workflow: Proposal

For big ideas that need phases and design.

1. Discover proposal template (see Repo Discovery above)
2. Research the codebase and any docs/ directory for relevant context
3. Think through the design — motivation, approach, trade-offs
4. Break into shippable phases (each phase delivers user value)
5. Write acceptance criteria at both levels (overall + per-phase)
6. Create the proposal doc at `docs/proposals/<name>.md`
7. If `mkdocs.yml` exists, add the proposal to the nav under Proposals
8. If `docs/proposals/index.md` exists, add to the Active Proposals list
9. Create a GitHub issue for each phase using `gh issue create`:
   - Title: `<Proposal name>: Phase N — <phase name>`
   - Body: phase goal, acceptance criteria, tasks as checklist, link to proposal
   - Label: `enhancement`
10. Update the proposal doc with issue links for each phase
11. Commit to a new branch and push

### Proposal Quality Checklist

Before committing, verify:

- [ ] Summary is one clear paragraph
- [ ] Motivation explains why now
- [ ] Design covers user experience AND technical approach
- [ ] Every phase is independently shippable
- [ ] Acceptance criteria are testable (not vague)
- [ ] Open questions section exists (even if empty)
- [ ] Related section links to relevant docs, issues, or design docs
- [ ] Status is set to "Ready" (if issues created) or "Draft" (if not)

## Workflow: Feature Issue

For small, self-contained enhancements.

1. Discover feature template (see Repo Discovery above)
2. Create a GitHub issue using `gh issue create`:
   - Title: clear, action-oriented
   - Body: summary, acceptance criteria as checklist, doc references if relevant
   - Follow the repo's template format if one exists
   - Label: `enhancement`
3. Report the issue number and URL to the user

## Workflow: Bug Report

For problems and broken behavior.

1. Discover bug template (see Repo Discovery above)
2. Try to identify the relevant code by searching the codebase
3. Create a GitHub issue using `gh issue create`:
   - Title: `Bug: <concise description>`
   - Body: description, steps to reproduce (if known), expected vs actual,
     relevant code files/lines, related docs
   - Follow the repo's template format if one exists
   - Label: `bug`
4. Report the issue number and URL to the user

## Important Rules

- **Always use `gh issue create`** — it's repo-aware, handles auth
- **Always link back** — issues reference proposals, proposals reference issues
- **Proposals stay forever** — status changes, docs never move or get deleted
- **One proposal per feature** — don't cram multiple ideas into one doc
- **Phases must be shippable** — each delivers user value, not just "backend work"
- **Commit to a branch** — never push directly to main
- **Respect repo conventions** — if the repo has CLAUDE.md or CONTRIBUTING.md, read
  and follow its branch naming, commit message, and PR conventions
