We track work in Beads instead of Markdown. Run `bd quickstart` to see how.

# Task Management with Beads

**MANDATORY:** All work MUST be tracked in beads. Do NOT use todo_write, markdown checklists, or other tracking methods.

## When to Create Beads

1. **At conversation start** - If user describes work, immediately create beads
2. **When discovering subtasks** - Break down complex work into separate beads
3. **When finding new issues** - During audits, refactoring, or testing
4. **Before starting any significant work** - Never start work without a bead

## When to Update Beads

1. **Mark `in_progress`** - As soon as you start working on a bead
2. **Mark `completed`** - Immediately after finishing, with `--reason` explaining what was done
3. **Add dependencies** - Use `bd dep add` to link related work (blocks, parent-child, related)
4. **Update priority** - If urgency changes based on findings

## Required Workflow

```bash
# Start of work
bd list --status open          # Check what's ready
bd update <id> --status in_progress

# During work - if you discover subtasks
bd create "Subtask name" -p 0 -d "Description"
bd dep add <parent> <child> --type parent-child

# End of work
bd close <id> --reason "What was accomplished"
bd ready                       # See what's unblocked
```

## Common Patterns

**Epic with subtasks:**
```bash
bd create "Refactor module X" -t epic -p 0
bd create "Extract helpers" -p 0 -d "..."
bd create "Add tests" -p 0 -d "..."
bd dep add <epic-id> <subtask-id> --type parent-child
```

**Blocking dependencies:**
```bash
# Task B cannot start until Task A is done
bd dep add <task-b-id> <task-a-id> --type blocks
```

**Test coverage for refactoring:**
```bash
# Tests must be done before refactoring is complete
bd create "Add tests for X" -p 0
bd dep add <refactor-id> <test-id> --type blocks
```

## Check Your Work

- Run `bd ready` frequently to see unblocked work
- Run `bd list --status open` to see all open issues
- Run `bd show <id>` to see details and dependencies
- Use `bd dep tree <id>` to visualize dependency chains

## Anti-Patterns (DON'T DO THIS)

❌ Using todo_write tool
❌ Creating markdown TODO lists
❌ Saying "I'll do X, Y, Z" without creating beads
❌ Starting work without updating bead status
❌ Forgetting to close beads when done
❌ Creating giant beads instead of breaking them down


