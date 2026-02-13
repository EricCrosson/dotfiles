# CoderLM Server API Reference

All endpoints prefixed with `/api/v1`. Session-scoped endpoints require `X-Session-Id` header.
The CLI wrapper (`coderlm-cli`) handles headers and session management automatically.

## CLI Command Reference

Abbreviated as `cli` below.

### Session Management

```bash
# Create session (indexes project, caches session ID)
cli init [--cwd /path/to/project] [--port 3000]

# Server + session status
cli status

# Delete session
cli cleanup
```

### Codebase Structure

```bash
# File tree (depth 0 = unlimited)
cli structure [--depth 2]

# Annotate a file
cli define-file src/main.rs "CLI entrypoint, parses args and starts server"
cli redefine-file src/main.rs "Updated description"

# Tag file type: documentation, ignore, test, config, generated, custom
cli mark tests/integration.rs test
```

### Symbol Operations

```bash
# List symbols (filter by kind, file, or both)
cli symbols [--kind function] [--file src/main.rs] [--limit 50]

# Search symbols by name substring
cli search "handler" [--limit 20]

# Get full source code of a symbol
cli impl run_server --file src/main.rs

# Find call sites
cli callers scan_directory --file src/index/walker.rs [--limit 50]

# Find tests referencing a symbol
cli tests scan_directory --file src/index/walker.rs [--limit 20]

# List local variables in a function
cli variables scan_directory --file src/index/walker.rs

# Annotate a symbol
cli define-symbol scan_directory --file src/index/walker.rs "Walks codebase respecting gitignore"
cli redefine-symbol scan_directory --file src/index/walker.rs "Updated description"
```

### Content Operations

```bash
# Read lines from a file (0-indexed, end exclusive)
cli peek src/main.rs [--start 0] [--end 50]

# Regex search across all indexed files
cli grep "DashMap" [--max-matches 50] [--context-lines 2]

# Scope-aware grep: only match in code (skip comments and strings)
cli grep "DashMap" --scope code

# Compute byte-range chunks for a file
cli chunks src/main.rs [--size 5000] [--overlap 200]
```

### Annotations

```bash
# Save annotations (definitions + marks) to .coderlm/annotations.json
cli save-annotations

# Load annotations from disk (auto-loaded on session creation)
cli load-annotations
```

### History

```bash
# Session command history
cli history [--limit 50]
```

## Symbol Kinds

`function`, `method`, `class`, `struct`, `enum`, `trait`, `interface`, `constant`, `variable`, `type`, `module`

## Supported Languages (tree-sitter)

| Language   | Extensions                    |
| ---------- | ----------------------------- |
| Rust       | `.rs`                         |
| Python     | `.py`, `.pyi`                 |
| TypeScript | `.ts`, `.tsx`                 |
| JavaScript | `.js`, `.jsx`, `.mjs`, `.cjs` |
| Go         | `.go`                         |

All other file types appear in the file tree and are searchable via peek/grep, but do not produce symbols.

## Mark Types

`documentation`, `ignore`, `test`, `config`, `generated`, `custom`
