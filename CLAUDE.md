# Claude Code Instructions

## After Every Major Change

Before committing any performance-related changes (search, evaluation, move generation, board representation), run the benchmark tracker:

```bash
cargo run --release --bin benchmark_tracker
```

This records performance metrics to `benchmark_history.json` and compares against the previous benchmark to show percentage improvements.

## What Counts as a Major Change

- Search algorithm modifications (alpha-beta, move ordering, pruning)
- Evaluation function changes (PST, material, new terms)
- Move generation optimizations (bitboards, attack maps)
- Board representation changes (data structures, make/unmake)
- Any optimization listed in the README roadmap

## Interpreting Results

- **Perft nps**: Higher is better (raw move generation speed)
- **Search time**: Lower is better (ms per move)
- **Nodes searched**: Fewer nodes = better pruning/move ordering

A good change shows positive percentages for speed metrics and/or fewer nodes searched.

## Board Move APIs

- **`execute_move(&Move) -> Board`** — Clones the entire board. Simple but slow (~10x). Fine for UI code, UCI protocol handlers, book lookups, and tests where convenience matters more than speed.
- **`make_move(&Move) -> UndoInfo` / `unmake_move(&UndoInfo)`** — In-place mutation. Used in search, perft, and any hot path. Always pair make with unmake.

**Never use `execute_move` in performance-critical code** (search, perft, move generation, evaluation). Use `make_move`/`unmake_move` instead.
