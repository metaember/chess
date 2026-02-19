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
