# chess
Chess bot written in rust

## Improvement Roadmap

### Speed Optimizations

- [x] **Incremental PST Evaluation** ✓ DONE

  ~~Currently the evaluation function iterates all pieces on every leaf node, which is O(n_pieces) per eval call.~~ Now maintains running PST scores that update incrementally during `make_move`/`unmake_move`. Evaluation is now O(1) per position.

- [x] **Remove `pieces: Vec<Piece>` Redundancy** ✓ DONE

  ~~Added `iter_pieces()` method using bitboards. Updated hot paths (evaluation, material counting, pin detection) to use bitboards.~~ Fully removed the redundant `pieces: Vec<Piece>` from the Board struct. All piece data is now stored in `board_to_piece: [[Option<Piece>; 8]; 8]` (for O(1) position lookup) and `piece_bb: [[u64; 6]; 2]` (for iteration). This eliminates heap allocation overhead and simplifies make/unmake_move logic.

- [ ] **Lazy/Incremental Attack Map Updates** ⚠️ ATTEMPTED

  `recompute_attack_maps()` is called on every `make_move` and `unmake_move`, regenerating all sliding piece attacks from scratch. Lazy evaluation was attempted using atomics for thread-safety but the atomic overhead (~30-40% perft slowdown) outweighed the benefits. Future approaches: incremental updates that only recalculate affected sliding piece rays, or magic bitboards which would make full recomputation fast enough.

- [ ] **Stack-Allocated Move Lists**

  `get_legal_moves()` allocates a new `Vec<Move>` per node, causing heap allocation pressure. Replace with `ArrayVec<Move, 256>` (stack-allocated) or a thread-local move list pool. The maximum legal moves in any chess position is 218, so a fixed-size array suffices.

- [x] **Magic Bitboards for Sliding Pieces** ✓ DONE

  ~~Currently using classical ray attacks with blocker detection via `trailing_zeros`/`leading_zeros`.~~ Implemented magic bitboards with precomputed lookup tables indexed by `(occupancy * magic) >> shift`. Attack generation is now O(1) via table lookup. Benchmark shows ~4-6% speedup in search; perft unchanged (dominated by make/unmake move overhead, not attack generation).

### Strength Optimizations

- [x] **Enable Endgame PST Tables** ✓ DONE

  ~~The codebase defines `PAWNS_END` and `KING_END` tables but `is_endgame` is hardcoded to `false`.~~ Now uses tapered evaluation with phase detection: `phase = (queens*4 + rooks*2 + bishops + knights)`. Interpolates between middlegame and endgame PST scores based on remaining material.

- [x] **Killer Move Heuristic** ✓ DONE

  ~~Track the last 2 quiet moves that caused a beta cutoff at each ply depth.~~ Implemented in `SearchState` struct. Stores 2 killer moves per ply, prioritized in move ordering after TT move and captures (90,000 for first killer, 80,000 for second).

- [x] **History Heuristic** ✓ DONE

  ~~Maintain a `[Color][From][To]` table that accumulates a bonus each time a quiet move causes a beta cutoff, scaled by depth².~~ Implemented in `SearchState`. History scores indexed by `[color][from_sq][to_sq]`, used for quiet move ordering. Scores aged at each depth iteration to gradually forget old information.

- [x] **Principal Variation Search (PVS)** ✓ DONE

  ~~After searching the first move with a full window, search remaining moves with a null window `(-alpha-1, -alpha)`. If the null window search fails high, re-search with the full window.~~ Implemented in `negamax_with_tt_mut`. First move uses full window, subsequent moves use null window (-alpha-1, -alpha), with full re-search on fail high. Combined with LMR for later quiet moves. Benchmarks show ~12-28% faster search at depths 5-6.

- [x] **Aspiration Windows** ✓ DONE

  ~~In iterative deepening, use a narrow window centered on the previous iteration's score (e.g., ±25 centipawns) instead of (-∞, +∞).~~ Implemented in `iterative_deepening` and `iterative_deepening_timed`. After depth 1, uses ±25cp window centered on previous score. On fail-low/fail-high, doubles window and re-searches. Falls back to full window if window exceeds ±200cp.

- [x] **Check Extensions** ✓ DONE

  ~~When a move gives check, extend the search depth by 1 ply.~~ Implemented in `negamax_with_tt_mut`. After making a move, check if opponent is in check; if so, extend search depth by 1. Also prevents LMR from being applied to moves that give check. This avoids horizon effects where forcing check sequences are cut off prematurely.

- [x] **Static Exchange Evaluation (SEE)** ✓ DONE

  ~~Before searching a capture in quiescence, simulate the full exchange sequence on that square to determine if the capture is winning, equal, or losing.~~ Implemented `see()` function that simulates exchange sequences. In quiescence search, prunes losing captures when attacker_value > victim_value. Uses least-valuable-attacker ordering for accurate exchange simulation.

- [x] **Futility Pruning** ✓ DONE

  ~~At low depths (1-2 ply from leaf), if the static evaluation plus a margin is still below alpha, skip searching quiet moves entirely.~~ Implemented in `negamax_with_tt_mut`. At depths 1-2, skips quiet moves (non-captures, non-promotions) if static_eval + margin < alpha. Margins: depth 1 = 200cp, depth 2 = 500cp. Not applied when in check or for the first move.

- [ ] **Improved Evaluation Terms** ⚠️ ATTEMPTED

  The current eval is material + PST only. Attempted to add bishop pair (+30) and pawn structure (doubled/isolated penalties) but found that accessing `piece_bb` from within `evaluate_board` causes cache misses (piece_bb is 96 bytes away from the incrementally-updated material/PST fields). This caused 200-300% slowdown at depths 5-6 due to the high frequency of evaluation calls in quiescence search. To enable these terms, the solution is either:
  1. **Incremental tracking**: Add bishop/pawn counts to Board struct, update during make/unmake_move
  2. **Pawn hash table**: Cache pawn structure scores since pawns move infrequently

  Potential improvements (not yet implemented):
  - **Mobility**: Count pseudo-legal moves per piece, bonus for more active pieces
  - **King safety**: Bonus for pawn shield in front of castled king, penalty for open files near king

### Implementation Priority

| Phase | Task | Type | Expected Impact | Status |
|-------|------|------|-----------------|--------|
| 1 | Enable endgame PST interpolation | Strength | Quick win, minimal code | ✓ Done |
| 2 | Incremental PST evaluation | Speed | 15-20% faster | ✓ Done |
| 3 | Remove `pieces` Vec | Speed | 10-15% faster | ✓ Done |
| 4 | Killer moves + history heuristic | Strength | Much better move ordering | ✓ Done |
| 5 | Lazy/incremental attack maps | Speed | 10-20% faster | ⚠️ Attempted |
| 6 | PVS search | Strength | Better node efficiency | ✓ Done |
| 7 | Check extensions | Strength | Avoids horizon effects | ✓ Done |
| 8 | Magic bitboards | Speed | ~2x faster movegen | ✓ Done (4-6% speedup) |
| 9 | SEE pruning | Strength | Smaller quiescence tree | ✓ Done |
| 10 | Futility pruning | Strength | Smaller search tree | ✓ Done |
| 11 | Improved eval terms | Strength | Better positional play | ⚠️ Attempted |

---

## Remaining Optimizations

### High Impact (Do These First)

| Priority | Optimization | Type | Location | Notes |
|----------|--------------|------|----------|-------|
| **High** | Shorter mate preference | Strength | `search.rs:796` | Mate-in-1 currently scores the same as mate-in-5. Add per-ply penalty so engine finds fastest checkmate. |
| **High** | Stack-allocated move lists | Speed | `get_legal_moves()` | Replace `Vec<Move>` with `ArrayVec<Move, 218>`. Eliminates heap allocation at every node. |
| **High** | Remove debug path tracking | Speed | `search.rs:270` | `SearchResult.moves` Vec only needed for debugging. Use `#[cfg(debug_assertions)]` to exclude from release builds. |

### Medium Impact

| Priority | Optimization | Type | Location | Notes |
|----------|--------------|------|----------|-------|
| **Medium** | Slim `Move` struct | Speed | `types.rs:325` | Remove redundant `piece: Piece` field (~12 bytes). Can be looked up in O(1) via `board_to_piece`. |
| **Medium** | Penalize pawn-capture squares | Strength | `evaluate.rs:173` | In move ordering, downgrade moves where the piece lands on a square attacked by enemy pawns. |
| **Medium** | Bishop pair bonus (incremental) | Strength | `board.rs` | Cache misses blocked previous attempt. Add bishop count to Board struct, update during make/unmake, award +30cp for having both bishops. |
| **Medium** | Pawn structure via hash table | Strength | `evaluate.rs` | Cache doubled/isolated pawn penalties in hash table. Pawns move rarely, so cache hit rate is high. |
| **Medium** | Null move pruning | Strength | `search.rs` | Skip your turn; if opponent's best response still >= beta, prune the subtree. Typically 10-15% tree reduction. |
| **Medium** | Late Move Reductions (LMR) | Strength | `search.rs` | Search later moves at reduced depth. Already have PVS, but LMR on top provides additional gains. |
| **Medium** | Optimize pin-filter nested loop | Speed | `board.rs:1361` | Rewrite O(moves × pins) loop using position-indexed map for O(1) lookup. |

### Lower Priority (Polish)

| Priority | Optimization | Type | Location | Notes |
|----------|--------------|------|----------|-------|
| **Low** | King safety evaluation | Strength | `evaluate.rs` | Pawn shield bonus for pawns in front of castled king, penalty for open files near king. |
| **Low** | Mobility evaluation | Strength | `evaluate.rs` | Count pseudo-legal moves per piece, bonus for more active pieces. |
| **Low** | Opening book | Strength | `search.rs` | Polyglot book support to skip search for known good openings. |
| **Low** | Endgame tablebases | Strength | `search.rs` | Syzygy tablebase support for perfect play in 6-7 piece endings. |
| **Low** | Parallel search (Lazy SMP) | Speed | `search.rs` | Multi-threaded search with shared transposition table. |
| **Low** | Replace `ray_to` Vec allocation | Speed | `types.rs:139` | Return fixed-size array or iterator instead of allocating `Vec<Position>`. |

### Code Cleanup TODOs

| Location | Description |
|----------|-------------|
| `types.rs:325` | Remove redundant `piece` field from `Move` struct |
| `board.rs:450` | Stale comment about `piece_at()` speed—already fixed |
| `board.rs:1490` | Deprecate `get_king()` in favor of `get_king_position()` |
| `game.rs:68` | Better messages for checkmate vs stalemate |
| `movegen.rs:647` | Verify diagonal alignment check in `get_pin_for_bishop` |

## Attribution

- **Chess piece images**: [CBurnett piece set](https://github.com/lichess-org/lila/tree/master/public/piece/cburnett) from Lichess, licensed under [GPLv2+](https://www.gnu.org/licenses/gpl-2.0.html)

