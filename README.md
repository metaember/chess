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

- [ ] **Stack-Allocated Move Lists** ⚠️ ATTEMPTED

  `get_legal_moves()` allocates a new `Vec<Move>` per node, causing heap allocation pressure. Attempted `ArrayVec` and `SmallVec` but both showed performance decrease in practice. Stack allocation overhead and inline capacity limitations outweighed the benefits of avoiding heap allocations. Alternative approaches: thread-local move list pool or custom arena allocator.

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

- [x] **Null Move Pruning** ✓ DONE

  ~~Skip your turn; if opponent's best response still >= beta, prune the subtree.~~ Implemented with reduction of 2 plies. Applied at depth >= 3 when not in check and score is far from mate bounds. Typically provides 10-15% tree reduction in middlegame positions.

- [x] **Late Move Reductions (LMR)** ✓ DONE

  ~~Search later moves at reduced depth.~~ Implemented on top of PVS. First 4 moves searched at full depth, remaining quiet moves at reduced depth (depth - 1). Not applied when in check or when move gives check. Combined with PVS null window search for maximum efficiency.

- [x] **Improved Evaluation Terms** ✓ DONE

  ~~The eval was material + PST only. Previous attempt to add eval terms failed due to cache misses from accessing `piece_bb` in `evaluate_board`.~~ Solved via incremental tracking (bishop pair) and a pawn hash table (pawn structure). Now includes:
  - **Bishop pair**: +30cp MG / +50cp EG, tracked incrementally via `bishop_count: [u8; 2]`
  - **Pawn structure** (cached in 16K-entry pawn hash table): doubled pawns (-10/-15), isolated pawns (-15/-10), passed pawns (5-70 MG, 10-150 EG by rank)
  - **King safety**: pawn shield bonus (+10/pawn), open file near king penalty (-20/file), phase-weighted
  - **Rook on open files**: +20 open, +10 semi-open

  Gauntlet results (71 games at 1s+0.1s): 0 losses, 1 win, 70 draws vs old eval. Elo: +5 [+88, -78]. See SOTA comparison below for tuning opportunities.

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
| 11 | Null move pruning | Strength | Smaller search tree | ✓ Done |
| 12 | Late Move Reductions (LMR) | Strength | Better node efficiency | ✓ Done |
| 13 | Improved eval terms | Strength | Better positional play | ✓ Done |

---

## Evaluation vs State of the Art

Comparison of our current evaluation terms against Stockfish (classical, pre-NNUE), Ethereal (~3300 Elo), and Fruit. Values are in centipawns (MG = middlegame, EG = endgame).

### Bishop Pair

| | Ours | Stockfish | Ethereal | Fruit |
|---|---|---|---|---|
| **MG** | +30 | ~90 (polynomial) | +22 | +50 |
| **EG** | +50 | ~90 (single value) | +88 | +50 |
| **Method** | Flat bonus, count > 1 | Polynomial imbalance, interactions with all piece counts | Flat bonus, checks both square colors | Flat bonus, count-based |
| **Tracking** | Incremental `bishop_count` | Cached in material hash | Per eval call | Per eval call |

**Gaps**: Our EG value is low (50 vs Ethereal's 88). Bishop pair advantage grows in open endgames. Stockfish's polynomial system means the value scales with opponent pawn count. We should check bishops on both square colors (not just count) to handle promotion edge cases.

### Doubled Pawns

| | Ours | Stockfish 12 | Older Stockfish |
|---|---|---|---|
| **MG** | -10 | -11 | -13 to -23 (per file) |
| **EG** | **-15** | **-55** | -43 to -48 (per file) |

**Gaps**: MG is fine. **EG is far too small** (-15 vs -55). Doubled pawns are crippling in endgames — this is the single biggest value discrepancy. Older Stockfish also varied by file (center worse than edge).

### Isolated Pawns

| | Ours | Stockfish 12 | Older Stockfish |
|---|---|---|---|
| **MG** | -15 | -5 base (-20 on open file) | -25 to -60 (per file + opposed) |
| **EG** | -10 | -17 base (-42 on open file) | -30 to -52 (per file + opposed) |

**Gaps**: Our phase weighting is **backwards** — we penalize more in MG than EG. Stockfish does the opposite: isolated pawns are worse in endgames (easier targets with fewer pieces). We also don't distinguish isolated pawns on **open files** — Stockfish adds "WeakUnopposed" (-15/-25) when no enemy pawn blocks the file.

### Passed Pawns

| Rank (from pawn's perspective) | Ours MG | Ours EG | Stockfish 12 MG | Stockfish 12 EG |
|---|---|---|---|---|
| 2 | 5 | 10 | 9 | 28 |
| 3 | 5 | 10 | 15 | 31 |
| 4 | 10 | 20 | 17 | 39 |
| 5 | 20 | 40 | 64 | 70 |
| 6 | 40 | 80 | 171 | 177 |
| 7 | 70 | 150 | 277 | 260 |

**Gaps**: Low ranks are roughly OK. **Ranks 5-7 are dramatically undervalued** — our rank 7 passer (70/150) is about half of Stockfish's (277/260). The biggest missing feature is **king proximity scaling**: Stockfish adds huge bonuses when the enemy king is far from the promotion square and the friendly king is close. Formula: `(enemy_king_dist * 5 - friendly_king_dist * 2) * rank_weight`. Also missing: blocker awareness (is the square ahead empty/safe?) and supported passer bonus.

### King Safety

| | Ours | Stockfish (classical) |
|---|---|---|
| **Pawn shield** | +10/pawn, max +30 | Per-file, per-rank lookup table (0-141cp penalties) |
| **Open files** | -20/file near king | Implicit via shelter table + storm danger |
| **Attack zones** | None | Weighted attacker count (N=77, B=55, R=44, Q=10), min 2 attackers |
| **Safe checks** | None | Huge danger points (Queen=780, Rook=880, Knight=790, Bishop=435) |
| **Scoring** | Linear | **Quadratic**: `danger^2 / 4096` MG, `danger / 16` EG |
| **Pawn storm** | None | Per-rank, per-state tables for advancing enemy pawns |

**Gaps**: This is the area with **the most room for improvement**. Our king safety is extremely simplistic. Stockfish's quadratic scoring means multiple attackers compound super-linearly (3 attackers ~ 9x the penalty of 1). Safe check detection is critical — a rook that can safely check the king adds 880 danger points. The minimum-2-attackers threshold prevents false positives.

### Rook on Open/Semi-Open Files

| | Ours | Stockfish 12 | Ethereal |
|---|---|---|---|
| **Open MG** | +20 | **+44** | +34 |
| **Open EG** | +20 | +20 | +8 |
| **Semi-open MG** | +10 | +18 | +10 |
| **Semi-open EG** | +10 | +7 | +9 |
| **Rook on 7th** | None | Implicit via threats | +42 EG |

**Gaps**: Open file MG is roughly half of Stockfish's (+20 vs +44). Should also add a rook-on-7th-rank bonus (especially in endgame). Bonus is primarily a middlegame term — our flat single value doesn't capture this.

### Missing Eval Terms

Features present in SOTA engines that we don't implement:

| Feature | Stockfish Values | Est. Elo Impact | Difficulty |
|---|---|---|---|
| **Mobility** (bishop/rook square counting) | Lookup tables by piece type, 0-27 squares | +40 Elo | Medium* |
| **King attack zones + quadratic scoring** | `danger^2/4096`, attacker weights | +30-50 Elo | Hard |
| **King proximity to passed pawns** | `(enemy_dist*5 - friendly_dist*2) * rank_weight` | +15-25 Elo | Easy |
| **Backward pawns** | -8/-27 base, worse on open file | +10-20 Elo | Easy |
| **Connected pawns** | 0-85 MG by rank, phalanx/support modifiers | +15-25 Elo | Easy |
| **Knight/Bishop outposts** | +36/+12 for supported knight on rank 4-6 | +10-15 Elo | Easy |
| **Safe check detection** | 780-880 danger points per safe check | +10-20 Elo | Medium |
| **Pawn storm** | Per-rank/state tables for enemy pawns near king | +5-10 Elo | Medium |
| **Rook on 7th rank** | +42 to +98 EG | +5-10 Elo | Trivial |

### Tuning Priority

Ordered by expected impact and implementation effort:

1. **Fix doubled pawn EG** (-15 -> ~-50) — trivial value change, big correction
2. **Fix passed pawn ranks 5-7** (roughly double values) — trivial value change
3. **Add king proximity to passed pawns** — easy, already have `get_king_square()`
4. **Add mobility** — done (knight-only, see note below)
5. **Fix isolated pawn phase** (flip MG/EG) + add open-file modifier — easy
6. **Increase rook open file MG** (+20 -> ~+40) — trivial value change
7. **Add backward pawn penalty** — easy with existing bitboard masks
8. **Add connected pawn bonus** — easy, adjacent pawn detection
9. **Add knight outposts** — easy, pawn attack span check
10. **King attack zones + quadratic scoring** — hard but highest long-term ceiling

> **Why only knight mobility?** Bishop/rook mobility requires `bishop_attacks()` and `rook_attacks()` which go through magic bitboard lookups. These are already called in `compute_attack_map_static` during `make_move`, but by the time `evaluate_board` runs (millions of calls per search), the magic table data has been evicted from L1/L2 cache. Re-doing those lookups in eval caused **8x per-node slowdown**. Knight attacks use a simple 64-entry precomputed table (`ATTACK_TABLES.knight[]`) that stays hot in cache, so knight mobility is essentially free. A future optimization could piggyback bishop/rook mobility on the existing attack map computation in `recompute_attack_maps()`, but that requires architectural changes to thread eval data through the move/unmake path.

*Sources: [Stockfish classical eval (SF 10-12)](https://github.com/official-stockfish/Stockfish/blob/sf_12/src/evaluate.cpp), [Ethereal](https://github.com/AndyGrant/Ethereal), [Fruit 2.1](https://github.com/Warpten/Fruit-2.1), [Chessprogramming Wiki](https://www.chessprogramming.org/Evaluation), [MadChess mobility measurement](https://www.madchess.net/2020/02/01/madchess-3-0-beta-5c5d4fc-piece-mobility/)*

---

## Remaining Optimizations

### Speed

| Priority | Optimization | Type | Location | Notes |
|----------|--------------|------|----------|-------|
| **High** | Remove debug path tracking | Speed | `search.rs:270` | `SearchResult.moves` Vec only needed for debugging. Use `#[cfg(debug_assertions)]` to exclude from release builds. |
| **Medium** | Slim `Move` struct | Speed | `types.rs:325` | Remove redundant `piece: Piece` field (~12 bytes). Can be looked up in O(1) via `board_to_piece`. |
| **Medium** | Optimize pin-filter nested loop | Speed | `board.rs:1361` | Rewrite O(moves x pins) loop using position-indexed map for O(1) lookup. |
| **Low** | Replace `ray_to` Vec allocation | Speed | `types.rs:139` | Return fixed-size array or iterator instead of allocating `Vec<Position>`. |
| **Low** | Parallel search (Lazy SMP) | Speed | `search.rs` | Multi-threaded search with shared transposition table. |

### Strength

| Priority | Optimization | Type | Location | Notes |
|----------|--------------|------|----------|-------|
| **High** | Tune eval values to SOTA | Strength | `evaluate.rs` | See comparison table above. Doubled EG, passed pawn scaling, isolated phase are all wrong. |
| **High** | King proximity to passed pawns | Strength | `evaluate.rs` | Scale passed pawn bonus by king distances. Easy, high impact. |
| **High** | Mobility (bishop/rook) | Strength | `evaluate.rs` | Knight mobility done. Bishop/rook blocked by cache thrashing from magic BB lookups in eval (8x slowdown). Needs architectural change to compute during `recompute_attack_maps`. |
| **Medium** | Backward pawn penalty | Strength | `evaluate.rs` | Bitboard-based detection using existing masks. |
| **Medium** | Connected pawn bonus | Strength | `evaluate.rs` | Phalanx + support detection, scaled by rank. |
| **Medium** | Knight/Bishop outposts | Strength | `evaluate.rs` | Bonus for pieces on squares unreachable by enemy pawns. |
| **Medium** | King attack zones (quadratic) | Strength | `evaluate.rs` | Count weighted attackers near king, apply `danger^2/4096`. |
| **Low** | Opening book (larger) | Strength | `search.rs` | Polyglot support already integrated. Add larger opening books. |
| **Low** | Endgame tablebases | Strength | `search.rs` | Syzygy tablebase support for perfect play in 6-7 piece endings. |

### Board Move APIs

Two ways to apply a move:

| Method | Mechanism | Speed | Use in |
|--------|-----------|-------|--------|
| `execute_move(&Move) -> Board` | Clones entire board | ~10x slower | UI, UCI, book lookups, tests |
| `make_move(&Move) / unmake_move(&UndoInfo)` | In-place mutation | Fast | Search, perft, any hot path |

**Rule**: Never use `execute_move` in performance-critical code.

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

