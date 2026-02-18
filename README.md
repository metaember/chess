# chess
Chess bot written in rust

## Improvement Roadmap

### Speed Optimizations

- [ ] **Incremental PST Evaluation**

  Currently the evaluation function iterates all pieces on every leaf node, which is O(n_pieces) per eval call. Instead, maintain a running piece-square table score that updates incrementally during `make_move`/`unmake_move`. When a piece moves, subtract its old square value and add its new square value. When a piece is captured, subtract its value. This reduces evaluation to O(1) per move.

- [ ] **Remove `pieces: Vec<Piece>` Redundancy**

  The board maintains both bitboards AND a `pieces` vector. The `find_piece_index()` function does a linear scan O(n) on every move. Since bitboards already encode piece positions, remove the vector entirely and iterate pieces via `BitboardIter` over `piece_bb[color][piece_type]`. This eliminates the sync overhead and linear scans.

- [ ] **Lazy/Incremental Attack Map Updates**

  `recompute_attack_maps()` is called on every `make_move` and `unmake_move`, regenerating all sliding piece attacks from scratch. This is one of the hottest code paths. Instead, use lazy evaluation (only compute when needed for king safety) or incremental updates that only recalculate affected rays based on which squares changed.

- [ ] **Stack-Allocated Move Lists**

  `get_legal_moves()` allocates a new `Vec<Move>` per node, causing heap allocation pressure. Replace with `ArrayVec<Move, 256>` (stack-allocated) or a thread-local move list pool. The maximum legal moves in any chess position is 218, so a fixed-size array suffices.

- [ ] **Magic Bitboards for Sliding Pieces**

  Currently using classical ray attacks with blocker detection via `trailing_zeros`/`leading_zeros`. Magic bitboards use a precomputed lookup table indexed by `(occupancy * magic) >> shift`, reducing sliding piece attack generation to a single array lookup. Typically provides ~2x speedup for move generation.

### Strength Optimizations

- [ ] **Enable Endgame PST Tables**

  The codebase defines `PAWNS_END` and `KING_END` tables but `is_endgame` is hardcoded to `false`. Implement tapered evaluation with phase detection based on remaining material: `phase = (queens*4 + rooks*2 + bishops + knights) / 24`. Interpolate between middlegame and endgame scores: `score = (mg * phase + eg * (1-phase))`. This properly handles king activation and pawn advancement in endgames.

- [ ] **Killer Move Heuristic**

  Track the last 2 quiet moves that caused a beta cutoff at each ply depth. When ordering moves, prioritize these "killer moves" after the TT move and captures. Killers are often good in sibling nodes because similar positions have similar refutations. Store as `[[Option<Move>; 2]; MAX_PLY]`.

- [ ] **History Heuristic**

  Maintain a `[Color][From][To]` table that accumulates a bonus each time a quiet move causes a beta cutoff, scaled by depth². Use these history scores to order quiet moves after killers. This learns which moves are generally good across the search tree, improving move ordering over time.

- [ ] **Principal Variation Search (PVS)**

  After searching the first move with a full window, search remaining moves with a null window `(-alpha-1, -alpha)`. If the null window search fails high, re-search with the full window. Since most moves don't improve alpha, this saves the work of maintaining both bounds. Typically 10-20% node reduction.

- [ ] **Aspiration Windows**

  In iterative deepening, use a narrow window centered on the previous iteration's score (e.g., ±25 centipawns) instead of (-∞, +∞). If the search fails outside this window, re-search with a wider window. This prunes more aggressively when the score is stable between iterations.

- [ ] **Check Extensions**

  When a move gives check, extend the search depth by 1 ply. Checks are forcing moves that can dramatically change the evaluation, and extending them prevents horizon effects where the engine pushes bad news past its search horizon. Simple to implement: `if gives_check { depth += 1 }`.

- [ ] **Static Exchange Evaluation (SEE)**

  Before searching a capture in quiescence, simulate the full exchange sequence on that square to determine if the capture is winning, equal, or losing. Prune losing captures (e.g., QxP where the pawn is defended). This dramatically reduces quiescence search nodes without losing tactical accuracy.

- [ ] **Futility Pruning**

  At low depths (1-2 ply from leaf), if the static evaluation plus a margin is still below alpha, skip searching quiet moves entirely. The idea is that a quiet move is unlikely to improve the position by more than the margin. Typical margins: depth 1 = 200cp, depth 2 = 500cp.

- [ ] **Improved Evaluation Terms**

  The current eval is material + PST only. Add:
  - **Pawn structure**: Penalize doubled pawns (-10), isolated pawns (-20), bonus for passed pawns (+20 to +100 by rank)
  - **Mobility**: Count pseudo-legal moves per piece, bonus for more active pieces
  - **King safety**: Bonus for pawn shield in front of castled king, penalty for open files near king
  - **Bishop pair**: +30 bonus for having both bishops

### Implementation Priority

| Phase | Task | Type | Expected Impact |
|-------|------|------|-----------------|
| 1 | Enable endgame PST interpolation | Strength | Quick win, minimal code |
| 2 | Incremental PST evaluation | Speed | 15-20% faster |
| 3 | Remove `pieces` Vec | Speed | 10-15% faster |
| 4 | Killer moves + history heuristic | Strength | Much better move ordering |
| 5 | Lazy/incremental attack maps | Speed | 10-20% faster |
| 6 | PVS search | Strength | Better node efficiency |
| 7 | Check extensions | Strength | Avoids horizon effects |
| 8 | Magic bitboards | Speed | ~2x faster movegen |
| 9 | SEE pruning | Strength | Smaller quiescence tree |
| 10 | Futility pruning | Strength | Smaller search tree |
| 11 | Improved eval terms | Strength | Better positional play |



