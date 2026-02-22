//! Stack-allocated move list for high-performance search.
//!
//! This module provides `MoveList`, a fixed-size array that avoids heap allocations
//! during move generation. Each search node can create a MoveList on the stack
//! (about 1.5KB) instead of allocating a Vec on the heap.

use crate::types::CompactMove;

/// Maximum number of moves in any legal chess position.
/// The theoretical maximum is 218, but 256 provides nice alignment.
pub const MAX_MOVES: usize = 256;

/// Stack-allocated move list with optional scores for ordering.
///
/// This structure uses about 1.5KB of stack space:
/// - 1KB for moves (256 × 4 bytes)
/// - 512 bytes for scores (256 × 2 bytes)
///
/// The key design principles:
/// 1. No heap allocation - everything on the stack
/// 2. Copy-by-value iteration - `get()` returns a copy of the move (4 bytes)
/// 3. In-place operations - `retain()`, `swap()`, `pick_best()` modify in place
#[derive(Clone)]
pub struct MoveList {
    moves: [CompactMove; MAX_MOVES],
    scores: [i16; MAX_MOVES],
    count: usize,
}

impl MoveList {
    /// Create a new empty move list.
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            moves: [CompactMove::NONE; MAX_MOVES],
            scores: [0; MAX_MOVES],
            count: 0,
        }
    }

    /// Add a move to the list.
    #[inline(always)]
    pub fn push(&mut self, mv: CompactMove) {
        debug_assert!(self.count < MAX_MOVES, "MoveList overflow");
        self.moves[self.count] = mv;
        self.count += 1;
    }

    /// Add a move with an associated score.
    #[inline(always)]
    pub fn push_with_score(&mut self, mv: CompactMove, score: i16) {
        debug_assert!(self.count < MAX_MOVES, "MoveList overflow");
        self.moves[self.count] = mv;
        self.scores[self.count] = score;
        self.count += 1;
    }

    /// Get the number of moves in the list.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.count
    }

    /// Check if the list is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Get a move by index (copy by value - 4 bytes).
    /// This is the key to avoiding borrow checker issues in recursive search.
    #[inline(always)]
    pub fn get(&self, idx: usize) -> CompactMove {
        debug_assert!(idx < self.count, "MoveList index out of bounds");
        self.moves[idx]
    }

    /// Get a move's score by index.
    #[inline(always)]
    pub fn get_score(&self, idx: usize) -> i16 {
        debug_assert!(idx < self.count, "MoveList index out of bounds");
        self.scores[idx]
    }

    /// Set a move's score.
    #[inline(always)]
    pub fn set_score(&mut self, idx: usize, score: i16) {
        debug_assert!(idx < self.count, "MoveList index out of bounds");
        self.scores[idx] = score;
    }

    /// Swap two moves (and their scores).
    #[inline(always)]
    pub fn swap(&mut self, i: usize, j: usize) {
        self.moves.swap(i, j);
        self.scores.swap(i, j);
    }

    /// Clear the list (reset count, no need to clear data).
    #[inline(always)]
    pub fn clear(&mut self) {
        self.count = 0;
    }

    /// Pick the best move from position `start` to end and swap it to `start`.
    /// Uses selection sort - O(n) but better cache locality than full sort.
    /// Returns the best move.
    #[inline]
    pub fn pick_best(&mut self, start: usize) -> CompactMove {
        debug_assert!(start < self.count, "pick_best: start out of bounds");

        let mut best_idx = start;
        let mut best_score = self.scores[start];

        for i in (start + 1)..self.count {
            if self.scores[i] > best_score {
                best_score = self.scores[i];
                best_idx = i;
            }
        }

        if best_idx != start {
            self.swap(start, best_idx);
        }

        self.moves[start]
    }

    /// In-place filter: keep only moves for which the predicate returns true.
    /// Uses swap-and-truncate pattern for O(n) performance.
    #[inline]
    pub fn retain<F: FnMut(CompactMove) -> bool>(&mut self, mut f: F) {
        let mut write_idx = 0;
        for read_idx in 0..self.count {
            let mv = self.moves[read_idx];
            if f(mv) {
                if write_idx != read_idx {
                    self.moves[write_idx] = mv;
                    self.scores[write_idx] = self.scores[read_idx];
                }
                write_idx += 1;
            }
        }
        self.count = write_idx;
    }

    /// Check if the list contains a specific move.
    #[inline]
    pub fn contains(&self, mv: CompactMove) -> bool {
        for i in 0..self.count {
            if self.moves[i] == mv {
                return true;
            }
        }
        false
    }

    /// Sort all moves by score (descending). Use sparingly - pick_best is usually better.
    pub fn sort_by_score(&mut self) {
        // Simple insertion sort - good for small N and nearly-sorted data
        for i in 1..self.count {
            let mv = self.moves[i];
            let score = self.scores[i];
            let mut j = i;
            while j > 0 && self.scores[j - 1] < score {
                self.moves[j] = self.moves[j - 1];
                self.scores[j] = self.scores[j - 1];
                j -= 1;
            }
            self.moves[j] = mv;
            self.scores[j] = score;
        }
    }

    /// Iterate over all moves (by value).
    pub fn iter(&self) -> MoveListIter<'_> {
        MoveListIter {
            list: self,
            idx: 0,
        }
    }
}

impl Default for MoveList {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator over moves in a MoveList.
pub struct MoveListIter<'a> {
    list: &'a MoveList,
    idx: usize,
}

impl Iterator for MoveListIter<'_> {
    type Item = CompactMove;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.list.count {
            let mv = self.list.moves[self.idx];
            self.idx += 1;
            Some(mv)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.list.count - self.idx;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for MoveListIter<'_> {}

impl std::fmt::Debug for MoveList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MoveList")
            .field("count", &self.count)
            .field(
                "moves",
                &self.moves[..self.count.min(10)]
                    .iter()
                    .map(|m| m.to_uci())
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{MoveType, PieceType};

    #[test]
    fn test_movelist_basic() {
        let mut list = MoveList::new();
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);

        // Add some moves
        let m1 = CompactMove::new_quiet(12, 28, PieceType::Pawn); // e2e4
        let m2 = CompactMove::new_quiet(12, 20, PieceType::Pawn); // e2e3
        list.push_with_score(m1, 100);
        list.push_with_score(m2, 50);

        assert_eq!(list.len(), 2);
        assert!(!list.is_empty());
        assert_eq!(list.get(0), m1);
        assert_eq!(list.get(1), m2);
        assert_eq!(list.get_score(0), 100);
        assert_eq!(list.get_score(1), 50);
    }

    #[test]
    fn test_movelist_pick_best() {
        let mut list = MoveList::new();
        let m1 = CompactMove::new_quiet(0, 1, PieceType::Pawn);
        let m2 = CompactMove::new_quiet(0, 2, PieceType::Pawn);
        let m3 = CompactMove::new_quiet(0, 3, PieceType::Pawn);

        list.push_with_score(m1, 50);
        list.push_with_score(m2, 100);
        list.push_with_score(m3, 75);

        // Pick best from start - should move m2 to position 0
        let best = list.pick_best(0);
        assert_eq!(best, m2);
        assert_eq!(list.get(0), m2);
        assert_eq!(list.get_score(0), 100);
    }

    #[test]
    fn test_movelist_retain() {
        let mut list = MoveList::new();
        // Add some captures and quiet moves
        list.push(CompactMove::new_quiet(0, 8, PieceType::Pawn));
        list.push(CompactMove::new_capture(8, 17, PieceType::Pawn, PieceType::Pawn));
        list.push(CompactMove::new_quiet(1, 9, PieceType::Pawn));

        assert_eq!(list.len(), 3);

        // Keep only captures
        list.retain(|m| m.is_capture());

        assert_eq!(list.len(), 1);
        assert!(list.get(0).is_capture());
    }

    #[test]
    fn test_movelist_iter() {
        let mut list = MoveList::new();
        list.push(CompactMove::new_quiet(0, 8, PieceType::Pawn));
        list.push(CompactMove::new_quiet(1, 9, PieceType::Pawn));
        list.push(CompactMove::new_quiet(2, 10, PieceType::Pawn));

        let moves: Vec<_> = list.iter().collect();
        assert_eq!(moves.len(), 3);
    }
}
