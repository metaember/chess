use crate::types::CompactMove;

/// Transposition table entry flags
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TTFlag {
    /// Exact score (PV node)
    Exact,
    /// Score is a lower bound (cut node - failed high)
    LowerBound,
    /// Score is an upper bound (all node - failed low)
    UpperBound,
}

/// A single entry in the transposition table
#[derive(Debug, Clone, Copy)]
pub struct TTEntry {
    /// Zobrist hash of the position
    pub hash: u64,
    /// Depth of the search when this entry was stored
    pub depth: u8,
    /// Score of the position
    pub score: i32,
    /// Type of node/bound
    pub flag: TTFlag,
    /// Best move found (for move ordering)
    pub best_move: Option<CompactMove>,
}

impl TTEntry {
    pub fn empty() -> Self {
        TTEntry {
            hash: 0,
            depth: 0,
            score: 0,
            flag: TTFlag::Exact,
            best_move: None,
        }
    }
}

/// Transposition table for caching search results
pub struct TranspositionTable {
    /// Table entries (power of 2 size for fast modulo via bitmask)
    entries: Vec<TTEntry>,
    /// Bitmask for indexing (size - 1)
    size_mask: usize,
    /// Statistics
    pub hits: u64,
    pub stores: u64,
    pub collisions: u64,
}

impl TranspositionTable {
    /// Create a new transposition table with approximately `size_mb` megabytes of memory
    pub fn new(size_mb: usize) -> Self {
        let entry_size = std::mem::size_of::<TTEntry>();
        let num_entries = (size_mb * 1024 * 1024) / entry_size;
        // Round down to nearest power of 2
        let num_entries = num_entries.next_power_of_two() >> 1;
        let num_entries = num_entries.max(1024); // Minimum 1024 entries

        let entries = vec![TTEntry::empty(); num_entries];
        let size_mask = num_entries - 1;

        TranspositionTable {
            entries,
            size_mask,
            hits: 0,
            stores: 0,
            collisions: 0,
        }
    }

    /// Get the index for a given hash
    #[inline]
    fn index(&self, hash: u64) -> usize {
        (hash as usize) & self.size_mask
    }

    /// Probe the table for a position
    /// Returns Some((score, best_move)) if we can use the cached result
    pub fn probe(
        &mut self,
        hash: u64,
        depth: u8,
        alpha: i32,
        beta: i32,
    ) -> Option<(i32, Option<CompactMove>)> {
        let idx = self.index(hash);
        let entry = &self.entries[idx];

        // Check if this is the right position
        if entry.hash != hash {
            return None;
        }

        // Only use if searched to at least this depth
        if entry.depth < depth {
            return None;
        }

        self.hits += 1;

        // Check if we can use the score based on the bound type
        match entry.flag {
            TTFlag::Exact => Some((entry.score, entry.best_move)),
            TTFlag::LowerBound => {
                if entry.score >= beta {
                    Some((entry.score, entry.best_move))
                } else {
                    None
                }
            }
            TTFlag::UpperBound => {
                if entry.score <= alpha {
                    Some((entry.score, entry.best_move))
                } else {
                    None
                }
            }
        }
    }

    /// Get the best move from a previous search (for move ordering)
    pub fn get_best_move(&self, hash: u64) -> Option<CompactMove> {
        let idx = self.index(hash);
        let entry = &self.entries[idx];

        if entry.hash == hash {
            entry.best_move
        } else {
            None
        }
    }

    /// Store a search result in the table
    pub fn store(
        &mut self,
        hash: u64,
        depth: u8,
        score: i32,
        flag: TTFlag,
        best_move: Option<CompactMove>,
    ) {
        let idx = self.index(hash);
        let existing = &self.entries[idx];

        // Replacement strategy: always replace if:
        // 1. Empty entry
        // 2. Same position with lower depth
        // 3. Different position (collision)
        if existing.hash != 0 && existing.hash != hash {
            self.collisions += 1;
        }

        // Always replace for now (can add smarter replacement later)
        self.entries[idx] = TTEntry {
            hash,
            depth,
            score,
            flag,
            best_move,
        };
        self.stores += 1;
    }

    /// Clear all entries
    pub fn clear(&mut self) {
        for entry in &mut self.entries {
            *entry = TTEntry::empty();
        }
        self.hits = 0;
        self.stores = 0;
        self.collisions = 0;
    }

    /// Get the fill rate (percentage of entries used)
    pub fn fill_rate(&self) -> f64 {
        let used = self.entries.iter().filter(|e| e.hash != 0).count();
        (used as f64) / (self.entries.len() as f64) * 100.0
    }

    /// Get table size info
    pub fn info(&self) -> String {
        format!(
            "TT: {} entries, {:.1}% filled, {} hits, {} stores, {} collisions",
            self.entries.len(),
            self.fill_rate(),
            self.hits,
            self.stores,
            self.collisions
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{CompactMove, PieceType, MoveType};

    fn make_test_move() -> CompactMove {
        // e2e4 pawn double push
        CompactMove::new(12, 28, PieceType::Pawn, MoveType::DoublePawnPush, None)
    }

    #[test]
    fn test_tt_store_and_probe() {
        let mut tt = TranspositionTable::new(1);
        let hash = 0x123456789ABCDEF0u64;
        let mv = make_test_move();

        tt.store(hash, 4, 100, TTFlag::Exact, Some(mv));

        // Should find it
        let result = tt.probe(hash, 4, -1000, 1000);
        assert!(result.is_some());
        let (score, best_move) = result.unwrap();
        assert_eq!(score, 100);
        assert!(best_move.is_some());
    }

    #[test]
    fn test_tt_depth_requirement() {
        let mut tt = TranspositionTable::new(1);
        let hash = 0x123456789ABCDEF0u64;

        tt.store(hash, 3, 100, TTFlag::Exact, None);

        // Should not find it at higher depth
        assert!(tt.probe(hash, 4, -1000, 1000).is_none());

        // Should find it at same or lower depth
        assert!(tt.probe(hash, 3, -1000, 1000).is_some());
        assert!(tt.probe(hash, 2, -1000, 1000).is_some());
    }

    #[test]
    fn test_tt_lower_bound() {
        let mut tt = TranspositionTable::new(1);
        let hash = 0x123456789ABCDEF0u64;

        tt.store(hash, 4, 100, TTFlag::LowerBound, None);

        // Should return score if score >= beta
        assert!(tt.probe(hash, 4, -1000, 50).is_some());
        // Should not return if score < beta
        assert!(tt.probe(hash, 4, -1000, 150).is_none());
    }

    #[test]
    fn test_tt_upper_bound() {
        let mut tt = TranspositionTable::new(1);
        let hash = 0x123456789ABCDEF0u64;

        tt.store(hash, 4, 100, TTFlag::UpperBound, None);

        // Should return score if score <= alpha
        assert!(tt.probe(hash, 4, 150, 1000).is_some());
        // Should not return if score > alpha
        assert!(tt.probe(hash, 4, 50, 1000).is_none());
    }

    #[test]
    fn test_tt_clear() {
        let mut tt = TranspositionTable::new(1);
        let hash = 0x123456789ABCDEF0u64;

        tt.store(hash, 4, 100, TTFlag::Exact, None);
        assert!(tt.probe(hash, 4, -1000, 1000).is_some());

        tt.clear();
        assert!(tt.probe(hash, 4, -1000, 1000).is_none());
    }

}
