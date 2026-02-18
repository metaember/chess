import { useState, useCallback, useRef } from 'react'
import { newGame, makeMove as apiMakeMove } from './api'
import type { BoardResponse, MoveHistoryItem, EngineStats, MoveResponse } from './types'

export interface ChessGameState {
  gameId: string | null
  board: BoardResponse | null
  playerColor: 'white' | 'black'
  depth: number
  isFlipped: boolean
  selectedSquare: string | null
  lastMove: { from: string; to: string } | null
  moveHistory: MoveHistoryItem[]
  evalScore: number | null
  engineStats: EngineStats | null
  isThinking: boolean
  status: string
  gameStarted: boolean
}

const initialState: ChessGameState = {
  gameId: null,
  board: null,
  playerColor: 'white',
  depth: 4,
  isFlipped: false,
  selectedSquare: null,
  lastMove: null,
  moveHistory: [],
  evalScore: null,
  engineStats: null,
  isThinking: false,
  status: 'Select options and start a new game',
  gameStarted: false,
}

// Apply a move optimistically to update the FEN
function applyMoveToFen(fen: string, from: string, to: string, promotion?: string): string {
  const parts = fen.split(' ')
  const position = parts[0]
  const activeColor = parts[1]

  // Parse position into 2D array
  const board: (string | null)[][] = []
  const rows = position.split('/')
  for (const row of rows) {
    const boardRow: (string | null)[] = []
    for (const char of row) {
      if (/\d/.test(char)) {
        for (let i = 0; i < parseInt(char); i++) {
          boardRow.push(null)
        }
      } else {
        boardRow.push(char)
      }
    }
    board.push(boardRow)
  }

  // Convert algebraic to indices
  const fromFile = from.charCodeAt(0) - 'a'.charCodeAt(0)
  const fromRank = 8 - parseInt(from[1])
  const toFile = to.charCodeAt(0) - 'a'.charCodeAt(0)
  const toRank = 8 - parseInt(to[1])

  // Get the piece and move it
  let piece = board[fromRank][fromFile]
  board[fromRank][fromFile] = null

  // Handle promotion
  if (promotion && piece) {
    const isWhite = piece === piece.toUpperCase()
    piece = isWhite ? promotion.toUpperCase() : promotion.toLowerCase()
  }

  // Handle castling
  if (piece === 'K' || piece === 'k') {
    if (from === 'e1' && to === 'g1') {
      board[7][7] = null
      board[7][5] = 'R'
    } else if (from === 'e1' && to === 'c1') {
      board[7][0] = null
      board[7][3] = 'R'
    } else if (from === 'e8' && to === 'g8') {
      board[0][7] = null
      board[0][5] = 'r'
    } else if (from === 'e8' && to === 'c8') {
      board[0][0] = null
      board[0][3] = 'r'
    }
  }

  // Handle en passant
  if ((piece === 'P' || piece === 'p') && fromFile !== toFile && board[toRank][toFile] === null) {
    board[fromRank][toFile] = null
  }

  board[toRank][toFile] = piece

  // Convert back to FEN
  const newRows: string[] = []
  for (const row of board) {
    let fenRow = ''
    let emptyCount = 0
    for (const cell of row) {
      if (cell === null) {
        emptyCount++
      } else {
        if (emptyCount > 0) {
          fenRow += emptyCount.toString()
          emptyCount = 0
        }
        fenRow += cell
      }
    }
    if (emptyCount > 0) {
      fenRow += emptyCount.toString()
    }
    newRows.push(fenRow)
  }

  // Update active color
  const newActiveColor = activeColor === 'w' ? 'b' : 'w'

  // Simplified: just update position and active color, keep rest
  parts[0] = newRows.join('/')
  parts[1] = newActiveColor

  return parts.join(' ')
}

export function useChessGame() {
  const [state, setState] = useState<ChessGameState>(initialState)
  const previousBoardRef = useRef<BoardResponse | null>(null)

  const setPlayerColor = useCallback((color: 'white' | 'black') => {
    setState(s => ({ ...s, playerColor: color, isFlipped: color === 'black' }))
  }, [])

  const setDepth = useCallback((depth: number) => {
    setState(s => ({ ...s, depth }))
  }, [])

  const startNewGame = useCallback(async () => {
    setState(s => ({
      ...s,
      isThinking: true,
      status: 'Starting game...',
      selectedSquare: null,
      lastMove: null,
      moveHistory: [],
      evalScore: null,
      engineStats: null,
      gameStarted: true,
      isFlipped: s.playerColor === 'black',
    }))

    try {
      const data = await newGame(state.playerColor, state.depth)
      setState(s => ({
        ...s,
        gameId: data.game_id,
        board: data,
        evalScore: data.eval_score,
        isThinking: false,
        status: 'Your turn',
      }))
    } catch (error) {
      setState(s => ({
        ...s,
        isThinking: false,
        status: `Error: ${error instanceof Error ? error.message : 'Unknown error'}`,
        gameStarted: false,
      }))
    }
  }, [state.playerColor, state.depth])

  const selectSquare = useCallback((square: string) => {
    setState(s => ({ ...s, selectedSquare: square }))
  }, [])

  const clearSelection = useCallback(() => {
    setState(s => ({ ...s, selectedSquare: null }))
  }, [])

  const getLegalMovesForSquare = useCallback((square: string): MoveResponse[] => {
    if (!state.board) return []
    return state.board.legal_moves.filter(m => m.from === square)
  }, [state.board])

  const isLegalMove = useCallback((from: string, to: string): boolean => {
    if (!state.board) return false
    return state.board.legal_moves.some(m => m.from === from && m.to === to)
  }, [state.board])

  const isPromotionMove = useCallback((from: string, to: string): boolean => {
    if (!state.board) return false
    return state.board.legal_moves.some(m => m.from === from && m.to === to && m.promotion)
  }, [state.board])

  const makeMove = useCallback(async (from: string, to: string, promotion?: string) => {
    if (!state.gameId || !state.board) return

    // Save current board for potential rollback
    previousBoardRef.current = state.board

    // Apply optimistic update
    const optimisticFen = applyMoveToFen(state.board.fen, from, to, promotion)
    const optimisticBoard: BoardResponse = {
      ...state.board,
      fen: optimisticFen,
      active_color: state.board.active_color === 'white' ? 'black' : 'white',
      legal_moves: [], // Clear legal moves during thinking
    }

    setState(s => ({
      ...s,
      selectedSquare: null,
      lastMove: { from, to },
      board: optimisticBoard,
      moveHistory: [...s.moveHistory, { from, to, promotion }],
      isThinking: true,
      status: 'Engine thinking...',
    }))

    try {
      const data = await apiMakeMove(state.gameId, from, to, promotion)

      if (data.success) {
        setState(s => {
          const newHistory = [...s.moveHistory]
          if (newHistory.length > 0 && data.player_move_algebraic) {
            newHistory[newHistory.length - 1].algebraic = data.player_move_algebraic
          }

          if (data.engine_move) {
            newHistory.push({
              from: data.engine_move.from,
              to: data.engine_move.to,
              algebraic: data.engine_move.algebraic,
              eval: data.eval_score ?? undefined,
            })
          }

          return {
            ...s,
            board: data.board,
            moveHistory: newHistory,
            lastMove: data.engine_move
              ? { from: data.engine_move.from, to: data.engine_move.to }
              : s.lastMove,
            evalScore: data.eval_score,
            engineStats: data.engine_stats,
            isThinking: false,
            status: data.board.game_over ?? 'Your turn',
            gameStarted: !data.board.game_over,
          }
        })
      } else {
        // Rollback on failure
        setState(s => ({
          ...s,
          board: previousBoardRef.current ?? s.board,
          moveHistory: s.moveHistory.slice(0, -1),
          lastMove: null,
          isThinking: false,
          status: data.message,
        }))
      }
    } catch (error) {
      // Rollback on error
      setState(s => ({
        ...s,
        board: previousBoardRef.current ?? s.board,
        moveHistory: s.moveHistory.slice(0, -1),
        lastMove: null,
        isThinking: false,
        status: `Error: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }))
    }
  }, [state.gameId, state.board])

  return {
    state,
    setPlayerColor,
    setDepth,
    startNewGame,
    selectSquare,
    clearSelection,
    getLegalMovesForSquare,
    isLegalMove,
    isPromotionMove,
    makeMove,
  }
}
