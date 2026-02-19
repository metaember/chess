export interface BoardResponse {
  game_id: string
  fen: string
  board_display: string
  active_color: string
  is_check: boolean
  game_over: string | null
  legal_moves: MoveResponse[]
  eval_score: number | null
}

export interface MoveResponse {
  from: string
  to: string
  promotion: string | null
}

export interface EngineMoveInfo {
  from: string
  to: string
  algebraic: string
  human: string
}

export interface EngineStats {
  time_ms: number
  nodes_searched: number
  quiescent_nodes: number
  nodes_per_second: number
  depth: number
}

export interface MakeMoveResponse {
  success: boolean
  message: string
  board: BoardResponse
  engine_move: EngineMoveInfo | null
  player_move_algebraic: string | null
  eval_score: number | null
  engine_stats: EngineStats | null
}

export interface MoveHistoryItem {
  from: string
  to: string
  algebraic?: string
  promotion?: string
  eval?: number
}

export const PIECES: Record<string, string> = {
  'P': '♙', 'R': '♖', 'N': '♘', 'B': '♗', 'Q': '♕', 'K': '♔',
  'p': '♟', 'r': '♜', 'n': '♞', 'b': '♝', 'q': '♛', 'k': '♚'
}

// Lichess CBurnett piece images (GPLv2+)
export const PIECE_IMAGES: Record<string, string> = {
  'P': '/pieces/wP.svg', 'R': '/pieces/wR.svg', 'N': '/pieces/wN.svg',
  'B': '/pieces/wB.svg', 'Q': '/pieces/wQ.svg', 'K': '/pieces/wK.svg',
  'p': '/pieces/bP.svg', 'r': '/pieces/bR.svg', 'n': '/pieces/bN.svg',
  'b': '/pieces/bB.svg', 'q': '/pieces/bQ.svg', 'k': '/pieces/bK.svg'
}

export function parseFen(fen: string): Record<string, string> {
  const position = fen.split(' ')[0]
  const rows = position.split('/')
  const pieces: Record<string, string> = {}

  rows.forEach((row, rowIndex) => {
    let col = 0
    for (const char of row) {
      if (/\d/.test(char)) {
        col += parseInt(char)
      } else {
        const file = String.fromCharCode('a'.charCodeAt(0) + col)
        const rank = 8 - rowIndex
        pieces[file + rank] = char
        col++
      }
    }
  })

  return pieces
}

export function formatEval(score: number): string {
  const evalInPawns = score / 100
  if (Math.abs(evalInPawns) > 99) return evalInPawns > 0 ? '+M' : '-M'
  return (evalInPawns > 0 ? '+' : '') + evalInPawns.toFixed(1)
}

export function formatNumber(n: number): string {
  if (n >= 1000000) return (n / 1000000).toFixed(1) + 'M'
  if (n >= 1000) return Math.floor(n / 1000) + 'K'
  return n.toString()
}

export function generatePgn(moves: MoveHistoryItem[], result?: string): string {
  const date = new Date().toISOString().split('T')[0].replace(/-/g, '.')
  const headers = [
    '[Event "Rust Chess Engine Game"]',
    '[Site "Local"]',
    `[Date "${date}"]`,
    '[White "Player"]',
    '[Black "Engine"]',
    `[Result "${result || '*'}"]`,
    '',
  ].join('\n')

  const moveText: string[] = []
  for (let i = 0; i < moves.length; i += 2) {
    const moveNum = Math.floor(i / 2) + 1
    const whiteMove = moves[i]?.algebraic || `${moves[i]?.from}${moves[i]?.to}`
    const blackMove = moves[i + 1]
      ? (moves[i + 1]?.algebraic || `${moves[i + 1]?.from}${moves[i + 1]?.to}`)
      : ''

    if (blackMove) {
      moveText.push(`${moveNum}. ${whiteMove} ${blackMove}`)
    } else {
      moveText.push(`${moveNum}. ${whiteMove}`)
    }
  }

  return headers + moveText.join(' ') + (result ? ` ${result}` : '')
}

export function getLichessAnalysisUrl(fen: string): string {
  // Remove move counters for cleaner URL, encode the FEN
  const fenForUrl = fen.replace(/ /g, '_')
  return `https://lichess.org/analysis/standard/${fenForUrl}`
}
