export interface LiveGameSummary {
  game_id: string
  lichess_url: string
  bot_color: string
  opponent_name: string
  opponent_rating: number | null
  opponent_is_bot: boolean
  time_control: string
  speed: string
  provenance: string
  fen: string
  last_move_uci: string | null
  wtime_ms: number | null
  btime_ms: number | null
  eval_cp: number | null
  eval_mate: number | null
  depth: number | null
  started_at: string
}

export interface MoveEval {
  ply: number
  move_uci: string
  move_san: string
  eval_cp: number | null
  eval_mate: number | null
  depth: number | null
  pv: string | null
  nodes: number | null
  time_ms: number | null
  source: string | null
  clock_ms: number | null
}

export interface GameDetail {
  game_id: string
  lichess_url: string
  bot_color: string
  opponent_name: string
  opponent_rating: number | null
  opponent_is_bot: boolean
  time_control: string
  speed: string
  mode: string
  provenance: string
  status: string
  result: string
  termination: string | null
  started_at: string
  finished_at: string | null
  fen: string | null
  last_move_uci: string | null
  moves_uci: string | null
  wtime_ms: number | null
  btime_ms: number | null
  evals: MoveEval[]
}

export interface GameListItem {
  game_id: string
  lichess_url: string
  bot_color: string
  opponent_name: string
  opponent_rating: number | null
  opponent_is_bot: boolean
  time_control: string
  speed: string
  provenance: string
  result: string
  termination: string | null
  started_at: string
  finished_at: string | null
}

export function evalToWhiteCP(eval_cp: number | null, eval_mate: number | null): number | null {
  if (eval_mate !== null) {
    // Mate score: convert to large cp value from White's perspective
    // eval_mate is from White's perspective (positive = white mates)
    return eval_mate > 0 ? 10000 : -10000
  }
  if (eval_cp === null) return null
  // eval_cp is stored from White's perspective in the DB
  return eval_cp
}

export function formatEvalForDisplay(eval_cp: number | null, eval_mate: number | null): string {
  if (eval_mate !== null) {
    return eval_mate > 0 ? `+M${Math.abs(eval_mate)}` : `-M${Math.abs(eval_mate)}`
  }
  if (eval_cp === null) return '?'
  const pawns = eval_cp / 100
  return (pawns > 0 ? '+' : '') + pawns.toFixed(1)
}

export function provenanceLabel(provenance: string): string {
  switch (provenance) {
    case 'incoming_challenge': return 'Challenged'
    case 'outgoing_challenge': return 'Sought'
    case 'matchmaking': return 'Matchmaking'
    default: return provenance
  }
}

export function timeAgo(isoStr: string): string {
  const diff = Date.now() - new Date(isoStr).getTime()
  const mins = Math.floor(diff / 60000)
  if (mins < 1) return 'just now'
  if (mins < 60) return `${mins}m ago`
  const hrs = Math.floor(mins / 60)
  if (hrs < 24) return `${hrs}h ago`
  const days = Math.floor(hrs / 24)
  return `${days}d ago`
}
