import { useState, useEffect, useCallback, useRef, useMemo } from 'react'
import { Chess } from 'chess.js'
import { ChessBoard } from '@/components/ChessBoard'
import { EvalBar } from '@/components/EvalBar'
import { EvalChart } from '@/components/EvalChart'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { ExternalLink, ChevronLeft, ChevronRight, ChevronsLeft, ChevronsRight, Radio } from 'lucide-react'
import { useParams } from 'react-router-dom'
import { cn } from '@/lib/utils'
import type { GameDetail, GameListItem, LiveGameSummary, MoveEval } from '@/lib/spectate-types'
import { formatEvalForDisplay, provenanceLabel, timeAgo } from '@/lib/spectate-types'

const API_BASE = ''

function useLiveGames() {
  const [liveGames, setLiveGames] = useState<LiveGameSummary[]>([])
  useEffect(() => {
    const poll = () => {
      fetch(`${API_BASE}/api/live-games`)
        .then(r => r.ok ? r.json() : [])
        .then(setLiveGames)
        .catch(() => {})
    }
    poll()
    const id = setInterval(poll, 2000)
    return () => clearInterval(id)
  }, [])
  return liveGames
}

function useRecentGames() {
  const [games, setGames] = useState<GameListItem[]>([])
  const [loading, setLoading] = useState(true)
  useEffect(() => {
    fetch(`${API_BASE}/api/games?limit=50`)
      .then(r => r.ok ? r.json() : [])
      .then(g => { setGames(g); setLoading(false) })
      .catch(() => setLoading(false))
  }, [])
  return { games, loading }
}

function useGameDetail(gameId: string | null) {
  const [detail, setDetail] = useState<GameDetail | null>(null)
  const isLive = detail?.status === 'playing'

  useEffect(() => {
    if (!gameId) { setDetail(null); return }
    let cancelled = false

    const poll = () => {
      fetch(`${API_BASE}/api/games/${gameId}`)
        .then(r => r.ok ? r.json() : null)
        .then(d => { if (!cancelled) setDetail(d) })
        .catch(() => {})
    }
    poll()
    const id = setInterval(poll, isLive ? 1000 : 5000)
    return () => { cancelled = true; clearInterval(id) }
  }, [gameId, isLive])

  return detail
}

export default function Spectate() {
  const { gameId: routeGameId } = useParams<{ gameId?: string }>()
  const liveGames = useLiveGames()
  const { games: recentGames, loading: recentLoading } = useRecentGames()

  const [selectedGameId, setSelectedGameId] = useState<string | null>(routeGameId ?? null)
  const [viewPly, setViewPly] = useState<number | null>(null) // null = latest
  const autoFollowRef = useRef(true)

  // Sync route param into state
  useEffect(() => {
    if (routeGameId) setSelectedGameId(routeGameId)
  }, [routeGameId])

  // Auto-select the live game, or most recent finished game
  useEffect(() => {
    if (selectedGameId || routeGameId) return // User has picked a game or came via URL
    if (liveGames.length > 0) {
      setSelectedGameId(liveGames[0].game_id)
    } else if (recentGames.length > 0 && !recentLoading) {
      setSelectedGameId(recentGames[0].game_id)
    }
  }, [liveGames, recentGames, recentLoading, selectedGameId, routeGameId])

  // Auto-switch to new live game (disabled when navigated via direct URL)
  useEffect(() => {
    if (liveGames.length > 0 && autoFollowRef.current && !routeGameId) {
      setSelectedGameId(liveGames[0].game_id)
      setViewPly(null)
    }
  }, [liveGames])

  const detail = useGameDetail(selectedGameId)
  const isLive = detail?.status === 'playing'

  const evals = detail?.evals ?? []

  // Build per-ply positions from the UCI move string
  const positions = useMemo(() => {
    const movesStr = detail?.moves_uci
    if (!movesStr) return []
    const moves = movesStr.trim().split(/\s+/).filter(Boolean)
    const chess = new Chess()
    const result: { ply: number; fen: string; moveUci: string }[] = []
    for (let i = 0; i < moves.length; i++) {
      try {
        chess.move({ from: moves[i].slice(0, 2), to: moves[i].slice(2, 4), promotion: moves[i][4] as any })
        result.push({ ply: i, fen: chess.fen(), moveUci: moves[i] })
      } catch {
        break
      }
    }
    return result
  }, [detail?.moves_uci])

  const totalPlies = positions.length
  const latestPly = totalPlies > 0 ? totalPlies - 1 : null
  const displayPly = viewPly ?? latestPly

  // Get eval for the display ply (evals only have bot moves, find closest)
  const currentEval = evals.find(e => e.ply === displayPly)
  // If no exact match, find the most recent eval at or before this ply
  const effectiveEval = currentEval ?? [...evals].reverse().find(e => e.ply <= (displayPly ?? Infinity))
  const evalCp = effectiveEval?.eval_cp ?? null
  const evalMate = effectiveEval?.eval_mate ?? null

  // Navigation (step through all plies, not just evals)
  const goFirst = useCallback(() => { if (totalPlies > 0) setViewPly(0) }, [totalPlies])
  const goPrev = useCallback(() => {
    if (totalPlies === 0) return
    const cur = displayPly ?? totalPlies - 1
    if (cur > 0) setViewPly(cur - 1)
  }, [totalPlies, displayPly])
  const goNext = useCallback(() => {
    if (totalPlies === 0) return
    const cur = displayPly ?? totalPlies - 1
    if (cur < totalPlies - 1) setViewPly(cur + 1)
    else setViewPly(null)
  }, [totalPlies, displayPly])
  const goLast = useCallback(() => setViewPly(null), [])

  const handleChartClick = useCallback((ply: number) => setViewPly(ply), [])

  const handleGameSelect = useCallback((gameId: string) => {
    setSelectedGameId(gameId)
    setViewPly(null)
    autoFollowRef.current = false
  }, [])

  // Keyboard navigation
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'ArrowLeft') { e.preventDefault(); goPrev() }
      if (e.key === 'ArrowRight') { e.preventDefault(); goNext() }
      if (e.key === 'Home') { e.preventDefault(); goFirst() }
      if (e.key === 'End') { e.preventDefault(); goLast() }
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener('keydown', handler)
  }, [goFirst, goPrev, goNext, goLast])

  // Determine the FEN and last move from position array
  const currentPosition = displayPly !== null ? positions[displayPly] : null
  const displayFen = currentPosition?.fen ?? detail?.fen ?? 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
  const lastMoveUci = currentPosition?.moveUci ?? detail?.last_move_uci
  const lastMove = lastMoveUci ? { from: lastMoveUci.slice(0, 2), to: lastMoveUci.slice(2, 4) } : null

  // Board orientation: show from bot's perspective
  const isFlipped = detail?.bot_color === 'black'

  return (
    <div className="min-h-screen min-h-dvh flex flex-col">
      <div className="flex-1 flex flex-col max-w-7xl w-full mx-auto p-4 md:p-6 lg:p-8">
        {/* Header */}
        <header className="text-center py-3 md:py-4">
          <h1 className="text-2xl md:text-3xl font-bold tracking-tight">
            <span className="text-primary">Rust</span> Chess <span className="text-muted-foreground">Spectator</span>
          </h1>
        </header>

        <div className="flex-1 flex flex-col lg:flex-row gap-6 lg:gap-8 items-start lg:justify-center">
          {/* Board + eval bar */}
          <div className="flex gap-2 flex-shrink-0">
            <EvalBar evalCp={evalCp} evalMate={evalMate} />
            <div>
              <ChessBoard
                fen={displayFen}
                isFlipped={isFlipped}
                selectedSquare={null}
                lastMove={lastMove}
                legalMoves={[]}
                isCheck={false}
                activeColor="white"
                playerColor="white"
                gameOver={!isLive}
                onSquareClick={() => {}}
                onMove={() => {}}
              />

              {/* Eval chart */}
              <div className="mt-2">
                <EvalChart
                  evals={evals}
                  currentPly={displayPly}
                  onClickPly={handleChartClick}
                  height={80}
                />
              </div>

              {/* Navigation controls */}
              <div className="flex items-center justify-center gap-1 mt-2">
                <Button variant="ghost" size="sm" onClick={goFirst} className="h-8 w-8 p-0">
                  <ChevronsLeft className="w-4 h-4" />
                </Button>
                <Button variant="ghost" size="sm" onClick={goPrev} className="h-8 w-8 p-0">
                  <ChevronLeft className="w-4 h-4" />
                </Button>
                <span className="text-xs text-muted-foreground px-2 min-w-[60px] text-center">
                  {displayPly !== null ? `Move ${Math.floor(displayPly / 2) + 1}` : '—'}
                </span>
                <Button variant="ghost" size="sm" onClick={goNext} className="h-8 w-8 p-0">
                  <ChevronRight className="w-4 h-4" />
                </Button>
                <Button variant="ghost" size="sm" onClick={goLast} className="h-8 w-8 p-0">
                  <ChevronsRight className="w-4 h-4" />
                </Button>
              </div>
            </div>
          </div>

          {/* Side panel */}
          <div className="w-full lg:w-80 space-y-4">
            {/* Game info */}
            {detail && (
              <Card className="border-0 bg-card/50">
                <CardHeader className="p-4 pb-2">
                  <div className="flex items-center justify-between">
                    <CardTitle className="text-sm font-medium">
                      {isLive && <Radio className="w-3 h-3 inline mr-1 text-green-500 animate-pulse" />}
                      vs {detail.opponent_name}
                      {detail.opponent_rating && <span className="text-muted-foreground ml-1">({detail.opponent_rating})</span>}
                    </CardTitle>
                    <a href={detail.lichess_url} target="_blank" rel="noopener noreferrer">
                      <ExternalLink className="w-3.5 h-3.5 text-muted-foreground hover:text-primary" />
                    </a>
                  </div>
                </CardHeader>
                <CardContent className="p-4 pt-1">
                  <div className="flex gap-2 text-xs text-muted-foreground flex-wrap">
                    <span>{detail.time_control}</span>
                    <span>·</span>
                    <span>{detail.mode}</span>
                    <span>·</span>
                    <span>{provenanceLabel(detail.provenance)}</span>
                    {!isLive && (
                      <>
                        <span>·</span>
                        <span className="font-medium text-foreground">{detail.result}</span>
                      </>
                    )}
                  </div>
                  {/* Clock display for live games */}
                  {isLive && detail.wtime_ms != null && detail.btime_ms != null && (
                    <div className="flex justify-between mt-2 text-sm font-mono">
                      <span className={cn(detail.bot_color === 'white' ? 'text-primary' : 'text-muted-foreground')}>
                        W: {formatClock(detail.wtime_ms)}
                      </span>
                      <span className={cn(detail.bot_color === 'black' ? 'text-primary' : 'text-muted-foreground')}>
                        B: {formatClock(detail.btime_ms)}
                      </span>
                    </div>
                  )}
                </CardContent>
              </Card>
            )}

            {/* Current eval display */}
            {detail && (
              <EvalDisplay evalCp={evalCp} evalMate={evalMate} botColor={detail.bot_color} />
            )}

            {/* Move list with evals */}
            <Card className="border-0 bg-card/50">
              <CardHeader className="p-4 pb-2">
                <CardTitle className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                  Moves
                </CardTitle>
              </CardHeader>
              <CardContent className="p-4 pt-1">
                <MoveList evals={evals} currentPly={displayPly} onClickPly={handleChartClick} />
              </CardContent>
            </Card>

            {/* Game list sidebar */}
            <Card className="border-0 bg-card/50">
              <CardHeader className="p-4 pb-2">
                <CardTitle className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                  Recent Games
                </CardTitle>
              </CardHeader>
              <CardContent className="p-4 pt-1 max-h-64 overflow-y-auto space-y-1">
                {liveGames.map(g => (
                  <GameRow key={g.game_id} gameId={g.game_id} opponent={g.opponent_name}
                    rating={g.opponent_rating} result="*" botColor={g.bot_color} timeControl={g.time_control}
                    time={g.started_at} isLive selected={g.game_id === selectedGameId}
                    onClick={handleGameSelect} />
                ))}
                {recentGames.filter(g => !liveGames.some(l => l.game_id === g.game_id)).map(g => (
                  <GameRow key={g.game_id} gameId={g.game_id} opponent={g.opponent_name}
                    rating={g.opponent_rating} result={g.result} botColor={g.bot_color} timeControl={g.time_control}
                    time={g.started_at} selected={g.game_id === selectedGameId}
                    onClick={handleGameSelect} />
                ))}
                {recentLoading && <div className="text-xs text-muted-foreground text-center py-2">Loading...</div>}
                {!recentLoading && recentGames.length === 0 && liveGames.length === 0 && (
                  <div className="text-xs text-muted-foreground text-center py-4">
                    No games yet. Start the bot to see games here.
                  </div>
                )}
              </CardContent>
            </Card>
          </div>
        </div>
      </div>
    </div>
  )
}

function formatClock(ms: number): string {
  const totalSec = Math.max(0, Math.floor(ms / 1000))
  const min = Math.floor(totalSec / 60)
  const sec = totalSec % 60
  return `${min}:${sec.toString().padStart(2, '0')}`
}

function MoveList({ evals, currentPly, onClickPly }: {
  evals: MoveEval[]
  currentPly: number | null
  onClickPly: (ply: number) => void
}) {
  const containerRef = useRef<HTMLDivElement>(null)

  // Auto-scroll to current move
  useEffect(() => {
    if (containerRef.current && currentPly !== null) {
      const el = containerRef.current.querySelector(`[data-ply="${currentPly}"]`)
      el?.scrollIntoView({ block: 'nearest' })
    }
  }, [currentPly])

  if (evals.length === 0) {
    return <div className="text-xs text-muted-foreground text-center py-2">No moves yet</div>
  }

  // Group evals by move number (they're only bot moves, but show with move numbers)
  return (
    <div ref={containerRef} className="max-h-48 overflow-y-auto space-y-0.5 font-mono text-xs">
      {evals.map(ev => {
        const moveNum = Math.floor(ev.ply / 2) + 1
        const isWhite = ev.ply % 2 === 0
        const isActive = ev.ply === currentPly
        const evalStr = formatEvalForDisplay(ev.eval_cp, ev.eval_mate)

        return (
          <div
            key={ev.ply}
            data-ply={ev.ply}
            onClick={() => onClickPly(ev.ply)}
            className={cn(
              'flex items-center gap-2 px-2 py-0.5 rounded cursor-pointer hover:bg-muted/50',
              isActive && 'bg-primary/10 text-primary'
            )}
          >
            <span className="text-muted-foreground w-8 text-right">
              {moveNum}{isWhite ? '.' : '...'}
            </span>
            <span className="flex-1">{ev.move_san}</span>
            <span className={cn(
              'text-right w-12',
              ev.eval_cp !== null && ev.eval_cp > 50 ? 'text-zinc-300' :
              ev.eval_cp !== null && ev.eval_cp < -50 ? 'text-zinc-500' :
              'text-muted-foreground'
            )}>
              {ev.source === 'book' ? 'book' : evalStr}
            </span>
            {ev.depth && ev.source !== 'book' && (
              <span className="text-muted-foreground/50 w-6 text-right">d{ev.depth}</span>
            )}
          </div>
        )
      })}
    </div>
  )
}

function EvalDisplay({ evalCp, evalMate, botColor }: {
  evalCp: number | null
  evalMate: number | null
  botColor: string
}) {
  const evalStr = formatEvalForDisplay(evalCp, evalMate)
  const isBot = botColor === 'white'
  // Positive cp = white advantage. Bot winning if (white advantage && bot is white) or (black advantage && bot is black)
  const botWinning = evalCp !== null
    ? (isBot ? evalCp > 50 : evalCp < -50)
    : evalMate !== null
      ? (isBot ? evalMate > 0 : evalMate < 0)
      : false
  const botLosing = evalCp !== null
    ? (isBot ? evalCp < -50 : evalCp > 50)
    : evalMate !== null
      ? (isBot ? evalMate < 0 : evalMate > 0)
      : false

  return (
    <div className="text-center py-2">
      <span className={cn(
        'text-3xl font-bold font-mono',
        botWinning ? 'text-green-500' :
        botLosing ? 'text-red-500' :
        'text-foreground'
      )}>
        {evalStr}
      </span>
    </div>
  )
}

function GameRow({ gameId, opponent, rating, result, botColor, timeControl, time, isLive, selected, onClick }: {
  gameId: string
  opponent: string
  rating: number | null
  result: string
  botColor: string
  timeControl: string
  time: string
  isLive?: boolean
  selected: boolean
  onClick: (id: string) => void
}) {
  const isWin = (botColor === 'white' && result === '1-0') || (botColor === 'black' && result === '0-1')
  const isLoss = (botColor === 'white' && result === '0-1') || (botColor === 'black' && result === '1-0')

  return (
    <button
      onClick={() => onClick(gameId)}
      className={cn(
        'w-full text-left flex items-center gap-1.5 px-2 py-1.5 rounded text-xs hover:bg-muted/50 transition-colors whitespace-nowrap',
        selected && 'bg-primary/10'
      )}
    >
      {isLive && <Radio className="w-2.5 h-2.5 text-green-500 animate-pulse flex-shrink-0" />}
      <span className="flex-1 truncate min-w-0">{opponent}{rating ? ` (${rating})` : ''}</span>
      <span className="text-muted-foreground flex-shrink-0">{timeControl}</span>
      <span className={cn(
        'font-medium w-10 text-right flex-shrink-0',
        isLive ? 'text-green-500' :
        isWin ? 'text-green-500' :
        isLoss ? 'text-red-500' :
        'text-foreground'
      )}>
        {isLive ? 'LIVE' : result}
      </span>
      <span className="text-muted-foreground/50 w-10 text-right flex-shrink-0">{timeAgo(time)}</span>
    </button>
  )
}
