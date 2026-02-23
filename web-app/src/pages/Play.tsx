import { useState, useCallback } from 'react'
import { ChessBoard } from '@/components/ChessBoard'
import { GameControls } from '@/components/GameControls'
import { StatusBar } from '@/components/StatusBar'
import { EvalDisplay } from '@/components/EvalDisplay'
import { EngineStats } from '@/components/EngineStats'
import { MoveHistory } from '@/components/MoveHistory'
import { PromotionModal } from '@/components/PromotionModal'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { useChessGame } from '@/lib/useChessGame'
import { BarChart3, Gauge, Download, ExternalLink } from 'lucide-react'
import { cn } from '@/lib/utils'
import { generatePgn } from '@/lib/types'

export function PlayPage() {
  const {
    state,
    setPlayerColor,
    setDepth,
    startNewGame,
    selectSquare,
    clearSelection,
    isPromotionMove,
    makeMove,
  } = useChessGame()

  const [showEval, setShowEval] = useState(false)
  const [showStats, setShowStats] = useState(false)
  const [promotionPending, setPromotionPending] = useState<{ from: string; to: string } | null>(null)

  const handleMove = useCallback((from: string, to: string) => {
    if (isPromotionMove(from, to)) {
      setPromotionPending({ from, to })
    } else {
      makeMove(from, to)
    }
  }, [isPromotionMove, makeMove])

  const handlePromotion = useCallback((piece: string) => {
    if (promotionPending) {
      makeMove(promotionPending.from, promotionPending.to, piece)
      setPromotionPending(null)
    }
  }, [promotionPending, makeMove])

  const handleSquareClick = useCallback((square: string) => {
    // Block interaction if game over or not player's turn
    if (state.board?.game_over) return
    if (state.board?.active_color !== state.playerColor) return

    if (state.selectedSquare === square) {
      // Clicking same square deselects
      clearSelection()
    } else if (state.board?.legal_moves.some(m => m.from === square)) {
      // Clicking own piece selects it
      selectSquare(square)
    } else {
      // Clicking elsewhere clears selection
      clearSelection()
    }
  }, [state.selectedSquare, state.board, state.playerColor, selectSquare, clearSelection])

  const handleExportPgn = useCallback(() => {
    const result = state.board?.game_over
      ? (state.board.game_over.includes('White') ? '1-0' : state.board.game_over.includes('Black') ? '0-1' : '1/2-1/2')
      : '*'
    const pgn = generatePgn(state.moveHistory, result)

    // Copy to clipboard
    navigator.clipboard.writeText(pgn).then(() => {
      alert('PGN copied to clipboard!')
    }).catch(() => {
      // Fallback: download as file
      const blob = new Blob([pgn], { type: 'text/plain' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = 'game.pgn'
      a.click()
      URL.revokeObjectURL(url)
    })
  }, [state.moveHistory, state.board?.game_over])

  const handleAnalyzeOnLichess = useCallback(() => {
    // Build moves string directly (no headers, no result)
    const moveText: string[] = []
    for (let i = 0; i < state.moveHistory.length; i += 2) {
      const moveNum = Math.floor(i / 2) + 1
      const whiteMove = state.moveHistory[i]?.algebraic || `${state.moveHistory[i]?.from}${state.moveHistory[i]?.to}`
      const blackMove = state.moveHistory[i + 1]
        ? (state.moveHistory[i + 1]?.algebraic || `${state.moveHistory[i + 1]?.from}${state.moveHistory[i + 1]?.to}`)
        : ''
      if (blackMove) {
        moveText.push(`${moveNum}. ${whiteMove} ${blackMove}`)
      } else {
        moveText.push(`${moveNum}. ${whiteMove}`)
      }
    }
    const moves = moveText.join(' ')
    const url = `https://lichess.org/analysis/pgn/${encodeURIComponent(moves)}`
    window.open(url, '_blank')
  }, [state.moveHistory])

  return (
    <div className="flex-1 flex flex-col max-w-7xl w-full mx-auto p-4 md:p-6 lg:p-8">
      {/* Header */}
      <header className="text-center py-3 md:py-4">
        <h1 className="text-2xl md:text-3xl font-bold tracking-tight">
          <span className="text-primary">Rust</span> Chess Engine
        </h1>
      </header>

      {/* Main content */}
      <div className="flex-1 flex flex-col lg:flex-row gap-6 lg:gap-8 items-center lg:items-start lg:justify-center">
        {/* Board section */}
        <div className="flex-shrink-0">
          <ChessBoard
            fen={state.board?.fen ?? null}
            isFlipped={state.isFlipped}
            selectedSquare={state.selectedSquare}
            lastMove={state.lastMove}
            legalMoves={state.board?.legal_moves ?? []}
            isCheck={state.board?.is_check ?? false}
            activeColor={state.board?.active_color ?? 'white'}
            playerColor={state.playerColor}
            gameOver={!!state.board?.game_over}
            onSquareClick={handleSquareClick}
            onMove={handleMove}
          />
        </div>

        {/* Control panel */}
        <div className="w-full lg:w-80 space-y-4">
          <StatusBar
            status={state.status}
            isCheck={state.board?.is_check ?? false}
            isGameOver={!!state.board?.game_over}
            isThinking={state.isThinking}
          />

          <EvalDisplay score={state.evalScore} visible={showEval} />
          <EngineStats stats={state.engineStats} visible={showStats} />

          <GameControls
            playerColor={state.playerColor}
            depth={state.depth}
            onPlayerColorChange={setPlayerColor}
            onDepthChange={setDepth}
            onNewGame={startNewGame}
            isThinking={state.isThinking}
          />

          <Card className="border-0 bg-card/50">
            <CardHeader className="p-4 pb-2">
              <div className="flex items-center justify-between">
                <CardTitle className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                  Moves
                </CardTitle>
                <div className="flex gap-1">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => setShowEval(!showEval)}
                    className={cn('h-7 px-2 text-xs', showEval && 'text-primary')}
                  >
                    <Gauge className="w-3 h-3 mr-1" />
                    Eval
                  </Button>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => setShowStats(!showStats)}
                    className={cn('h-7 px-2 text-xs', showStats && 'text-primary')}
                  >
                    <BarChart3 className="w-3 h-3 mr-1" />
                    Stats
                  </Button>
                </div>
              </div>
            </CardHeader>
            <CardContent className="p-4 pt-2 space-y-3">
              <MoveHistory moves={state.moveHistory} showEval={showEval} />
              {state.moveHistory.length > 0 && (
                <div className="flex gap-2">
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={handleExportPgn}
                    className="flex-1 text-xs"
                  >
                    <Download className="w-3 h-3 mr-1" />
                    Export PGN
                  </Button>
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={handleAnalyzeOnLichess}
                    className="flex-1 text-xs"
                  >
                    <ExternalLink className="w-3 h-3 mr-1" />
                    Lichess
                  </Button>
                </div>
              )}
            </CardContent>
          </Card>
        </div>
      </div>

      <PromotionModal
        open={!!promotionPending}
        isWhite={state.playerColor === 'white'}
        onSelect={handlePromotion}
        onClose={() => setPromotionPending(null)}
      />
    </div>
  )
}
