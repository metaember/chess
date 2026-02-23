import { useMemo } from 'react'
import type { MoveEval } from '@/lib/spectate-types'

interface EvalChartProps {
  evals: MoveEval[]
  currentPly: number | null
  onClickPly?: (ply: number) => void
  height?: number
}

const CLAMP = 600 // ±6 pawns in centipawns

export function EvalChart({ evals, currentPly, onClickPly, height = 120 }: EvalChartProps) {
  const { points, areaAbove, areaBelow } = useMemo(() => {
    if (evals.length === 0) return { points: '', areaAbove: '', areaBelow: '' }

    const w = 100 // viewBox percentage width
    const h = height
    const midY = h / 2
    const step = evals.length > 1 ? w / (evals.length - 1) : w

    const pts: [number, number][] = evals.map((ev, i) => {
      const x = evals.length > 1 ? i * step : w / 2
      let cp = 0
      if (ev.eval_mate !== null) {
        cp = ev.eval_mate > 0 ? CLAMP : -CLAMP
      } else if (ev.eval_cp !== null) {
        cp = Math.max(-CLAMP, Math.min(CLAMP, ev.eval_cp))
      }
      // Positive eval = white advantage = above center = lower y
      const y = midY - (cp / CLAMP) * midY
      return [x, y]
    })

    const polyline = pts.map(([x, y]) => `${x},${y}`).join(' ')

    // Area fill: split into above-center (white) and below-center (black)
    // Above center area (white advantage): from line up to midY
    const abovePts = pts.map(([x, y]) => `${x},${Math.min(y, midY)}`)
    const aboveArea = `${abovePts.join(' ')} ${pts[pts.length - 1][0]},${midY} ${pts[0][0]},${midY}`

    const belowPts = pts.map(([x, y]) => `${x},${Math.max(y, midY)}`)
    const belowArea = `${belowPts.join(' ')} ${pts[pts.length - 1][0]},${midY} ${pts[0][0]},${midY}`

    return { points: polyline, areaAbove: aboveArea, areaBelow: belowArea }
  }, [evals, height])

  if (evals.length === 0) {
    return (
      <div className="w-full bg-card/50 rounded border border-border flex items-center justify-center text-muted-foreground text-xs" style={{ height }}>
        No eval data yet
      </div>
    )
  }

  const viewW = 100
  const midY = height / 2
  const step = evals.length > 1 ? viewW / (evals.length - 1) : viewW

  // Current ply indicator
  const currentEvalIdx = currentPly !== null ? evals.findIndex(e => e.ply === currentPly) : -1
  const indicatorX = currentEvalIdx >= 0 ? currentEvalIdx * step : null

  const handleClick = (e: React.MouseEvent<SVGSVGElement>) => {
    if (!onClickPly || evals.length === 0) return
    const rect = e.currentTarget.getBoundingClientRect()
    const xPct = ((e.clientX - rect.left) / rect.width) * viewW
    const idx = Math.round(xPct / step)
    const clamped = Math.max(0, Math.min(evals.length - 1, idx))
    onClickPly(evals[clamped].ply)
  }

  return (
    <div className="w-full bg-card/50 rounded border border-border overflow-hidden">
      <svg
        viewBox={`0 0 ${viewW} ${height}`}
        preserveAspectRatio="none"
        className="w-full cursor-pointer"
        style={{ height }}
        onClick={handleClick}
      >
        {/* White advantage area (above center) */}
        <polygon points={areaAbove} fill="rgba(255,255,255,0.15)" />
        {/* Black advantage area (below center) */}
        <polygon points={areaBelow} fill="rgba(0,0,0,0.25)" />

        {/* Center line */}
        <line x1="0" y1={midY} x2={viewW} y2={midY} stroke="currentColor" strokeOpacity="0.2" strokeWidth="0.5" />

        {/* Eval line */}
        <polyline points={points} fill="none" stroke="currentColor" strokeWidth="1" strokeOpacity="0.6" vectorEffect="non-scaling-stroke" />

        {/* Current move indicator */}
        {indicatorX !== null && (
          <line x1={indicatorX} y1="0" x2={indicatorX} y2={height} stroke="hsl(var(--primary))" strokeWidth="1.5" vectorEffect="non-scaling-stroke" />
        )}
      </svg>
    </div>
  )
}
