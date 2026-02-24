import { useMemo } from 'react'
import type { MoveEval } from '@/lib/spectate-types'

interface EvalChartProps {
  evals: MoveEval[]
  currentPly: number | null
  onClickPly?: (ply: number) => void
  height?: number
}

const CLAMP = 600 // ±6 pawns in centipawns

function evalToCp(ev: MoveEval): number {
  if (ev.eval_mate !== null) return ev.eval_mate > 0 ? CLAMP : -CLAMP
  if (ev.eval_cp !== null) return Math.max(-CLAMP, Math.min(CLAMP, ev.eval_cp))
  return 0
}

export function EvalChart({ evals, currentPly, onClickPly, height = 120 }: EvalChartProps) {
  const { points, areaAbove, areaBelow, pts } = useMemo(() => {
    if (evals.length === 0) return { points: '', areaAbove: '', areaBelow: '', pts: [] as [number, number][] }

    const w = 100
    const h = height
    const midY = h / 2
    const step = evals.length > 1 ? w / (evals.length - 1) : w

    const pts: [number, number][] = evals.map((ev, i) => {
      const x = evals.length > 1 ? i * step : w / 2
      const cp = evalToCp(ev)
      const y = midY - (cp / CLAMP) * midY
      return [x, y]
    })

    const polyline = pts.map(([x, y]) => `${x},${y}`).join(' ')

    const abovePts = pts.map(([x, y]) => `${x},${Math.min(y, midY)}`)
    const aboveArea = `${abovePts.join(' ')} ${pts[pts.length - 1][0]},${midY} ${pts[0][0]},${midY}`

    const belowPts = pts.map(([x, y]) => `${x},${Math.max(y, midY)}`)
    const belowArea = `${belowPts.join(' ')} ${pts[pts.length - 1][0]},${midY} ${pts[0][0]},${midY}`

    return { points: polyline, areaAbove: aboveArea, areaBelow: belowArea, pts }
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

  // Current ply dot
  const currentEvalIdx = currentPly !== null ? evals.findIndex(e => e.ply === currentPly) : -1
  // If no exact match, find closest eval at or before currentPly
  const dotIdx = currentEvalIdx >= 0
    ? currentEvalIdx
    : currentPly !== null
      ? evals.reduce((best, ev, i) => ev.ply <= currentPly ? i : best, -1)
      : -1
  const dotX = dotIdx >= 0 ? pts[dotIdx]?.[0] : null
  const dotY = dotIdx >= 0 ? pts[dotIdx]?.[1] : null

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

        {/* Zero line */}
        <line x1="0" y1={midY} x2={viewW} y2={midY}
          stroke="rgba(255,255,255,0.4)" strokeWidth="1" vectorEffect="non-scaling-stroke" />

        {/* Eval line */}
        <polyline points={points} fill="none"
          stroke="rgba(255,255,255,0.85)" strokeWidth="2" vectorEffect="non-scaling-stroke" />

        {/* Current move dot */}
        {dotX !== null && dotY !== null && (
          <circle cx={dotX} cy={dotY} r="4"
            fill="hsl(var(--primary))" stroke="rgba(0,0,0,0.5)" strokeWidth="1"
            vectorEffect="non-scaling-stroke" />
        )}
      </svg>
    </div>
  )
}
