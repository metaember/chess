import { useMemo, useCallback } from 'react'
import {
  ResponsiveContainer,
  AreaChart,
  Area,
  XAxis,
  YAxis,
  ReferenceLine,
  ReferenceDot,
  Tooltip,
} from 'recharts'
import type { MoveEval } from '@/lib/spectate-types'
import { formatEvalForDisplay } from '@/lib/spectate-types'

interface EvalChartProps {
  evals: MoveEval[]
  currentPly: number | null
  botColor?: string
  onClickPly?: (ply: number) => void
  height?: number
}

const CLAMP = 600 // ±6 pawns in centipawns

function evalToCp(ev: MoveEval): number {
  if (ev.eval_mate !== null) return ev.eval_mate > 0 ? CLAMP : -CLAMP
  if (ev.eval_cp !== null) return Math.max(-CLAMP, Math.min(CLAMP, ev.eval_cp))
  return 0
}

interface DataPoint {
  idx: number
  ply: number
  cp: number
  white: number
  black: number
  label: string
  san: string
}

export function EvalChart({ evals, currentPly, botColor, onClickPly, height = 100 }: EvalChartProps) {
  const isWhiteBot = botColor === 'white'
  const data = useMemo<DataPoint[]>(() => {
    return evals.map((ev, i) => {
      const cp = evalToCp(ev)
      return {
        idx: i,
        ply: ev.ply,
        cp,
        white: Math.max(0, cp),
        black: Math.min(0, cp),
        label: formatEvalForDisplay(ev.eval_cp, ev.eval_mate),
        san: ev.move_san,
      }
    })
  }, [evals])

  const currentDotIdx = useMemo(() => {
    if (currentPly === null || data.length === 0) return null
    const exact = data.findIndex(d => d.ply === currentPly)
    if (exact >= 0) return exact
    // Closest eval at or before currentPly
    let best = -1
    for (let i = 0; i < data.length; i++) {
      if (data[i].ply <= currentPly) best = i
    }
    return best >= 0 ? best : null
  }, [data, currentPly])

  const handleClick = useCallback((state: any) => {
    if (!onClickPly || !state?.activePayload?.[0]) return
    const point = state.activePayload[0].payload as DataPoint
    onClickPly(point.ply)
  }, [onClickPly])

  if (evals.length === 0) {
    return (
      <div className="w-full bg-card/50 rounded border border-border flex items-center justify-center text-muted-foreground text-xs" style={{ height }}>
        No eval data yet
      </div>
    )
  }

  return (
    <div className="w-full bg-card/50 rounded border border-border overflow-hidden">
      <ResponsiveContainer width="100%" height={height}>
        <AreaChart data={data} onClick={handleClick} margin={{ top: 2, right: 2, bottom: 2, left: 2 }}>
          <defs>
            <linearGradient id="whiteGrad" x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor="rgba(255,255,255,0.3)" />
              <stop offset="100%" stopColor="rgba(255,255,255,0.05)" />
            </linearGradient>
            <linearGradient id="blackGrad" x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor="rgba(0,0,0,0.05)" />
              <stop offset="100%" stopColor="rgba(0,0,0,0.4)" />
            </linearGradient>
          </defs>
          <XAxis dataKey="idx" hide />
          <YAxis domain={[-CLAMP, CLAMP]} hide />
          <ReferenceLine y={0} stroke="rgba(255,255,255,0.4)" strokeWidth={1} />
          <Tooltip
            content={({ active, payload }) => {
              if (!active || !payload?.[0]) return null
              const d = payload[0].payload as DataPoint
              const moveNum = Math.floor(d.ply / 2) + 1
              const side = d.ply % 2 === 0 ? '.' : '...'
              const botWinning = isWhiteBot ? d.cp > 50 : d.cp < -50
              const botLosing = isWhiteBot ? d.cp < -50 : d.cp > 50
              const evalColor = botWinning ? 'text-green-500' : botLosing ? 'text-red-500' : 'text-foreground'
              return (
                <div className="bg-popover border border-border rounded px-2 py-1 text-xs shadow-lg">
                  <span className="text-muted-foreground">{moveNum}{side}</span>{' '}
                  <span className="font-medium">{d.san}</span>{' '}
                  <span className={`font-mono font-semibold ${evalColor}`}>{d.label}</span>
                </div>
              )
            }}
          />
          <Area
            type="monotone"
            dataKey="white"
            stroke="none"
            fill="url(#whiteGrad)"
            baseLine={0}
            isAnimationActive={false}
          />
          <Area
            type="monotone"
            dataKey="black"
            stroke="none"
            fill="url(#blackGrad)"
            baseLine={0}
            isAnimationActive={false}
          />
          <Area
            type="monotone"
            dataKey="cp"
            stroke="rgba(255,255,255,0.85)"
            strokeWidth={2}
            fill="none"
            isAnimationActive={false}
            dot={false}
            activeDot={{ r: 4, fill: 'hsl(var(--primary))', stroke: 'rgba(0,0,0,0.5)', strokeWidth: 1 }}
          />
          {currentDotIdx !== null && data[currentDotIdx] && (
            <ReferenceDot
              x={data[currentDotIdx].idx}
              y={data[currentDotIdx].cp}
              r={5}
              fill="hsl(var(--primary))"
              stroke="rgba(0,0,0,0.5)"
              strokeWidth={1}
            />
          )}
        </AreaChart>
      </ResponsiveContainer>
    </div>
  )
}
