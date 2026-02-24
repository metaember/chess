import { formatEvalForDisplay } from '@/lib/spectate-types'

interface EvalBarProps {
  evalCp: number | null
  evalMate: number | null
  height?: number
}

export function EvalBar({ evalCp, evalMate, height = 480 }: EvalBarProps) {
  // Convert eval to a percentage for the white portion (bottom)
  // 0cp = 50%, clamp at ±600cp for visual range
  let whitePct = 50
  if (evalMate !== null) {
    whitePct = evalMate > 0 ? 96 : 4
  } else if (evalCp !== null) {
    // Sigmoid-like mapping: ±600cp → ~4%-96%
    const clamped = Math.max(-600, Math.min(600, evalCp))
    whitePct = 50 + (clamped / 600) * 46
  }

  const displayText = formatEvalForDisplay(evalCp, evalMate)
  const isWhiteAdvantage = (evalCp !== null && evalCp > 0) || (evalMate !== null && evalMate > 0)

  return (
    <div
      className="relative w-8 rounded overflow-hidden flex-shrink-0 border border-border"
      style={{ height }}
    >
      {/* Black portion (top) */}
      <div
        className="absolute top-0 left-0 right-0 bg-zinc-800 transition-all duration-500 ease-out"
        style={{ height: `${100 - whitePct}%` }}
      />
      {/* White portion (bottom) */}
      <div
        className="absolute bottom-0 left-0 right-0 bg-zinc-100 transition-all duration-500 ease-out"
        style={{ height: `${whitePct}%` }}
      />
      {/* Center line (50% mark) */}
      <div className="absolute left-0 right-0 top-1/2 h-px bg-muted-foreground/40" />
      {/* Score label */}
      <div
        className={`absolute left-0 right-0 text-center text-[9px] font-bold leading-tight ${
          isWhiteAdvantage ? 'bottom-1 text-zinc-800' : 'top-1 text-zinc-200'
        }`}
      >
        {displayText}
      </div>
    </div>
  )
}
