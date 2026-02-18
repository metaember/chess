import { cn } from '@/lib/utils'
import { formatEval } from '@/lib/types'

interface EvalDisplayProps {
  score: number | null
  visible: boolean
}

export function EvalDisplay({ score, visible }: EvalDisplayProps) {
  if (!visible || score === null) return null

  const evalClass = score > 50 ? 'text-white' : score < -50 ? 'text-muted-foreground' : 'text-muted-foreground/70'

  return (
    <div className="text-center py-2">
      <div className={cn('text-3xl font-bold font-mono', evalClass)}>
        {formatEval(score)}
      </div>
      <div className="text-[10px] text-muted-foreground uppercase tracking-wider mt-1">
        Engine evaluation
      </div>
    </div>
  )
}
