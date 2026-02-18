import { useRef, useEffect } from 'react'
import type { MoveHistoryItem } from '@/lib/types'
import { formatEval } from '@/lib/types'
import { cn } from '@/lib/utils'

interface MoveHistoryProps {
  moves: MoveHistoryItem[]
  showEval: boolean
}

export function MoveHistory({ moves, showEval }: MoveHistoryProps) {
  const listRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (listRef.current) {
      listRef.current.scrollTop = listRef.current.scrollHeight
    }
  }, [moves])

  if (moves.length === 0) {
    return (
      <div className="text-center text-muted-foreground text-sm py-4">
        No moves yet
      </div>
    )
  }

  const movePairs: { num: number; white?: MoveHistoryItem; black?: MoveHistoryItem }[] = []
  for (let i = 0; i < moves.length; i += 2) {
    movePairs.push({
      num: Math.floor(i / 2) + 1,
      white: moves[i],
      black: moves[i + 1],
    })
  }

  return (
    <div
      ref={listRef}
      className="font-mono text-xs bg-muted/30 rounded-lg p-2 max-h-[160px] overflow-y-auto"
    >
      {movePairs.map(({ num, white, black }) => (
        <div
          key={num}
          className={cn(
            'flex py-1 border-b border-border/30 last:border-0',
            'items-center'
          )}
        >
          <span className="text-muted-foreground w-7">{num}.</span>
          <span className="w-14 text-foreground">
            {white?.algebraic || (white ? `${white.from}${white.to}` : '')}
          </span>
          <span className="w-14 text-foreground">
            {black?.algebraic || (black ? `${black.from}${black.to}` : '')}
          </span>
          {showEval && black?.eval !== undefined && (
            <span className="text-[10px] text-muted-foreground ml-auto">
              {formatEval(black.eval)}
            </span>
          )}
        </div>
      ))}
    </div>
  )
}
