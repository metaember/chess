import { cn } from '@/lib/utils'

interface StatusBarProps {
  status: string
  isCheck: boolean
  isGameOver: boolean
  isThinking: boolean
}

export function StatusBar({ status, isCheck, isGameOver, isThinking }: StatusBarProps) {
  return (
    <div
      className={cn(
        'text-center py-3 px-4 rounded-lg font-medium transition-colors',
        isGameOver && 'bg-primary/20 text-primary',
        isCheck && !isGameOver && 'bg-orange-500/20 text-orange-400',
        isThinking && 'text-muted-foreground',
        !isGameOver && !isCheck && !isThinking && 'bg-muted/50'
      )}
    >
      {status}
    </div>
  )
}
