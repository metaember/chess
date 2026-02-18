import * as React from 'react'
import * as SelectPrimitive from '@radix-ui/react-select'
import { Check } from 'lucide-react'
import { Button } from '@/components/ui/button'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Card, CardContent } from '@/components/ui/card'
import { cn } from '@/lib/utils'

interface GameControlsProps {
  playerColor: 'white' | 'black'
  depth: number
  onPlayerColorChange: (color: 'white' | 'black') => void
  onDepthChange: (depth: number) => void
  onNewGame: () => void
  isThinking: boolean
}

// Custom select item that shows description in dropdown but only name in trigger
const DifficultyOption = React.forwardRef<
  React.ComponentRef<typeof SelectPrimitive.Item>,
  { value: string; name: string; desc: string }
>(({ value, name, desc }, ref) => (
  <SelectPrimitive.Item
    ref={ref}
    value={value}
    className={cn(
      "relative flex w-full cursor-default select-none items-center rounded-sm py-2 pl-2 pr-8 text-sm outline-none focus:bg-accent focus:text-accent-foreground data-[disabled]:pointer-events-none data-[disabled]:opacity-50"
    )}
  >
    <span className="absolute right-2 flex h-3.5 w-3.5 items-center justify-center">
      <SelectPrimitive.ItemIndicator>
        <Check className="h-4 w-4" />
      </SelectPrimitive.ItemIndicator>
    </span>
    <div className="flex flex-col">
      <SelectPrimitive.ItemText>{name}</SelectPrimitive.ItemText>
      <span className="text-xs text-muted-foreground">{desc}</span>
    </div>
  </SelectPrimitive.Item>
))
DifficultyOption.displayName = "DifficultyOption"

export function GameControls({
  playerColor,
  depth,
  onPlayerColorChange,
  onDepthChange,
  onNewGame,
  isThinking,
}: GameControlsProps) {
  return (
    <Card className="border-0 bg-card/50">
      <CardContent className="p-4 space-y-4">
        <div className="grid grid-cols-2 gap-3">
          <div className="space-y-1.5">
            <label className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Play as
            </label>
            <Select value={playerColor} onValueChange={(v) => onPlayerColorChange(v as 'white' | 'black')}>
              <SelectTrigger className="bg-input">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="white">White</SelectItem>
                <SelectItem value="black">Black</SelectItem>
              </SelectContent>
            </Select>
          </div>
          <div className="space-y-1.5">
            <label className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Difficulty
            </label>
            <Select value={depth.toString()} onValueChange={(v) => onDepthChange(parseInt(v))}>
              <SelectTrigger className="bg-input">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <DifficultyOption value="1" name="Beginner" desc="Depth 1 · Instant" />
                <DifficultyOption value="2" name="Easy" desc="Depth 2 · Instant" />
                <DifficultyOption value="3" name="Medium" desc="Depth 3 · Instant" />
                <DifficultyOption value="4" name="Hard" desc="Depth 4 · ~0.1s" />
                <DifficultyOption value="5" name="Expert" desc="Depth 5 · ~1-2s" />
                <DifficultyOption value="6" name="Master" desc="Depth 6 · ~30s" />
                <DifficultyOption value="7" name="Grandmaster" desc="Depth 7 · ~5min" />
                <DifficultyOption value="8" name="Maximum" desc="Depth 8 · Very slow" />
              </SelectContent>
            </Select>
          </div>
        </div>

        <Button
          onClick={onNewGame}
          disabled={isThinking}
          className="w-full"
          size="lg"
        >
          {isThinking ? 'Thinking...' : 'New Game'}
        </Button>
      </CardContent>
    </Card>
  )
}
