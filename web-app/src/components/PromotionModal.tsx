import {
  Dialog,
  DialogContent,
  DialogTitle,
} from '@/components/ui/dialog'
import { PIECE_IMAGES } from '@/lib/types'
import { cn } from '@/lib/utils'

interface PromotionModalProps {
  open: boolean
  isWhite: boolean
  onSelect: (piece: string) => void
  onClose: () => void
}

export function PromotionModal({ open, isWhite, onSelect, onClose }: PromotionModalProps) {
  const pieces = [
    { char: isWhite ? 'Q' : 'q', code: 'q', name: 'Queen' },
    { char: isWhite ? 'R' : 'r', code: 'r', name: 'Rook' },
    { char: isWhite ? 'B' : 'b', code: 'b', name: 'Bishop' },
    { char: isWhite ? 'N' : 'n', code: 'n', name: 'Knight' },
  ]

  return (
    <Dialog open={open} onOpenChange={(o) => !o && onClose()}>
      <DialogContent className="sm:max-w-md">
        <DialogTitle className="text-center">Choose promotion piece</DialogTitle>
        <div className="flex justify-center gap-3 py-4">
          {pieces.map((p) => (
            <button
              key={p.code}
              onClick={() => onSelect(p.code)}
              className={cn(
                'w-16 h-16 p-2 rounded-lg transition-all',
                'bg-dark-square hover:bg-selected',
                'border-2 border-transparent hover:border-primary',
                'flex items-center justify-center'
              )}
              title={p.name}
            >
              <img src={PIECE_IMAGES[p.char]} alt={p.name} className="w-full h-full" />
            </button>
          ))}
        </div>
      </DialogContent>
    </Dialog>
  )
}
