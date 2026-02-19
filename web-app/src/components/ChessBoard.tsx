import { useState, useCallback, useRef } from 'react'
import { cn } from '@/lib/utils'
import { PIECE_IMAGES, parseFen } from '@/lib/types'
import type { MoveResponse } from '@/lib/types'

interface ChessBoardProps {
  fen: string | null
  isFlipped: boolean
  selectedSquare: string | null
  lastMove: { from: string; to: string } | null
  legalMoves: MoveResponse[]
  isCheck: boolean
  activeColor: string
  playerColor: string
  gameOver: boolean
  onSquareClick: (square: string) => void
  onMove: (from: string, to: string) => void
}

export function ChessBoard({
  fen,
  isFlipped,
  selectedSquare,
  lastMove,
  legalMoves,
  isCheck,
  activeColor,
  playerColor,
  gameOver,
  onSquareClick,
  onMove,
}: ChessBoardProps) {
  const [isDragging, setIsDragging] = useState(false)
  const [dragFrom, setDragFrom] = useState<string | null>(null)
  const [dragPosition, setDragPosition] = useState<{ x: number; y: number } | null>(null)
  const boardRef = useRef<HTMLDivElement>(null)
  const suppressClickRef = useRef(false)

  const pieces = fen ? parseFen(fen) : {}
  const files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  const ranks = ['8', '7', '6', '5', '4', '3', '2', '1']

  const displayFiles = isFlipped ? [...files].reverse() : files
  const displayRanks = isFlipped ? [...ranks].reverse() : ranks

  const legalMovesFromSelected = selectedSquare
    ? legalMoves.filter(m => m.from === selectedSquare)
    : []

  const getKingSquare = (): string | null => {
    const kingChar = activeColor === 'white' ? 'K' : 'k'
    for (const [sq, piece] of Object.entries(pieces)) {
      if (piece === kingChar) return sq
    }
    return null
  }

  const canDrag = (square: string): boolean => {
    if (gameOver) return false
    if (activeColor !== playerColor) return false
    return legalMoves.some(m => m.from === square)
  }

  const handleMouseDown = useCallback((e: React.MouseEvent, square: string) => {
    if (e.button !== 0) return
    if (!canDrag(square)) return

    e.preventDefault()
    setIsDragging(true)
    setDragFrom(square)
    setDragPosition({ x: e.clientX, y: e.clientY })
    onSquareClick(square)
  }, [gameOver, activeColor, playerColor, legalMoves, onSquareClick])

  const handleMouseMove = useCallback((e: React.MouseEvent) => {
    if (!isDragging) return
    setDragPosition({ x: e.clientX, y: e.clientY })
  }, [isDragging])

  const handleMouseUp = useCallback((e: React.MouseEvent) => {
    if (!isDragging || !dragFrom) {
      setIsDragging(false)
      setDragFrom(null)
      setDragPosition(null)
      return
    }

    const element = document.elementFromPoint(e.clientX, e.clientY)
    const squareEl = element?.closest('[data-square]') as HTMLElement | null
    const toSquare = squareEl?.dataset.square

    if (toSquare && toSquare !== dragFrom) {
      const to = convertCastlingClick(dragFrom, toSquare, pieces)
      if (legalMoves.some(m => m.from === dragFrom && m.to === to)) {
        onMove(dragFrom, to)
      }
    }

    // Suppress the click event that fires after mouseup
    suppressClickRef.current = true
    setIsDragging(false)
    setDragFrom(null)
    setDragPosition(null)
  }, [isDragging, dragFrom, legalMoves, pieces, onMove])

  const handleTouchStart = useCallback((e: React.TouchEvent, square: string) => {
    if (!canDrag(square)) return

    e.preventDefault()
    const touch = e.touches[0]
    setIsDragging(true)
    setDragFrom(square)
    setDragPosition({ x: touch.clientX, y: touch.clientY })
    onSquareClick(square)
  }, [gameOver, activeColor, playerColor, legalMoves, onSquareClick])

  const handleTouchMove = useCallback((e: React.TouchEvent) => {
    if (!isDragging) return
    e.preventDefault()
    const touch = e.touches[0]
    setDragPosition({ x: touch.clientX, y: touch.clientY })
  }, [isDragging])

  const handleTouchEnd = useCallback((e: React.TouchEvent) => {
    if (!isDragging || !dragFrom) {
      setIsDragging(false)
      setDragFrom(null)
      setDragPosition(null)
      return
    }

    const touch = e.changedTouches[0]
    const element = document.elementFromPoint(touch.clientX, touch.clientY)
    const squareEl = element?.closest('[data-square]') as HTMLElement | null
    const toSquare = squareEl?.dataset.square

    if (toSquare && toSquare !== dragFrom) {
      const to = convertCastlingClick(dragFrom, toSquare, pieces)
      if (legalMoves.some(m => m.from === dragFrom && m.to === to)) {
        onMove(dragFrom, to)
      }
    }

    setIsDragging(false)
    setDragFrom(null)
    setDragPosition(null)
  }, [isDragging, dragFrom, legalMoves, pieces, onMove])

  const handleSquareClick = useCallback((square: string) => {
    // Suppress click events that fire after drag operations
    if (suppressClickRef.current) {
      suppressClickRef.current = false
      return
    }
    if (isDragging) return

    // Handle move completion when a square is already selected
    if (selectedSquare && selectedSquare !== square) {
      const to = convertCastlingClick(selectedSquare, square, pieces)
      if (legalMoves.some(m => m.from === selectedSquare && m.to === to)) {
        onMove(selectedSquare, to)
        return
      }
    }

    // Pass through to parent for selection logic
    onSquareClick(square)
  }, [isDragging, selectedSquare, legalMoves, pieces, onSquareClick, onMove])

  const kingSquare = isCheck ? getKingSquare() : null
  const dragPiece = dragFrom ? pieces[dragFrom] : null

  // Size the board to fill available space
  // Mobile: 90vw, Desktop: up to 70vh or 700px
  return (
    <div className="relative w-[90vw] max-w-[500px] md:w-[65vh] md:max-w-[700px] aspect-square">
      <div
        ref={boardRef}
        className="grid grid-cols-8 grid-rows-8 w-full h-full rounded-lg overflow-hidden shadow-2xl"
        onMouseMove={handleMouseMove}
        onMouseUp={handleMouseUp}
        onMouseLeave={handleMouseUp}
        onTouchMove={handleTouchMove}
        onTouchEnd={handleTouchEnd}
      >
        {displayRanks.map((rank, row) =>
          displayFiles.map((file, col) => {
            const square = file + rank
            const piece = pieces[square]
            const isLight = (row + col) % 2 === 0
            const isSelected = square === selectedSquare
            const isLastMoveSquare = lastMove && (square === lastMove.from || square === lastMove.to)
            const isLegalTarget = legalMovesFromSelected.some(m => m.to === square)
            const isCapture = isLegalTarget && piece
            const isCastleTarget = selectedSquare && isCastlingRookSquare(selectedSquare, square, pieces, legalMoves)
            const isKingInCheck = square === kingSquare
            const isBeingDragged = square === dragFrom && isDragging

            return (
              <div
                key={square}
                data-square={square}
                className={cn(
                  'relative flex items-center justify-center cursor-pointer select-none transition-colors duration-100',
                  isLight ? 'bg-light-square' : 'bg-dark-square',
                  isSelected && 'bg-selected',
                  isLastMoveSquare && !isSelected && 'bg-last-move',
                  isKingInCheck && 'bg-check'
                )}
                onMouseDown={(e) => handleMouseDown(e, square)}
                onTouchStart={(e) => handleTouchStart(e, square)}
                onClick={() => handleSquareClick(square)}
              >
                {/* Piece */}
                {piece && (
                  <img
                    src={PIECE_IMAGES[piece]}
                    alt={piece}
                    draggable={false}
                    className={cn(
                      'z-10 w-[85%] h-[85%] transition-opacity pointer-events-none',
                      isBeingDragged && 'opacity-30'
                    )}
                  />
                )}

                {/* Legal move indicator */}
                {(isLegalTarget || isCastleTarget) && !isCapture && (
                  <div className="absolute w-[28%] h-[28%] rounded-full bg-legal-move" />
                )}

                {/* Capture indicator */}
                {isLegalTarget && isCapture && (
                  <div className="absolute w-[90%] h-[90%] rounded-full border-4 border-legal-move" />
                )}

                {/* Coordinates */}
                {col === 7 && (
                  <span className={cn(
                    'absolute bottom-0.5 right-1 text-[10px] font-semibold pointer-events-none',
                    isLight ? 'text-dark-square' : 'text-light-square'
                  )}>
                    {rank}
                  </span>
                )}
                {row === 7 && (
                  <span className={cn(
                    'absolute top-0.5 left-1 text-[10px] font-semibold pointer-events-none',
                    isLight ? 'text-dark-square' : 'text-light-square'
                  )}>
                    {file}
                  </span>
                )}
              </div>
            )
          })
        )}
      </div>

      {/* Drag ghost */}
      {isDragging && dragPosition && dragPiece && (
        <img
          src={PIECE_IMAGES[dragPiece]}
          alt={dragPiece}
          draggable={false}
          className="fixed pointer-events-none z-50 w-16 h-16"
          style={{
            left: dragPosition.x,
            top: dragPosition.y,
            transform: 'translate(-50%, -50%)',
          }}
        />
      )}
    </div>
  )
}

function convertCastlingClick(from: string, to: string, pieces: Record<string, string>): string {
  const piece = pieces[from]
  const target = pieces[to]

  if (piece === 'K') {
    if (target === 'R' && to === 'h1') return 'g1'
    if (target === 'R' && to === 'a1') return 'c1'
  } else if (piece === 'k') {
    if (target === 'r' && to === 'h8') return 'g8'
    if (target === 'r' && to === 'a8') return 'c8'
  }
  return to
}

function isCastlingRookSquare(
  from: string,
  to: string,
  pieces: Record<string, string>,
  legalMoves: MoveResponse[]
): boolean {
  const piece = pieces[from]
  if (piece === 'K' || piece === 'k') {
    const castleMoves = legalMoves.filter(m => m.from === from)
    if (to === 'h1' && castleMoves.some(m => m.to === 'g1')) return true
    if (to === 'a1' && castleMoves.some(m => m.to === 'c1')) return true
    if (to === 'h8' && castleMoves.some(m => m.to === 'g8')) return true
    if (to === 'a8' && castleMoves.some(m => m.to === 'c8')) return true
  }
  return false
}
