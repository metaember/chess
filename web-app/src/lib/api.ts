import type { BoardResponse, MakeMoveResponse } from './types'

const API_BASE = '/api'

export async function newGame(playerColor: string, depth: number): Promise<BoardResponse> {
  const response = await fetch(`${API_BASE}/new-game`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ player_color: playerColor, depth })
  })

  if (!response.ok) {
    throw new Error('Failed to start new game')
  }

  return response.json()
}

export async function makeMove(
  gameId: string,
  from: string,
  to: string,
  promotion?: string
): Promise<MakeMoveResponse> {
  const response = await fetch(`${API_BASE}/move`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ game_id: gameId, from, to, promotion })
  })

  if (!response.ok) {
    throw new Error('Failed to make move')
  }

  return response.json()
}
