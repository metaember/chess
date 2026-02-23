import { Routes, Route, Link, useLocation } from 'react-router-dom'
import { PlayPage } from '@/pages/Play'
import Spectate from '@/pages/Spectate'
import { cn } from '@/lib/utils'

export default function App() {
  const location = useLocation()

  return (
    <div className="min-h-screen min-h-dvh flex flex-col">
      {/* Navigation */}
      <nav className="border-b border-border bg-card/50">
        <div className="max-w-7xl mx-auto px-4 md:px-6 lg:px-8 flex items-center gap-6 h-12">
          <span className="text-lg font-bold tracking-tight">
            <span className="text-primary">Rust</span> Chess
          </span>
          <div className="flex gap-1">
            <Link
              to="/"
              className={cn(
                'px-3 py-1.5 text-sm rounded-md transition-colors',
                location.pathname === '/'
                  ? 'bg-primary/10 text-primary font-medium'
                  : 'text-muted-foreground hover:text-foreground'
              )}
            >
              Play
            </Link>
            <Link
              to="/spectate"
              className={cn(
                'px-3 py-1.5 text-sm rounded-md transition-colors',
                location.pathname.startsWith('/spectate')
                  ? 'bg-primary/10 text-primary font-medium'
                  : 'text-muted-foreground hover:text-foreground'
              )}
            >
              Spectate
            </Link>
          </div>
        </div>
      </nav>

      {/* Routes */}
      <Routes>
        <Route path="/" element={<PlayPage />} />
        <Route path="/spectate" element={<Spectate />} />
        <Route path="/spectate/:gameId" element={<Spectate />} />
      </Routes>
    </div>
  )
}
