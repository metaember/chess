import type { EngineStats as EngineStatsType } from '@/lib/types'
import { formatNumber } from '@/lib/types'

interface EngineStatsProps {
  stats: EngineStatsType | null
  visible: boolean
}

export function EngineStats({ stats, visible }: EngineStatsProps) {
  if (!visible || !stats) return null

  return (
    <div className="grid grid-cols-4 gap-2 p-3 bg-muted/30 rounded-lg">
      <StatItem label="Time" value={`${stats.time_ms}ms`} />
      <StatItem label="Depth" value={stats.depth.toString()} />
      <StatItem label="Nodes" value={formatNumber(stats.nodes_searched + stats.quiescent_nodes)} />
      <StatItem label="N/sec" value={formatNumber(stats.nodes_per_second)} />
    </div>
  )
}

function StatItem({ label, value }: { label: string; value: string }) {
  return (
    <div className="text-center">
      <div className="text-sm font-semibold font-mono text-foreground">{value}</div>
      <div className="text-[10px] text-muted-foreground uppercase tracking-wider">{label}</div>
    </div>
  )
}
