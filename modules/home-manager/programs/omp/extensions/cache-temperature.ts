import type { ExtensionAPI } from "@oh-my-pi/pi-coding-agent";

/**
 * Cache-temperature oracle for the omp status area.
 *
 * Renders a widget below the editor answering "will my next message be a
 * cache hit?" and, once a miss is certain, "should I /shake first?".
 *
 * The widget is deliberately static: text is pushed only when the state
 * actually changes (first response, expiry boundary, compaction, session
 * switch), so nothing ticks or moves. Warm state shows the wall-clock time
 * the cache expires, not a countdown.
 *
 * Warm vs cold is decided from elapsed time since the last provider response:
 *
 * - OpenRouter pins requests to the same endpoint for 10 minutes after the
 *   last request (sticky routing), so 600s is the default ceiling.
 * - Anthropic's default prompt-cache TTL is 5 minutes with free refresh on
 *   use; omp's `providers.cacheRetention = auto` keep-alive refreshes may
 *   keep it warm even longer, so the oracle errs conservative (warns early).
 * - `OMP_CACHE_TTL_SECONDS` overrides the window (minimum 5s).
 *
 * Miss size comes from live context usage: a cold request re-reads the whole
 * context at full input price, so `/shake` (mechanical, local, free) first
 * shrinks the miss whenever the cache is already cold anyway.
 */

export const DEFAULT_TTL_SECONDS = 600;
export const MIN_TTL_SECONDS = 5;

export interface CacheState {
  /** Epoch ms of the last provider response; null before the first one. */
  lastResponseAt: number | null;
  /** Compaction rewrites the context, invalidating the cached prefix. */
  compacted: boolean;
}

export interface ContextUsageLike {
  tokens: number;
}

export type CacheStatus = "no-data" | "warm" | "cold" | "cold-compacted";

export interface WidgetModel {
  status: CacheStatus;
  /** Widget lines; empty means "hide the widget". */
  lines: string[];
}

export function resolveTtlSeconds(env: Record<string, string | undefined> = process.env): number {
  const raw = env.OMP_CACHE_TTL_SECONDS;
  if (raw === undefined || raw === "") return DEFAULT_TTL_SECONDS;
  const parsed = Number(raw);
  if (!Number.isFinite(parsed)) return DEFAULT_TTL_SECONDS;
  return Math.max(MIN_TTL_SECONDS, Math.floor(parsed));
}

export function formatClock(epochMs: number): string {
  const d = new Date(epochMs);
  const h = String(d.getHours()).padStart(2, "0");
  const m = String(d.getMinutes()).padStart(2, "0");
  return `${h}:${m}`;
}

export function formatTokens(tokens: number): string {
  if (tokens >= 1_000_000) return `${(tokens / 1_000_000).toFixed(1)}M`;
  if (tokens >= 1_000) return `${Math.round(tokens / 1_000)}k`;
  return String(tokens);
}

export function widgetModel(
  state: CacheState,
  now: number,
  ttl: number,
  usage: ContextUsageLike | undefined,
): WidgetModel {
  const miss = usage === undefined ? "an unknown number of tokens" : `≈ ${formatTokens(usage.tokens)} tok`;
  if (state.lastResponseAt === null) return { status: "no-data", lines: [] };
  if (state.compacted) {
    return {
      status: "cold-compacted",
      lines: [`⚑ cache COLD (context rewritten) · next request re-reads ${miss} at full price`],
    };
  }
  const expiresAt = state.lastResponseAt + ttl * 1000;
  if (now < expiresAt) {
    return {
      status: "warm",
      lines: [`⚑ cache warm · expires ${formatClock(expiresAt)}`],
    };
  }
  return {
    status: "cold",
    lines: [
      `⚑ cache COLD (expired ${formatClock(expiresAt)}) · miss certain · ${miss} at full input price · /shake first (local, free)`,
    ],
  };
}

export default function cacheTemperature(pi: ExtensionAPI): void {
  let state: CacheState = { lastResponseAt: null, compacted: false };
  // Serialized last-pushed widget content; pushing is suppressed while the
  // rendering is byte-identical so the text never moves on screen.
  let lastPushed: string | null = null;
  const ttl = resolveTtlSeconds();

  pi.on("after_provider_response", () => {
    state = { lastResponseAt: Date.now(), compacted: false };
  });

  pi.on("session_compact", () => {
    state = { ...state, compacted: true };
  });

  pi.on("session_switch", () => {
    state = { lastResponseAt: null, compacted: false };
  });

  pi.on("session_branch", () => {
    state = { lastResponseAt: null, compacted: false };
  });

  pi.on("session_start", async (_event, ctx) => {
    if (!ctx.hasUI) return;
    ctx.setInterval(() => {
      const usage = typeof ctx.getContextUsage === "function" ? ctx.getContextUsage() : ctx.ui.getContextUsage?.();
      const model = widgetModel(state, Date.now(), ttl, usage);
      if (model.lines.length === 0) {
        // Only push the hide when something was actually shown before.
        if (lastPushed !== null) {
          ctx.ui.setWidget("cache-temperature", undefined, { placement: "belowEditor" });
          lastPushed = null;
        }
        return;
      }
      const serialized = model.lines.join("\n");
      if (serialized === lastPushed) return;
      ctx.ui.setWidget("cache-temperature", model.lines, { placement: "belowEditor" });
      lastPushed = serialized;
    }, 1000);
  });
}
