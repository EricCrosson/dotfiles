import { describe, expect, test } from "bun:test";
import ext, {
  formatClock,
  formatTokens,
  DEFAULT_TTL_SECONDS,
  MIN_TTL_SECONDS,
  resolveTtlSeconds,
  widgetModel,
  type CacheState,
  type UsageStatsLike,
} from "../modules/home-manager/programs/omp/extensions/cache-temperature.ts";

const NOW = 1_800_000_000_000;

function stateAt(ageMs: number | null, compacted = false): CacheState {
  return { lastResponseAt: ageMs === null ? null : NOW - ageMs, compacted };
}

const usage = { tokens: 118_000, contextWindow: 262_144, percent: 45 };
const stats: UsageStatsLike = { input: 8, cacheRead: 92, cacheWrite: 0 };

describe("resolveTtlSeconds", () => {
  test("defaults to OpenRouter sticky-routing window", () => {
    expect(DEFAULT_TTL_SECONDS).toBe(600);
    expect(MIN_TTL_SECONDS).toBe(5);
    expect(resolveTtlSeconds({})).toBe(600);
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "" })).toBe(600);
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "not-a-number" })).toBe(600);
  });

  test("honors override and clamps to the minimum", () => {
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "30" })).toBe(30);
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "0" })).toBe(5);
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "-10" })).toBe(5);
    expect(resolveTtlSeconds({ OMP_CACHE_TTL_SECONDS: "2.9" })).toBe(5);
  });
});

describe("widgetModel", () => {
  test("hides before the first provider response", () => {
    const model = widgetModel(stateAt(null), NOW, 600, usage, stats);
    expect(model.status).toBe("no-data");
    expect(model.lines).toEqual([]);
  });
  test("warm shows hit ratio and expiry timestamp", () => {
    const model = widgetModel(stateAt(120_000), NOW, 600, usage, stats);
    expect(model.status).toBe("warm");
    expect(model.lines).toHaveLength(1);
    expect(model.lines[0]).toContain("cache warm");
    expect(model.lines[0]).toContain("(92% hit)");
    expect(model.lines[0]).toContain(`expires ${formatClock(NOW + 480_000)}`);
    // miss size is intentionally not shown while warm — noise, not value
    expect(model.lines[0]).not.toContain("tok");
  });

  test("omits hit ratio when usage statistics are empty", () => {
    const model = widgetModel(stateAt(120_000), NOW, 600, usage, {
      input: 0,
      cacheRead: 0,
      cacheWrite: 0,
    });
    expect(model.status).toBe("warm");
    expect(model.lines[0]).not.toContain("% hit");
    expect(model.lines[0]).toContain(`expires ${formatClock(NOW + 480_000)}`);
  });

  test("the expiry boundary itself is already cold", () => {
    // lastResponseAt + ttl == now: expired.
    const model = widgetModel(stateAt(600_000), NOW, 600, usage, stats);
    expect(model.status).toBe("cold");
  });

  test("cold names the expired timestamp and advises /shake", () => {
    const model = widgetModel(stateAt(700_000), NOW, 600, usage, stats);
    expect(model.status).toBe("cold");
    expect(model.lines[0]).toContain(`COLD (expired ${formatClock(NOW - 100_000)})`);
    expect(model.lines[0]).toContain("/shake first");
    expect(model.lines[0]).toContain("118k tok");
  });

  test("cold-compacted after compaction regardless of age", () => {
    const model = widgetModel(stateAt(1000, true), NOW, 600, usage, stats);
    expect(model.status).toBe("cold-compacted");
    expect(model.lines[0]).toContain("cache COLD (context rewritten)");
  });

  test("renders without usage data", () => {
    const model = widgetModel(stateAt(120_000), NOW, 600, undefined, stats);
    expect(model.status).toBe("warm");
    expect(model.lines).toHaveLength(1);
    expect(model.lines[0]).not.toContain("tok");
  });
});

describe("formatting", () => {
  test("formatClock renders local HH:MM", () => {
    const d = new Date(2026, 8, 15, 15, 42, 7); // local time, zero-based month
    expect(formatClock(d.getTime())).toBe("15:42");
    expect(formatClock(new Date(2026, 0, 2, 3, 4, 5).getTime())).toBe("03:04");
  });

  test("formatTokens", () => {
    expect(formatTokens(518)).toBe("518");
    expect(formatTokens(118_000)).toBe("118k");
    expect(formatTokens(1_234_567)).toBe("1.2M");
  });
});

type Handler = (event: unknown, ctx: unknown) => unknown;

interface Harness {
  fire(name: string, event?: unknown): void;
  tick(): void;
  widgets: Array<{ key: string; content: unknown; options: unknown }>;
}

/** Drive the default factory against a fake ExtensionAPI/ExtensionContext. */
function makeHarness(env: Record<string, string> = {}): Harness {
  const handlers: Record<string, Handler> = {};
  const savedEnv = process.env.OMP_CACHE_TTL_SECONDS;
  process.env.OMP_CACHE_TTL_SECONDS = env.OMP_CACHE_TTL_SECONDS;
  try {
    (ext as (pi: unknown) => void)({
      on: (name: string, handler: Handler) => {
        handlers[name] = handler;
      },
    });
  } finally {
    if (savedEnv === undefined) delete process.env.OMP_CACHE_TTL_SECONDS;
    else process.env.OMP_CACHE_TTL_SECONDS = savedEnv;
  }

  let intervalFn: (() => void) | undefined;
  const widgets: Harness["widgets"] = [];
  const ctx = {
    hasUI: true,
    getContextUsage: () => usage,
    sessionManager: {
      getUsageStatistics: () => stats,
    },
    ui: {
      setWidget: (key: string, content: unknown, options: unknown) => {
        widgets.push({ key, content, options });
      },
    },
    setInterval: (fn: () => void) => {
      intervalFn = fn;
      return 1;
    },
    clearTimer: () => {},
  };
  return {
    fire: (name: string, event: unknown = {}) => {
      handlers[name]?.(event, ctx);
    },
    tick: () => intervalFn?.(),
    widgets,
  };
}

describe("extension wiring", () => {
  test("pushes nothing before the first response", () => {
    const harness = makeHarness();
    harness.fire("session_start");
    harness.tick();
    expect(harness.widgets).toHaveLength(0);
  });

  test("publishes warm once after the response, then stays static", () => {
    const harness = makeHarness();
    harness.fire("session_start");
    harness.fire("after_provider_response");
    harness.tick();
    expect(harness.widgets).toHaveLength(1);
    const first = harness.widgets[0];
    expect(first.key).toBe("cache-temperature");
    expect(first.options).toEqual({ placement: "belowEditor" });
    expect(first.content).toEqual(expect.arrayContaining([expect.stringContaining("cache warm")]));
    expect(JSON.stringify(first.content)).toContain("92% hit");
    expect(JSON.stringify(first.content)).toContain("expires");

    // Identical rendering must not re-push: the text never moves.
    harness.tick();
    harness.tick();
    expect(harness.widgets).toHaveLength(1);
  });

  test("pushes exactly once more when the expiry boundary passes", () => {
    const harness = makeHarness();
    harness.fire("session_start");
    harness.fire("after_provider_response");
    harness.tick();
    expect(harness.widgets).toHaveLength(1);

    const realNow = Date.now;
    try {
      Date.now = () => realNow() + 700_000;
      harness.tick();
      expect(harness.widgets).toHaveLength(2);
      expect(JSON.stringify(harness.widgets[1].content)).toContain("COLD (expired");
      // Cold line is static too.
      harness.tick();
      expect(harness.widgets).toHaveLength(2);
    } finally {
      Date.now = realNow;
    }
  });

  test("marks the cache cold after compaction and hides on switch", () => {
    const harness = makeHarness();
    harness.fire("session_start");
    harness.fire("after_provider_response");
    harness.tick();
    harness.fire("session_compact");
    harness.tick();
    const compactPush = harness.widgets.at(-1);
    expect(JSON.stringify(compactPush?.content)).toContain("context rewritten");

    harness.fire("session_switch");
    harness.tick();
    const last = harness.widgets.at(-1);
    expect(last?.content).toBeUndefined();
    // Now hidden, further ticks stay silent.
    harness.tick();
    expect(harness.widgets.at(-1)?.content).toBeUndefined();
  });

  test("respects OMP_CACHE_TTL_SECONDS from the environment", () => {
    const harness = makeHarness({ OMP_CACHE_TTL_SECONDS: "10" });
    harness.fire("session_start");
    harness.fire("after_provider_response");
    harness.tick();
    expect(harness.widgets).toHaveLength(1);

    const realNow = Date.now;
    try {
      Date.now = () => realNow() + 11_000;
      harness.tick();
      expect(harness.widgets).toHaveLength(2);
      expect(JSON.stringify(harness.widgets[1].content)).toContain("COLD (expired");
    } finally {
      Date.now = realNow;
    }
  });
});
