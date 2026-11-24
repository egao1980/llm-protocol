# llm-protocol

CLOS **turns + typed parts** for [cl-stack](https://github.com/egao1980/cl-stack). `generate` / `stream-generate` (chat-style) and `respond` / `stream-respond` (Responses-style items). OpenAI HTTP is a **separate** backend repo.

**Adapter, not core.** Do **not** add this to `blackboard-protocol` `:depends-on`. Demiurge **consumes** this; it does not define it.

| System | Role |
|--------|------|
| `llm-protocol` (`stack-llm`) | GFs, parts, turns, items, `embed`, tokens/context, mock, provider catalog |
| [`llm-protocol-openai`](https://github.com/egao1980/llm-protocol-openai) (`stack-llm-openai`) | `chat/completions` + `/responses` + `/embeddings` |
| [`llm-protocol-anthropic`](https://github.com/egao1980/llm-protocol-anthropic) (`stack-llm-anthropic`) | Messages API (official / vLLM / llama-server) |
| `llm-protocol/capability` | `:llm` catalogue + `complete` → `generate` + `embed` |
| `llm-protocol/schema` | `schema-protocol` + `schema-protocol-json` → `llm-response-output` |
| `llm-protocol/router` | `llm-router-backend` + CLOS policies (fallback / budget / latency) |
| `llm-protocol/telemetry` | GenAI semconv spans around `generate` / `respond` / `embed` |

MCP sampling (`create-message` → `generate`) is [`ai-agent-protocol/mcp`](https://github.com/egao1980/ai-agent-protocol) — **not** here.

Brief: [`llm.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/llm.md) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

```lisp
(asdf:load-system "llm-protocol")
(let ((b (stack-llm:make-mock-llm-backend)))
  (stack-llm:llm-response-text (stack-llm:generate b "hi")))
;; ⇒ "echo: hi"
(stack-llm:llm-response-text (stack-llm:respond b "hi"))
;; ⇒ "echo: hi"  ; mock: items→turns then generate
```

```lisp
(stack-llm:generate b (list (stack-llm:system-turn "be brief")
                            (stack-llm:user-turn "ping"))
                    :settings (stack-llm:make-llm-settings :temperature 0)
                    :tools (list (stack-llm:make-llm-tool :name "sum")))
```

Role on the **turn**; type on the **part**. Responses grain is `llm-item` (`llm-message-item`, `llm-function-call-item`, …). `turns->items` / `items->turns` convert. Tools are descriptors, not executors.

**Tokens / context** (0.3.0, core, still dep-free). `count-tokens` default is `ceiling(length / 4)` for strings; turns sum `turn-text`; parts use text; sequences sum. Exact tokenizers belong in backends. `context-window` reads `llm-model-info` (or a provider-wide default). `fit-turns` drops the oldest non-system turns until the budget fits and keeps every `:system` turn. Policy is an integer token budget or `token-fit-policy` (`:reserve` subtracted from the window).

```lisp
(stack-llm:count-tokens b "abcd") ; ⇒ 1
(stack-llm:fit-turns (list (stack-llm:system-turn "sys")
                           (stack-llm:user-turn "old")
                           (stack-llm:user-turn "new"))
                     b :policy 2)
```

**Provider catalog** (0.2.1) is name → `llm-backend`. Not LiteLLM. Not `make-llm-catalogue` (that is capability `:llm` ops). Register `llm-model-info` for `context-window` and prices (`input-price` / `output-price` are **USD per 1M tokens**; backends may store per-token figures if they document it — the router treats them as per-1M). Routing and spend ceilings are `llm-protocol/router`.

```lisp
(let ((cat (stack-llm:make-in-memory-provider-catalog)))
  (stack-llm:register-provider cat "mock" (stack-llm:make-mock-llm-backend))
  (multiple-value-bind (b model)
      (stack-llm:resolve-backend cat "mock")
    (declare (ignore model))
    (stack-llm:llm-response-text (stack-llm:generate b "hi"))))
```

Refs: `"anthropic:claude"` / `"anthropic"` / `:vllm` / `("openai" "gpt-4o")`. An `llm-backend` is identity. Missing name → `llm-unknown-provider` (`use-value`). Nil `*llm-catalog*` → `llm-missing-backend` (`use-value`).

Lookup **what a backend can do** is `capability-protocol`, not a second flag object:

```lisp
(asdf:load-system "llm-protocol/capability")
(let ((cat (stack-llm:make-llm-catalogue (stack-llm:make-mock-llm-backend))))
  (stack-capability:capability-supported-p cat :llm-tools)      ; T
  (stack-capability:capability-supported-p cat :llm-responses)  ; T (mock)
  (stack-capability:capability-supported-p cat :llm-vision)     ; NIL
  (stack-capability:capability-supported-p cat :llm-embeddings)) ; T (mock)
```

`embed` takes a string or sequence of strings → `llm-embed-result`. `embed-query` returns the first float vector.

```lisp
(let ((b (stack-llm:make-mock-llm-backend)))
  (stack-llm:embed-query b "hi" :dimensions 8))
```

Structured output is `schema-protocol` (`defschema`) + `schema-protocol-json` (JSON Schema emit). Not a PydanticAI `Agent`:

```lisp
(asdf:load-system "llm-protocol/schema")
(stack-schema:defschema city ()
  (name string)
  (country string))
(let ((r (stack-llm:generate b "Oslo" :output 'city)))
  (slot-value (stack-llm:llm-response-output r) 'name))
```

When `:output` is set and parse fails, GENERATE applies `*structured-output-repair*` (default `:repair`) — this is **not** HTTP `retry`:

1. **retry-with-repair-prompt** — one extra GENERATE that includes the raw completion and a schema hint
2. **relaxed parse** — first JSON object/array in mixed text or markdown fences (`` ``` `` / `` ```json ``)
3. **text fallback** — leave `llm-response-output` NIL; do not signal

`:signal` (or bind `*structured-output-repair*` to `nil`) restores `llm-output-error` + `ignore-output` / `use-value`. Per-call override: `:output-repair` or `llm-settings-output-repair`. Mock advertises `(backend-supports-p backend :structured-output)`.

```lisp
(let ((stack-llm:*structured-output-repair* :relaxed))
  (stack-llm:generate b "city" :output 'city))
;; markdown-fenced or prose-wrapped JSON parses without a second GENERATE
```

Content: `llm-response-content` / `llm-response-parts` (blocks), `llm-response-text` (text parts only), `llm-response-thinking` (reasoning).

**Retry by failure class.** `llm-retry-class` sorts a failure: `:rate-limit` (429), `:overloaded` (503 / 529), `:transient` (408 / 409 / other 5xx), `:context-overflow`, or NIL (auth, bad request). A `retry-policy` (default `*retry-policy*`: 5 attempts, 2 s doubling, 30 s cap) says which classes to retry; `retry-delay` honours the provider's `Retry-After` (`llm-http-error-retry-after`) and waits twice as long on `:overloaded`. `call-with-retry-policy` / `with-retry-policy` run a call under it (injectable `:sleep`). `llm-context-overflow` is never retried as is — backends signal it when `context-overflow-text-p` matches the error body, and the caller compacts first.

**Typed provider errors (0.7.0).** Handlers bind on type: `llm-rate-limited` (429), `llm-overloaded` (503 / 529, or any 5xx that says "overloaded"), `llm-auth-error` (401 / 403) and `llm-not-found` (404) are `llm-http-error`s that also carry `llm-error-provider` / `-url` / `-model`. `make-llm-http-error` is the one status → type mapping backends call (it makes the condition; the backend signals it), with `llm-http-error-class` as its pure half; overflow wording only counts on 400 / 413. `parse-retry-after` reads a `Retry-After` header (seconds or an IMF-fixdate; anything else → NIL, never signals). `llm-transport-error` (`kind` one of `+transport-error-kinds+`, `cause` the original condition) is for requests that got no HTTP answer: retry class `:transient`, except `:canceled`. `transport-error-kind-for` maps an http-protocol condition onto a kind by class name, so llm-protocol stays dependency-free. Auth and not-found are never retried. `make-retry-policy :jitter J` (default 0) turns a backoff delay D into D × (1 − J × R), R from `*retry-random-state*`; a `Retry-After` delay is never jittered.

**Truncated streams.** A streamed response that closes without a terminal signal (no finish reason, end sentinel or completed event) is not a success: backends call `signal-incomplete-response`, which signals `llm-incomplete-response` (an `llm-http-error`, retryable, class `:transient`) carrying the partial response (`llm-incomplete-response-partial`, finish reason `:incomplete`). Restarts: `use-partial-response` (helper `invoke-use-partial-response`) returns the partial response; `use-value` substitutes one; an outer `retry` re-runs the call.

```lisp
(stack-llm:with-retry-policy (:policy (stack-llm:make-retry-policy :max-attempts 3))
  (stack-llm:generate b "hi"))
```

**Tokens anchored on usage.** `llm-usage` carries `cache-read-tokens` / `cache-write-tokens`; `llm-usage-prompt-tokens` is the whole measured prompt. `compaction-part-p` marks a provider-side compaction summary (Anthropic `compaction` block) and `compaction-part-text` reads it. `(anchored-token-count backend turns :usage u :anchor-count n)` counts the first N turns (the prompt U measured plus its reply) as measured and estimates only the turns after them — far closer than the chars/4 heuristic over the whole history.

**Router** (`llm-protocol/router`). `llm-router-backend` implements the full GF surface by delegating through a `routing-policy`. Policies are CLOS — wrap to compose (`budget-policy` around `fallback-chain-policy`). `fallback-chain-policy` advances on `llm-error` subtypes and retryable `llm-http-error` (429 / 5xx / 408 / 409) — try the next backend, do not add a second same-backend retry loop. `budget-policy` accounts `llm-usage` per scope string against an `llm-budget` (token + cost ceilings). Over ceiling → `llm-budget-exceeded` with restarts `continue-anyway`, `use-cheaper-model`, `abort`. `least-latency-policy` picks the lowest EWMA.

```lisp
(asdf:load-system "llm-protocol/router")
(let* ((a (stack-llm:make-mock-llm-backend :prefix "a: "))
       (b (stack-llm:make-mock-llm-backend :prefix "b: "))
       (policy (stack-llm:make-budget-policy
                :inner (stack-llm:make-fallback-chain-policy :candidates (list a b))
                :budget (stack-llm:make-llm-budget :max-tokens 10000)))
       (r (stack-llm:make-llm-router-backend :policy policy :candidates (list a b))))
  (stack-llm:llm-response-text (stack-llm:generate r "hi")))
```

**Telemetry** (`llm-protocol/telemetry`). Optional — core stays dep-free. `:around` methods wrap `generate` / `stream-generate` / `respond` / `stream-respond` / `embed` with `telemetry-protocol:with-span` + `instrument-gen-ai-span`. Safe against the no-op default backend. Cost is USD from `llm-usage` × `llm-model-info` `input-price` / `output-price` (USD per 1M; catalog `:models` fills gaps).

```lisp
(asdf:load-system "llm-protocol/telemetry")
(stack-telemetry:use-recording-telemetry)
(stack-llm:generate (stack-llm:make-mock-llm-backend) "hi")
(stack-telemetry:recorded-spans stack-telemetry:*telemetry-backend*)
```

**Provider compaction.** `(server-compact backend turns :instructions s :tools ts)` → `(values new-turns compacted-p response)`: the system turns, then the provider's summary part (Anthropic: compaction on demand). Probe `(backend-supports-p b :compaction)`; client lookup is the `:llm-compaction` capability (`make-llm-catalogue`). No summary → the turns back and NIL. A router compacts on its first native candidate. `generate` / `stream-generate` on a backend without native compaction get `portable-turns`: a summary that opens the conversation becomes a `:system` turn (`+portable-summary-prefix+`); a threshold summary, whose turns are still there, is dropped.

**Part state.** `(part-state part)` → a storable plist (`:type` first, no hash tables); `part-from-state` reads it back, `turn-state` / `state-turn` for turns. Thinking signatures and backend parts (methods on `part-state` / `state-part`) survive, so journals replay exactly; an unknown `:type` reads back as NIL. `make-mock-llm-backend :compactor (lambda (turns &key instructions))` compacts natively in tests.

## License

MIT — see [LICENSE](LICENSE).
