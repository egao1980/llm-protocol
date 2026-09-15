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

## License

MIT — see [LICENSE](LICENSE).
