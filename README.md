# llm-protocol

CLOS **turns + typed parts** for [cl-stack](https://github.com/egao1980/cl-stack). `generate` / `stream-generate` (chat-style) and `respond` / `stream-respond` (Responses-style items). OpenAI HTTP is a **separate** backend repo.

**Adapter, not core.** Do **not** add this to `blackboard-protocol` `:depends-on`. Demiurge **consumes** this; it does not define it.

| System | Role |
|--------|------|
| `llm-protocol` (`stack-llm`) | GFs, parts, turns, items, `embed`, mock, provider catalog |
| [`llm-protocol-openai`](https://github.com/egao1980/llm-protocol-openai) (`stack-llm-openai`) | `chat/completions` + `/responses` + `/embeddings` |
| [`llm-protocol-anthropic`](https://github.com/egao1980/llm-protocol-anthropic) (`stack-llm-anthropic`) | Messages API (official / vLLM / llama-server) |
| `llm-protocol/capability` | `:llm` catalogue + `complete` → `generate` + `embed` |
| `llm-protocol/schema` | `schema-protocol` + `schema-protocol-json` → `llm-response-output` |

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

**Provider catalog** (0.2.1) is name → `llm-backend`. Not LiteLLM. Not a router/budget. Not `make-llm-catalogue` (that is capability `:llm` ops).

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

Content: `llm-response-content` / `llm-response-parts` (blocks), `llm-response-text` (text parts only), `llm-response-thinking` (reasoning).

## License

MIT — see [LICENSE](LICENSE).
