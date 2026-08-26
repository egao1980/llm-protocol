# llm-protocol

CLOS **turns + typed parts** for [cl-stack](https://github.com/egao1980/cl-stack). One `generate` / `stream-generate`. OpenAI-compatible HTTP is a backend, not the protocol.

**Adapter, not core.** Do **not** add this to `blackboard-protocol` `:depends-on`. Demiurge **consumes** this; it does not define it.

| System | Role |
|--------|------|
| `llm-protocol` (`stack-llm`) | GFs, parts, turns, mock |
| `llm-backend-openai` (`stack-llm-openai`) | OpenAI-compat `chat/completions` |
| `llm-protocol/capability` | `complete` → `generate` |
| `llm-protocol/mcp` | `make-mcp-sampling-handler` |

Brief: [`llm.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/llm.md) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

```lisp
(asdf:load-system "llm-protocol")
(let ((b (stack-llm:make-mock-llm-backend)))
  (stack-llm:llm-response-text (stack-llm:generate b "hi")))
;; ⇒ "echo: hi"
```

```lisp
(stack-llm:generate b (list (stack-llm:system-turn "be brief")
                            (stack-llm:user-turn "ping"))
                    :settings (stack-llm:make-llm-settings :temperature 0)
                    :tools (list (stack-llm:make-llm-tool :name "sum")))
```

Role on the **turn**; type on the **part** (`llm-text-part`, `llm-tool-call-part`, `llm-tool-result-part`, `llm-thinking-part`, `llm-image-part`). Tools are descriptors, not executors.

Live HTTP tests: `LLM_OPENAI_LIVE=1`. `stream-generate` is a GF; OpenAI backend signals `llm-unsupported` in wave-1.

## License

MIT — see [LICENSE](LICENSE).
