# llm-protocol

CLOS **`generate`** / **`list-models`** for [cl-stack](https://github.com/egao1980/cl-stack). OpenAI-compatible HTTP (`/chat/completions`, `/models`) — LM Studio / OpenRouter / openai.com.

**Adapter, not core.** Do **not** add this to `blackboard-protocol` `:depends-on`. Implements capability `:llm-generation` `complete` in the optional `llm-protocol/capability` system. Optional MCP `sampling/createMessage` helper maps to `generate` — does not invent a second `create-message`.

| System | Role |
|--------|------|
| `llm-protocol` (`stack-llm`) | GFs, types, in-process mock |
| `llm-backend-openai` (`stack-llm-openai`) | OpenAI-compat HTTP |
| `llm-protocol/capability` | `complete` on `llm-generation-capability` |
| `llm-protocol/mcp` | `make-mcp-sampling-handler` |

Brief: [`llm.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/llm.md) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

```lisp
(asdf:load-system "llm-protocol")
(let ((b (stack-llm:make-mock-llm-backend)))
  (stack-llm:llm-result-text (stack-llm:generate b "hi")))
;; ⇒ "echo: hi"
```

```lisp
(asdf:load-system "llm-backend-openai")
;; OPENAI_API_KEY / OPENAI_BASE_URL (default http://127.0.0.1:1234/v1)
(stack-llm-openai:use-openai-compat-backend)
(stack-llm:generate stack-llm:*llm-backend* "hello" :model "gpt-4o-mini")
```

Live HTTP tests: `LLM_OPENAI_LIVE=1`. Wave-1 does not stream.

OCI: `ghcr.io/egao1980/cl-systems/llm-protocol:0.1.0` · `llm-backend-openai:0.1.0`. Cookbook: [cl-stack/docs/cookbooks/llm.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/llm.md).

## License

MIT — see [LICENSE](LICENSE).
