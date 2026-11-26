# A configured model is a value

A provider value holds the model, the key and the tuning. Each tuning method returns a new
value and leaves the one it was called on alone, so one model can be the base of several
configurations, shared freely between them.

```scala fuel
compile
frame
  label A model
  code
      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")
frame
  label A terse variant of it
  highlight prompted  a new value, with a system prompt
  highlight limit(256)  and another, with a limit on its output
  code
      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")
      val messenger = bard.prompted(t"Answer in one line.").limit(256)
frame
  label And a florid one, from the same base
  highlight warmth(0.9)  bard itself has no prompt and no temperature
  code
      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")
      val messenger = bard.prompted(t"Answer in one line.").limit(256)
      val sonneteer = bard.prompted(t"Answer in iambic pentameter.").warmth(0.9)
##
```

[Providers and models](../../soundness/modules/llm.md#providers-and-models) lists the tuning
methods, and [immutability](../../philosophy/immutability.md) what sharing a value costs.
