# Error Handling

Soundness handles errors generically, separating code that can fail from the decision
of what to do when it does. An operation declares in its type the errors it may raise,
and the caller chooses a strategy — throw, recover with a default, accumulate several
failures, or treat the failure as a checked outcome — without the operation needing to
know which. This makes failure visible in the types, with the discipline of checked
exceptions but none of their rigidity, and it lets the same fallible code be used
unsafely while prototyping and then made totally safe later, with no rewrite. The
errors themselves are [expressive, immutable values](expressive-errors.md).
