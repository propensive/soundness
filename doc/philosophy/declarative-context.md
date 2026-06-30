# Declarative Context

Soundness configures behaviour declaratively, through contextual values in scope rather
than through arguments threaded by hand or flags consulted at runtime. Choosing an
output format, an error strategy, a character encoding, or a particular implementation
is a matter of bringing the right `given` into scope, after which the choice applies
automatically to everything within that scope. Because the configuration is
declarative, it is independent of control flow and easy to reason about: what is in
scope determines what happens, wherever the affected code runs.
