# Decoupling

Soundness modules are decoupled: unrelated libraries do not depend on one another
directly, even when they need to interoperate. Small shared abstractions — typeclasses
describing only what is needed — let two modules cooperate without either knowing of
the other, so each can be adopted à la carte and can evolve on its own. The seamless
integration that users experience comes from these common interfaces, not from a web of
hard dependencies between components that ought to remain separate.
