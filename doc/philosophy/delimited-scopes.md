# Delimited Scopes

Soundness uses context functions to delimit the blocks within which an extra capability
or piece of context is available. Entering such a block — to handle errors, to run
concurrent tasks, to hold a resource open — makes the relevant contextual values
available inside it and withdraws them at its boundary. The reach of a capability is
therefore visible in the structure of the code itself: a capability applies exactly
where the block says it does, and not beyond. This structural confinement is what
[capture checking](capture-checking.md) then enforces in the types.
