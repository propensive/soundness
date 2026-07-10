# Immutability

Soundness values are immutable: once constructed, a value never changes, and any
operation that would alter it instead returns a new value. This removes whole
categories of bug — aliasing surprises, data races, and changes felt at a distance —
and makes a value safe to share freely across threads and scopes without defensive
copying. An immutable value is also easier to reason about, because its meaning is
fixed at the moment it is created and cannot be invalidated by code running somewhere
else.
