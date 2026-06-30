# Optionality

Soundness represents an absent value with a flat `Optional`, not with `null` and not
with `Option`. Unlike `null`, an `Optional` is visible in the type and cannot be
dereferenced by accident; unlike `Option`, it adds no wrapper around the value, so a
present value is simply the value itself and optionality does not stack into nested
layers. Absence is dealt with where it arises rather than threaded through the whole
program, which keeps the common case — a value that is present — direct and
unencumbered. In Soundness, `null` is treated as both unrepresentable and unnecessary.
