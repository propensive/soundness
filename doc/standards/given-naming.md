# Soundness Contextual-Value Naming Standards

_(Stub — scope for review; full content to follow. May instead be folded into
`naming.md`.)_

This standard will define how importable `given` values are named and grouped, so
that bringing one into scope reads clearly at the import site. It will document the
`<component>.<family>.<name>` scheme — a contextual value lives in a package object
named for its family (the plural role it fills) and has a descriptive name for the
particular choice it represents, as in `strategies.throwUnsafely`,
`charEncoders.utf8Encoder`, `formatting.compactJsonFormatting`, and
`logFormats.standardLogFormat`. It will set out how to choose the family name, how
the name should read as the object of an `import`, and the conventions for the
package objects that collect related givens for à-la-carte import.
