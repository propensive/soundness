# Soundness Message Standards

_(Stub — scope for review; full content to follow.)_

This standard will define the house style for human-readable messages written with
the `m"…"` interpolator — the text of `fulminate` errors and `eucalyptus` log
events alike, since both share one style. It will cover the rules already applied
across the codebase: messages are lowercase, terse, present-tense, and carry no
trailing period; they interpolate the relevant context rather than describing it in
prose; they name the specific resource at fault (a path, a port, a URL, an
identifier) and quantify when it is cheap to do so; and they never embed secrets or
whole payloads. It will also cover how messages compose from typed values through
`Communicable`, so that formatting lives in one place and a message stays
structured rather than pre-rendered.
