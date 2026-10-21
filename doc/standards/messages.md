# Soundness Message Standards

This document defines the house style for human-readable messages written with the
`m"…"` interpolator: the text of `fulminate` errors, the `Message` carried by an
`eucalyptus` log event, and the diagnostics a macro reports. All three share one style,
because a reader meets them in the same places — a terminal, a log, a compiler error — and
should not be able to tell from the prose which module wrote it. It complements
`logging.md` (when and what to log) and `naming.md` (how error types are named). Examples
are drawn from `lib/*/src/core/*.scala`.

## 1. Register

A message states a fact about the world at the moment it was written, as a knowledgeable
colleague would say it aloud: calm, specific and brief.

- **Lowercase.** A message begins in lowercase, unless its first word is a proper noun or
  an identifier (`Burdock can only repackage a JAR file, not a directory`).
- **No trailing period.** A message is a fragment that the surrounding rendering punctuates;
  it is embedded in a stack trace, prefixed with a code, or listed as a bullet, and a
  terminal period would be wrong in each.
- **Present tense, indicative mood.** State what *is*, not what *happened* or what *should*
  be: `the JAR has no manifest`, not `the JAR did not have a manifest` or `the JAR should
  have a manifest`. The exception is an operation that failed as a whole, which is past
  tense because the failure is the fact: `could not list the releases of $repository (HTTP
  $status)`.
- **Terse.** Say the one thing the reader needs in order to act. No apology (`sorry`), no
  alarm (`fatal`, `!`), no advice unless the remedy is both certain and short, and no
  restatement of what the error type's name already says.
- **No jargon from the implementation.** A message names things in the reader's terms —
  a port, a path, a header, a field — never a class, a method or a variable of the
  library that raised it. `unknown variables reference` is addressed to a DAP client; `null
  frameRef in scopes()` would not be.

## 2. Structure

A message has a subject, which is the specific thing at fault, and a predicate, which is
what is wrong with it. Lead with the subject when it is the thing the reader will look for:

    m"the value $value is not a valid ISO 8601 repeating interval"
    m"dependency $hash is neither published nor cached"
    m"a command cannot end with an escape character"

Interpolate context; do not describe it. A message that says `the file could not be read`
makes the reader find the file; `${path} could not be read` does not. Name the specific
resource at fault — a path, a port, a URL, an identifier, a header name, a line and column —
and quantify whenever the number is already to hand: `${items.size} CBOR issues`, not `some
CBOR issues`.

Where the cause is known and distinct from the symptom, append it in parentheses or after a
colon, in that order of preference:

    m"could not list the releases of $repository (HTTP $status)"
    m"the gRPC call failed with status $status: $detail"

A message that aggregates others says how many, and lets the `List[Message]` instance of
`Communicable` render them as bullets beneath it:

    m"${List.size(errors)} accrued errors: $joined"

## 3. Vocabulary

- *is not a valid X* for a value that fails to parse as X; *is not a known X* for a value
  that parses but is not in the set.
- *could not* + verb for an operation that failed in the environment (network, filesystem,
  process); *cannot* + verb for a request the library refuses by construction (`Burdock can
  only repackage a JAR file`).
- *has no X* for a missing part; *already has X* for a conflicting one.
- *must* only for a rule the reader can satisfy by changing their input: `a port number
  must be specified after a colon`.
- Prefer the singular, concrete noun (`the manifest`, `this JAR`, `the document`) to a
  category (`input`, `data`, `object`), and the definite article when there is one such
  thing in context.
- Never *invalid*, *illegal*, *bad* or *error* as the whole predicate: they name the fact
  that a message exists, not its content.

## 4. What a message must not contain

- **Secrets.** No token, password, key material, cookie or `Authorization` value, even
  partially. Name the credential's *source* if it helps (`the GITHUB_TOKEN environment
  variable`), never its value.
- **Payloads.** No request or response body, no file contents, no serialized document.
  Quote the offending *fragment* — a line, a token, a field — and the position.
- **Stack traces or exception text.** A message that wraps a Java exception restates the
  cause in the terms of §3; the original is attached as the error's cause, not pasted into
  its text.
- **Formatting.** No ANSI escapes, no Markdown, no alignment spaces: a `Message` is
  structured, and the renderer decides how it looks.

## 5. Composition through `Communicable`

A `Message` is not a string. The `m"…"` interpolator keeps every interpolated value as a
`Message` part, converted through the value's `Communicable` instance, so formatting lives in
one place per type rather than at each message. Consequences:

- Interpolate the typed value, not its rendering: `m"…$path…"`, not `m"…${path.show}…"`.
  The `Path` instance decides how a path reads in prose, and every message about paths
  agrees.
- Give a type that appears in messages a `Communicable` instance in its companion, usually
  derived from its `Textualizable` or `Showable` instance; do not render by hand at the
  call site.
- Text that may be empty or padded is quoted by the `Text` instance (`“ ”`) so that an
  absent or whitespace value is visible; never add quotation marks of your own around an
  interpolated `Text`.
- A list of messages is a list, not a joined string: `List[Message]` renders as bullets, one
  per line. Do not `join` messages with commas.
- Compile-time diagnostics raised by a macro carry their `SN-` code in the standard prefix
  (`m"[↯$errorCode] $message"`); the message body follows this standard unchanged.

## 6. Log events

A log event is a value of a module's `Event` type, and its `Communicable` instance renders
the message. The same register applies, with one addition: an event names the operation and
its subject (`resolved $hash to $url`, `listening on $port`), in the past tense when it has
completed and the present participle when it is beginning. Levels are chosen by
`logging.md`; the message does not restate the level (`warning:` is not part of a message).

## 7. Checklist

Before adding a message, read it as it will appear — after `[↯SN-…]`, in a list of
twelve, or at the end of a stack trace — and check:

1. lowercase first letter, no trailing period, present tense;
2. the specific resource at fault is interpolated, as a typed value;
3. a number is given when one is known;
4. nothing secret, no payload, no implementation names;
5. the error type's name and the message do not say the same thing.
