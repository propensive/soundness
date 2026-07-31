# Correctness

Correctness is the end that Soundness's other principles serve: a program should be
constrained to behave correctly, and the language should help ensure that it does.
Where most libraries make correctness merely possible, Soundness tries to make
incorrectness impossible — moving checks to compiletime, ruling out invalid states, and
making failure visible in the types — so that a program which compiles is already known
to be right about a great deal. This is what "sound by default" means: safety is the
starting point, not an extra to be bolted on later.

## What "a great deal" amounts to

The claim is worth making concrete. In a program built on Soundness — each line below
with the relevant module's names in scope — the following are compile errors rather than
possibilities:

```scala
2012-Feb-30                            // a date that does not exist
Eur(3.01) + Gbp(2.50)                  // money in mismatched currencies
url"https:/example.com"                // a malformed URL
(3*Metre) + (4*Second)                 // incoherent dimensions
Br(Hr)                                 // a void element given children
json.verify[Employee].nope             // a field the schema does not declare
```

And the following are *visible in a signature*, so a caller cannot fail to know about
them: that an operation might fail and how; that it might block; that it needs the
network, the filesystem, or a random source; that it logs, and what.

None of this is exotic. Each is an ordinary mistake that ordinary programs make, caught
by construction rather than by review.

## Where the checking happens

Three mechanisms carry most of the weight, and they are described in their own right
elsewhere.

Values are checked **as they are constructed**, so possessing one is proof it is
well-formed — the subject of [safety by construction](safety-by-construction.md).

Types are shaped so that **invalid states have no representation**, and every transition
between states is **total** — the two golden rules, in
[impossible states](impossible-states.md) and [total transitions](total-transitions.md).

Requirements are stated **honestly in signatures**, so nothing an operation needs is
hidden and nothing it declares is unused — [honest signatures](honest-signatures.md).

## The conviction underneath

Any static analysis a programmer can perform, the compiler can perform better; and any
analysis the compiler can perform, it should.

A programmer checking by eye that every `match` covers every case does the compiler's
job, less reliably, and does it again after every change. The compiler does it on every
build, exhaustively, and reports every site affected by adding an enumeration case. The
question is therefore never *whether* to do the analysis but *who* does it — and the
answer is the same every time.

This is why exhaustivity warnings are treated as errors rather than as noise, why
`-Wunused` is on, and why a deliberately partial match must say so with `.absolve`
rather than being silently permitted. The discipline costs nothing except accidental
partiality, which is the kind that becomes a bug.

## What correctness is not

**It is not verification.** Soundness does not prove that a program computes the right
answer. It rules out large, well-understood classes of wrongness — malformed values,
impossible states, unhandled failures, escaped resources — and leaves the domain logic
to the programmer and the tests.

**It is not a guarantee about the outside world.** A `Hostname` is syntactically valid;
whether it resolves is not a static question. The discipline is to encode what is
genuinely determined by the value and to report the rest as typed failures, rather than
to invent a type that promises more than it can deliver.

**It is not free of judgement.** Every check moved to compiletime is a constraint on what
can be written, and a constraint that rules out more mistakes than it rules out
legitimate programs is a good one. That trade is made case by case, and it is possible to
get wrong in both directions.

## What it costs

Compiletime, mostly. Type-level computation, inlining and macro expansion are work the
compiler does on every build so the machine need not do it on every run, and a Soundness
project compiles more slowly than an equivalent one written loosely.

The second cost is a steeper start. A library that accepts `String` everywhere is easier
to begin with than one that wants a `Path on Linux`, a `Port` and a `MediaType`; the
payment comes back at the point where the loose version would have been debugging a
production incident. That is a real trade, and worth making knowingly rather than by
assumption.

See [zero cost](zero-cost.md) for why the runtime bill, at least, is close to nothing.
