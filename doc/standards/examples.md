# Example Data

Soundness draws its examples from Shakespeare. Every tutorial, API overlay, test, benchmark and
source comment that needs a name, a place, a record or an arbitrary string takes it from the
plays, preferring the well-known ones. The examples then read as one body of work: a reader who
has met `Person(t"Hamlet", 30)` in one tutorial meets the same cast in the next.

`etc/check-example-data.py`, run by `make build`, holds the line: no file may gain an example
from the retired vocabulary beyond what `etc/example-data-baseline.tsv` records.

## What is retired

- **People, real or invented.** No Alice and Bob, no Ada Lovelace, no Jon. This includes pets
  (Rex), other fiction (Gondor, *Dune*) and brands (Acme, the Beatles).
- **Company structure.** No `Company(ceo, cto)`, `Org`, `Firm`, `Employee`, `Department`,
  `salary` or `boss`, and no `Role`, `Worker`, `Team` or `Crew` standing in for one.
- **Metasyntactic placeholders.** No `foo`, `bar`, `baz`, `qux`, `quux` or `bippy`, as a value
  or as a type, and no "lorem ipsum".

## Principles

- **Exemplify the feature, not the old example.** A replacement need not resemble what it
  replaces. An `Employee` does not become a courtier: the old example is discarded and a new one
  chosen for what the sample or test must show — a nested record, two fields of one type, a list
  to update, an `Int` where `Text` was expected. That structural property is the only thing an
  example has to keep.
- **One play per example.** Hamlet duels Laertes, not Tybalt. Data taken from a single play
  reads as a scene rather than a list.
- **Prefer the well-known plays:** *Hamlet*, *Romeo and Juliet*, *Macbeth*, *Othello*, *King
  Lear*, *A Midsummer Night's Dream*, *The Tempest*, *Twelfth Night*, *Much Ado About Nothing*,
  *The Merchant of Venice*, *Julius Caesar*, and the *Henry IV* and *Henry V* plays.
- **Reuse the palette.** The shapes below recur everywhere. Reach for one of them before
  inventing another, and invent another whenever none shows the feature.
- **Avoid the characters who share a retired name.** Peter Quince, John of Gaunt and Paulina
  read as the arbitrary people they replace, and the check counts them.
- **Shakespeare is the one real person.** A sample book may name him as its author.

## The palette

```tabular
column  To show
column  Shape
column  Data
row  a flat record of `Text` and `Int`  `Person(name: Text, age: Int)`  Hamlet 30, Juliet 13, Lear 80; a plausible age where the text gives none
row  a record in a record; two fields of one type; field paths  `Play(title: Text, hero: Person, villain: Person)`, `Duel(challenger: Person, opponent: Person)`  Othello and Iago; Hamlet and Laertes; `hero.name`, `villain.age`
row  a list field; deep optics; an update inside a collection  `Play(title: Text, acts: List[Act])`, `Act(number: Int, scenes: List[Scene])`, `Scene(number: Int, place: Text)`  *Macbeth*: "A heath", "Dunsinane"
row  a list of records  `Troupe(name: Text, players: List[Person])`  Quince, Bottom, Flute, Snug, Snout, Starveling
row  a sum type  `enum Genre { case Comedy, Tragedy, History }`; `Verse` with `Sonnet(number: Int)` and `Song(title: Text)`
row  several scalar types  `Speech(speaker: Text, act: Int, scene: Int, aside: Boolean)`, `Sonnet(number: Int, firstLine: Text)`  Sonnet 18, "Shall I compare thee to a summer's day?"
row  an optional or defaulted field  `Person(name: Text, age: Optional[Int])`, `aside: Boolean = false`  the Fool, the Porter
row  a map field  `Play(title: Text, cast: Map[Text, Person])`
row  a recursive type  `Lineage(name: Text, heirs: List[Lineage])`  Lear; Goneril, Regan, Cordelia
row  an attribute beside an element; a keyed record  `Play(title: Text, @attribute year: Int)`  *Hamlet* 1603, *Macbeth* 1606
row  a name in parts  `FullName(first: Text, last: Text)`  Nick Bottom, Francis Flute, Toby Belch; Caius Martius Coriolanus
row  text that is deliberately not ASCII    Cæsar, Æmilia (the Folio's spellings)
row  two parties and an eavesdropper    Romeo, Juliet and Tybalt
row  hosts, users, paths, addresses    `elsinore.local`, the user `hamlet`, `/home/hamlet`, `juliet@example.com`, "Hamlet's Printer"
row  places and institutions    Elsinore, Verona, Venice, Dunsinane, the Globe
row  animals    Crab, Tray, Blanch, Sweetheart, Graymalkin
row  arbitrary strings, keys, path segments, identifiers    names or places from one play: `/verona/capulet/juliet`
row  arbitrary type names in a rule about types  `Play`, `Act`, `Scene`, `Sonnet`  `Play.Scene`, not `PlayScene`; `read[Play in Json]`
row  filler prose    a soliloquy
```

Addresses keep the reserved example domains (`example.com`, `.example`, `.local`): only the
local part and the host label are Shakespeare's.

## When the string's length matters

Some results depend on the length of a sample string: a rendered table's column widths, padded
or aligned text, a byte offset, an encoded length, a benchmark's payload size. A test will say
when such a result has gone wrong; an `// result` comment in `doc/api/*.tel` will not, since
nothing checks it. So where a length is load-bearing, choose a name of the length required, and
where a shown result does change, evaluate the sample again rather than edit the result by hand.

```tabular
column  Length
column  Names
row  3  Hal, Nym, Sly
row  4  Lear, Puck, Iago, Hero, Kent
row  5  Romeo, Viola, Ariel, Regan, Edgar, Feste
row  6  Hamlet, Juliet, Portia, Oberon, Banquo, Brutus, Cassio
row  7  Macbeth, Othello, Titania, Ophelia, Horatio, Miranda, Shylock
row  8  Prospero, Mercutio, Cordelia, Benedick, Beatrice, Falstaff, Rosalind
row  9  Desdemona, Cleopatra
```

An example that exists to show a defect keeps the defect: `verona..capulet` is still a path with
an empty segment, and an invented Unicode name must still not exist.

## What is not an example

The check counts examples, not vocabulary. These stay as they are:

- names that are the API or the protocol: units such as `Newton` and `Henry`; `Role` in
  harlequin, sibylline, synesthesia and bitumen; `Bar` and the chart bars; parasite's `Worker`;
  X.509's `Organization`; a reactor's boss thread;
- vendored and captured data: the IANA registry, the CommonMark specification, third-party
  OpenAPI documents and schemas, W3C samples, Unicode data, *War and Peace*;
- test vectors published by a standard, whose bytes are fixed (their variable names are not);
- released migration notes, which are a record of what was written;
- generic neutral data where no person or company is implied: shapes, colours, fruit, "Hello
  world", a pangram, and real cities where geography or time is the subject.

When the check counts one of these, name the file in the script's `GENUINE` table instead of
rewording it.

## Working through the residue

`python3 etc/check-example-data.py --list '<glob>'` prints every counted line under a path as a
queue; `--totals` prints the gauge. Lower a file's row with `--update` once a change has removed
its examples.
