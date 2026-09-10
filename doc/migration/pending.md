# Changes since 0.65.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.65.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## polyvinyl

- `Specification.fields` is now `List[(Text, Member)]` (ordered), not `Map[Text, Member]`.
  Implementations must return the fields in the order a named tuple's elements should take;
  a `Map` can be converted with `List.from(map.stdlib)`, but its order is arbitrary. (#1974)
- `Member.Record`'s second parameter, formerly `map: Map[Text, Member]`, is now
  `fields: List[(Text, Member)]`. (#1974)
- `Structural.transform` is now polymorphic: `def transform[value](data: Origin, make: Origin
  => value): constructor[value]`, formerly `def transform(data: Origin, make: Origin => Record):
  constructor[Record]`. An instance can no longer be written as a lambda (a polymorphic method
  is not a SAM); write `new Structural[constructor]: …` with the type members `Self`, `Origin`
  and `Form`, or take a polymorphic function `[value] => (Origin, Origin => value) =>
  constructor[value]` as `JsonBlueprint.structural` does. (#1974)
- New: `Specification.tuple(value: Expr[Origin])`, a macro helper beside `build`, returning
  `Expr[NamedTuple.AnyNamedTuple]`. Declare `transparent inline def tuple(value: Origin):
  NamedTuple.AnyNamedTuple = ${tuple('value)}` in a specification object to produce named
  tuples with one element per field, in field order, read eagerly at construction. An
  `Intensional` result of the form `success raises error` becomes an element of type `success`,
  with a `Tactic[error]` required at the call site. (#1974)
- `polyvinyl.core` now depends on `contingency.core`. (#1974)

## jacinta

- `JsonBlueprint.Doc.properties` is now `Json` (the schema's `properties` object, kept in
  document order), not `Map[Text, JsonBlueprint.Property]`; `JsonBlueprint.Doc.fields` returns
  `List[(Text, Member)]`. Use `JsonBlueprint.entries(doc.properties)` for the decoded
  `List[(Text, JsonBlueprint.Property)]`. (#1974)
- `JsonBlueprint.Property.properties` and `.items` are now `Optional[Json]`, not
  `Optional[Map[Text, Json]]`; `objectFields` and `arrayFields` return `List[(Text, Member)]`.
  (#1974)
- New: `JsonBlueprint.entries(json: Json): List[(Text, JsonBlueprint.Property)]` and
  `JsonBlueprint.structural[name, constructor[_]](lambda: [value] => (Json, Json => value) =>
  constructor[value])`. (#1974)
- The givens `JsonBlueprint.array`, `optionalArray`, `module` and `optionalModule` keep their
  names and labels but are now explicit `Structural` instances (see polyvinyl). (#1974)

## stratiform

- `TelBlueprint.fieldsOf` returns `List[(Text, Member)]` in schema order, not `Map[Text,
  Member]`; `TelBlueprint.fields` likewise. (#1974)
