## Internationalization

### About

A program which speaks several languages must supply a translation of every message, in
every language which it claims to support. The gaps are the bug: the missing French
translation is discovered by a French user.

Here the set of languages is tracked in the type of the multilingual value itself, so a set
of translations which lacks a required language is a compile error, not a fallback at
[runtime]. A multilingual value is a `Polyglot`, whose type parameter records exactly which
languages it has. Selecting a language needs a `Locale`, and a `Locale` for a language which
the value does not have does not typecheck.

### On multilingual text

The usual approach to translation is a resource bundle: a file of keyed strings for each
language, looked up by name at runtime. Nothing connects the keys in the code to the keys in
the files. A missing translation, a misspelled key, or a file for one language which lags
behind the others, all surface as fallbacks at runtime, usually in front of the one user who
chose that language.

Here the translations are in the code, and the set of languages is in the type:

- Each language is a value, which lifts a translation into a `Polyglot` of that one language.
- Alternatives are combined with `|`, and the combined type records all the languages which
  were provided.
- Code which requires a particular set of languages states it in a type, and only a complete
  translation satisfies it.

A locale which is a type parameter, not a string at runtime, means that a missing
translation is a compile error, which is
[safety by construction](../philosophy/safety-by-construction.md).

Everything comes from the `soundness` package. The second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Providing translations

Each supported language is a value which wraps a translation: `en`, `fr`, `de`, `es` and
`pl`. Translations are combined with `|`, and the type records what has been provided:

```scala
val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
```

A `Polyglot[Text, en & fr]` is a text which is available in English *and* in French.
Supplying only English where both are required does not compile. That is the point: the
demand for a French translation is enforced where the value is written, and is not
discovered by a French user.

```scala fuel
compile
errors show
##
val incomplete: Polyglot[Text, en & fr] = en(t"Hello")
```

### Selecting a language

A `Locale` names the language to read. Applying the polyglot, with a locale in scope, gives
the value for that language:

```scala fuel
run
##
locally:
  given locale: Locale[en & fr] = Locale(fr)
  Out.println(greeting())

locally:
  given locale: Locale[en & fr] = Locale(en)
  Out.println(greeting())
```

The type of the locale must cover languages which the polyglot has, so asking a value for a
language which it does not have is a compile error:

```scala fuel
compile
errors show
##
locally:
  given locale: Locale[en & de] = Locale(de)
  greeting()
```

### A locale from runtime data

The user's language usually arrives at runtime, from an HTTP header, an
[environment variable](environment.md) or a setting. A language code is
[decoded](decoding.md#decoding-a-value) to a `Locale`, and a code which is not recognized
falls back to English:

```scala
val locale = t"fr".as[Locale[en & pl & fr & de & es]]
```

With that locale in scope, every polyglot which covers those languages is read in the user's
language, and the completeness of every translation was already checked when the code was
compiled.

### Beyond text

A `Polyglot` is generic in what it holds, and is not fixed to text. A date format, a
convention for currency, or a whole page of content can vary by language through the same
mechanism, with the same guarantee that no supported language is missing.
[Time](time.md#describing-recurrences) uses a locale to describe a recurrence in words, and
[Catalogs](catalogs.md) keeps translations in files where that is wanted.
