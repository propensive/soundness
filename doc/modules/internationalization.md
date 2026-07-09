## Internationalization

### About

A program that speaks several languages must supply a translation of every message in every
language it claims to support, and the gaps are the bug: the missing French translation is
discovered by a French user. Soundness tracks the set of languages in the type of the
multilingual value itself, so a translation set that lacks a required language is a compile
error, not a runtime fallback.

A multilingual value is a `Polyglot`, whose type parameter records exactly which languages it
carries. Selecting a language needs a `Locale`, and a `Locale` for a language the value does not
carry simply does not typecheck.

### On multilingual text

The usual approach to translation is a resource bundle: a file of keyed strings per language,
looked up by name at runtime. Nothing connects the keys in the code to the keys in the files, so
a missing translation, a misspelled key, or a language file that lags behind the others all
surface as runtime fallbacks — usually in front of the one user who chose that language.

Soundness puts the translations in the code and the language set in the type. Each language is a
value that lifts a translation into a single-language `Polyglot`, alternatives combine with `|`,
and the combined type is the intersection of the languages provided. Code that requires a
particular set of languages states it in a type, and only a complete translation satisfies it.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Providing translations

Each supported language — `en`, `fr`, `de`, `es`, `pl` — is a value that wraps a translation, and
translations combine with `|`. The type records what has been provided:

```scala
val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
```

A `Polyglot[Text, en & fr]` is a text available in English *and* French; requiring one where only
`en(t"Hello")` was supplied does not compile, which is the point — the demand for a French
translation is enforced where the value is written, not discovered by a French user.

### Selecting a language

A `Locale` names the language to read, and applying the polyglot with a locale in scope yields
that language's value:

```scala
given Locale[en & fr] = Locale(fr)

greeting()   // t"Bonjour"
```

Because the locale's type must be one of the polyglot's languages, asking a value for a language
it does not carry is a compile error.

### A locale from runtime data

The user's language usually arrives at runtime — from an HTTP header, an environment variable, a
setting. A language code decodes to a `Locale`, falling back to English for a code that is not
recognised:

```scala
val locale = t"fr".decode[Locale[en & pl & fr & de & es]]
```

With that locale in scope, every polyglot covering those languages reads in the user's language,
and the completeness of every translation was already checked when the code compiled.

### Beyond text

A `Polyglot` is generic in what it carries, not fixed to text. A date format, a currency
convention, a whole page of content can vary by language through the same mechanism, with the
same guarantee that no supported language is missing.
