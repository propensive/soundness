- ## Internationalization
+ ## Internationalisation

- ### About
+ ### Présentation

- A program which speaks several languages must supply a translation of every message, in
- every language which it claims to support. The gaps are the bug: the missing French
- translation is discovered by a French user.
+ Un programme qui parle plusieurs langues doit fournir une traduction de chaque message, dans
+ chaque langue qu'il prétend prendre en charge. Les lacunes sont le bogue : la traduction
+ française manquante est découverte par un utilisateur francophone.

- Here the set of languages is tracked in the type of the multilingual value itself, so a set
- of translations which lacks a required language is a compile error, not a fallback at
- [runtime]. A multilingual value is a `Polyglot`, whose type parameter records exactly which
- languages it has. Selecting a language needs a `Locale`, and a `Locale` for a language which
- the value does not have does not typecheck.
+ Ici, l'ensemble des langues est suivi dans le type de la valeur multilingue elle-même : un
+ ensemble de traductions auquel manque une langue requise est donc une erreur de compilation, et
+ non un repli [à l'exécution][exécution]. Une valeur multilingue est un `Polyglot`, dont le
+ paramètre de type enregistre exactement les langues qu'il possède. Sélectionner une langue exige
+ une `Locale`, et une `Locale` pour une langue que la valeur ne possède pas ne passe pas le typage.

- ### On multilingual text
+ ### Le texte multilingue

- The usual approach to translation is a resource bundle: a file of keyed strings for each
- language, looked up by name at runtime. Nothing connects the keys in the code to the keys in
- the files. A missing translation, a misspelled key, or a file for one language which lags
- behind the others, all surface as fallbacks at runtime, usually in front of the one user who
- chose that language.
+ L'approche habituelle de la traduction est un resource bundle : un fichier de chaînes indexées
+ par clé pour chaque langue, recherchées par leur nom à l'exécution. Rien ne relie les clés du
+ code aux clés des fichiers. Une traduction manquante, une clé mal orthographiée, ou le fichier
+ d'une langue qui est en retard sur les autres, tout cela apparaît sous forme de replis à
+ l'exécution, généralement sous les yeux du seul utilisateur qui a choisi cette langue.

- Here the translations are in the code, and the set of languages is in the type:
+ Ici, les traductions sont dans le code, et l'ensemble des langues est dans le type :

- - Each language is a value, which lifts a translation into a `Polyglot` of that one language.
- - Alternatives are combined with `|`, and the combined type records all the languages which
-   were provided.
- - Code which requires a particular set of languages states it in a type, and only a complete
-   translation satisfies it.
+ - Chaque langue est une valeur, qui élève une traduction en un `Polyglot` de cette seule langue.
+ - Les alternatives se combinent avec `|`, et le type combiné enregistre toutes les langues
+   fournies.
+ - Le code qui exige un ensemble particulier de langues l'énonce dans un type, et seule une
+   traduction complète le satisfait.

- A locale which is a type parameter, not a string at runtime, means that a missing
- translation is a compile error, which is
- [safety by construction](../../philosophy/safety-by-construction.md).
+ Une locale qui est un paramètre de type, et non une chaîne à l'exécution, signifie qu'une
+ traduction manquante est une erreur de compilation : c'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md).

- Everything comes from the `soundness` package. The second import lets the samples print
- their results:
+ Tout provient du package `soundness`. Le deuxième import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Providing translations
+ ### Fournir des traductions

- Each supported language is a value which wraps a translation: `en`, `fr`, `de`, `es` and
- `pl`. Translations are combined with `|`, and the type records what has been provided:
+ Chaque langue prise en charge est une valeur qui enveloppe une traduction : `en`, `fr`, `de`,
+ `es` et `pl`. Les traductions se combinent avec `|`, et le type enregistre ce qui a été fourni :

```scala
val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
```

- A `Polyglot[Text, en & fr]` is a text which is available in English *and* in French.
- Supplying only English where both are required does not compile. That is the point: the
- demand for a French translation is enforced where the value is written, and is not
- discovered by a French user.
+ Un `Polyglot[Text, en & fr]` est un texte disponible en anglais *et* en français. Ne fournir que
+ l'anglais là où les deux sont requis ne compile pas. C'est tout l'intérêt : l'exigence d'une
+ traduction française est imposée là où la valeur est écrite, et n'est pas découverte par un
+ utilisateur francophone.

```scala fuel
compile
errors show
##
val incomplete: Polyglot[Text, en & fr] = en(t"Hello")
```

- ### Selecting a language
+ ### Sélectionner une langue

- A `Locale` names the language to read. Applying the polyglot, with a locale in scope, gives
- the value for that language:
+ Une `Locale` nomme la langue à lire. Appliquer le polyglot, avec une locale dans la portée, donne
+ la valeur pour cette langue :

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

- The type of the locale must cover languages which the polyglot has, so asking a value for a
- language which it does not have is a compile error:
+ Le type de la locale doit couvrir des langues que possède le polyglot : demander à une valeur une
+ langue qu'elle ne possède pas est donc une erreur de compilation :

```scala fuel
compile
errors show
##
locally:
  given locale: Locale[en & de] = Locale(de)
  greeting()
```

- ### A locale from runtime data
+ ### Une locale à partir de données d'exécution

- The user's language usually arrives at runtime, from an HTTP header, an
- [environment variable](environment.md) or a setting. A language code is
- [decoded](decoding.md#decoding-a-value) to a `Locale`, and a code which is not recognized
- falls back to English:
+ La langue de l'utilisateur arrive généralement à l'exécution, depuis un en-tête HTTP, une
+ [variable d'environnement](environment.md) ou un réglage. Un code de langue est
+ [décodé](decoding.md#decoding-a-value) en une `Locale`, et un code non reconnu se replie sur
+ l'anglais :

```scala
val locale = t"fr".as[Locale[en & pl & fr & de & es]]
```

- With that locale in scope, every polyglot which covers those languages is read in the user's
- language, and the completeness of every translation was already checked when the code was
- compiled.
+ Avec cette locale dans la portée, chaque polyglot qui couvre ces langues est lu dans la langue
+ de l'utilisateur, et la complétude de chaque traduction a déjà été vérifiée lors de la
+ compilation du code.

- ### Beyond text
+ ### Au-delà du texte

- A `Polyglot` is generic in what it holds, and is not fixed to text. A date format, a
- convention for currency, or a whole page of content can vary by language through the same
- mechanism, with the same guarantee that no supported language is missing.
- [Time](time.md#describing-recurrences) uses a locale to describe a recurrence in words, and
- [Catalogs](catalogs.md) keeps translations in files where that is wanted.
+ Un `Polyglot` est générique dans ce qu'il contient, et n'est pas figé au texte. Un format de
+ date, une convention monétaire, ou toute une page de contenu peuvent varier selon la langue par
+ le même mécanisme, avec la même garantie qu'aucune langue prise en charge ne manque.
+ [Temps](time.md#describing-recurrences) utilise une locale pour décrire une récurrence en mots,
+ et [Catalogues](catalogs.md) conserve les traductions dans des fichiers lorsqu'on le souhaite.

- ### Related
+ ### Voir aussi

- - [Display](display.md), [money](money.md) and [web pages](pages.md) render values for a locale.
+ - [Affichage](display.md), [monnaie](money.md) et [pages web](pages.md) rendent des valeurs pour
+   une locale.
