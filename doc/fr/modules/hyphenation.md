- ## Hyphenation
+ ## Césure

- ### About
+ ### Présentation

- Breaking a word at the end of a line is acceptable only at certain points, and finding those
- points is the job of [hyphenation](https://en.wikipedia.org/wiki/Syllabification). Words are
- hyphenated with the same algorithm, and the same pattern files, as
- [TeX](https://en.wikipedia.org/wiki/TeX), whose typesetting the technique was invented for.
- A word gains its permissible break points, is hyphenated with a chosen character, or is
- split into its syllables.
+ Couper un mot en fin de ligne n'est acceptable qu'à certains points, et trouver ces points est
+ le travail de la [césure](https://en.wikipedia.org/wiki/Syllabification). Les mots sont coupés
+ avec le même algorithme, et les mêmes fichiers de motifs, que
+ [TeX](https://en.wikipedia.org/wiki/TeX), pour la composition duquel la technique a été
+ inventée. Un mot reçoit ses points de coupure admissibles, est coupé avec un caractère choisi,
+ ou est découpé en syllabes.

- The patterns of a language are a [contextual value], and American English is provided. The
- set of patterns can be extended, or replaced from any TeX pattern file, where a word breaks
- in a way which the standard patterns do not know.
+ Les motifs d'une langue sont une [valeur contextuelle], et l'anglais américain est fourni.
+ L'ensemble des motifs peut être étendu, ou remplacé à partir de n'importe quel fichier de motifs
+ TeX, lorsqu'un mot se coupe d'une façon que les motifs standard ne connaissent pas.

- ### On hyphenation
+ ### La césure

- Where a word may break is not something which an algorithm can read from its spelling.
- *hy-phen-ation* breaks between syllables which no simple rule identifies. Liang's algorithm,
- which was built for TeX, answers this with a dictionary of thousands of small scoring
- patterns, derived from real hyphenation dictionaries. It remains the standard, because it is
- accurate and fast, and its pattern files exist for many languages.
+ L'endroit où un mot peut se couper n'est pas quelque chose qu'un algorithme peut lire dans son
+ orthographe. *hy-phen-ation* se coupe entre des syllabes qu'aucune règle simple n'identifie.
+ L'algorithme de Liang, conçu pour TeX, y répond avec un dictionnaire de milliers de petits
+ motifs pondérés, dérivés de vrais dictionnaires de césure. Il reste la norme, car il est précis
+ et rapide, et ses fichiers de motifs existent pour de nombreuses langues.

- That algorithm is implemented here, against the standard pattern files, and is offered as
- three operations on [text](text.md), with the language supplied as a [given]:
+ Cet algorithme est implémenté ici, sur les fichiers de motifs standard, et proposé sous forme
+ de trois opérations sur le [texte](text.md), la langue étant fournie par un [given] :

```tabular
- column  Operation
- column  Returns
- column  For
- row  `hyphenate`  the `Text` with a hyphen character at each permissible break  text to be set in lines
- row  `syllables`  a `Sequence[Text]` of syllables and runs of non-letters  analysis of a word's parts
- row  `breakPoints`  an `Array[Int]` of break positions  code that places the breaks itself
+ column  Opération
+ column  Renvoie
+ column  Pour
+ row  `hyphenate`  le `Text` avec un caractère de césure à chaque coupure admissible  un texte à composer en lignes
+ row  `syllables`  une `Sequence[Text]` de syllabes et de suites de non-lettres  l'analyse des parties d'un mot
+ row  `breakPoints`  un `Array[Int]` de positions de coupure  un code qui place lui-même les coupures
```

- Breaking text at the right points is a pure function of the language's patterns, and stays
- one, which is [immutability](../../philosophy/immutability.md) in a domain which tempts
- stateful iteration.
+ Couper un texte aux bons endroits est une fonction pure des motifs de la langue, et le reste,
+ ce qui est l'[immuabilité](../../philosophy/immutability.md) dans un domaine qui incite à
+ l'itération avec état.

- Everything comes from the `soundness` package, with the English patterns in scope. The last
- import lets the samples print their results:
+ Tout provient du package `soundness`, avec les motifs anglais dans la portée. Le dernier import
+ permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import hyphenations.englishHyphenation
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Hyphenating
+ ### Couper les mots

- `hyphenate` inserts a hyphen character at each permissible break. The default is the
- Unicode [soft hyphen](https://en.wikipedia.org/wiki/Soft_hyphen), which is invisible unless
- a line actually breaks there. Passing a visible character shows the break points:
+ `hyphenate` insère un caractère de césure à chaque coupure admissible. Par défaut, c'est le
+ [trait d'union conditionnel](https://en.wikipedia.org/wiki/Soft_hyphen) d'Unicode, invisible
+ à moins qu'une ligne ne se coupe effectivement à cet endroit. Passer un caractère visible montre
+ les points de coupure :

```scala fuel
run
##
Out.println(t"hyphenation".hyphenate(hyphen = '-'))
Out.println(t"presentation".hyphenate(hyphen = '-'))
```

- `leftMin` and `rightMin` keep a minimum number of characters attached before the first
- break, and after the last, so that a break never strands one or two letters:
+ `leftMin` et `rightMin` gardent un nombre minimal de caractères attachés avant la première
+ coupure, et après la dernière, de sorte qu'une coupure ne laisse jamais une ou deux lettres
+ isolées :

```scala fuel
run
##
Out.println(t"hyphenation".hyphenate(hyphen = '-', leftMin = 4))
Out.println(t"presentation".hyphenate(hyphen = '-', leftMin = 4))
```

- ### Syllables
+ ### Syllabes

- `syllables` splits text into its parts. Each syllable is a segment, and each run of
- characters which are not letters passes through as a segment of its own:
+ `syllables` découpe un texte en ses parties. Chaque syllabe est un segment, et chaque suite de
+ caractères qui ne sont pas des lettres passe comme un segment à part entière :

```scala fuel
run
##
Out.println(t"the algorithm".syllables.to[List].inspect)
```

- ### Break points
+ ### Points de coupure

- `breakPoints` gives the raw positions at which a word may break. It is for code which places
- the breaks itself, such as an algorithm for wrapping lines, instead of inserting characters:
+ `breakPoints` donne les positions brutes auxquelles un mot peut se couper. C'est pour un code
+ qui place lui-même les coupures, comme un algorithme de retour à la ligne, au lieu d'insérer des
+ caractères :

```scala fuel
run
##
Out.println(t"hyphenation".breakPoints.to[List].inspect)
```

- ### Text that is not one word
+ ### Un texte qui n'est pas un seul mot

- Hyphenation applies to letters, and text is rarely only letters. Each run of letters is
- hyphenated independently. Everything else, such as spaces, punctuation and digits, passes
- through untouched:
+ La césure s'applique aux lettres, et un texte est rarement fait uniquement de lettres. Chaque
+ suite de lettres est coupée indépendamment. Tout le reste, comme les espaces, la ponctuation et
+ les chiffres, passe intact :

```scala fuel
run
##
Out.println(t"hello, world!".hyphenate(hyphen = '-'))
Out.println(t"the algorithm runs".hyphenate(hyphen = '-'))
```

- Both words of the first text are too short to break. `syllables` follows the same rule, and
- gives the runs which are not letters as segments of their own, so putting the segments back
- together reproduces the original text exactly.
+ Les deux mots du premier texte sont trop courts pour être coupés. `syllables` suit la même
+ règle, et donne les suites qui ne sont pas des lettres comme des segments à part entière :
+ recoller les segments reproduit donc exactement le texte d'origine.

- ### Extending the patterns
+ ### Étendre les motifs

- A word from a specialist field, which the standard patterns break wrongly, or not at all,
- is taught with an extra pattern, in TeX's notation, layered over the existing set:
+ Un mot d'un domaine spécialisé, que les motifs standard coupent mal, ou pas du tout, s'enseigne
+ avec un motif supplémentaire, dans la notation de TeX, superposé à l'ensemble existant :

```scala fuel
run
##
locally:
  given hyphenation: Hyphenation = englishHyphenation.extending(patterns = List(t"klm9nop"))
  Out.println(t"klmnop".hyphenate(hyphen = '-'))
```

- Patterns are merged by taking the higher score at each position. An added pattern with an
- odd score therefore restores a break which a standard pattern with an even score had
- forbidden. That is how one word is corrected, without disturbing the rest of the language.
+ Les motifs se fusionnent en prenant le score le plus élevé à chaque position. Un motif ajouté de
+ score impair rétablit donc une coupure qu'un motif standard de score pair avait interdite. C'est
+ ainsi qu'on corrige un mot, sans perturber le reste de la langue.

- Some words no pattern gets right. An *exception* states a word's breaks outright, and takes
- precedence over the algorithm:
+ Certains mots, aucun motif ne les traite correctement. Une *exception* énonce directement les
+ coupures d'un mot, et a priorité sur l'algorithme :

```scala fuel
run
##
locally:
  given hyphenation: Hyphenation = englishHyphenation.extending(exceptions = List(t"hy-phenation"))
  Out.println(t"hyphenation".hyphenate(hyphen = '-'))
```

- The standard English set already has the list of exceptions which TeX ships, so `associate`
- breaks as `as-so-ciate`, and `table` as `ta-ble`, where the patterns alone would not.
+ L'ensemble anglais standard contient déjà la liste d'exceptions que fournit TeX : `associate`
+ se coupe donc en `as-so-ciate`, et `table`
+ en `ta-ble`, là où les motifs seuls ne le feraient pas.

- A whole language is supplied in the same way. `Hyphenation.fromTex` reads any TeX pattern
- file, so the languages which are published for TeX are available by loading their patterns.
+ Une langue entière se fournit de la même façon. `Hyphenation.fromTex` lit n'importe quel fichier
+ de motifs TeX : les langues publiées pour TeX sont donc disponibles en chargeant leurs motifs.

- ### When no patterns are available
+ ### Quand aucun motif n'est disponible

- A language for which no set of patterns has been loaded hyphenates nothing, and does not
- guess. `hyphenate` gives the text unchanged, and `breakPoints` is empty. An algorithm for
- wrapping lines therefore falls back to breaking at spaces, which is the correct behavior,
- and does not invent breaks in a language whose rules it does not know.
+ Une langue pour laquelle aucun ensemble de motifs n'a été chargé ne coupe rien, et ne devine
+ pas. `hyphenate` rend le texte inchangé, et `breakPoints` est vide. Un algorithme de retour à la
+ ligne se replie donc sur les coupures aux espaces, ce qui est le comportement correct, et
+ n'invente pas de coupures dans une langue dont il ne connaît pas les règles.

- ### Related
+ ### Voir aussi

- - [Tables](tables.md) wraps the text of a column with hyphenation.
- - [Text](text.md) describes the other operations on text.
- - [PDF](pdf.md) and [Pages](pages.md) set paragraphs which break across lines.
+ - [Tableaux](tables.md) renvoie à la ligne le texte d'une colonne avec césure.
+ - [Texte](text.md) décrit les autres opérations sur le texte.
+ - [PDF](pdf.md) et [Pages](pages.md) composent des paragraphes qui se coupent sur plusieurs
+   lignes.
