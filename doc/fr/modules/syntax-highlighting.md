- ## Syntax Highlighting
+ ## Coloration syntaxique

- ### About
+ ### Présentation

- Scala source code is highlighted by the Scala compiler itself. The language is not
- approximated with regular expressions. The real tokenizer, and, when asked, the whole front
- end of the compiler, is run over the source, so the highlighting is exactly as the compiler
- reads it.
+ Le code source Scala est coloré par le compilateur Scala lui-même. Le langage n'est pas
+ approché par des expressions régulières. Le véritable analyseur lexical, et, sur demande, tout
+ le frontal du compilateur, est exécuté sur le code source : la coloration est donc exactement
+ celle de la lecture du compilateur.

- The result is a `SourceCode` of accented tokens, which is displayed as styled
- [terminal](terminal.md#styled-text) output, or as [HTML](html.md). At the deeper levels it
- has the *types* of its expressions, the compiler's diagnostics, and even completions.
+ Le résultat est un `SourceCode` de jetons accentués, qui s'affiche sous forme de sortie de
+ [terminal](terminal.md#styled-text) stylisée, ou en [HTML](html.md). Aux niveaux plus profonds,
+ il porte les *types* de ses expressions, les diagnostics du compilateur, et même des
+ complétions.

- ### On highlighting
+ ### La coloration

- Scala resists casual highlighting. Its soft keywords are keywords only in certain positions.
- Its syntax for givens, and its significant whitespace, defeat grammars based on patterns.
- The highlighting files of editors chase the language, release by release. The one program
- which always tokenizes Scala correctly is the compiler, so the reliable highlighter is the
- compiler, asked politely.
+ Scala résiste à la coloration superficielle. Ses mots-clés souples ne sont des mots-clés qu'à
+ certaines positions. Sa syntaxe des givens, et son indentation significative, mettent en échec
+ les grammaires fondées sur des motifs. Les fichiers de coloration des éditeurs courent après le
+ langage, version après version. Le seul programme qui découpe toujours correctement Scala en
+ jetons est le compilateur : le colorateur fiable est donc le compilateur, interrogé poliment.

- It is driven at three depths, each of which is chosen by a given in the package
- `highlighting`:
+ Il est piloté à trois profondeurs, chacune choisie par un given du package `highlighting` :

```tabular
- column  Depth
- column  Given
- column  Needs
- column  Gives
- row  tokenized  `tokenizedScala`, the default  the text alone  classified tokens
- row  typechecked  `typecheckedScala`  a compiler and a classpath  types and diagnostics, from the frontend
- row  typechecked  `sessionScala`  a warm compiler session  the same, within the session
- row  compiled  `compiledScala`  a compiler and a classpath  the diagnostics only later phases produce
+ column  Profondeur
+ column  Given
+ column  Exige
+ column  Donne
+ row  découpé en jetons  `tokenizedScala`, par défaut  le texte seul  des jetons classés
+ row  typé  `typecheckedScala`  un compilateur et un classpath  types et diagnostics, issus du frontal
+ row  typé  `sessionScala`  une session de compilateur déjà chaude  la même chose, au sein de la session
+ row  compilé  `compiledScala`  un compilateur et un classpath  les diagnostics que seules produisent les phases ultérieures
```

- A highlighted token which knows its type is
- [composability](../../philosophy/composability.md) between the compiler and whatever displays
- the code.
+ Un jeton coloré qui connaît son type, c'est la
+ [composabilité](../../philosophy/composability.md) entre le compilateur et ce qui affiche le
+ code.

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*
```

- ### Highlighting
+ ### Coloration

- `Scala.highlight` tokenizes source into lines of accented tokens, such as keywords,
- identifiers, numbers, strings and modifiers, ready to display:
+ `Scala.highlight` découpe le code source en lignes de jetons accentués, comme les mots-clés, les
+ identifiants, les nombres, les chaînes et les modificateurs, prêts à l'affichage :

```scala
val code = Scala.highlight(t"val xs = List(1, 2, 3)")
```

- The accents are the categories to which a palette assigns colors:
+ Les accents sont les catégories auxquelles une palette attribue des couleurs :

```tabular
- column  Accent
- column  Marks
- row  `Keyword`  keywords, other than modifiers
- row  `Modifier`  modifier keywords
- row  `Term`, `Typal`  definitions
- row  `Symbol`  operators
- row  `Parens`  brackets
- row  `Number`, `String`  literals, including interpolated ones
- row  `Error`, `Unparsed`  tokens that did not come out cleanly
+ column  Accent
+ column  Marque
+ row  `Keyword`  les mots-clés, autres que les modificateurs
+ row  `Modifier`  les mots-clés modificateurs
+ row  `Term`, `Typal`  les définitions
+ row  `Symbol`  les opérateurs
+ row  `Parens`  les parenthèses et crochets
+ row  `Number`, `String`  les littéraux, y compris interpolés
+ row  `Error`, `Unparsed`  les jetons qui ne sont pas sortis proprement
```

- The default depth is tokenized, which needs nothing but the text. `Java.highlight` does the
- same for Java source.
+ La profondeur par défaut est le découpage en jetons, qui n'exige rien d'autre que le texte.
+ `Java.highlight` fait de même pour le code source Java.

- A palette, as a given, maps the accents to [colors](colors.md), and the ANSI renderers,
- numbered or plain, turn a `SourceCode` into terminal output. Fenced `scala` and `java` code
- blocks in a [Markdown](markdown.md) document are displayed through the same machinery.
+ Une palette, sous forme de given, associe les accents à des [couleurs](colors.md), et les
+ moteurs de rendu ANSI, numérotés ou bruts, transforment un `SourceCode` en sortie de terminal.
+ Les blocs de code `scala` et `java` délimités d'un document [Markdown](markdown.md) s'affichent
+ par la même machinerie.

- ### Types and diagnostics
+ ### Types et diagnostics

- With a [compiler](compiler.md) and a [classpath](classpath.md) in scope, `typecheckedScala`
- runs the front end, and each identifier has the type which the compiler gave it. That is the
- difference between coloring `xs` as an identifier, and knowing that it is a `List[Int]`:
+ Avec un [compilateur](compiler.md) et un [classpath](classpath.md) dans la portée,
+ `typecheckedScala` exécute le frontal, et chaque identifiant a le type que lui a donné le
+ compilateur. C'est la différence entre colorer `xs` comme un identifiant, et savoir que c'est
+ une `List[Int]` :

```scala fuel
typecheck
##
given scalac: Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
given classpath: LocalClasspath = LocalClasspath()
import highlighting.typecheckedScala

val typed = Scala.highlight(t"val xs = List(1, 2, 3)")
```

- `typecheckedScala` comes from the module `harlequin.typed`, which is the only part of the
- highlighter that depends on the compiler.
+ `typecheckedScala` provient du module `harlequin.typed`, la seule partie du colorateur qui
+ dépend du compilateur.

- Source which is ill-typed does not fail to highlight. It is highlighted *with its errors*.
- Each `Diagnostic` has its span and its message, which is what documentation tools and
- editors need:
+ Un code source mal typé ne fait pas échouer la coloration. Il est coloré *avec ses erreurs*.
+ Chaque `Diagnostic` a sa plage et son message, ce dont ont besoin les outils de documentation
+ et les éditeurs :

```scala
val diagnostics = Scala.highlight(t"""val n: Int = "oops"""").diagnostics
```

- ### Completions
+ ### Complétions

- A caret position turns highlighting into completion. The compiler's own interactive engine
- proposes what could follow, with kinds and signatures:
+ Une position de curseur transforme la coloration en complétion. Le moteur interactif propre au
+ compilateur propose ce qui pourrait suivre, avec les natures et les signatures :

```scala
val source = t"val xs = List(1, 2, 3)\nval y = xs.m"
val completions = Scala.highlight(source, caret = source.length.z).completions
```

- Under a typechecked depth, the proposals here are `map`, `max`, `mkString` and the other
- members of `List[Int]` which begin with `m`. They come from the same compiler which will
- eventually compile the code, so they are never guesses.
+ À une profondeur typée, les propositions sont ici `map`, `max`, `mkString` et les autres
+ membres de `List[Int]` qui commencent par `m`. Elles proviennent du même compilateur qui
+ compilera finalement le code : ce ne sont donc jamais des suppositions.

- Members after a `.` are the easy case. A bare identifier which is being typed is harder,
- because the statement which contains it does not typecheck. That is precisely its state
- while it is being written, and a batch compilation discards the code. Completions therefore
- come from the compiler's interactive driver, which keeps code that has errors. Terms and
- types which are in scope are completed wherever the caret is, and in the position of a type,
- the offers are narrowed to types, modules and packages.
+ Les membres après un `.` sont le cas facile. Un identifiant nu en cours de saisie est plus
+ difficile, car l'instruction qui le contient ne passe pas le typage. C'est précisément son état
+ pendant qu'on l'écrit, et une compilation par lots écarte le code. Les complétions proviennent
+ donc du pilote interactif du compilateur, qui conserve le code qui comporte des erreurs. Les
+ termes et les types dans la portée sont complétés où que soit le curseur, et à la position
+ d'un type, les propositions se limitent aux types, aux modules et aux packages.

- ### Completing keywords and dynamic members
+ ### Compléter les mots-clés et les membres dynamiques

- Two kinds of proposal which the compiler cannot make are supplied beside its own.
+ Deux sortes de propositions que le compilateur ne peut pas faire sont fournies à côté des
+ siennes.

- *Keywords* are grammatical, not semantic. What may follow depends on the tokens before the
- caret, not on any table of symbols. A trie over reversed contexts of tokens, derived from a
- corpus of real Scala, and looking back only as far as remains relevant, answers which
- keywords could come next, and what the grammar expects there. `case ` therefore offers what
- a case may begin with, and a position which must be a type offers no keywords at all.
+ Les *mots-clés* sont grammaticaux, et non sémantiques. Ce qui peut suivre dépend des jetons qui
+ précèdent le curseur, et non d'une quelconque table des symboles. Un trie sur des contextes de
+ jetons inversés, dérivé d'un corpus de vrai code Scala, et ne regardant en arrière qu'aussi
+ loin que cela reste pertinent, indique quels mots-clés pourraient venir ensuite, et ce que la
+ grammaire attend à cet endroit. `case ` propose donc ce par quoi un cas peut commencer, et une
+ position qui doit être un type ne propose aucun mot-clé.

- *Dynamic* members have no symbols to offer at all. Where the caret sits on a selection whose
- qualifier derives from `scala.Dynamic`, the companion of the type is asked which members its
- refinement admits, if it implements the interface for saying so. That is how a
- [foreign](foreign-interop.md) value is completed with the members which its foreign
- declarations name, and how any `Dynamic` type can offer honest completions, instead of none.
+ Les membres *dynamiques* n'ont aucun symbole à proposer. Lorsque le curseur se trouve sur une
+ sélection dont le qualificateur dérive de `scala.Dynamic`, on demande au compagnon du type
+ quels membres admet son raffinement, s'il implémente l'interface qui permet de le dire. C'est
+ ainsi qu'une valeur [étrangère](foreign-interop.md) est complétée avec les membres que nomment
+ ses déclarations étrangères, et que tout type `Dynamic` peut offrir des complétions honnêtes,
+ au lieu d'aucune.

- ### Related
+ ### Voir aussi

- - [Compiler](compiler.md) describes running the Scala compiler from a program.
- - [LSP](lsp.md) serves completions and diagnostics to an editor.
- - [Markdown](markdown.md) displays highlighted code blocks.
- - [Terminal](terminal.md) and [HTML](html.md) are the two forms of output.
+ - [Compilateur](compiler.md) décrit l'exécution du compilateur Scala depuis un programme.
+ - [LSP](lsp.md) fournit complétions et diagnostics à un éditeur.
+ - [Markdown](markdown.md) affiche des blocs de code colorés.
+ - [Terminal](terminal.md) et [HTML](html.md) sont les deux formes de sortie.
