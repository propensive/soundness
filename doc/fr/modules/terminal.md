- ## Terminal
+ ## Terminal

- ### About
+ ### Présentation

- An interactive program in a terminal needs three things which the raw console does not give:
- styled output, input from the keyboard as events, and a way to lay out and refresh regions of
- the screen. All three are provided:
+ Un programme interactif dans un terminal a besoin de trois choses que la console brute ne
+ fournit pas : une sortie stylée, les entrées du clavier sous forme d'événements, et un moyen
+ de disposer et de rafraîchir des régions de l'écran. Les trois sont fournies :

- - The `e"…"` [interpolator] writes text with ANSI styles, as a value.
- - An `interactive` block puts the terminal in raw mode, and turns keystrokes into a typed
-   stream of events.
- - A layout layer composes panels, editors and menus into interfaces, full-screen or inline,
-   which redraw without flicker.
+ - L'[interpolateur] `e"…"` écrit du texte avec des styles ANSI, sous forme de valeur.
+ - Un bloc `interactive` met le terminal en mode brut, et transforme les frappes au clavier
+   en un flux typé d'événements.
+ - Une couche de mise en page compose des panneaux, des éditeurs et des menus en interfaces,
+   en plein écran ou en ligne, qui se redessinent sans scintillement.

- ### On terminals
+ ### À propos des terminaux

- The terminal is a protocol in which control travels in the same channel as content. Styling,
- movement of the cursor and reports from the keyboard all travel as escape sequences, mixed
- into the text.
+ Le terminal est un protocole dans lequel le contrôle circule dans le même canal que le
+ contenu. Le style, les déplacements du curseur et les signalements du clavier circulent tous
+ sous forme de séquences d'échappement, mêlées au texte.

- A program which prints escape codes directly ties itself to one terminal's dialect, and
- corrupts output which ends up in a file. A program which reads its input a byte at a time
- must decode the sequences of several bytes which keys send. And raw mode which is left
- enabled after a crash wrecks the user's shell.
+ Un programme qui affiche directement des codes d'échappement se lie au dialecte d'un seul
+ terminal, et corrompt la sortie qui finit dans un fichier. Un programme qui lit ses entrées
+ octet par octet doit décoder les séquences de plusieurs octets qu'envoient les touches. Et
+ un mode brut resté activé après un plantage ruine le shell de l'utilisateur.

- Here the protocol is kept at the edges:
+ Ici, le protocole est maintenu en périphérie :

- - Styled text is a `Teletype`: a value which carries its styles. It is rendered to the
-   capabilities of the *actual* terminal when it is output, and to plain text where there is
-   no terminal.
- - Input arrives as typed events, decoded from the sequences.
- - The raw-mode session is a scoped block, which restores the terminal however it exits. That
-   is [delimited scopes](../../philosophy/delimited-scopes.md), applied to the terminal.
+ - Le texte stylé est un `Teletype` : une valeur qui porte ses styles. Il est rendu selon les
+   capacités du terminal *réel* au moment de la sortie, et en texte brut là où il n'y a pas
+   de terminal.
+ - Les entrées arrivent sous forme d'événements typés, décodés à partir des séquences.
+ - La session en mode brut est un bloc délimité, qui restaure le terminal quelle que soit
+   la manière dont il se termine. Ce sont les
+   [portées délimitées](../../philosophy/delimited-scopes.md), appliquées au terminal.

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*
import stdios.javaLangSystemStdio
import termcapDefinitions.xtermTrueColorTermcap
```

- The second import says where output goes, and the third says what the terminal can display:
- here, full 24-bit color.
+ Le deuxième import indique où va la sortie, et le troisième ce que le terminal peut
+ afficher : ici, la couleur 24 bits complète.

- ### Styled text
+ ### Texte stylé

- The `e"…"` interpolator styles the spans which it marks. The styles are values, and the
- [colors](colors.md) are colors:
+ L'interpolateur `e"…"` applique un style aux étendues qu'il marque. Les styles sont des
+ valeurs, et les [couleurs](colors.md) sont des couleurs :

```scala
val message = e"$Bold(Warning): ${Fg(WebColors.Red)}(disk full)"
```

```scala fuel
run
##
Out.println(message)
```

- `Bold`, `Italic`, `Underline` and their siblings mark spans, and `Fg` and `Bg` color them.
+ `Bold`, `Italic`, `Underline` et leurs semblables marquent des étendues, et `Fg` et `Bg` les
+ colorent.

- Colors nest, which raw ANSI does not allow. An escape code sets a color. It cannot restore
- the color which was there before, and can only reset to the default, so a colored region
- inside another colored region ends by clearing both. A `Teletype` tracks the style as a
- stack instead, so the enclosing color is restored where the inner region ends:
+ Les couleurs s'imbriquent, ce que l'ANSI brut ne permet pas. Un code d'échappement définit
+ une couleur. Il ne peut pas restaurer la couleur qui était là auparavant, et peut seulement
+ revenir à la couleur par défaut ; une région colorée à l'intérieur d'une autre région
+ colorée se termine donc en effaçant les deux. Un `Teletype` suit plutôt le style sous forme
+ de pile, de sorte que la couleur englobante est restaurée là où se termine la région
+ intérieure :

```scala fuel
run
##
import WebColors.{Gold, Indigo, HotPink, White}

Out.println(e"$Gold(gold, $Indigo(indigo, $HotPink(hot pink), indigo) $White(and) gold)")
```

- Each substitution is a *transformation* of the prevailing style, not an absolute setting,
- which is what makes that work. It means that a substitution can depend on what it modifies.
- A type becomes usable as one by being given a `Stylize` instance:
+ Chaque substitution est une *transformation* du style en vigueur, et non une définition
+ absolue, et c'est ce qui permet cela. Cela signifie qu'une substitution peut dépendre de ce
+ qu'elle modifie. Un type devient utilisable comme telle en recevant une instance de
+ `Stylize` :

```scala
case object Toggle

given toggleStylize: Stylize[Toggle.type] = _ => Stylize(style => style.copy(bold = !style.bold))
```

```scala fuel
run
##
Out.println(e"plain, $Toggle(bold, $Toggle(plain again), bold), plain")
```

- `$Toggle(…)` makes plain text bold, and bold text plain. It decides from the style which it
- finds, and does not impose a fixed one.
+ `$Toggle(…)` met le texte brut en gras, et le texte gras en brut. Elle décide d'après le
+ style qu'elle trouve, et n'en impose pas un fixe.

- A `Teletype` behaves as [text](text.md#any-textual-type). It is cut, joined and padded, with
- its styles preserved. It is rendered for each terminal: in full 24-bit color on a capable
- terminal, in the nearest color of the palette on an older one, and as plain text when the
- output is not a terminal at all, so that logs never fill with escape codes.
- [Environment](environment.md#the-machine) describes how the capabilities of the real terminal
- are detected.
+ Un `Teletype` se comporte comme du [texte](text.md#any-textual-type). On le découpe, le
+ joint et le complète, en préservant ses styles. Il est rendu pour chaque terminal : en
+ couleur 24 bits complète sur un terminal qui en est capable, dans la couleur la plus proche
+ de la palette sur un terminal plus ancien, et en texte brut lorsque la sortie n'est pas du
+ tout un terminal, afin que les journaux ne se remplissent jamais de codes d'échappement.
+ [Environnement](environment.md#the-machine) décrit comment les capacités du terminal réel
+ sont détectées.

- ### Keyboard input
+ ### Entrées clavier

- `interactive` opens a raw-mode session. Within it, the terminal's events arrive as typed
- values: keypresses, resizes of the window, changes of focus, and pastes. The session runs
- under [supervision](concurrency.md), and takes the console and the
- [environment](environment.md) from context. An application's
- [`Cli`](cli.md) provides the console:
+ `interactive` ouvre une session en mode brut. À l'intérieur, les événements du terminal
+ arrivent sous forme de valeurs typées : frappes de touches, redimensionnements de la
+ fenêtre, changements de focus et collages. La session s'exécute sous
+ [supervision](concurrency.md), et prend la console et
+ l'[environnement](environment.md) dans le contexte. Le [`Cli`](cli.md) d'une application
+ fournit la console :

```scala fuel
typecheck
##
import environments.javaBaseEnvironment
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads

given console: Console = Console(summon[Stdio])

def handle(char: Char): Unit = ()
def finish(): Unit = ()
def resize(rows: Int, cols: Int): Unit = ()

supervise:
  interactive: terminal ?=>
    terminal.eventIterator().each:
      case Keypress.CharKey(char)  => handle(char)
      case Keypress.Ctrl('C')      => finish()
      case Terminal.Info.WindowSize(rows, cols) => resize(rows, cols)
      case _                       => ()
```

- The decoding covers combinations of modifiers, function keys, the kitty keyboard protocol
- and bracketed paste. `Ctrl(Alt(Left))` is therefore a value to match, not a sequence of bytes
- to recognize.
+ Le décodage couvre les combinaisons de modificateurs, les touches de fonction, le protocole
+ clavier de kitty et le collage entre crochets (bracketed paste). `Ctrl(Alt(Left))` est donc
+ une valeur à filtrer, et non une suite d'octets à reconnaître.

- A `Keypress` is an ordinary `CharKey`, a `FunctionKey`, one of the named editing keys, or an
- `EscapeSeq` for a sequence with no other name. The named keys are `Tab`, `Enter`,
- `Backspace`, `Delete`, `Escape`, `Insert`, `Home`, `End`, `PageUp`, `PageDown` and the
- arrows. Each may be wrapped in the modifiers which were held with it, nesting outwards:
+ Un `Keypress` est un `CharKey` ordinaire, une `FunctionKey`, l'une des touches d'édition
+ nommées, ou une `EscapeSeq` pour une séquence sans autre nom. Les touches nommées sont
+ `Tab`, `Enter`, `Backspace`, `Delete`, `Escape`, `Insert`, `Home`, `End`, `PageUp`,
+ `PageDown` et les flèches. Chacune peut être enveloppée dans les modificateurs qui étaient
+ maintenus avec elle, en s'imbriquant vers l'extérieur :

```scala
val selectAll = Keypress.Ctrl('A')
val newLine = Keypress.Shift(Keypress.Enter)
```

- The wrappers accept only what can meaningfully be modified. `Shift(CharKey('a'))` does not
- typecheck: a shifted `a` is `CharKey('A')`, and the shift has already been applied.
+ Les enveloppes n'acceptent que ce qui peut raisonnablement être modifié.
+ `Shift(CharKey('a'))` ne passe pas la vérification de types : un `a` avec Maj est
+ `CharKey('A')`, et la touche Maj a déjà été appliquée.

- A keypress is displayed with a Unicode symbol in brackets for each special key, joined with
- `+` to what it modifies, as in `[⇧]+[↵]`, `[⌃]+[C]` or `[⌥]+[→]`:
+ Une frappe est affichée avec un symbole Unicode entre crochets pour chaque touche spéciale,
+ relié par `+` à ce qu'il modifie, comme dans `[⇧]+[↵]`, `[⌃]+[C]` ou `[⌥]+[→]` :

```tabular
- column  Keypress
- column  Renders as
- row  `CharKey('a')`  `[a]`
- row  `CharKey(' ')`  `[␣]`
- row  `FunctionKey(5)`  `[F5]`
- row  `Tab`, `Enter`  `[⇥]`, `[↵]`
- row  `Backspace`, `Delete`, `Insert`  `[⌫]`, `[⌦]`, `[⎀]`
- row  `Escape`  `[⎋]`
- row  `Up`, `Down`, `Left`, `Right`  `[↑]`, `[↓]`, `[←]`, `[→]`
- row  `Home`, `End`  `[↖]`, `[↘]`
- row  `PageUp`, `PageDown`  `[⇞]`, `[⇟]`
- row  `EscapeSeq(id, …)`  `[⎋id]`
- row  `Shift(…)`, `Ctrl(…)`, `Alt(…)`, `Meta(…)`  `[⇧]+…`, `[⌃]+…`, `[⌥]+…`, `[⌘]+…`
+ column  Frappe
+ column  Rendu
+ row  `CharKey('a')`  `[a]`
+ row  `CharKey(' ')`  `[␣]`
+ row  `FunctionKey(5)`  `[F5]`
+ row  `Tab`, `Enter`  `[⇥]`, `[↵]`
+ row  `Backspace`, `Delete`, `Insert`  `[⌫]`, `[⌦]`, `[⎀]`
+ row  `Escape`  `[⎋]`
+ row  `Up`, `Down`, `Left`, `Right`  `[↑]`, `[↓]`, `[←]`, `[→]`
+ row  `Home`, `End`  `[↖]`, `[↘]`
+ row  `PageUp`, `PageDown`  `[⇞]`, `[⇟]`
+ row  `EscapeSeq(id, …)`  `[⎋id]`
+ row  `Shift(…)`, `Ctrl(…)`, `Alt(…)`, `Meta(…)`  `[⇧]+…`, `[⌃]+…`, `[⌥]+…`, `[⌘]+…`
```

- The vocabulary is deliberately separate from anything which produces or consumes it. The
- terminal decodes escape sequences into these values, and
- [web automation](web-automation.md) turns them into the actions which a browser understands,
- without either needing to know about the other.
+ Ce vocabulaire est délibérément séparé de tout ce qui le produit ou le consomme. Le
+ terminal décode les séquences d'échappement en ces valeurs, et
+ l'[automatisation web](web-automation.md) les transforme en actions que comprend un
+ navigateur, sans que l'un ait besoin de connaître l'autre.

- Optional capabilities, such as tracking of the mouse, reporting of focus and the alternate
- screen, are switched on by importing the corresponding given from `terminalFeatures`, and
- are switched off again when the session ends.
+ Les capacités optionnelles, comme le suivi de la souris, le signalement du
+ focus et l'écran alternatif, s'activent en important le given correspondant depuis
+ `terminalFeatures`, et se désactivent à nouveau à la fin de la session.

- ### Line editing and menus
+ ### Édition de ligne et menus

- A line of input, with movement of the cursor and the editing keys handled, is a
- `LineEditor`. A choice among options is a `SelectMenu`. Each is a pure state machine, which
- takes an event and gives a new state, and is asked for its result in one call:
+ Une ligne de saisie, avec la gestion des déplacements du curseur et des touches d'édition,
+ est un `LineEditor`. Un choix parmi des options est un `SelectMenu`. Chacun est un automate
+ à états pur, qui prend un événement et donne un nouvel état, et auquel on demande son
+ résultat en un seul appel :

```scala fuel
typecheck
##
supervise:
  interactive: terminal ?=>
    LineEditor().ask: text =>
      Out.println(t"You entered: $text")
```

- ### Layout
+ ### Mise en page

- Interfaces which are larger than a line are composed from panes:
+ Les interfaces plus grandes qu'une ligne se composent de volets :

```tabular
- column  Combinator
- column  Makes
- row  `panel`  a pane of content
- row  `editor`, `menu`  an interactive widget: a `LineEditor` or a menu of options
- row  `strip`  panes side by side, as columns
- row  `stack`  panes one above another, as rows
- row  `border`  a frame around a pane
+ column  Combinateur
+ column  Produit
+ row  `panel`  un volet de contenu
+ row  `editor`, `menu`  un widget interactif : un `LineEditor` ou un menu d'options
+ row  `strip`  des volets côte à côte, en colonnes
+ row  `stack`  des volets les uns au-dessus des autres, en lignes
+ row  `border`  un cadre autour d'un volet
```

- `conduct` runs the arrangement. It runs full-screen, on the alternate buffer, or *inline*, as
- a live block at the cursor, which leaves the scrollback intact:
+ `conduct` exécute l'agencement. Il s'exécute en plein écran, sur le tampon alternatif, ou
+ *en ligne*, comme un bloc vivant au niveau du curseur, ce qui laisse l'historique de
+ défilement intact :

```scala fuel
typecheck
##
import ultimatum.border

supervise:
  interactive: terminal ?=>
    val pages = List(t"Home", t"Settings", t"About")
    val sidebar = border(BorderStyle.rounded)(menu(pages, pages.head.or(t""), maxWidth = 20))
    val body = border(BorderStyle.heavy)(editor(LineEditor()))

    conduct(Tui.Occupancy.Inline)(strip(sidebar, body))
```

- Sizes are solved as fractions, with minima and maxima, and focus moves between widgets with
- Tab. Live status is embedded in the same way: progress bars, spinners, meters and step
- indicators. [Gauges](gauges.md) describes them.
+ Les tailles sont résolues sous forme de fractions, avec des minima et des maxima, et le
+ focus passe d'un widget à l'autre avec Tab. L'état en direct s'intègre de la même manière :
+ barres de progression, indicateurs d'activité, jauges et indicateurs d'étapes.
+ [Jauges](gauges.md) les décrit.

- ### Redrawing without flicker
+ ### Redessiner sans scintillement

- Rendering keeps a model of what is actually on the screen, and each frame is compared with
- it. A repaint therefore overprints only the runs which changed, and an identical frame emits
- nothing at all. Full-screen output is buffered, and flushed as one write, not as a stream of
- small ones. A change of geometry, or a resize, takes the path of a full redraw, because
- nothing about the previous contents can then be relied upon.
+ Le rendu conserve un modèle de ce qui est réellement à l'écran, et chaque trame est
+ comparée à ce modèle. Un rafraîchissement ne réécrit donc que les portions qui ont changé,
+ et une trame identique n'émet rien du tout. La sortie plein écran est mise en tampon, et
+ vidée en une seule écriture, et non en une suite de petites écritures. Un changement de
+ géométrie, ou un redimensionnement, emprunte le chemin d'un redessin complet, car on ne peut
+ alors plus se fier à rien du contenu précédent.

- An inline block, which leaves the scrollback intact, has a further problem. The terminal may
- reflow its lines when the window narrows, moving the block out from under the cursor. An
- inline block therefore works out again where it is after a resize, so that a running
- interface survives a reflow, and does not scribble over the scrollback above it.
+ Un bloc en ligne, qui laisse l'historique de défilement intact, pose un problème
+ supplémentaire. Le terminal peut redistribuer ses lignes quand la fenêtre se rétrécit,
+ déplaçant le bloc hors de portée du curseur. Un bloc en ligne recalcule donc sa position
+ après un redimensionnement, afin qu'une interface en cours d'exécution survive à une
+ redistribution, et ne griffonne pas sur l'historique de défilement au-dessus d'elle.

- ### Related
+ ### Voir aussi

- - [Colors](colors.md) describes the colors which `Fg` and `Bg` take.
- - [Tables](tables.md) and [Gauges](gauges.md) draw structured output in a terminal.
- - [Command-line interfaces](cli.md) reads a program's arguments and completes them.
- - [Terminal emulator](terminal-emulator.md) is the other side: it interprets the escape
-   sequences which a program writes.
- - [Processes](processes.md#on-a-terminal) runs another program on a pseudo-terminal.
+ - [Couleurs](colors.md) décrit les couleurs que prennent `Fg` et `Bg`.
+ - [Tableaux](tables.md) et [Jauges](gauges.md) dessinent une sortie structurée dans un
+   terminal.
+ - [Interfaces en ligne de commande](cli.md) lit les arguments d'un programme et les
+   complète.
+ - [Émulateur de terminal](terminal-emulator.md) est l'autre côté : il interprète les
+   séquences d'échappement qu'écrit un programme.
+ - [Processus](processes.md#on-a-terminal) exécute un autre programme sur un
+   pseudo-terminal.
