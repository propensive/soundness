- ## Language Server Protocol
+ ## Language Server Protocol

- ### About
+ ### Présentation

- A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server is
- one call to `Lsp.listen`. Its block registers a handler for each feature which the server
- provides, and the server then serves over standard input and output. The protocol underneath,
- with its JSON-RPC messages, their length-prefixed framing, the bookkeeping of open documents and
- the capabilities announced to the editor, is handled. The code which remains is the logic of
- the language.
+ Un serveur [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
+ tient en un appel à `Lsp.listen`. Son bloc enregistre un gestionnaire pour chaque
+ fonctionnalité que fournit le serveur, et le serveur sert ensuite sur l'entrée et la sortie
+ standard. Le protocole sous-jacent, avec ses messages JSON-RPC, leur tramage préfixé par la
+ longueur, la comptabilité des documents ouverts et les capacités annoncées à l'éditeur, est
+ pris en charge. Le code qui reste est la logique du langage.

- The vocabulary of the protocol is modeled as ordinary types. A `Position` is a line and a
- character. A `Range` is two positions. A `Diagnostic` is a range with a severity and a message.
- A hover response, a completion list and a set of document symbols each have their type. A
- handler receives these as typed values and returns them as typed values, never as JSON
- assembled by hand.
+ Le vocabulaire du protocole est modélisé par des types ordinaires. Une `Position` est une
+ ligne et un caractère. Un `Range` est deux positions. Un `Diagnostic` est une plage avec une
+ gravité et un message. Une réponse de survol, une liste de complétions et un ensemble de
+ symboles de document ont chacun leur type. Un gestionnaire les reçoit et les renvoie sous
+ forme de valeurs typées, jamais sous forme de JSON assemblé à la main.

- ### On the protocol
+ ### À propos du protocole

- An editor which understands LSP can use the server of any language, and a language which
- provides a server works in any such editor. That leverage is why the protocol exists. It is also
- why the protocol is large: dozens of request and notification types, each with its own JSON
- shape, exchanged over a framed stream. Implementing it by hand means marshaling JSON and
- tracking the versions of documents, which is work with nothing to do with the language being
- served.
+ Un éditeur qui comprend LSP peut utiliser le serveur de n'importe quel langage, et un langage
+ qui fournit un serveur fonctionne dans n'importe quel éditeur de ce type. C'est pour cet effet
+ de levier que le protocole existe. C'est aussi pourquoi il est vaste : des dizaines de types
+ de requêtes et de notifications, chacun avec sa propre forme JSON, échangés sur un flux tramé.
+ L'implémenter à la main oblige à sérialiser du JSON et à suivre les versions des documents, un
+ travail qui n'a rien à voir avec le langage servi.

- That work is done once here. Each message type is a Scala type with a derived JSON codec. Each
- request is dispatched to the matching registered handler. The open documents are tracked, with
- incremental edits applied. What is left to write is the part specific to a language: what a
- hover shows, which completions to offer, which diagnostics to report. Declaring the methods of
- a server as typed handlers keeps the implementation
- [honest](../../philosophy/honest-signatures.md) about what it supports.
+ Ce travail est fait une fois pour toutes ici. Chaque type de message est un type Scala avec un
+ codec JSON dérivé. Chaque requête est répartie vers le gestionnaire enregistré correspondant.
+ Les documents ouverts sont suivis, et les modifications incrémentales appliquées. Ce qui reste
+ à écrire est la partie propre à un langage : ce que montre un survol, quelles complétions
+ proposer, quels diagnostics signaler. Déclarer les méthodes d'un serveur comme des
+ gestionnaires typés garde l'implémentation
+ [honnête](../../philosophy/honest-signatures.md) sur ce qu'elle prend en charge.

- Everything comes from the `soundness` package. The LSP vocabulary itself, which is the handler
- registration, the ambient document and the protocol types, comes from `Lsp`, imported inside
- the server's own object, where it takes precedence over any name the umbrella exports:
+ Tout provient du package `soundness`. Le vocabulaire LSP lui-même, c'est-à-dire
+ l'enregistrement des gestionnaires, le document ambiant et les types du protocole, provient de
+ `Lsp`, importé à l'intérieur de l'objet du serveur, où il a la précédence sur tout nom
+ qu'exporte le package parapluie :

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
import probates.awaitProbate
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
```

- The samples on this page serve or launch language servers, so they are compiled but not run.
+ Les exemples de cette page servent ou lancent des serveurs de langage ; ils sont donc compilés
+ mais pas exécutés.

- ### Defining a server
+ ### Définir un serveur

- A server registers its features in the block given to `Lsp.listen`. There is no record of
- capabilities to maintain. Each capability is advertised to the editor exactly when its handler
- is registered, so the declaration can never disagree with the implementation. An option which a
- capability needs beyond its presence, such as completion trigger characters or a legend for
- semantic tokens, is a parameter of the registration.
+ Un serveur enregistre ses fonctionnalités dans le bloc donné à `Lsp.listen`. Il n'y a pas de
+ registre de capacités à maintenir. Chaque capacité est annoncée à l'éditeur exactement
+ lorsque son gestionnaire est enregistré ; la déclaration ne peut donc jamais contredire
+ l'implémentation. Une option dont une capacité a besoin au-delà de sa présence, comme les
+ caractères déclencheurs de complétion ou une légende pour les jetons sémantiques, est un
+ paramètre de l'enregistrement.

- Inside a handler, the context of the request is ambient, so no URIs or parameters are threaded
- by hand:
+ À l'intérieur d'un gestionnaire, le contexte de la requête est ambiant ; aucune URI ni aucun
+ paramètre n'est donc passé à la main :

```tabular
- column  Name
- column  Is
- row  `document`  a live view of the document the request concerns: its text, line access, position/offset conversion and the word under a position
- row  `workspace`  the other open documents, and what the editor reported at initialization
- row  `client`  the channel back to the editor
- row  `position`, `range`, `newName`, …  the request's payload
+ column  Nom
+ column  Est
+ row  `document`  une vue vivante du document que concerne la requête : son texte, l'accès aux lignes, la conversion position/décalage et le mot sous une position
+ row  `workspace`  les autres documents ouverts, et ce que l'éditeur a indiqué à l'initialisation
+ row  `client`  le canal de retour vers l'éditeur
+ row  `position`, `range`, `newName`, …  la charge utile de la requête
```

- Here is a whole server, with its `main` in the resident-daemon idiom described below:
+ Voici un serveur complet, avec son `main` dans l'idiome du démon résident décrit plus bas :

```scala
object DemoServer:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.listen(t"Demo", t"0.1.0"):
          hover:
            document.word(position).let: word =>
              Hover(MarkupContent(value = t"**$word**"))

          complete():
            CompletionList
              ( items = List
                  ( CompletionItem(label = t"alpha", kind = CompletionItemKind.Keyword),
                    CompletionItem(label = t"beta",  kind = CompletionItemKind.Keyword) ) )

          opened:
            client.publishDiagnostics
              ( document.uri,
                List
                  ( Diagnostic
                      ( range    = document.fullRange,
                        severity = DiagnosticSeverity.Warning,
                        message  = t"a diagnostic for the whole document" ) ) )

          command(t"demo.run"):
            raise(Lsp.Error(Lsp.Error.Reason.RequestFailed, t"nothing to run"))
            Unset
      Exit.Ok
```

- The fragments which follow each belong inside such a `listen` block.
+ Les fragments qui suivent appartiennent chacun à un tel bloc `listen`.

- Registrations exist for the full surface of the protocol. Definitions, references, document
- symbols, formatting, renaming, code actions, signature help, folding, semantic tokens, call and
- type hierarchies, workspace symbols, file operations and the `*/resolve` family all follow the
- same shape: the payload is ambient, and a typed result comes out.
+ Des enregistrements existent pour toute la surface du protocole. Définitions, références,
+ symboles de document, formatage, renommage, actions de code, aide à la signature, repliement,
+ jetons sémantiques, hiérarchies d'appels et de types, symboles de l'espace de travail,
+ opérations sur les fichiers et la famille `*/resolve` suivent tous la même forme : la charge
+ utile est ambiante, et il en sort un résultat typé.

- ### Documents and diagnostics
+ ### Documents et diagnostics

- The server tracks each open document as the editor reports it, and applies incremental changes
- at the protocol's UTF-16 offsets. The lifecycle is observed by registering `opened`, `changed`,
- `saved` and `closed` handlers, each of which sees the current state of the document. From any
- handler, `client` is the channel back to the editor. That is how a server pushes diagnostics,
- which are errors and warnings, rather than waiting to be asked, as the `opened` registration
- above does:
+ Le serveur suit chaque document ouvert tel que l'éditeur le signale, et applique les
+ modifications incrémentales aux décalages UTF-16 du protocole. Le cycle de vie s'observe en
+ enregistrant des gestionnaires `opened`, `changed`, `saved` et `closed`, qui voient chacun
+ l'état courant du document. Depuis n'importe quel gestionnaire, `client` est le canal de
+ retour vers l'éditeur. C'est ainsi qu'un serveur pousse des diagnostics, c'est-à-dire des
+ erreurs et des avertissements, plutôt que d'attendre qu'on les lui demande, comme le fait
+ l'enregistrement `opened` ci-dessus :

```scala fuel
typecheck
##
object DiagnosingServer:
  import Lsp.*

  def serve(): Unit = supervise:
    Lsp.listen(t"Diagnosing", t"0.1.0"):
      opened:
        client.publishDiagnostics
          ( document.uri,
            List
              ( Diagnostic
                  ( range    = document.fullRange,
                    severity = DiagnosticSeverity.Warning,
                    message  = t"a diagnostic for the whole document" ) ) )
```

- `client` also carries `showMessage` and `logMessage`, for notices shown to the user and lines
- written to the editor's log.
+ `client` porte aussi `showMessage` et `logMessage`, pour les avis montrés à l'utilisateur et
+ les lignes écrites dans le journal de l'éditeur.

- ### Observing the traffic
+ ### Observer le trafic

- The standard output of a server is reserved for the wire protocol, so the server cannot print a
- record of what it is exchanging. `Lsp.listen` takes an optional observer instead. It sees each
- message as the JSON body it was carried as: inbound before it is parsed, so a message which
- fails to decode is observed too, and outbound before it is framed:
+ La sortie standard d'un serveur est réservée au protocole de communication ; le serveur ne
+ peut donc pas y afficher un relevé de ce qu'il échange. `Lsp.listen` prend à la place un
+ observateur optionnel. Celui-ci voit chaque message sous la forme du corps JSON qui l'a
+ transporté : en entrée avant son analyse, si bien qu'un message qui ne se décode pas est
+ observé lui aussi, et en sortie avant son tramage :

```scala
val observer = new Lsp.Observer:
  def received(message: Text): Unit = Err.println(t"recv $message")
  def sent(message: Text): Unit = Err.println(t"send $message")
```

- Passing it as `Lsp.listen(t"Demo", t"0.1.0", observer)` traces the exchange on standard error.
+ Le passer sous la forme `Lsp.listen(t"Demo", t"0.1.0", observer)` trace l'échange sur la
+ sortie d'erreur standard.

- What the observer does with a message is the server's own concern. It might stream it to a
- second process, write it to a trace file, or do nothing at all. Omitting the parameter observes
- nothing.
+ Ce que l'observateur fait d'un message est l'affaire du serveur. Il peut le transmettre en
+ continu à un second processus, l'écrire dans un fichier de trace, ou ne rien faire du tout.
+ Omettre le paramètre n'observe rien.

- ### Reporting errors
+ ### Signaler les erreurs

- A handler which cannot answer raises a typed fault, and continues, as the `demo.run` command
- above does with `Lsp.Error(Lsp.Error.Reason.RequestFailed, t"nothing to run")`.
+ Un gestionnaire qui ne peut pas répondre lève une faute typée, et continue, comme le fait la
+ commande `demo.run` ci-dessus avec
+ `Lsp.Error(Lsp.Error.Reason.RequestFailed, t"nothing to run")`.

- A raised fault pre-empts the result of the handler. For a request, it becomes a JSON-RPC error
- response, with the wire code of the reason and the id of the request. For a notification,
- which may not be answered, it is reported through `window/logMessage`. A request for a document
- which is not open, and a command which was never registered, are answered the same way.
+ Une faute levée prend le pas sur le résultat du gestionnaire. Pour une requête, elle devient
+ une réponse d'erreur JSON-RPC, avec le code de transmission de la raison et l'identifiant de la
+ requête. Pour une notification, à laquelle on ne peut pas répondre, elle est signalée par
+ `window/logMessage`. Une requête concernant un document qui n'est pas ouvert, et une commande
+ qui n'a jamais été enregistrée, reçoivent une réponse de la même manière.

- ### Talking to a server
+ ### Parler à un serveur

- The same vocabulary reads in the other direction. `Lsp.Server` describes a language server which
- this process can speak to: a command to launch, or a pair of streams already connected to one.
- `session` opens the channel, lends a connection for the duration of a block, and disposes of
- both afterwards. A connection cannot escape the block, so it can never outlive the server which
- answers it:
+ Le même vocabulaire se lit dans l'autre sens. `Lsp.Server` décrit un serveur de langage
+ auquel ce processus peut parler : une commande à lancer, ou une paire de flux déjà connectés à
+ un serveur. `session` ouvre le canal, prête une connexion pour la durée d'un bloc, et libère
+ les deux ensuite. Une connexion ne peut pas s'échapper du bloc ; elle ne peut donc jamais
+ survivre au serveur qui lui répond :

```scala
def query(source: Text): Optional[Lsp.Hover] = supervise:
  Lsp.Server(sh"rust-analyzer").session: server ?=>
    server.initialize(root = t"file:///project")
    server.initialized()
    server.open(t"file:///project/src/main.rs", t"rust", source)
    server.hover(t"file:///project/src/main.rs", Lsp.Position(10, 4))
```

- Requests return the same types which the handlers of a server return: `Optional[Hover]`,
- `CompletionList`, `List[Location]`. Notifications, which are `open`, `edit`, `save` and
- `close`, return nothing. An error response from the server is raised as an `Lsp.Error` which
- carries the reason its wire code names, and is not awaited forever. Requests are answered on a
- task of their own, so several may be in flight at once, and may come back in any order.
+ Les requêtes renvoient les mêmes types que les gestionnaires d'un serveur : `Optional[Hover]`,
+ `CompletionList`, `List[Location]`. Les notifications, à savoir `open`, `edit`, `save` et
+ `close`, ne renvoient rien. Une réponse d'erreur du serveur est levée sous forme d'une
+ `Lsp.Error` qui porte la raison que nomme son code de transmission, et n'est pas attendue
+ indéfiniment. Les requêtes reçoivent leur réponse sur une tâche propre ; plusieurs peuvent
+ donc être en cours à la fois, et revenir dans n'importe quel ordre.

- What a server says unbidden, such as diagnostics, window messages and progress, reaches an
- `Lsp.Listener`. Its methods all default to doing nothing, so a client implements only what it
- acts on. A listener is supplied contextually:
+ Ce qu'un serveur dit sans qu'on le lui demande, comme les diagnostics, les messages de fenêtre
+ et la progression, parvient à un `Lsp.Listener`. Ses méthodes ne font rien par défaut ; un
+ client n'implémente donc que ce sur quoi il agit. Un écouteur est fourni contextuellement :

```scala
given diagnostics: Lsp.Listener = new Lsp.Listener:
  override def diagnostics(uri: Text, version: Optional[Int], reports: List[Lsp.Diagnostic])
  :   Unit =
    reports.each { report => Err.println(t"$uri: ${report.message}") }
```

- ### Proxying a server
+ ### Servir de proxy à un serveur

- A proxy is both halves at once. It serves an editor over its own standard input and output,
- while it holds a session with a real language server. It forwards everything between them, and
- amends what it chooses to. `Lsp.proxy` runs that exchange, and its block registers the
- amendments:
+ Un proxy est les deux moitiés à la fois. Il sert un éditeur sur sa propre entrée et sa propre
+ sortie standard, tout en maintenant une session avec un vrai serveur de langage. Il transmet
+ tout entre eux, et amende ce qu'il choisit d'amender. `Lsp.proxy` mène cet échange, et son
+ bloc enregistre les amendements :

```scala
object DemoProxy:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.proxy(Lsp.Server(sh"rust-analyzer")):
          rewrite.capabilities(_.copy(hoverProvider = true))

          rewrite.hover: hover =>
            hover.copy(contents = MarkupContent(value = t"_(proxied)_ ${hover.contents.value}"))

          rewrite.diagnostics(_.filter(_.severity != DiagnosticSeverity.Hint))
      Exit.Ok
```

- Everything which is not registered is forwarded byte for byte: methods which this library does
- not model, capabilities it does not know about, and the editor's own request ids, which are
- never rewritten. The relay is asynchronous in both directions, so a proxy changes what an
- exchange says, and not how it behaves.
+ Tout ce qui n'est pas enregistré est transmis octet pour octet : les méthodes que cette
+ bibliothèque ne modélise pas, les capacités qu'elle ne connaît pas, et les identifiants de
+ requêtes propres à l'éditeur, qui ne sont jamais réécrits. Le relais est asynchrone dans les
+ deux sens ; un proxy change donc ce que dit un échange, et non la manière dont il se
+ comporte.

- Each `rewrite` combinator decodes the payload into the same type which the server side uses,
- applies the function, and encodes it again:
+ Chaque combinateur `rewrite` décode la charge utile dans le même type que celui qu'utilise le
+ côté serveur, applique la fonction, et l'encode à nouveau :

```tabular
- column  Combinator
- column  Amends
- column  As
- row  `rewrite.capabilities`  the result of `initialize`  `ServerCapabilities`
- row  `rewrite.hover`  the result of `textDocument/hover`  `Hover`
- row  `rewrite.completions`  the result of `textDocument/completion`  `CompletionList`
- row  `rewrite.definitions`  the result of `textDocument/definition`  `List[Location]`
- row  `rewrite.symbols`  the result of `textDocument/documentSymbol`  `List[DocumentSymbol]`
- row  `rewrite.codeActions`  the result of `textDocument/codeAction`  `List[CodeAction]`
- row  `rewrite.codeLenses`  the result of `textDocument/codeLens`  `List[CodeLens]`
- row  `rewrite.inlayHints`  the result of `textDocument/inlayHint`  `List[InlayHint]`
- row  `rewrite.signatureHelp`  the result of `textDocument/signatureHelp`  `SignatureHelp`
- row  `rewrite.diagnostics`  the reports in `textDocument/publishDiagnostics`  `List[Diagnostic]`
- row  `rewrite.result`  the result of the named method  `Json`
- row  `rewrite.notification`  the parameters of the named notification from the server  `Json`
+ column  Combinateur
+ column  Amende
+ column  En tant que
+ row  `rewrite.capabilities`  le résultat d'`initialize`  `ServerCapabilities`
+ row  `rewrite.hover`  le résultat de `textDocument/hover`  `Hover`
+ row  `rewrite.completions`  le résultat de `textDocument/completion`  `CompletionList`
+ row  `rewrite.definitions`  le résultat de `textDocument/definition`  `List[Location]`
+ row  `rewrite.symbols`  le résultat de `textDocument/documentSymbol`  `List[DocumentSymbol]`
+ row  `rewrite.codeActions`  le résultat de `textDocument/codeAction`  `List[CodeAction]`
+ row  `rewrite.codeLenses`  le résultat de `textDocument/codeLens`  `List[CodeLens]`
+ row  `rewrite.inlayHints`  le résultat de `textDocument/inlayHint`  `List[InlayHint]`
+ row  `rewrite.signatureHelp`  le résultat de `textDocument/signatureHelp`  `SignatureHelp`
+ row  `rewrite.diagnostics`  les rapports de `textDocument/publishDiagnostics`  `List[Diagnostic]`
+ row  `rewrite.result`  le résultat de la méthode nommée  `Json`
+ row  `rewrite.notification`  les paramètres de la notification nommée venant du serveur  `Json`
```

- A result which the server sent as `null`, having nothing to say, is passed on as it stands, so
- a rewriter never has to spell out the empty case. For whole messages there are two more hooks,
- `rewrite.outbound` and `rewrite.inbound`, each returning what should become of the message:
+ Un résultat que le serveur a envoyé sous forme de `null`, n'ayant rien à dire, est transmis
+ tel quel ; un réécrivain n'a donc jamais à traiter explicitement le cas vide. Pour les
+ messages entiers, il existe deux autres points d'accroche, `rewrite.outbound` et
+ `rewrite.inbound`, qui renvoient chacun ce que doit devenir le message :

```scala fuel
parse
##
object QuietProxy:
  import Lsp.*

  def serve(): Unit = supervise:
    Lsp.proxy(Lsp.Server(sh"rust-analyzer")):
      rewrite.outbound: (method, message) =>
        if method == t"textDocument/inlineValue" then Transit.Drop else Transit.Forward
```

- A hook answers with one of four `Transit` values:
+ Un point d'accroche répond par l'une de quatre valeurs `Transit` :

```tabular
- column  Transit
- column  Does
- row  `Transit.Forward`  passes the message on unchanged
- row  `Transit.Rewrite`  passes on a different message
- row  `Transit.Drop`  swallows it
- row  `Transit.Answer`  answers a request here, so that the server upstream never sees it
+ column  Transit
+ column  Effet
+ row  `Transit.Forward`  transmet le message inchangé
+ row  `Transit.Rewrite`  transmet un message différent
+ row  `Transit.Drop`  l'absorbe
+ row  `Transit.Answer`  répond ici à une requête, que le serveur en amont ne voit jamais
```

- A proxy is not limited to what the editor asks. The session with the server upstream is in
- scope as `upstream` throughout the registration block, so a hook may ask a question of its own,
- typically for the project state which only the server knows. `rewrite.connected` runs once, on
- a task of its own, as soon as that session is open:
+ Un proxy n'est pas limité à ce que demande l'éditeur. La session avec le serveur en amont est
+ dans la portée sous le nom `upstream` dans tout le bloc d'enregistrement ; un point d'accroche
+ peut donc poser sa propre question, typiquement sur l'état du projet que seul le serveur
+ connaît. `rewrite.connected` s'exécute une fois, sur une tâche propre, dès que cette session
+ est ouverte :

```scala fuel
parse
##
object ClasspathProxy:
  import Lsp.*
  import formatting.compactJsonFormatting

  def serve(): Unit = supervise:
    Lsp.proxy(Lsp.Server(sh"jdtls")):
      val classpath: Promise[Json] = Promise()

      rewrite.connected:
        classpath.offer(upstream.execute(t"java.project.getClasspaths").or(List[Text]().in[Json]))

      rewrite.inbound: (method, message) =>
        if method == t"language/status"
        then async(Err.println(t"classpath: ${classpath.await()}"))

        Transit.Forward
```

- Answers come back to the caller which asked for them, and never reach the editor. A response to
- a request which the proxy made itself is routed to whoever awaits it, and is not relayed.
+ Les réponses reviennent à l'appelant qui les a demandées, et n'atteignent jamais l'éditeur.
+ Une réponse à une requête que le proxy a faite lui-même est acheminée vers celui qui
+ l'attend, et n'est pas relayée.

- Mind which thread each hook runs on. A request awaited on the task which reads the server's
- messages, which is the task which would deliver the answer, would wait forever:
+ Prenez garde au thread sur lequel s'exécute chaque point d'accroche. Une requête attendue sur
+ la tâche qui lit les messages du serveur, c'est-à-dire la tâche qui livrerait la réponse,
+ attendrait indéfiniment :

```tabular
- column  Hook
- column  Runs on
- column  May await
- row  `rewrite.inbound`  the task that reads the server's messages  no; make the request in an `async` block, as above
- row  `rewrite.outbound`  the loop that reads the editor  yes, though the editor waits with it
- row  `rewrite.connected`  a task of its own  yes, freely
+ column  Point d'accroche
+ column  S'exécute sur
+ column  Peut attendre
+ row  `rewrite.inbound`  la tâche qui lit les messages du serveur  non ; faites la requête dans un bloc `async`, comme ci-dessus
+ row  `rewrite.outbound`  la boucle qui lit l'éditeur  oui, bien que l'éditeur attende avec elle
+ row  `rewrite.connected`  une tâche propre  oui, librement
```

- ### Running the server
+ ### Exécuter le serveur

- `Lsp.listen` serves standard input until it is exhausted. A server object therefore supplies a
- small `main` in the resident-daemon idiom, as `DemoServer` above does: the `cli` entry point,
- an `execute` block, and `supervise` for the tasks which the server runs. An editor configured
- to launch the object as its language server exchanges JSON-RPC with it over the pipe, and each
- request arrives at the matching handler.
+ `Lsp.listen` sert l'entrée standard jusqu'à son épuisement. Un objet serveur fournit donc un
+ petit `main` dans l'idiome du démon résident, comme le fait `DemoServer` ci-dessus : le point
+ d'entrée `cli`, un bloc `execute`, et `supervise` pour les tâches qu'exécute le serveur. Un
+ éditeur configuré pour lancer l'objet comme serveur de langage échange du JSON-RPC avec lui
+ par le tube, et chaque requête parvient au gestionnaire correspondant.

- ### Fast startup
+ ### Démarrage rapide

- The `cli` entry point runs the server as a resident [daemon](daemons.md). The first invocation
- launches a background JVM, and every later one connects to the process which is already
- running. An editor starts and restarts a language server often, so it matters to avoid the
- JVM's startup cost on each launch, and to keep the optimizations which the just-in-time
- compiler has accumulated. Packaging the server's JAR with the `xek` builder, from
- [propensive/xek](https://github.com/propensive/xek), assembles a small native launcher which
- starts, or connects to, the daemon, and forwards standard input, output and signals to it:
+ Le point d'entrée `cli` exécute le serveur comme un [démon](daemons.md) résident. La première
+ invocation lance une JVM en arrière-plan, et chaque invocation ultérieure se connecte au
+ processus déjà en cours d'exécution. Un éditeur démarre et redémarre souvent un serveur de
+ langage ; il importe donc d'éviter le coût de démarrage de la JVM à chaque lancement, et de
+ conserver les optimisations accumulées par le compilateur à la volée. Empaqueter le JAR du
+ serveur avec le constructeur `xek`, issu de
+ [propensive/xek](https://github.com/propensive/xek), assemble un petit lanceur natif qui
+ démarre le démon ou s'y connecte, et lui transmet l'entrée standard, la sortie standard et les
+ signaux :

```sh
xek build demo-server.jar
```

- ### A thin launcher
+ ### Un lanceur léger

- By default the launcher bundles every dependency into one fat JAR. Adding a dependency on the
- on-demand dependency loader, and wrapping the body of the entry point in `externalize`,
- distributes the server as a thin launcher instead. Its dependencies are fetched and cached the
- first time they are needed. `externalize` hashes each dependency as the server's own module
- compiles, and records the list in the compiled artifact. When that artifact is later repackaged,
- each dependency which is published somewhere is referenced by URL and hash rather than inlined,
- so the launcher stays small.
+ Par défaut, le lanceur regroupe toutes les dépendances dans un seul gros JAR. Ajouter une
+ dépendance au chargeur de dépendances à la demande, et envelopper le corps du point d'entrée
+ dans `externalize`, distribue plutôt le serveur sous forme de lanceur léger. Ses dépendances
+ sont téléchargées et mises en cache la première fois qu'on en a besoin. `externalize` calcule
+ le hachage de chaque dépendance lors de la compilation du module propre au serveur, et
+ enregistre la liste dans l'artefact compilé. Lorsque cet artefact est ensuite réempaqueté,
+ chaque dépendance publiée quelque part est référencée par URL et hachage plutôt
+ qu'intégrée ; le lanceur reste ainsi petit.

- ### Related
+ ### Voir aussi

- - [Daemons](daemons.md) describes the resident process which `cli` runs a server as.
- - [CLI](cli.md) describes the `cli` and `execute` entry points.
- - [RPC](rpc.md) describes the JSON-RPC and `Content-Length` framing which the protocol uses.
- - [Debugging](debugging.md) serves the sibling Debug Adapter Protocol.
- - [Concurrency](concurrency.md) describes `supervise`, `async` and `Promise`.
- - [Processes](processes.md) describes the `sh"…"` command which launches a server.
- - [Syntax highlighting](syntax-highlighting.md) and [TEL](tel.md) describe the trees which a server reports on, and [packaging](packaging.md) the thin launcher it ships as.
+ - [Démons](daemons.md) décrit le processus résident sous lequel `cli` exécute un serveur.
+ - [CLI](cli.md) décrit les points d'entrée `cli` et `execute`.
+ - [RPC](rpc.md) décrit le JSON-RPC et le tramage `Content-Length` qu'utilise le protocole.
+ - [Débogage](debugging.md) sert le protocole frère, Debug Adapter Protocol.
+ - [Concurrence](concurrency.md) décrit `supervise`, `async` et `Promise`.
+ - [Processus](processes.md) décrit la commande `sh"…"` qui lance un serveur.
+ - [Coloration syntaxique](syntax-highlighting.md) et [TEL](tel.md) décrivent les arbres sur
+   lesquels un serveur fait ses rapports, et [empaquetage](packaging.md) le lanceur léger sous
+   lequel il est distribué.
