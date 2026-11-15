- ## Debugging
+ ## Débogage

- ### About
+ ### Présentation

- A JVM can be debugged from another JVM over the
- [Java Debug Wire Protocol](https://docs.oracle.com/en/java/javase/21/docs/specs/jdwp/jdwp-spec.html),
- and these types speak it directly over a socket. A debuggee is launched under the debug agent,
- or attached to where it already runs. The [session] which results sets breakpoints, steps
- threads, reads frames and variables, evaluates expressions in the stopped program, and receives
- the events of the VM as typed values. Nothing depends on `com.sun.jdi`.
+ Une JVM peut être déboguée depuis une autre JVM via le [Java Debug Wire
+ Protocol](https://docs.oracle.com/en/java/javase/21/docs/specs/jdwp/jdwp-spec.html), et ces types
+ le parlent directement sur un socket. Un programme débogué est lancé sous l'agent de débogage, ou
+ rejoint là où il s'exécute déjà. La [session] qui en résulte pose des points d'arrêt, fait avancer
+ les threads pas à pas, lit les trames et les variables, évalue des expressions dans le programme
+ arrêté, et reçoit les événements de la VM sous forme de valeurs typées. Rien ne dépend de
+ `com.sun.jdi`.

- The same session serves an editor over the
- [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/).
+ La même session sert un éditeur via le
+ [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/).

- ### On debugging from the outside
+ ### Déboguer depuis l'extérieur

- The JVM's own debugging interface is a Java API which wraps a wire protocol, and it shows. The
- types are mutable, the errors are unchecked, and the shape of a session, which is to connect,
- suspend, request, resume and disconnect, is left to the caller to get right. Anything built on
- it inherits all this, which is why debuggers are large programs and not small ones.
+ L'interface de débogage propre à la JVM est une API Java qui enveloppe un protocole réseau,
+ et cela se voit. Les types sont mutables, les erreurs sont non vérifiées, et le déroulement
+ d'une session, qui consiste à se connecter, suspendre, demander, reprendre et se
+ déconnecter, est laissé à la charge de l'appelant. Tout ce qui est construit dessus en
+ hérite, c'est pourquoi les débogueurs sont de gros programmes et non de petits.

- The protocol underneath is simple: length-prefixed packets, big-endian, with the widths of
- identifiers negotiated once at the start of the connection. Modeling it directly gives typed
- identifiers which cannot be confused with each other, a session confined to a scope by
- [capture checking](../../philosophy/capture-checking.md), and events as a stream to consume and
- not callbacks to register.
+ Le protocole sous-jacent est simple : des paquets préfixés par leur longueur, gros-boutistes,
+ avec des largeurs d'identifiants négociées une fois au début de la connexion. Le modéliser
+ directement donne des identifiants typés qui ne peuvent pas être confondus entre eux, une
+ session confinée à une portée par le
+ [contrôle des captures](../../philosophy/capture-checking.md), et des événements sous forme
+ de flux à consommer et non de callbacks à enregistrer.

- Everything comes from the `soundness` package. A session runs its reader as a task, so it needs
- a [thread model](concurrency.md) in scope and lives inside `supervise`:
+ Tout provient du package `soundness`. Une session exécute son lecteur comme une tâche ; elle
+ a donc besoin d'un [modèle de threads](concurrency.md) dans la portée et vit à l'intérieur
+ de `supervise` :

```scala fuel
feature experimental.captureChecking
##
import soundness.*

import environments.javaBaseEnvironment
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import socketBackends.javaBaseSockets
import strategies.throwUnsafely
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

- The samples on this page launch and connect to other processes, so they are compiled but not
- run.
+ Les exemples de cette page lancent d'autres processus et s'y connectent ; ils sont donc
+ compilés mais pas exécutés.

- ### Opening a session
+ ### Ouvrir une session

- A `Debuggee` describes a program to run under the debug agent: a command, the port to listen on,
- and whether the VM should suspend before running `main`, which a breakpoint in initialization
- needs. Its `session` method lends a `Debug` [capability] to a block, which is reached through
- `debug`:
+ Un `Debuggee` décrit un programme à exécuter sous l'agent de débogage : une commande, le port
+ d'écoute, et si la VM doit se suspendre avant d'exécuter `main`, ce dont a besoin un point
+ d'arrêt dans l'initialisation. Sa méthode `session` prête une [capacité] `Debug` à un bloc,
+ à laquelle on accède par `debug` :

```scala
def inspect(classpath: Text): List[Text] = supervise:
  val target: Debuggee = Debuggee(sh"java -cp $classpath com.example.Main", port = 5005)

  target.session:
    debug.threads().map(debug.name(_))
```

- The command is launched with the agent option inserted, and the connection waits for the VM to
- begin listening. When the block ends, whether it returns or fails, everything is torn down: the
- socket is closed and the process is stopped.
+ La commande est lancée avec l'option de l'agent insérée, et la connexion attend que la VM
+ commence à écouter. Lorsque le bloc se termine, qu'il renvoie une valeur ou échoue, tout est
+ démonté : le socket est fermé et le processus est arrêté.

- A `Debugger` opens the same kind of session against an endpoint where a suspended VM is already
- listening, for attaching to something which is already running.
+ Un `Debugger` ouvre le même genre de session vers un endpoint où une VM suspendue écoute
+ déjà, pour se rattacher à quelque chose qui s'exécute déjà.

- The session handle is a capability confined to its block. It cannot escape, and so cannot be used
- after the debuggee has gone.
+ La poignée de session est une capacité confinée à son bloc. Elle ne peut pas s'en échapper,
+ et ne peut donc pas être utilisée après la disparition du programme débogué.

- ### Threads and frames
+ ### Threads et trames

- The threads of the VM are listed, named, suspended and resumed, individually or together. The
- call stack of a suspended thread reads as frames paired with their locations:
+ Les threads de la VM sont listés, nommés, suspendus et repris, individuellement ou
+ ensemble. La pile d'appels d'un thread suspendu se lit comme des trames associées à leurs
+ emplacements :

```scala
def stacks()(using Debug): Unit =
  debug.suspend()
  debug.threads().each: thread =>
    debug.frames(thread).each: (frame, location) =>
      Out.println(t"${debug.name(thread)}: ${location.index}")
  debug.resume()
```

- ### Breakpoints
+ ### Points d'arrêt

- A breakpoint is set on a source file and line, or on a class and method. It takes a *handler*,
- which runs whenever a thread stops there.
+ Un point d'arrêt se pose sur un fichier source et une ligne, ou sur une classe et une
+ méthode. Il prend un *gestionnaire*, qui s'exécute chaque fois qu'un thread s'y arrête.

- The handler is lent a `Halt`: a view of the stopped thread, with its `frames()`, its
- `variables()`, and the exception in flight if that is why it stopped. A `Halt` cannot outlive
- the stop, since once the thread resumes, every identifier it handed out may be stale. A handler
- which wants the thread left suspended says so with `remain()`. Otherwise the thread resumes when
- the handler returns:
+ Le gestionnaire reçoit en prêt un `Halt` : une vue du thread arrêté, avec ses `frames()`, ses
+ `variables()`, et l'exception en cours si c'est la raison de l'arrêt. Un `Halt` ne peut pas
+ survivre à l'arrêt, puisqu'une fois le thread repris, chaque identifiant qu'il a fourni peut
+ être périmé. Un gestionnaire qui veut que le thread reste suspendu l'indique avec
+ `remain()`. Sinon, le thread reprend lorsque le gestionnaire se termine :

```scala
def watch()(using Debug): Unit =
  debug.breakpoint(t"Main.scala", Ordinal.uniary(42)): stop ?=>
    stop.variables().each: variable =>
      Out.println(t"${variable.name}: ${variable.erased}")

  debug.breakpoint(t"com.example.Ledger", t"deposit"): stop ?=>
    stop.remain()
```

- Breakpoints on inlined code work by position. The file and line are where the code was written,
- not where the compiler placed it, recovered from the source maps which the compiler records.
+ Les points d'arrêt sur du code inliné fonctionnent par position. Le fichier et la ligne sont
+ ceux où le code a été écrit, et non ceux où le compilateur l'a placé, retrouvés à partir des
+ tables de correspondance des sources qu'enregistre le compilateur.

- Each kind of request runs its handler on a different event:
+ Chaque type de requête exécute son gestionnaire sur un événement différent :

```tabular
- column  Request
- column  Runs when
- column  Event kind
- row  `breakpoint`  a thread reaches a source line, a method's entry or a location  `Breakpoint`
- row  `logpoint`  a thread reaches a location, printing a message and resuming at once  `Breakpoint`
- row  `watch`  a field is written, or with `access = true` read  `FieldModified`, `FieldAccess`
- row  `exceptions`  an exception is thrown  `Exception`
- row  `step`  a stepping thread lands  `SingleStep`
+ column  Requête
+ column  S'exécute lorsque
+ column  Type d'événement
+ row  `breakpoint`  un thread atteint une ligne source, l'entrée d'une méthode ou un emplacement  `Breakpoint`
+ row  `logpoint`  un thread atteint un emplacement, affiche un message et reprend aussitôt  `Breakpoint`
+ row  `watch`  un champ est écrit, ou lu avec `access = true`  `FieldModified`, `FieldAccess`
+ row  `exceptions`  une exception est levée  `Exception`
+ row  `step`  un thread en pas à pas s'arrête  `SingleStep`
```

- A breakpoint returns the identifier of its request, which is what later clears it. The *suspend
- policy* decides how much of the VM stops when the breakpoint is hit:
+ Un point d'arrêt renvoie l'identifiant de sa requête, qui sert ensuite à le supprimer. La
+ *politique de suspension* décide quelle part de la VM s'arrête lorsque le point d'arrêt est
+ atteint :

```tabular
- column  Suspend policy
- column  Stops
- row  `Jdwp.SuspendPolicy.All`  the whole VM
- row  `Jdwp.SuspendPolicy.EventThread`  only the thread that hit it
- row  `Jdwp.SuspendPolicy.None`  nothing at all
+ column  Politique de suspension
+ column  Arrête
+ row  `Jdwp.SuspendPolicy.All`  la VM entière
+ row  `Jdwp.SuspendPolicy.EventThread`  seulement le thread qui l'a atteint
+ row  `Jdwp.SuspendPolicy.None`  rien du tout
```

```scala
def once(location: Jdwp.Location)(using Debug): Unit =
  val request = debug.breakpoint(location, Jdwp.SuspendPolicy.EventThread)
  debug.clear(Jdwp.EventKind.Breakpoint, request)
```

- ### Stepping
+ ### Pas à pas

- Stepping is a request against a suspended thread, with a handler for where it lands. The *depth*
- says whether to step into a call (`Jdwp.StepDepth.Into`), over it (`Over`), or out of the
- current frame (`Out`). An inlined call steps as the source reads, into the inlined body line by
- line, and not as the bytecode was laid out:
+ Le pas à pas est une requête sur un thread suspendu, avec un gestionnaire pour l'endroit où
+ il s'arrête. La *profondeur* indique s'il faut entrer dans un appel (`Jdwp.StepDepth.Into`),
+ le passer (`Over`), ou sortir de la trame courante (`Out`). Un appel inliné s'exécute pas à
+ pas comme se lit le code source, dans le corps inliné ligne par ligne, et non selon la
+ disposition du bytecode :

```scala
def stepOver(thread: ThreadId)(using Debug): Unit =
  debug.step(thread, Jdwp.StepDepth.Over): stop ?=>
    stop.frames().head.let: (frame, location) =>
      Out.println(stop.describe(location)(0))
```

- ### Evaluating expressions
+ ### Évaluer des expressions

- With the `Evaluator` in scope, an expression is compiled against the locals of the stopped frame
- and injected into the running debuggee. So `total + 1` in a handler means what it would mean at
- that line of the source. A debugger's console and hover use this, and it is also how an editor
- shows the type which the compiler inferred for a name.
+ Avec l'`Evaluator` dans la portée, une expression est compilée par rapport aux variables
+ locales de la trame arrêtée et injectée dans le programme débogué en cours d'exécution.
+ Ainsi, `total + 1` dans un gestionnaire signifie ce qu'il signifierait à cette ligne du code
+ source. La console et le survol d'un débogueur s'en servent, et c'est aussi ainsi qu'un
+ éditeur affiche le type que le compilateur a inféré pour un nom.

- ### Events
+ ### Événements

- Underneath the handlers, the VM reports what happened as composite events. `events` is the
- [stream](streams.md) of them, so the main loop of a debugger can be an ordinary traversal:
+ Sous les gestionnaires, la VM signale ce qui s'est passé sous forme d'événements composites.
+ `events` en est le [flux](streams.md) ; la boucle principale d'un débogueur peut donc être un
+ parcours ordinaire :

```scala
def trace()(using Debug): Unit =
  debug.events.each: composite =>
    composite.events.each:
      case Jdwp.Event.Breakpoint(request, thread, location) => Out.println(t"stopped at ${location.index}")
      case Jdwp.Event.SingleStep(request, thread, location) => Out.println(t"stepped to ${location.index}")
      case other                                            => ()
```

- Requests and replies are correlated asynchronously. Many commands may be in flight at once, and
- an event which arrives during one does not disturb it.
+ Les requêtes et les réponses sont mises en correspondance de manière asynchrone. De
+ nombreuses commandes peuvent être en cours en même temps, et un événement qui arrive pendant
+ l'une d'elles ne la perturbe pas.

- ### Serving an editor
+ ### Servir un éditeur

- `Dap.listen` serves the session over the Debug Adapter Protocol on a TCP port. Any editor which
- speaks it, which is VS Code and most others, can launch, stop, step, inspect variables and
- evaluate through this implementation. Completions, hover types and inline-aware stepping all
- pass through to the editor.
+ `Dap.listen` sert la session via le Debug Adapter Protocol sur un port TCP. Tout éditeur qui
+ le parle, c'est-à-dire VS Code et la plupart des autres, peut lancer, arrêter, avancer pas à
+ pas, inspecter des variables et évaluer au travers de cette implémentation. Les
+ complétions, les types au survol et le pas à pas conscient de l'inlining passent tous
+ jusqu'à l'éditeur.

- ### The protocol vocabulary
+ ### Le vocabulaire du protocole

- `Jdwp` holds the model of the protocol itself, for work below the level which the session
- exposes. The identifier types are tagged views of one [opaque type]. They cost nothing
- at [runtime], and cannot be interchanged at [compiletime]:
+ `Jdwp` contient le modèle du protocole lui-même, pour un travail en dessous du niveau
+ qu'expose la session. Les types d'identifiants sont des vues taguées d'un même
+ [type opaque]. Ils ne coûtent rien [à l'exécution][exécution], et ne peuvent pas être
+ intervertis [à la compilation][compilation] :

```tabular
- column  Type
- column  Identifies
- row  `ObjectId`  an object
- row  `ThreadId`  a thread
- row  `ThreadGroupId`  a thread group
- row  `StringId`  a string object
- row  `ClassLoaderId`  a class loader
- row  `ReferenceTypeId`  a class, interface or array type
- row  `MethodId`  a method
- row  `FieldId`  a field
- row  `FrameId`  a stack frame
+ column  Type
+ column  Identifie
+ row  `ObjectId`  un objet
+ row  `ThreadId`  un thread
+ row  `ThreadGroupId`  un groupe de threads
+ row  `StringId`  un objet chaîne
+ row  `ClassLoaderId`  un chargeur de classes
+ row  `ReferenceTypeId`  une classe, une interface ou un type tableau
+ row  `MethodId`  une méthode
+ row  `FieldId`  un champ
+ row  `FrameId`  un cadre de pile
```

- Beside them sit `Jdwp.Location`, the tagged `Jdwp.Value`s, the `Jdwp.Modifier`s which constrain
- an event request, and the `Jdwp.Event`s which a suspended VM sends back.
+ À côté d'eux se trouvent `Jdwp.Location`, les `Jdwp.Value` taguées, les `Jdwp.Modifier`
+ qui restreignent une requête d'événement, et les `Jdwp.Event` que renvoie une VM suspendue.

- `Jdwp.Reader` and `Jdwp.Writer` are the hand-written big-endian codecs. They know the sizes of
- identifiers negotiated with the VM when the session opened, which is why they are constructed
- with those sizes rather than assuming them.
+ `Jdwp.Reader` et `Jdwp.Writer` sont les codecs gros-boutistes écrits à la main. Ils
+ connaissent les tailles des identifiants négociées avec la VM à l'ouverture de la session,
+ c'est pourquoi ils sont construits avec ces tailles plutôt que de les supposer.

- ### Related
+ ### Voir aussi

- - [Processes](processes.md) describes the `sh"…"` commands which a debuggee is launched with.
- - [Concurrency](concurrency.md) describes `supervise` and the thread models a session runs
-   under.
- - [Sockets](sockets.md) describes the TCP connections which the protocols travel over.
- - [LSP](lsp.md) serves an editor over the sibling Language Server Protocol.
- - [Stack traces](stack-traces.md) describes the frames of a stopped program as values.
- - [Capture checking](../../philosophy/capture-checking.md) explains how a session handle is
-   confined to its block.
- - [Filesystem](filesystem.md) describes the handles whose shape a debugging session shares.
+ - [Processus](processes.md) décrit les commandes `sh"…"` avec lesquelles un programme débogué
+   est lancé.
+ - [Concurrence](concurrency.md) décrit `supervise` et les modèles de threads sous lesquels
+   s'exécute une session.
+ - [Sockets](sockets.md) décrit les connexions TCP par lesquelles transitent les protocoles.
+ - [LSP](lsp.md) sert un éditeur via le protocole voisin, le Language Server Protocol.
+ - [Traces de pile](stack-traces.md) décrit les trames d'un programme arrêté sous forme de
+   valeurs.
+ - [Contrôle des captures](../../philosophy/capture-checking.md) explique comment une poignée
+   de session est confinée à son bloc.
+ - [Système de fichiers](filesystem.md) décrit les poignées dont une session de débogage
+   partage la forme.
