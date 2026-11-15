- ## Language Models
+ ## Modèles de langage

- ### About
+ ### Présentation

- A conversation with a large language model is typed at every step. The provider and the model
- are values. A conversation is a [session] which accumulates its history and its usage of tokens.
- A reply is a value which says why the model stopped. Streamed output is pulled off the live
- connection. Tools are ordinary Scala methods which the model may call. A structured answer is
- decoded to a case class, against a schema derived from it.
+ Une conversation avec un grand modèle de langage est typée à chaque étape. Le fournisseur et le
+ modèle sont des valeurs. Une conversation est une [session] qui accumule son historique et sa
+ consommation de jetons. Une réponse est une valeur qui indique pourquoi le modèle s'est arrêté.
+ La sortie diffusée en flux est tirée de la connexion active. Les outils sont des méthodes Scala
+ ordinaires que le modèle peut appeler. Une réponse structurée est décodée en une case class,
+ selon un schéma qui en est dérivé.

- One vocabulary serves Anthropic's Messages API, OpenAI's Chat Completions and Responses APIs,
- any server compatible with the former, such as vLLM, Ollama or Groq, and Google's Gemini.
+ Un même vocabulaire sert l'API Messages d'Anthropic, les API Chat Completions et Responses
+ d'OpenAI, tout serveur compatible avec Chat Completions, comme vLLM, Ollama ou Groq, et Gemini de
+ Google.

- ### On language models
+ ### Sur les modèles de langage

- The API of each provider is a JSON dialect: a request with the model, the messages so far and a
- handful of tuning knobs; a reply with content blocks, a stop reason and token counts; a streaming
- variant which sends deltas; and a protocol for tool calls, which returns arguments as JSON for
- the program to act on and report back. The dialects differ in every detail and agree in every
- concept, and the concepts are what a program cares about.
+ L'API de chaque fournisseur est un dialecte JSON : une requête avec le modèle, les messages
+ échangés jusque-là et une poignée de réglages ; une réponse avec des blocs de contenu, une
+ raison d'arrêt et des décomptes de jetons ; une variante en flux qui envoie des deltas ; et un
+ protocole d'appels d'outils, qui renvoie des arguments en JSON pour que le programme agisse et
+ rende compte. Les dialectes diffèrent dans chaque détail et s'accordent sur chaque concept, et
+ ce sont les concepts qui importent à un programme.

- The concepts are modeled once here, as messages, replies, usage, tools and stop reasons, and
- translated onto each wire by a *dialect* for each provider. A conversation is a session which
- lives in a block. The pure values it produces leave freely, while the session itself, and any
- stream in flight, belong to the block, so a connection is not held open by accident.
+ Les concepts sont modélisés ici une seule fois, sous forme de messages, de réponses, de
+ consommation, d'outils et de raisons d'arrêt, et traduits sur chaque protocole par un
+ *dialecte* propre à chaque fournisseur. Une conversation est une session qui vit dans un bloc.
+ Les valeurs pures qu'elle produit en sortent librement, tandis que la session elle-même, et tout
+ flux en cours, appartiennent au bloc ; une connexion n'est donc pas maintenue ouverte par
+ accident.

- Everything comes from the `soundness` package, with the HTTP backend, network access and logging
- which a client needs:
+ Tout provient du package `soundness`, avec le backend HTTP, l'accès réseau et la journalisation
+ dont un client a besoin :

```scala fuel
feature experimental.captureChecking
##
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import codepages.utf8Codepage
```

- The samples on this page call a provider over the network, so they are compiled but not run.
+ Les exemples de cette page appellent un fournisseur sur le réseau ; ils sont donc compilés mais
+ pas exécutés.

- ### Providers and models
+ ### Fournisseurs et modèles

- A provider value names the model and carries the API key. Its tuning is applied by methods which
- each return a new value, so a configured model is a plain [immutable] value which can be shared
- and reused:
+ Une valeur de fournisseur nomme le modèle et porte la clé d'API. Ses réglages s'appliquent par
+ des méthodes qui renvoient chacune une nouvelle valeur ; un modèle configuré est donc une simple
+ valeur [immuable] que l'on peut partager et réutiliser :

```scala
val key = t"sk-ant-…"

val claude = Anthropic(t"claude-sonnet-4-5", key).prompted(t"Be terse.").limit(1024)
val gpt = OpenAI(t"gpt-5", t"sk-…").warmth(0.2)
val gemini = Gemini(t"gemini-2.5-pro", t"AIza…")
val local = OpenAI.compatible(url"http://localhost:11434/v1", t"llama3")
```

- Each method sets one knob:
+ Chaque méthode définit un réglage :

```tabular
- column  Method
- column  Sets
- row  `prompted`  the system prompt
- row  `limit`  the maximum output tokens
- row  `warmth`  the temperature
- row  `sampling`  the nucleus threshold
- row  `stopping`  the stop sequences
- row  `iterating`  the tool-loop limit
- row  `primed`  a history to start from
- row  `on`  an alternative base URL
+ column  Méthode
+ column  Définit
+ row  `prompted`  le prompt système
+ row  `limit`  le nombre maximal de jetons en sortie
+ row  `warmth`  la température
+ row  `sampling`  le seuil d'échantillonnage nucleus
+ row  `stopping`  les séquences d'arrêt
+ row  `iterating`  la limite de la boucle d'outils
+ row  `primed`  un historique de départ
+ row  `on`  une URL de base alternative
```

- A knob left unset is omitted from the request, and is not sent as a default, so the provider's
- own defaults apply. An OpenAI-compatible server takes an optional key. OpenAI's Responses API is
- reached from an `OpenAI` value with `responses`.
+ Un réglage non défini est omis de la requête, et n'est pas envoyé comme valeur par défaut ; ce
+ sont donc les valeurs par défaut du fournisseur qui s'appliquent. Un serveur compatible OpenAI
+ prend une clé optionnelle. On atteint l'API Responses d'OpenAI à partir d'une valeur `OpenAI`
+ avec `responses`.

- ### A conversation
+ ### Une conversation

- A session is opened on a provider with `session`, and inside the block the `llm` accessor reaches
- it. `ask` sends a message and returns the model's `Reply`. The session records both, so a second
- `ask` continues the conversation:
+ Une session s'ouvre sur un fournisseur avec `session`, et dans le bloc, l'accesseur `llm` y
+ donne accès. `ask` envoie un message et renvoie la `Reply` du modèle. La session enregistre les
+ deux ; un second `ask` poursuit donc la conversation :

```scala fuel
typecheck
##
val reply: Llm.Reply = claude.session:
  llm.ask(t"What is the tallest mountain in Estonia?")
  llm.ask(t"And how tall is it?")

val answer: Text = reply.text
val stop: Llm.Stop = reply.stop
val usage: Llm.Usage = reply.usage
```

- `text` is the assistant's answer. `stop` is `Llm.Stop.Ended`, or the reason the model stopped
- early. `usage` holds the tokens which the last turn cost.
+ `text` est la réponse de l'assistant. `stop` vaut `Llm.Stop.Ended`, ou la raison pour laquelle
+ le modèle s'est arrêté prématurément. `usage` contient les jetons qu'a coûtés le dernier tour.

- A `Reply` is pure data: its message, its stop reason, its usage, and the model and identifier
- which the provider reported. It may therefore leave the block, as may a snapshot of
- `llm.history` or the folded `llm.usage`. The session itself belongs to the block. The connection
- closes when the block ends, so the session must not be kept beyond it.
+ Une `Reply` est une donnée pure : son message, sa raison d'arrêt, sa consommation, ainsi que le
+ modèle et l'identifiant que le fournisseur a indiqués. Elle peut donc sortir du bloc, tout comme
+ un instantané de `llm.history` ou le `llm.usage` cumulé. La session elle-même appartient au
+ bloc. La connexion se ferme quand le bloc se termine ; la session ne doit donc pas être conservée
+ au-delà.

- ### Streaming
+ ### Diffusion en flux

- `stream` sends a message and returns a `Response`. Its `text` is an [iterator](collections.md)
- of fragments, pulled synchronously off the connection as they arrive, and its `events` are the
- finer-grained deltas. When the stream has been consumed, `reply()` assembles the complete reply
- and commits the turn to the history. It does so even after partial consumption, since the
- remainder is read on demand. A stream which is abandoned commits nothing, so a failed turn is
- simply retried:
+ `stream` envoie un message et renvoie une `Response`. Son `text` est un
+ [itérateur](collections.md) de fragments, tirés de manière synchrone de la connexion à leur
+ arrivée, et ses `events` sont les deltas plus fins. Une fois le flux consommé, `reply()`
+ assemble la réponse complète et inscrit le tour dans l'historique. Elle le fait même après une
+ consommation partielle, puisque le reste est lu à la demande. Un flux abandonné n'inscrit rien ;
+ un tour échoué se relance donc simplement :

```scala fuel
typecheck
##
claude.session:
  val response = llm.stream(t"Write a limerick about Tallinn.")
  response.text.each(Out.print(_))
  response.reply()
```

- The response, like the session, is confined to the block.
+ La réponse, comme la session, est confinée au bloc.

- ### Tools
+ ### Outils

- A tool is a method annotated `@ability` and described with `@about`, on an object which a
- `Toolkit` is built from. The JSON schema for the parameters of each tool is derived from the
- parameter types of the method, so the model is told exactly what the method accepts. A contextual
- parameter which the method needs is summoned where the toolkit is constructed, and is not
- exposed to the model:
+ Un outil est une méthode annotée `@ability` et décrite avec `@about`, sur un objet à partir
+ duquel on construit une `Toolkit`. Le schéma JSON des paramètres de chaque outil est dérivé des
+ types des paramètres de la méthode ; le modèle sait donc exactement ce que la méthode accepte.
+ Un paramètre contextuel dont la méthode a besoin est invoqué là où la toolkit est construite, et
+ n'est pas exposé au modèle :

```scala
object Broker:
  val quotes = Map(t"AAPL" -> 189.3, t"MSFT" -> 412.7)

  @ability
  @about(t"Look up the current price of a stock ticker")
  def price(ticker: Text): Double = quotes.at(ticker).or(0.0)

val broker = Toolkit(Broker)
```

```scala fuel
run
##
broker.specs.each(spec => Out.println(spec.name))
```

- With a `Toolkit` given in scope, `ask` runs the whole loop. It offers the tools to the model,
- runs the tool which the model calls, reports the result, and continues until the model answers
- in text or the iteration limit is reached. A tool which fails, or a call with malformed
- arguments, is reported to the model as an error result which it can recover from, and does not
- abort the conversation:
+ Avec une `Toolkit` given dans la portée, `ask` exécute toute la boucle. Il propose les outils au
+ modèle, exécute l'outil que le modèle appelle, rapporte le résultat, et continue jusqu'à ce que
+ le modèle réponde en texte ou que la limite d'itérations soit atteinte. Un outil qui échoue, ou
+ un appel aux arguments mal formés, est signalé au modèle comme un résultat d'erreur dont il peut
+ se remettre, et n'interrompt pas la conversation :

```scala fuel
typecheck
##
claude.session:
  given Toolkit = broker
  llm.ask(t"Compare AAPL and MSFT.")
```

- Here the model would call `price` twice, and then answer. Every turn which the loop makes is
- committed to the history, so the record shows the calls the model made and what it was told. A
- loop which does not converge raises `Llm.Error` with the reason `ToolLoopExceeded`, at the limit
- set by `iterating`. The `@about` annotation is shared with [MCP](mcp.md), so one description
- serves a method exposed both as an MCP tool and as a model ability.
+ Ici, le modèle appellerait `price` deux fois, puis répondrait. Chaque tour que fait la boucle est
+ inscrit dans l'historique ; la trace montre donc les appels qu'a faits le modèle et ce qu'on lui
+ a répondu. Une boucle qui ne converge pas lève `Llm.Error` avec la raison `ToolLoopExceeded`, à
+ la limite fixée par `iterating`. L'annotation `@about` est partagée avec [MCP](mcp.md) ; une
+ même description sert donc une méthode exposée à la fois comme outil MCP et comme capacité du
+ modèle.

- ### Structured answers
+ ### Réponses structurées

- `solicit` asks for an answer of a given type. The JSON schema of the type is derived, and the
- document which the model supplies is decoded to the type. The result is a value, not text to be
- parsed. How the schema reaches the model is the business of the dialect. Anthropic's wire
- constrains the reply to it natively, and elsewhere it is presented as the one tool which the
- model must call. The caller names a type and receives a value, on every provider, including
- local servers which have no mode for structured output:
+ `solicit` demande une réponse d'un type donné. Le schéma JSON du type est dérivé, et le document
+ que fournit le modèle est décodé dans ce type. Le résultat est une valeur, et non un texte à
+ analyser. La façon dont le schéma parvient au modèle est l'affaire du dialecte. Le protocole
+ d'Anthropic y contraint nativement la réponse, et ailleurs, il est présenté comme l'unique outil
+ que le modèle doit appeler. L'appelant nomme un type et reçoit une valeur, chez tout fournisseur,
+ y compris les serveurs locaux qui n'ont pas de mode de sortie structurée :

```scala
case class Verdict(ticker: Text, rating: Text, confidence: Double)
```

```scala fuel
typecheck
##
local.session:
  llm.solicit[Verdict](t"Summarize your recommendation for AAPL.")
```

- A reply which ignores the forced tool, or a document which does not decode, raises `Llm.Error`
- with the reason `Malformed`. A malformed answer is an error to handle, and not a value which looks
- right.
+ Une réponse qui ignore l'outil imposé, ou un document qui ne se décode pas, lève `Llm.Error` avec
+ la raison `Malformed`. Une réponse mal formée est une erreur à traiter, et non une valeur qui a
+ l'air correcte.

- ### Batches
+ ### Lots

- Many questions which need no answer today go through Anthropic's Message Batches API, which runs
- them asynchronously at half the price. `elicitAll` takes a list of requests, each a prompt with
- an identifier of the caller's choosing, checked by `Llm.Id.request` before anything is sent. It
- returns a batch: a pure record of its identifier, status and counts, whose answers are decoded to
- the named type once it has ended:
+ Beaucoup de questions qui n'exigent pas de réponse aujourd'hui passent par l'API Message Batches
+ d'Anthropic, qui les exécute de façon asynchrone à moitié prix. `elicitAll` prend une liste de
+ requêtes, chacune un prompt avec un identifiant au choix de l'appelant, vérifié par
+ `Llm.Id.request` avant tout envoi. Elle renvoie un lot : un enregistrement pur de son
+ identifiant, de son statut et de ses décomptes, dont les réponses sont décodées dans le type
+ nommé une fois qu'il est terminé :

```scala fuel
typecheck
##
val requests = List
  ( Llm.Request(Llm.Id.request(t"aapl"), t"Summarize your recommendation for AAPL."),
    Llm.Request(Llm.Id.request(t"msft"), t"Summarize your recommendation for MSFT.") )

val batch = claude.prompted(t"Be terse.").elicitAll[Verdict](requests)

batch.await().outcomes().each: outcome =>
  outcome.result match
    case verdict: Verdict => Out.println(t"${outcome.id}: ${verdict.rating}")
    case error: Llm.Error => Out.println(t"${outcome.id} failed: ${error.message}")
```

- Outcomes arrive in the provider's order, not the order of submission, which is why each carries
- the identifier of its request. A batch survives the process which submitted it.
- `claude.batch[Verdict](id)` finds it again by its identifier, with its current status.
- Identifiers are typed by what they name, as an `Llm.Id[Llm.Request]`, an `Llm.Id[Llm.Reply]` or
- an `Llm.Id[Anthropic.Batch[?]]`, so the identifier of a reply can never be handed to a method
- which expects a batch's.
+ Les résultats arrivent dans l'ordre du fournisseur, et non dans l'ordre de soumission ; c'est
+ pourquoi chacun porte l'identifiant de sa requête. Un lot survit au processus qui l'a soumis.
+ `claude.batch[Verdict](id)` le retrouve par son identifiant, avec son statut actuel. Les
+ identifiants sont typés par ce qu'ils nomment, comme un `Llm.Id[Llm.Request]`, un
+ `Llm.Id[Llm.Reply]` ou un `Llm.Id[Anthropic.Batch[?]]` ; l'identifiant d'une réponse ne peut
+ donc jamais être passé à une méthode qui attend celui d'un lot.

- A prompt shared by every request in a batch, or by every turn of a long session, is worth caching
- at the provider. `claude.caching` marks the system prompt as a cache breakpoint, and
- `Llm.Usage.cacheRead` reports how much of each request was read from the cache.
+ Un prompt partagé par toutes les requêtes d'un lot, ou par tous les tours d'une longue session,
+ mérite d'être mis en cache chez le fournisseur. `claude.caching` marque le prompt système comme
+ point d'arrêt du cache, et `Llm.Usage.cacheRead` indique quelle part de chaque requête a été lue
+ depuis le cache.

- ### Messages and content
+ ### Messages et contenu

- The vocabulary which the dialects share is small. An `Llm.Message` has a `Role`, which is user
- or assistant, and a list of `Llm.Content`, so nothing is lost in translation:
+ Le vocabulaire que partagent les dialectes est restreint. Un `Llm.Message` a un `Role`,
+ utilisateur ou assistant, et une liste de `Llm.Content` ; rien n'est donc perdu à la traduction :

```tabular
- column  Content
- column  Holds
- row  `Textual`  text
- row  `Graphic`  an image, from inline bytes or a URL
- row  `Document`  a document, from inline bytes or a URL
- row  `ToolUse`  a tool call, with its arguments
- row  `ToolResult`  a tool result
- row  `Thinking`  the model's thinking, where the provider exposes it
- row  `Redacted`  thinking the provider returns only in encrypted form
- row  `Opaque`  any content a provider sends that is not otherwise modeled, verbatim
+ column  Contenu
+ column  Contient
+ row  `Textual`  du texte
+ row  `Graphic`  une image, à partir d'octets en ligne ou d'une URL
+ row  `Document`  un document, à partir d'octets en ligne ou d'une URL
+ row  `ToolUse`  un appel d'outil, avec ses arguments
+ row  `ToolResult`  un résultat d'outil
+ row  `Thinking`  la réflexion du modèle, là où le fournisseur l'expose
+ row  `Redacted`  une réflexion que le fournisseur ne renvoie que sous forme chiffrée
+ row  `Opaque`  tout contenu qu'envoie un fournisseur et qui n'est pas modélisé autrement, tel quel
```

- An image or a document comes from an `Llm.Content.Source`: `Inline` bytes with a media type, or
- a `Remote` URL:
+ Une image ou un document provient d'une `Llm.Content.Source` : des octets `Inline` avec un type
+ de média, ou une URL `Remote` :

```scala
val question = Llm.Message(Llm.Role.User, t"How many bones are in the human hand?")

val illustrated = Llm.Message
  ( Llm.Role.User,
    List
      ( Llm.Content.Textual(t"What is in this picture?"),
        Llm.Content.Graphic(Llm.Content.Source.Remote(url"https://example.com/hand.png")) ) )
```

- `ask` and `stream` accept a message as readily as text, which is how images and documents are
- sent.
+ `ask` et `stream` acceptent un message aussi volontiers que du texte ; c'est ainsi que l'on
+ envoie images et documents.

- ### Errors and retries
+ ### Erreurs et nouvelles tentatives

- Every failure is an `Llm.Error` whose reason is one of a fixed set, or `Provider`, which carries
- a code the provider reported which maps to none of them, so the wire's own error is never lost. A
- rate limit or an overload is retried automatically, up to three times and honoring the delay
- which the provider asks for, before it is raised:
+ Tout échec est une `Llm.Error` dont la raison fait partie d'un ensemble fixe, ou `Provider`, qui
+ porte un code signalé par le fournisseur ne correspondant à aucune d'elles ; l'erreur propre au
+ protocole n'est donc jamais perdue. Une limite de débit ou une surcharge est retentée
+ automatiquement, jusqu'à trois fois et en respectant le délai que demande le fournisseur, avant
+ d'être levée :

```tabular
- column  Reason
- column  Meaning
- row  `Unreachable`  the provider could not be reached
- row  `Unauthorized`  the API key was missing, invalid or expired
- row  `Invalid`  the provider rejected the request as invalid
- row  `NotFound`  the model or endpoint does not exist
- row  `TooLarge`  the conversation exceeds the model's context window
- row  `RateLimited`  the provider imposed a rate limit; retried first
- row  `Overloaded`  the provider is temporarily overloaded; retried first
- row  `Malformed`  the provider's reply could not be interpreted
- row  `Interrupted`  the stream ended before the message was complete
- row  `ToolLoopExceeded`  the tool-use loop exceeded its iteration limit
- row  `Provider`  a code the provider reported that maps to none of the others
+ column  Raison
+ column  Signification
+ row  `Unreachable`  le fournisseur n'a pas pu être joint
+ row  `Unauthorized`  la clé d'API était absente, invalide ou expirée
+ row  `Invalid`  le fournisseur a rejeté la requête comme invalide
+ row  `NotFound`  le modèle ou l'endpoint n'existe pas
+ row  `TooLarge`  la conversation dépasse la fenêtre de contexte du modèle
+ row  `RateLimited`  le fournisseur a imposé une limite de débit ; retentée d'abord
+ row  `Overloaded`  le fournisseur est temporairement surchargé ; retentée d'abord
+ row  `Malformed`  la réponse du fournisseur n'a pas pu être interprétée
+ row  `Interrupted`  le flux s'est terminé avant que le message soit complet
+ row  `ToolLoopExceeded`  la boucle d'utilisation d'outils a dépassé sa limite d'itérations
+ row  `Provider`  un code signalé par le fournisseur qui ne correspond à aucune des autres raisons
```

- A handler matches on the reason:
+ Un gestionnaire filtre sur la raison :

```scala fuel
typecheck
##
recover:
  case error: Llm.Error => error.reason match
    case Llm.Error.Reason.Unauthorized => Out.println(t"check the API key")
    case Llm.Error.Reason.TooLarge     => Out.println(t"shorten the conversation")
    case _                             => Out.println(error.detail)
. protect:
    claude.session(llm.ask(t"Hello"))
```

- ### Counting tokens
+ ### Compter les jetons

- Anthropic reports the token count of a prospective conversation without running it, which is how
- a program decides whether a document fits before sending it:
+ Anthropic indique le nombre de jetons d'une conversation envisagée sans l'exécuter ; c'est ainsi
+ qu'un programme décide si un document tient avant de l'envoyer :

```scala fuel
typecheck
##
val tokens: Int = claude.countTokens(List(question), system = t"Be terse.")
```

- ### Testing without a provider
+ ### Tester sans fournisseur

- Every provider is a dialect over an HTTP backend, so a program which talks to a model is tested
- by substituting a backend which returns scripted responses. The dialect tests in the library do
- exactly that. Seventy-odd offline tests replay recorded replies, streamed deltas and error
- envelopes through each dialect, and prove with compiletime checks that a session or a live
- stream cannot escape its block. A program built on the same vocabulary inherits that
- testability. The model is a value, and a value can be faked.
+ Chaque fournisseur est un dialecte au-dessus d'un backend HTTP ; un programme qui dialogue avec
+ un modèle se teste donc en substituant un backend qui renvoie des réponses scénarisées. Les tests
+ de dialectes de la bibliothèque font exactement cela. Quelque soixante-dix tests hors ligne
+ rejouent des réponses enregistrées, des deltas en flux et des enveloppes d'erreur à travers
+ chaque dialecte, et prouvent par des vérifications à la compilation qu'une session ou un flux
+ actif ne peut s'échapper de son bloc. Un programme bâti sur le même vocabulaire hérite de cette
+ testabilité. Le modèle est une valeur, et une valeur peut être simulée.

- ### Related
+ ### Voir aussi

- - [MCP](mcp.md) exposes the same annotated methods as tools for any assistant.
- - [ACP](acp.md) drives a coding agent, instead of a model directly.
- - [HTTP client](http-client.md) describes the backend which every dialect speaks over.
- - [JSON](json.md#schemas) describes the schemas derived for tools and structured answers.
- - [Errors](errors.md) describes `recover` and the other strategies which meet an `Llm.Error`.
- - [Diffing](diffing.md) describes the redrafts which a model's edits are applied as.
+ - [MCP](mcp.md) expose les mêmes méthodes annotées comme outils pour tout assistant.
+ - [ACP](acp.md) pilote un agent de programmation, au lieu d'un modèle directement.
+ - [Client HTTP](http-client.md) décrit le backend sur lequel parle chaque dialecte.
+ - [JSON](json.md#schemas) décrit les schémas dérivés pour les outils et les réponses structurées.
+ - [Erreurs](errors.md) décrit `recover` et les autres stratégies qui traitent une `Llm.Error`.
+ - [Différences](diffing.md) décrit les révisions sous la forme desquelles s'appliquent les
+   modifications d'un modèle.
