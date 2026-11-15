- ## HTTP Server
+ ## Serveur HTTP

- ### About
+ ### Présentation

- An HTTP server is a handler: a function from a request to a response, served on a port. One
- is run with a single expression. The handler reads the request which is in scope, matches on
- its method and path, and returns a typed response, whose `Content-Type` follows from the
- value it carries.
+ Un serveur HTTP est un gestionnaire : une fonction d'une requête vers une réponse, servie sur un
+ port. On en lance un avec une seule expression. Le gestionnaire lit la requête qui est dans la
+ portée, filtre sur sa méthode et son chemin, et renvoie une réponse typée, dont le
+ `Content-Type` découle de la valeur qu'elle transporte.

- The server speaks modern HTTP/1.1, with keep-alive, pipelining, chunked bodies, TLS,
- `100-continue` and protocol upgrades, on a virtual thread for each connection. It speaks
- HTTP/2 on any connection whose TLS handshake negotiates it.
+ Le serveur parle un HTTP/1.1 moderne, avec keep-alive, pipelining, corps découpés en morceaux,
+ TLS, `100-continue` et changements de protocole, sur un thread virtuel par connexion. Il parle
+ HTTP/2 sur toute connexion dont la poignée de main TLS le négocie.

- Two neighboring protocols sit above HTTP/1.1.
- [WebSockets](https://en.wikipedia.org/wiki/WebSocket) upgrade a request into a stream of
- messages, which is handled as a state machine. HTTP/2 carries many requests over one
- connection, for browsers and for the protocols in the style of gRPC which require it.
+ Deux protocoles voisins se placent au-dessus de HTTP/1.1. Les
+ [WebSockets](https://en.wikipedia.org/wiki/WebSocket) transforment une requête en un flux de
+ messages, traité comme une machine à états. HTTP/2 transporte de nombreuses requêtes sur une
+ seule connexion, pour les navigateurs et pour les protocoles dans le style de gRPC qui
+ l'exigent.

- ### On serving HTTP
+ ### Servir du HTTP

- Server frameworks tend to arrive as inversion of control: DSLs for routing, controller
- classes, annotation processors, and a runtime which calls the application, instead of the
- reverse. The protocol underneath is simpler than the frameworks suggest. A request arrives,
- and a response is computed. A typed language can express that directly, with routing as
- ordinary pattern matching, and content negotiation as [typeclasses][typeclass].
+ Les frameworks serveur se présentent généralement sous forme d'inversion de contrôle : des DSL
+ de routage, des classes contrôleurs, des processeurs d'annotations, et un environnement
+ d'exécution qui appelle l'application, au lieu de l'inverse. Le protocole sous-jacent est plus
+ simple que ne le laissent entendre les frameworks. Une requête arrive, et une réponse est
+ calculée. Un langage typé peut l'exprimer directement, le routage étant un simple filtrage par
+ motif, et la négociation de contenu des [classes de types][classe de types].

- Here a handler is a block of code, and everything else is values. A handler which states what
- it needs, and what it may fail with, has an
- [honest signature](../../philosophy/honest-signatures.md), and that is why a server can be tested
- without a socket.
+ Ici, un gestionnaire est un bloc de code, et tout le reste est fait de valeurs. Un gestionnaire
+ qui indique ce dont il a besoin, et ce avec quoi il peut échouer, a une
+ [signature honnête](../../philosophy/honest-signatures.md), et c'est pourquoi un serveur peut
+ être testé sans socket.

- Everything comes from the `soundness` package. The [concurrency](concurrency.md) context
- which a server runs under is in scope:
+ Tout vient du package `soundness`. Le contexte de [concurrence](concurrency.md) sous lequel
+ s'exécute un serveur est dans la portée :

```scala
import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import textSanitizers.skipSanitizer
import threads.virtualThreads
import webserverErrorPages.minimalErrorPage
```

```tabular
- column  Import
- column  Decides
- row  `threads.virtualThreads`, `probates.awaitProbate`  how the server's tasks run, and what happens to them when their scope ends
- row  `webserverErrorPages.minimalErrorPage`  what a client is sent when a handler fails
- row  `logging.silentLogging`  that requests are not logged; [Logging](logging.md) has the alternatives
- row  `formatting.compactJsonFormatting`  how a JSON body is written
- row  `charsets`, `codepages`, `textSanitizers`  how bytes become text, and text becomes bytes
- row  `strategies.throwUnsafely`  that an error is thrown as an exception
+ column  Import
+ column  Détermine
+ row  `threads.virtualThreads`, `probates.awaitProbate`  comment s'exécutent les tâches du serveur, et ce qu'il advient d'elles quand leur portée se termine
+ row  `webserverErrorPages.minimalErrorPage`  ce qui est envoyé à un client quand un gestionnaire échoue
+ row  `logging.silentLogging`  que les requêtes ne sont pas journalisées ; [Journalisation](logging.md) présente les alternatives
+ row  `formatting.compactJsonFormatting`  comment un corps JSON est écrit
+ row  `charsets`, `codepages`, `textSanitizers`  comment les octets deviennent du texte, et le texte des octets
+ row  `strategies.throwUnsafely`  qu'une erreur est levée sous forme d'exception
```

- The samples on this page bind a port, so they are compiled but not run.
+ Les exemples de cette page lient un port : ils sont donc compilés mais pas exécutés.

- ### A server
+ ### Un serveur

- `SocketServer` listens on a port, and runs the handler for each request. The port is bound
- when `handle` is called, and the service which `handle` returns stops the server when it is
- canceled. The service runs on tasks of the enclosing `supervise`, so the block lasts as long
- as the server does.
+ `SocketServer` écoute sur un port, et exécute le gestionnaire pour chaque requête. Le port est
+ lié quand `handle` est appelé, et le service que renvoie `handle` arrête le serveur quand il est
+ annulé. Le service s'exécute sur des tâches du `supervise` englobant : le bloc dure donc aussi
+ longtemps que le serveur.

- Within the handler, `request` is the current request, with its method, target, headers,
- cookies and body. Routing is therefore a `match`:
+ Dans le gestionnaire, `request` est la requête courante, avec sa méthode, sa cible, ses
+ en-têtes, ses cookies et son corps. Le routage est donc un `match` :

```scala fuel
frame
  label One response to every request
  code
      def hello(): Unit = supervise:
        val server = SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"Hello, world!")
frame
  label Routed on the request's target
  code
      val homePage = t"<h1>Welcome</h1>"

      def routed(): Unit = supervise:
        SocketServer(8080).handle:
          request.target match
            case t"/"       => Http.Response(Http.Ok)(homePage)
            case t"/status" => Http.Response(Http.Ok)(t"OK")
            case _          => Http.Response(Http.NotFound)(t"No such page")
##
```

- ### Responses
+ ### Réponses

- `Http.Response` takes a status, headers as named arguments, and a body. The body may be of any
- type with a `Servable` instance, such as text, bytes, [HTML](html.md) or [JSON](json.md). The
- instance supplies the media type, so what is served, and how it is labeled, cannot disagree:
+ `Http.Response` prend un statut, des en-têtes sous forme d'arguments nommés, et un corps. Le
+ corps peut être de n'importe quel type ayant une instance de `Servable`, comme du texte, des
+ octets, du [HTML](html.md) ou du [JSON](json.md). L'instance fournit le type de média : ce qui
+ est servi, et la façon dont c'est étiqueté, ne peuvent donc pas être en désaccord :

```scala
val status = Http.Response(Http.Ok, cacheControl = t"no-cache")(t"""{"status": "ok"}""".read[Json])
val moved = Http.Response(Http.Found, location = t"/elsewhere")()
```

- The headers are checked as they are for a [request](http-client.md#headers): a header's value
- must have the type which that header takes.
+ Les en-têtes sont vérifiés comme ils le sont pour une [requête](http-client.md#headers) : la
+ valeur d'un en-tête doit avoir le type que cet en-tête prend.

- This is the `Content-Type` which each kind of body is served with:
+ Voici le `Content-Type` avec lequel chaque sorte de corps est servie :

```tabular
- column  Body
- column  `Content-Type`
- row  `Text`  `text/plain`
- row  `Data`  `application/octet-stream`
- row  `Document[Html]`  `text/html; charset=UTF-8`
- row  `Json`  `application/json; charset=UTF-8`
- row  `Xml`  `application/xml; charset=UTF-8`
- row  `Yaml`  `application/yaml; charset=UTF-8`
- row  `Tel`  `application/vnd.tel`
- row  `Cbor`  `application/cbor`
- row  `Protobuf`  `application/protobuf`
- row  `Chain[Sse]`  `text/event-stream`
- row  any other type with a `Media` instance  the media type it declares
+ column  Corps
+ column  `Content-Type`
+ row  `Text`  `text/plain`
+ row  `Data`  `application/octet-stream`
+ row  `Document[Html]`  `text/html; charset=UTF-8`
+ row  `Json`  `application/json; charset=UTF-8`
+ row  `Xml`  `application/xml; charset=UTF-8`
+ row  `Yaml`  `application/yaml; charset=UTF-8`
+ row  `Tel`  `application/vnd.tel`
+ row  `Cbor`  `application/cbor`
+ row  `Protobuf`  `application/protobuf`
+ row  `Chain[Sse]`  `text/event-stream`
+ row  tout autre type ayant une instance de `Media`  le type de média qu'il déclare
```

- The instances for the format types are in the package `servables`.
- [Media types](media-types.md#types-that-know-their-media-type) describes how a type of one's
- own declares a media type.
+ Les instances pour les types de formats se trouvent dans le package `servables`.
+ [Types de médias](media-types.md#types-that-know-their-media-type) décrit comment un type que
+ l'on définit soi-même déclare un type de média.

- `Redirect`, `NotFound` and `NoCache` wrap content in the common shapes of response. The body
- of a request is read as a typed value with `as`, whether it is a
- [multipart](media-types.md#multipart) upload or a [form submission](forms.md#submissions).
- `basicAuth` guards a response behind credentials, and [OAuth](oauth.md#in-a-web-application)
- guards one behind a token.
+ `Redirect`, `NotFound` et `NoCache` enveloppent un contenu dans les formes courantes de réponse.
+ Le corps d'une requête se lit comme une valeur typée avec `as`, qu'il s'agisse d'un envoi
+ [multipart](media-types.md#multipart) ou d'une [soumission de formulaire](forms.md#submissions).
+ `basicAuth` protège une réponse derrière des identifiants, et
+ [OAuth](oauth.md#in-a-web-application) derrière un jeton.

- ### Request and response bodies
+ ### Corps des requêtes et des réponses

- The body of a request is a [stream](streams.md), not a block of bytes which must arrive before
- the handler runs. A large upload is consumed as it is received, and the body of a response is
- framed from its stream a block at a time, so neither is held in memory.
+ Le corps d'une requête est un [flux](streams.md), et non un bloc d'octets qui devrait arriver
+ avant l'exécution du gestionnaire. Un envoi volumineux est consommé au fur et à mesure de sa
+ réception, et le corps d'une réponse est découpé en trames depuis son flux un bloc à la fois :
+ ni l'un ni l'autre n'est donc conservé en mémoire.

- The body belongs to the handler's scope. It is the connection's, and the connection ends. A
- handler which wants to keep a body past its response therefore reads it into memory
- explicitly, with `memoize`.
+ Le corps appartient à la portée du gestionnaire. Il appartient à la connexion, et la connexion
+ se termine. Un gestionnaire qui veut conserver un corps au-delà de sa réponse le lit donc
+ explicitement en mémoire, avec `memoize`.

- ### WebSockets
+ ### WebSockets

- A request is upgraded to a WebSocket by returning a `webSocket` handler, which treats the
- connection as a state machine. Each incoming message produces a reply, a change of state, or
- the end of the conversation. Messages are typed: raw frames, text, or any type carried over
- JSON.
+ Une requête est transformée en WebSocket en renvoyant un gestionnaire `webSocket`, qui traite la
+ connexion comme une machine à états. Chaque message entrant produit une réponse, un changement
+ d'état, ou la fin de la conversation. Les messages sont typés : trames brutes, texte, ou
+ n'importe quel type transporté en JSON.

```scala
case class Ping(value: Int)

def pinger(): Unit = supervise:
  SocketServer(8080).handle:
    webSocket(): (ping: Ping over Json) =>
      Control.Reply(Ping(ping.value + 1).over[Json], ())
```

- A handler answers each message with a `Control`:
+ Un gestionnaire répond à chaque message par un `Control` :

```tabular
- column  Answer
- column  Sends a message
- column  Then
- row  `Control.Reply(message, state)`  yes  carries on
- row  `Control.Continue(state)`  no  carries on
- row  `Control.Conclude(message, state)`  yes  closes
- row  `Control.Terminate`  no  closes at once
+ column  Réponse
+ column  Envoie un message
+ column  Ensuite
+ row  `Control.Reply(message, state)`  oui  continue
+ row  `Control.Continue(state)`  non  continue
+ row  `Control.Conclude(message, state)`  oui  ferme
+ row  `Control.Terminate`  non  ferme immédiatement
```

- The handler's state, which is `()` here, is passed from one message to the next. Where a
- state is optional, and is left out, the previous one carries over.
+ L'état du gestionnaire, qui vaut `()` ici, est transmis d'un message au suivant. Quand un état
+ est optionnel, et omis, le précédent est conservé.

- The implementation handles the protocol's sharp edges: masking, fragmentation, ping and pong,
- validation of UTF-8, and close codes. It conforms to the
- [Autobahn](https://github.com/crossbario/autobahn-testsuite) test suite.
+ L'implémentation gère les aspérités du protocole : masquage, fragmentation, ping et pong,
+ validation de l'UTF-8, et codes de fermeture. Elle est conforme à la suite de tests
+ [Autobahn](https://github.com/crossbario/autobahn-testsuite).

- ### HTTP/2
+ ### HTTP/2

- A listening socket with TLS offers `h2`, and then `http/1.1`, by ALPN. The protocol which a
- connection speaks follows from how it arrives:
+ Un socket d'écoute avec TLS propose `h2`, puis `http/1.1`, par ALPN. Le protocole que parle une
+ connexion découle de la façon dont elle arrive :

```tabular
- column  Connection
- column  Protocol
- row  cleartext, to a `SocketServer`  HTTP/1.1
- row  TLS, negotiating `h2`  HTTP/2
- row  TLS, negotiating `http/1.1` or nothing  HTTP/1.1
- row  cleartext h2c with prior knowledge, over an `Http2.Endpoint`  HTTP/2
+ column  Connexion
+ column  Protocole
+ row  en clair, vers un `SocketServer`  HTTP/1.1
+ row  TLS, négociant `h2`  HTTP/2
+ row  TLS, négociant `http/1.1` ou rien  HTTP/1.1
+ row  h2c en clair avec connaissance préalable, sur un `Http2.Endpoint`  HTTP/2
```

- The handler's contract is unchanged, so an existing application serves HTTP/2 with no
- alteration:
+ Le contrat du gestionnaire est inchangé : une application existante sert donc HTTP/2 sans
+ aucune modification :

```scala
def secure(context: javax.net.ssl.SSLContext): Unit = supervise:
  SocketServer(8443, ssl = context).handle:
    Http.Response(Http.Ok)(t"Hello over h2!")
```

- Each stream which a client initiates is handled on a virtual thread of its own, so the
- requests which share one connection run concurrently. The engine implements flow control,
- trailers, and full [HPACK](https://datatracker.ietf.org/doc/html/rfc7541) compression of
- headers. It tolerates the types of frame which it does not act on, such as `PRIORITY`,
- `PUSH_PROMISE` and extensions, and does not fail the connection.
- [Streams](streams.md#tuning-backpressure) describes how its flow control applies
- backpressure.
+ Chaque flux qu'initie un client est traité sur son propre thread virtuel : les requêtes qui
+ partagent une connexion s'exécutent donc de façon concurrente. Le moteur implémente le contrôle
+ de flux, les trailers, et la compression complète des en-têtes
+ [HPACK](https://datatracker.ietf.org/doc/html/rfc7541). Il tolère les types de trames sur
+ lesquels il n'agit pas, tels que `PRIORITY`, `PUSH_PROMISE` et les extensions, sans faire
+ échouer la connexion. [Flux](streams.md#tuning-backpressure) décrit comment son contrôle de flux
+ applique la contre-pression.

- Cleartext h2c with prior knowledge is the flavor used between services. It is spoken over an
- `Http2.Endpoint`, which is also a transport for the [HTTP client](http-client.md), and which
- carries the [gRPC](rpc.md) support built on top of it.
+ Le h2c en clair avec connaissance préalable est la variante utilisée entre services. Il est
+ parlé sur un `Http2.Endpoint`, qui est aussi un transport pour le
+ [client HTTP](http-client.md), et qui porte la prise en charge de [gRPC](rpc.md) construite
+ par-dessus.

- ### Front-ends
+ ### Frontaux

- Cleartext HTTP/1.1 connections are served by one of two front-ends. The choice is made by a
- `Frontend` given, in scope when `handle` is called:
+ Les connexions HTTP/1.1 en clair sont servies par l'un de deux frontaux. Le choix se fait par un
+ given `Frontend`, dans la portée au moment de l'appel à `handle` :

```tabular
- column  Front-end
- column  Runs a handler
- column  A blocking handler stalls
- row  `frontends.threadPerConnectionFrontend`, the default  on its connection's virtual thread  only its own connection
- row  `frontends.reactiveFrontend`  inline on one of a fixed fleet of selector threads  every connection on that loop
+ column  Frontal
+ column  Exécute un gestionnaire
+ column  Un gestionnaire bloquant bloque
+ row  `frontends.threadPerConnectionFrontend`, par défaut  sur le thread virtuel de sa connexion  uniquement sa propre connexion
+ row  `frontends.reactiveFrontend`  directement sur l'un d'un ensemble fixe de threads sélecteurs  toutes les connexions de cette boucle
```

- On the default front-end, a handler may block: on a [database](database.md), on a file, or on
- another service. The reactive front-end is an event loop, in which each selector thread
- serves a batch of ready connections each time it wakes:
+ Avec le frontal par défaut, un gestionnaire peut bloquer : sur une
+ [base de données](database.md), sur un fichier, ou sur un autre service. Le frontal réactif est
+ une boucle d'événements, dans laquelle chaque thread sélecteur sert un lot de connexions prêtes
+ à chaque réveil :

```scala
import frontends.reactiveFrontend

def reactive(): Unit = supervise:
  SocketServer(8080).handle:
    Http.Response(Http.Ok)(t"Hello, world!")
```

- The reactive front-end serves small requests at about one and a half times the rate of the
- default, and allocates less for each request, because nothing is scheduled for each request.
- On a loopback benchmark it is level with Netty and Undertow.
+ Le frontal réactif sert les petites requêtes à environ une fois et demie le débit du frontal
+ par défaut, et alloue moins pour chaque requête, car rien n'est planifié pour chaque requête.
+ Sur un benchmark en boucle locale, il fait jeu égal avec Netty et Undertow.

- The price is its contract. A handler which blocks stalls every connection on that loop. The
- reactive front-end therefore suits handlers which compute their response from what they
- already have, or which hand off to other tasks and answer from memory. Requests which the loop
- cannot serve inline are handed to a virtual thread, and served by the default path: chunked
- or oversized bodies, `Expect: 100-continue`, and protocol upgrades. A WebSocket handler
- therefore works under either front-end.
+ Le prix à payer est son contrat. Un gestionnaire qui bloque bloque toutes les connexions de
+ cette boucle. Le frontal réactif convient donc aux gestionnaires qui calculent leur réponse à
+ partir de ce qu'ils ont déjà, ou qui délèguent à d'autres tâches et répondent depuis la mémoire.
+ Les requêtes que la boucle ne peut pas servir directement sont confiées à un thread virtuel, et
+ servies par le chemin par défaut : corps découpés en morceaux ou surdimensionnés,
+ `Expect: 100-continue`, et changements de protocole. Un gestionnaire WebSocket fonctionne donc
+ avec l'un ou l'autre frontal.

- ### Related
+ ### Voir aussi

- - [HTTP client](http-client.md) makes requests with the same vocabulary of methods, statuses
-   and headers.
- - [HTML](html.md) and [JSON](json.md#json-over-http) describe the bodies most often served.
- - [Forms](forms.md) renders an HTML form and reads its submission.
- - [Sockets](sockets.md) is the layer beneath.
- - [Daemons](daemons.md) keeps a server running as a service.
- - [MCP](mcp.md), [web pages](pages.md), [patterns](patterns.md) and [cryptography](cryptography.md) describe what a server hosts, matches and secures.
+ - [Client HTTP](http-client.md) effectue des requêtes avec le même vocabulaire de méthodes, de
+   statuts et d'en-têtes.
+ - [HTML](html.md) et [JSON](json.md#json-over-http) décrivent les corps les plus souvent servis.
+ - [Formulaires](forms.md) rend un formulaire HTML et lit sa soumission.
+ - [Sockets](sockets.md) est la couche inférieure.
+ - [Démons](daemons.md) maintient un serveur en fonctionnement en tant que service.
+ - [MCP](mcp.md), [pages web](pages.md), [motifs](patterns.md) et
+   [cryptographie](cryptography.md) décrivent ce qu'un serveur héberge, reconnaît et sécurise.
