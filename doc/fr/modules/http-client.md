- ## HTTP Client
+ ## Client HTTP

- ### About
+ ### Présentation

- An [HTTP](https://en.wikipedia.org/wiki/HTTP) request is one expression: a
- [URL](network-addresses.md#urls), a method such as `fetch`, and the type the response should be
- read as. Each part is typed. The URL is checked at [compiletime], a header's value must suit the
- header it sets, a request body carries its own media type, and the response body is read as
- whichever type is asked for.
+ Une requête [HTTP](https://en.wikipedia.org/wiki/HTTP) est une seule expression : une
+ [URL](network-addresses.md#urls), une méthode comme `fetch`, et le type dans lequel lire la
+ réponse. Chaque partie est typée. L'URL est vérifiée [à la compilation][compilation], la valeur
+ d'un en-tête doit convenir à l'en-tête qu'elle définit, un corps de requête porte son propre
+ type de média, et le corps de la réponse se lit dans le type demandé.

- Failure is typed too. A status outside the success range and a connection that cannot be made
- are two distinct errors, and a method which makes requests declares both. A request also needs
- the [capability] `Online` in scope, so a method that reaches the network says so in its
- signature, and one that should not reach it cannot.
+ L'échec est typé lui aussi. Un statut hors de la plage de succès et une connexion impossible à
+ établir sont deux erreurs distinctes, et une méthode qui fait des requêtes déclare les deux. Une
+ requête a aussi besoin de la [capacité] `Online` dans la portée ; une méthode qui accède au
+ réseau l'indique donc dans sa signature, et une méthode qui ne devrait pas y accéder ne le peut
+ pas.

- ### On HTTP requests
+ ### Sur les requêtes HTTP

- A typical HTTP client takes a URL as a string, headers as a map from strings to strings, and a
- body as bytes. It hands back a response whose status must be checked by hand and whose body must
- be decoded by hand. The URL may be malformed, a header name may be misspelled, and the body's
- media type may not match its bytes. None of this is discovered until the request runs, if then.
+ Un client HTTP typique prend une URL sous forme de chaîne, des en-têtes sous forme de map de
+ chaînes vers des chaînes, et un corps sous forme d'octets. Il renvoie une réponse dont le statut
+ doit être vérifié à la main et dont le corps doit être décodé à la main. L'URL peut être mal
+ formée, le nom d'un en-tête peut être mal orthographié, et le type de média du corps peut ne pas
+ correspondre à ses octets. Rien de tout cela n'est découvert avant l'exécution de la requête, et
+ encore.

- Here each of those mistakes is found by the compiler:
+ Ici, chacune de ces erreurs est détectée par le compilateur :

```tabular
- column  Part of a request
- column  How it is checked
- row  the URL  a `url"…"` [interpolator] validates it where it is written
- row  a header  a named argument, whose value has the type that header takes
- row  the request body  a typed value which supplies its own `Content-Type`
- row  the response body  read with `receive`, as the type named
- row  a failing status  an `Http.Error`, declared by the method that may raise it
- row  a failed connection  a `Connect.Error`, declared likewise
- row  access to the network  the `Online` capability, required in scope
+ column  Partie d'une requête
+ column  Comment elle est vérifiée
+ row  l'URL  un [interpolateur] `url"…"` la valide là où elle est écrite
+ row  un en-tête  un argument nommé, dont la valeur a le type que prend cet en-tête
+ row  le corps de la requête  une valeur typée qui fournit son propre `Content-Type`
+ row  le corps de la réponse  lu avec `receive`, dans le type nommé
+ row  un statut d'échec  une `Http.Error`, déclarée par la méthode qui peut la lever
+ row  une connexion échouée  une `Connect.Error`, déclarée de même
+ row  l'accès au réseau  la capacité `Online`, requise dans la portée
```

- Reporting failure through the types, and leaving the caller to decide what a failure does, is
- [error handling](../../philosophy/error-handling.md) as it works everywhere else.
+ Signaler l'échec par les types, et laisser l'appelant décider de ce que fait un échec, c'est la
+ [gestion des erreurs](../../philosophy/error-handling.md) telle qu'elle fonctionne partout
+ ailleurs.

- ### What a request needs in scope
+ ### Ce dont une requête a besoin dans la portée

- Everything comes from the `soundness` package. A request also depends on several decisions that
- are made by importing a [given] from a [choice package], so that none is made silently:
+ Tout provient du package `soundness`. Une requête dépend aussi de plusieurs décisions qui se
+ prennent en important un [given] depuis un [package de choix], de sorte qu'aucune n'est prise
+ en silence :

```scala
import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
```

```tabular
- column  Import
- column  Decides
- row  `charsets.utf8Charset`, `codepages.utf8Codepage`  how bytes become text, and text becomes bytes
- row  `errorDiagnostics.stackTracesDiagnostics`  that an error records a stack trace
- row  `httpBackends.javaNetHttp`  which implementation sends the request: here, the JVM's `java.net.http`
- row  `internetAccess.online`  that this code may use the network
- row  `logging.silentLogging`  that the requests are not logged; [logging](logging.md) has the alternatives
- row  `strategies.throwUnsafely`  that an error is thrown as an exception; [errors](errors.md#choosing-a-strategy) has the other strategies
+ column  Import
+ column  Détermine
+ row  `charsets.utf8Charset`, `codepages.utf8Codepage`  comment les octets deviennent du texte, et le texte des octets
+ row  `errorDiagnostics.stackTracesDiagnostics`  qu'une erreur enregistre une trace de pile
+ row  `httpBackends.javaNetHttp`  quelle implémentation envoie la requête : ici, le `java.net.http` de la JVM
+ row  `internetAccess.online`  que ce code peut utiliser le réseau
+ row  `logging.silentLogging`  que les requêtes ne sont pas journalisées ; [journalisation](logging.md) présente les alternatives
+ row  `strategies.throwUnsafely`  qu'une erreur est levée sous forme d'exception ; [erreurs](errors.md#choosing-a-strategy) présente les autres stratégies
```

- ### A GET request
+ ### Une requête GET

- `fetch` sends a GET request to a URL. It gives the response, and `receive` reads the response's
- body as the type named:
+ `fetch` envoie une requête GET à une URL. Elle donne la réponse, et `receive` lit le corps de la
+ réponse dans le type nommé :

```scala fuel
typecheck
##
val page: Text = url"https://example.com/".fetch().receive[Text]
```

- The type passed to `receive` chooses how the body is read. `Text` decodes it with the charset in
- scope, `Data` gives its bytes, and a [stream](streams.md) delivers it a part at a time without
- holding the whole body in memory. A type with a decoder for a format reads the same way, so a
- [JSON](json.md#json-over-http) response is received as `Json`.
+ Le type passé à `receive` détermine la façon dont le corps est lu. `Text` le décode avec le jeu
+ de caractères de la portée, `Data` donne ses octets, et un [flux](streams.md) le délivre morceau
+ par morceau sans garder tout le corps en mémoire. Un type doté d'un décodeur pour un format se
+ lit de la même façon ; une réponse [JSON](json.md#json-over-http) se reçoit donc sous forme de
+ `Json`.

- The response reports its status without its body being read:
+ La réponse indique son statut sans que son corps soit lu :

```scala fuel
typecheck
##
val status: Http.Status = url"https://example.com/".fetch().status
```

- For a page that exists this is `Http.Ok`.
+ Pour une page qui existe, c'est `Http.Ok`.

- The URL is a typed value, not a string, so a malformed one never reaches the network. The
- interpolator rejects it:
+ L'URL est une valeur typée, et non une chaîne ; une URL mal formée n'atteint donc jamais le
+ réseau. L'interpolateur la rejette :

```scala fuel
compile
errors show
##
url"https://example.com:port/"
```

- A URL which is not known until the program runs is
- [parsed at runtime](network-addresses.md#parsing-at-runtime) instead, and the parsing is where a
- bad one is reported.
+ Une URL inconnue avant l'exécution du programme est plutôt
+ [analysée à l'exécution](network-addresses.md#parsing-at-runtime), et c'est lors de l'analyse
+ qu'une URL incorrecte est signalée.

- ### Headers
+ ### En-têtes

- A request header is a named argument to `fetch`. Its name is the header's name in camel case, and
- its type is whatever that header carries: `accept` takes a [media type](media-types.md), not any
- text.
+ Un en-tête de requête est un argument nommé de `fetch`. Son nom est celui de l'en-tête en camel
+ case, et son type est ce que porte cet en-tête : `accept` prend un
+ [type de média](media-types.md), et non n'importe quel texte.

```scala fuel
typecheck
##
url"https://example.com/".fetch(accept = media"application/json").receive[Text]
```

- A value of the wrong type for its header does not compile:
+ Une valeur du mauvais type pour son en-tête ne compile pas :

```scala fuel
compile
errors show
##
url"https://example.com/".fetch(accept = t"application/json")
```

- Nor does a header whose name is misspelled, since no header of that name takes any value:
+ Un en-tête dont le nom est mal orthographié non plus, puisqu'aucun en-tête de ce nom ne prend de
+ valeur :

```scala fuel
compile
errors show
##
url"https://example.com/".fetch(acept = media"application/json")
```

- ### Posting a body
+ ### Envoyer un corps

- `submit` sends a request that has a body. Its first parameter list takes the method and any
- headers, and its second takes the body:
+ `submit` envoie une requête dotée d'un corps. Sa première liste de paramètres prend la méthode et
+ les éventuels en-têtes, et la seconde prend le corps :

```scala fuel
typecheck
##
url"https://example.com/submit".submit(Http.Post, accept = media"application/json")(t"Hello world")
```

- `Http.Put`, `Http.Delete` and the others name the remaining methods.
+ `Http.Put`, `Http.Delete` et les autres nomment les méthodes restantes.

- The body is a typed value, and its type decides the request's `Content-Type`, so the header can
- never disagree with the bytes that follow it. Text is sent as `text/plain`, a `Query` is sent as
- a [form submission](forms.md#submissions), and a `Json` value is sent as `application/json`.
- [Types that know their media type](media-types.md#types-that-know-their-media-type) describes how
- a type of one's own takes part.
+ Le corps est une valeur typée, et son type détermine le `Content-Type` de la requête ; l'en-tête
+ ne peut donc jamais contredire les octets qui le suivent. Le texte est envoyé en `text/plain`,
+ une `Query` est envoyée comme une [soumission de formulaire](forms.md#submissions), et une valeur
+ `Json` est envoyée en `application/json`.
+ [Types qui connaissent leur type de média](media-types.md#types-that-know-their-media-type)
+ décrit comment un type que l'on définit soi-même y prend part.

- ### Reading the response
+ ### Lire la réponse

- `receive` reads the body as any type that can be read from a response: `Text`, `Data`, a stream,
- or a value parsed from the body. The status is an ordinary value, to inspect or to return, so a
- program decides what a given status means.
+ `receive` lit le corps dans tout type qui peut être lu depuis une réponse : `Text`, `Data`, un
+ flux, ou une valeur analysée à partir du corps. Le statut est une valeur ordinaire, à inspecter
+ ou à renvoyer ; c'est donc le programme qui décide de ce que signifie un statut donné.

- By default a response outside the success range is not read at all: `receive` raises an
- `Http.Error`, described under [Errors](#errors).
+ Par défaut, une réponse hors de la plage de succès n'est pas lue du tout : `receive` lève une
+ `Http.Error`, décrite sous [Erreurs](#errors).

- ### Sessions
+ ### Sessions

- A single `fetch` opens a connection and closes it. Where several requests go to the same origin,
- a [session] opens one connection and lends it to a block, and every request made in the block
- shares it:
+ Un seul `fetch` ouvre une connexion et la ferme. Là où plusieurs requêtes vont vers la même
+ origine, une [session] ouvre une connexion et la prête à un bloc, et toutes les requêtes faites
+ dans le bloc la partagent :

```scala fuel
typecheck
##
import soundness.httpUrlSessional
import socketBackends.javaBaseSockets

val request = Http.Request(Http.Get, 1.1, t"example.com".as[Host], t"/", Nil,
    new Spring[Data]:
      def apply()(using Tactic[Truncation.Error]): Stream[Data] over Credit =
        Http.emptyBody())

url"https://example.com".session: session ?=>
  session.fetch(request).status
  session.fetch(request).status
```

- The protocol is fixed for as long as the session lasts. For `https`, the server and client agree
- on it during the TLS handshake, by
- [ALPN](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation):
+ Le protocole est fixé pour toute la durée de la session. Pour `https`, le serveur et le client
+ s'accordent sur lui pendant la poignée de main TLS, par
+ [ALPN](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation) :

```tabular
- column  Scheme
- column  Negotiated
- column  Session
- row  `https`  `h2`  *multiplexed* HTTP/2: fetches interleave over one connection
- row  `https`  `http/1.1`  *sequential* HTTP/1.1
- row  `http`  nothing  *sequential* HTTP/1.1, keep-alive pinning one connection
+ column  Schéma
+ column  Négocié
+ column  Session
+ row  `https`  `h2`  HTTP/2 *multiplexé* : les requêtes s'entrelacent sur une seule connexion
+ row  `https`  `http/1.1`  HTTP/1.1 *séquentiel*
+ row  `http`  rien  HTTP/1.1 *séquentiel*, le keep-alive maintenant une seule connexion
```

- The block's boundary is enforced by the compiler. A response body that is still streaming from
- the live connection cannot be returned from the block, because the connection it reads from
- closes when the block ends. A value that has been read into memory can be returned freely.
+ La limite du bloc est imposée par le compilateur. Un corps de réponse encore en cours de
+ diffusion depuis la connexion active ne peut pas être renvoyé hors du bloc, car la connexion
+ depuis laquelle il lit se ferme quand le bloc se termine. Une valeur qui a été lue en mémoire
+ peut être renvoyée librement.

- A response that is left unread does no harm. The next fetch first discards whatever remains of
- the previous body, so that it starts at the next response.
+ Une réponse laissée non lue ne cause aucun tort. La requête suivante ignore d'abord ce qui reste
+ du corps précédent, de sorte qu'elle commence à la réponse suivante.

- ### Choosing a transport
+ ### Choisir un transport

- The implementation that sends requests is a [given], chosen by import:
+ L'implémentation qui envoie les requêtes est un [given], choisi par import :

```tabular
- column  Import
- column  Sends requests with
- row  `httpBackends.javaNetHttp`  the JVM's own `java.net.http` client
- row  `httpBackends.soundnessHttp`  Soundness's own HTTP codecs, directly over a [socket](sockets.md)
+ column  Import
+ column  Envoie les requêtes avec
+ row  `httpBackends.javaNetHttp`  le client `java.net.http` propre à la JVM
+ row  `httpBackends.soundnessHttp`  les codecs HTTP propres à Soundness, directement sur un [socket](sockets.md)
```

- A `locally` block confines a different choice to a few lines:
+ Un bloc `locally` confine un choix différent à quelques lignes :

```scala fuel
typecheck
##
locally:
  import httpBackends.soundnessHttp
  url"https://example.com/".fetch().status
```

- The `soundnessHttp` backend keeps HTTP/1.1 connections alive and reuses them for later requests
- to the same origin. Over TLS it negotiates HTTP/2 or HTTP/1.1 by ALPN and speaks whichever the
- server chose. If a kept connection turns out to have been closed by the server, the request is
- retried once on a new connection. Other platforms supply backends of their own in the same way.
+ Le backend `soundnessHttp` garde les connexions HTTP/1.1 ouvertes et les réutilise pour les
+ requêtes ultérieures vers la même origine. Sur TLS, il négocie HTTP/2 ou HTTP/1.1 par ALPN et
+ parle celui que le serveur a choisi. S'il s'avère qu'une connexion conservée a été fermée par le
+ serveur, la requête est retentée une fois sur une nouvelle connexion. D'autres plateformes
+ fournissent leurs propres backends de la même façon.

- ### Redirects
+ ### Redirections

- A redirect is followed by default, up to a limit. Importing a stricter policy stops that, and a
- redirect response is then an `Http.Error` carrying the redirect's status:
+ Une redirection est suivie par défaut, jusqu'à une limite. Importer une politique plus stricte
+ l'empêche, et une réponse de redirection est alors une `Http.Error` qui porte le statut de la
+ redirection :

```scala
import httpRedirections.doNotFollowRedirects
```

- ### Errors
+ ### Erreurs

- A request fails in one of two ways, and each is an error of its own type:
+ Une requête échoue de l'une de deux façons, et chacune est une erreur de son propre type :

```tabular
- column  Error
- column  Raised when
- column  Carries
- row  `Http.Error`  a response is outside the success range  the status and headers
- row  `Connect.Error`  no connection can be made: an unresolvable host, a refused connection, a TLS failure  the reason
+ column  Erreur
+ column  Levée quand
+ column  Porte
+ row  `Http.Error`  une réponse est hors de la plage de succès  le statut et les en-têtes
+ row  `Connect.Error`  aucune connexion ne peut être établie : un hôte impossible à résoudre, une connexion refusée, un échec TLS  la raison
```

- Under the `throwUnsafely` [strategy] imported above, either is thrown as an exception. A method
- that should not throw declares the errors instead, with [`raises`][raises], and its caller chooses
- what to do about them; [Errors](errors.md) covers the choices.
+ Sous la [stratégie] `throwUnsafely` importée plus haut, l'une ou l'autre est levée sous forme
+ d'exception. Une méthode qui ne devrait pas lever d'exception déclare plutôt les erreurs, avec
+ [`raises`][raises], et son appelant choisit quoi en faire ; [Erreurs](errors.md) couvre les
+ choix possibles.

- `capture` runs an expression that is expected to fail, and gives the error. An `Http.Error` has
- the status, so the caller can react to what went wrong:
+ `capture` exécute une expression dont on attend l'échec, et donne l'erreur. Une `Http.Error`
+ contient le statut ; l'appelant peut donc réagir à ce qui s'est mal passé :

```scala fuel
typecheck
##
capture[Http.Error](url"https://example.com/missing".fetch().receive[Text]).status
```

- For a missing page this is `Http.NotFound`.
+ Pour une page manquante, c'est `Http.NotFound`.

- A `Connect.Error` has a `reason`, which says why no connection was made:
+ Une `Connect.Error` a une `reason`, qui indique pourquoi aucune connexion n'a été établie :

```scala fuel
typecheck
##
capture[Connect.Error](url"http://no-such-host.invalid/".fetch()).reason
```

```tabular
- column  Reason
- column  Means
- row  `Dns`  the host's name could not be resolved
- row  `Refused`  the host refused the connection
- row  `Ssl(reason)`  the TLS layer could not be established; its own reason is `Handshake`, `Key`, `Peer` or `Protocol`
- row  `Timeout`  the server did not respond within the time limit
- row  `Unknown`  the failure was not recognized as one of the others
+ column  Raison
+ column  Signification
+ row  `Dns`  le nom de l'hôte n'a pas pu être résolu
+ row  `Refused`  l'hôte a refusé la connexion
+ row  `Ssl(reason)`  la couche TLS n'a pas pu être établie ; sa propre raison est `Handshake`, `Key`, `Peer` ou `Protocol`
+ row  `Timeout`  le serveur n'a pas répondu dans le délai imparti
+ row  `Unknown`  l'échec n'a été reconnu comme aucun des autres
```

- A TLS failure is therefore told apart from a network failure, and its reason says at which stage
- it happened. An expired certificate, a certificate for the wrong host, a revoked certificate, a
- broken cipher suite and an undersized Diffie–Hellman group are each refused during the
- handshake. The client is tested against the [badssl.com](https://badssl.com/) collection of
- deliberately broken endpoints, so each refusal is verified.
+ Un échec TLS se distingue donc d'un échec réseau, et sa raison indique à quelle étape il s'est
+ produit. Un certificat expiré, un certificat pour le mauvais hôte, un certificat révoqué, une
+ suite de chiffrement défaillante et un groupe Diffie–Hellman trop petit sont chacun refusés
+ pendant la poignée de main. Le client est testé sur la collection
+ [badssl.com](https://badssl.com/) d'endpoints délibérément défectueux ; chaque refus est donc
+ vérifié.

- Sometimes a connection must be made to a host whose certificate cannot be validated, such as a
- development server with a self-signed certificate. A relaxed `TlsAcceptance` in scope permits it.
- Accepting an unvalidated certificate is then a decision that can be found by searching the code
- for the import.
+ Il faut parfois se connecter à un hôte dont le certificat ne peut être validé, comme un serveur
+ de développement doté d'un certificat auto-signé. Une `TlsAcceptance` assouplie dans la portée
+ le permet. Accepter un certificat non validé est alors une décision que l'on retrouve en
+ cherchant l'import dans le code.

- ### Related
+ ### Voir aussi

- - [HTTP Server](http-server.md) answers requests with the same `Http` vocabulary of methods,
-   statuses and headers.
- - [JSON over HTTP](json.md#json-over-http) posts a `Json` value and receives one.
- - [OAuth](oauth.md#credentials-for-a-typed-client) obtains the token an authorized request sends.
- - [Network addresses](network-addresses.md) covers URLs, hostnames and ports.
+ - [Serveur HTTP](http-server.md) répond aux requêtes avec le même vocabulaire `Http` de
+   méthodes, de statuts et d'en-têtes.
+ - [JSON sur HTTP](json.md#json-over-http) envoie une valeur `Json` et en reçoit une.
+ - [OAuth](oauth.md#credentials-for-a-typed-client) obtient le jeton qu'envoie une requête
+   autorisée.
+ - [Adresses réseau](network-addresses.md) couvre les URL, les noms d'hôte et les ports.
