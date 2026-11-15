- ## Service Discovery
+ ## Découverte de services

- ### About
+ ### Présentation

- A program which serves something on the local network, such as a build daemon, a printer or a
- game, can be found without anyone being told its address.
- [DNS-based service discovery](https://www.rfc-editor.org/rfc/rfc6763) names a service type,
- such as `_fury._tcp`, and its instances, such as `Elsinore._fury._tcp.local`.
- [Multicast DNS](https://www.rfc-editor.org/rfc/rfc6762) carries those names on the link without
- a nameserver. Every host answers for its own.
+ Un programme qui sert quelque chose sur le réseau local, comme un démon de build, une imprimante
+ ou un jeu, peut être trouvé sans que personne n'ait à recevoir son adresse.
+ La [découverte de services fondée sur DNS](https://www.rfc-editor.org/rfc/rfc6763) nomme un
+ type de service, comme `_fury._tcp`, et ses instances, comme `Elsinore._fury._tcp.local`.
+ Le [DNS multicast](https://www.rfc-editor.org/rfc/rfc6762) transporte ces noms sur le lien sans
+ serveur de noms. Chaque hôte répond pour les siens.

- A service is advertised, others' services are browsed for, and an instance is resolved to its
- host, port and addresses, through a `Discovery.Backend`. The backend provided here is an mDNS
- responder over sockets. A later backend may delegate to a system responder such as Bonjour or
- Avahi.
+ Un service s'annonce, les services des autres se recherchent, et une instance se résout en son
+ hôte, son port et ses adresses, au moyen d'un `Discovery.Backend`. Le backend fourni ici est un
+ répondeur mDNS sur des sockets. Un backend ultérieur pourra déléguer à un répondeur du système
+ comme Bonjour ou Avahi.

- ### On discovery
+ ### La découverte

- Everything comes from the `soundness` package. The second import chooses a
- [strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
- results:
+ Tout provient du package `soundness`. Le deuxième import choisit une
+ [stratégie](errors.md#choosing-a-strategy) pour les erreurs, et le troisième permet aux
+ exemples d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- The samples which build names and records run here. Those which touch the network are compiled
- but not run.
+ Les exemples qui construisent des noms et des enregistrements s'exécutent ici. Ceux qui touchent
+ au réseau sont compilés mais pas exécutés.

- ### Services and instances
+ ### Services et instances

- A service type is a registered name of up to fifteen characters and a transport. An instance is
- one UTF-8 label, spaces and dots included, under that type. Both compute their DNS-SD names when
- they are constructed, under validation, so a name over the DNS limits is reported then, and not
- on the wire:
+ Un type de service est un nom enregistré d'au plus quinze caractères et un transport. Une
+ instance est une étiquette UTF-8, espaces et points compris, sous ce type. Les deux calculent
+ leurs noms DNS-SD à leur construction, avec validation : un nom qui dépasse les limites de DNS
+ est donc signalé à ce moment-là, et non sur le réseau :

```scala
val fury = Discovery.Service(t"fury", Tcp)
```

```scala fuel
run
##
Out.println(fury.dnsName.show)
Out.println(Discovery.Instance(t"Hamlet's Build Daemon", fury).dnsName.show)
```

- A service name over fifteen characters is rejected:
+ Un nom de service de plus de quinze caractères est rejeté :

```scala fuel
run
errors show
##
Discovery.Service(t"a-very-long-service-name", Tcp)
```

- An instance says more about itself in a TXT record: `key=value` pairs, read without regard to
- case, with the first occurrence of a key winning:
+ Une instance en dit davantage sur elle-même dans un enregistrement TXT : des paires
+ `key=value`, lues sans tenir compte de la casse, la première occurrence d'une clé l'emportant :

```scala
val txt = Discovery.Txt(t"fingerprint" -> t"ab12", t"version" -> t"0.71")
```

```scala fuel
run
##
Out.println(txt(t"Fingerprint").inspect)
Out.println(txt(t"missing").inspect)
```

- ### Advertising
+ ### Annonce

- An advertisement is a loan. The instance is probed for, claimed, and announced. If another host
- holds the name, the instance is renamed, to `Elsinore (2)`. It stays advertised for the block,
- and a goodbye is sent when the block ends, or when the `Monitor` is canceled, which unwinds it:
+ Une annonce est un prêt. L'instance est sondée, revendiquée et annoncée. Si un autre hôte détient
+ le nom, l'instance est renommée en `Elsinore (2)`. Elle reste annoncée pendant le bloc, et un
+ adieu est envoyé quand le bloc se termine, ou quand le `Monitor` est annulé, ce qui la défait :

```scala fuel
typecheck
##
import socketBackends.javaBaseSockets
import discoveryBackends.mdnsSockets
import threads.virtualThreads
import probates.awaitProbate
import logging.silentLogging

supervise:
  val description = Discovery.Description(t"Elsinore", tcp"8443", txt)

  fury.advertise(description):
    summon[Discovery.Advertisement].instance.dnsName
```

- The name inside the block is the name the instance was claimed with, which may not be the name
- it asked for.
+ Le nom au sein du bloc est le nom sous lequel l'instance a été revendiquée, qui n'est peut-être
+ pas celui qu'elle a demandé.

- ### Browsing and resolving
+ ### Recherche et résolution

- A browse is a loan too. It lends a `Discovery.Browser`, whose `events` are the instances
- appearing on and leaving the link, as they happen, and whose `instances` are those currently
- known. Resolving an instance gives its host name, port, TXT record and addresses, within a
- timeout:
+ Une recherche est aussi un prêt. Elle prête un `Discovery.Browser`, dont les `events` sont les
+ instances qui apparaissent sur le lien et le quittent, au fur et à mesure, et dont les
+ `instances` sont celles actuellement connues. Résoudre une instance donne son nom d'hôte, son
+ port, son enregistrement TXT et ses adresses, dans un délai imparti :

```scala fuel
typecheck
##
import abstractables.millisecondsAbstractable

val found: Promise[List[Endpoint[Port]]] = Promise()

supervise:
  fury.browse:
    summon[Discovery.Browser].events.each:
      case Discovery.Event.Found(instance) =>
        val resolution = instance.resolve(5000L)
        if resolution.txt(t"fingerprint") == t"ab12" then found.offer(resolution.endpoints)

      case Discovery.Event.Lost(instance) => ()
```

- ### The responder
+ ### Le répondeur

- `discoveryBackends.mdnsSockets` is the mDNS responder over multicast [sockets](sockets.md). It
- joins `224.0.0.251:5353` and `[ff02::fb]:5353` on every interface which is up,
- multicast-capable and has an address of the group's family, with address and port reuse, so it
- runs beside the system's own responder. One responder per program is the intent, so bind the
- given once, rather than summoning it at each use. It opens its sockets at the first loan, and
- closes them after the last.
+ `discoveryBackends.mdnsSockets` est le répondeur mDNS sur des [sockets](sockets.md) multicast.
+ Il rejoint `224.0.0.251:5353` et `[ff02::fb]:5353` sur chaque interface active, capable de
+ multicast et dotée d'une adresse de la famille du groupe, avec réutilisation d'adresse et de
+ port : il fonctionne donc à côté du propre répondeur du système. L'intention est d'avoir un
+ répondeur par programme : liez donc le given une seule fois, plutôt que de l'invoquer à chaque
+ utilisation. Il ouvre ses sockets au premier prêt, et les ferme après le dernier.

- Two hosts which claim one name at once are settled as RFC 6762 prescribes. Each sees the other's
- probe. The one proposing the lexicographically lower records yields, and when it probes again a
- second later, the winner defends the name, so the loser is renamed. A host which turns up later,
- asserting a name which this responder has established, makes it probe for the name afresh, and
- rename if the probe is answered. That is why `Advertisement.instance` is read live, and not
- copied.
+ Deux hôtes qui revendiquent un même nom au même moment sont départagés comme le prescrit la
+ RFC 6762. Chacun voit la sonde de l'autre. Celui qui propose les enregistrements
+ lexicographiquement inférieurs cède, et lorsqu'il sonde de nouveau une seconde plus tard, le
+ gagnant défend le nom : le perdant est donc renommé. Un hôte qui apparaît plus tard, en
+ revendiquant un nom que ce répondeur a établi, le fait sonder le nom à nouveau, et le renommer
+ si la sonde reçoit une réponse. C'est pourquoi `Advertisement.instance` est lu en direct, et non
+ copié.

- What the responder does for a loan, which is probing, yielding, a conflict, claiming,
- withdrawing, browsing and resolving, is logged as `Discovery.Activity` events through the
- `Loggable` in scope at the loan. That is silent unless a [log sink](logging.md) is. Its
- background work between loans is not logged.
+ Ce que fait le répondeur pour un prêt, c'est-à-dire sonder, céder, un conflit, revendiquer, se
+ retirer, rechercher et résoudre, est journalisé sous forme d'événements `Discovery.Activity` par
+ le `Loggable` dans la portée au moment du prêt. C'est silencieux à moins qu'un
+ [puits de journalisation](logging.md) ne soit présent. Son travail en arrière-plan entre les
+ prêts n'est pas journalisé.

- For a program under test, `Mdns.Transport.Bus` is an in-memory link. Responders joined to one
- bus hear one another and nothing else, so the protocol, with its probing, renaming and goodbyes,
- runs deterministically without a network.
+ Pour un programme en test, `Mdns.Transport.Bus` est un lien en mémoire. Des répondeurs reliés à
+ un même bus s'entendent les uns les autres et rien d'autre : le protocole, avec ses sondages,
+ ses renommages et ses adieux, s'exécute donc de façon déterministe sans réseau.

- ### Errors
+ ### Erreurs

- Validation, a name which cannot be claimed, a resolution which times out, and a responder which
- cannot start are each a `Discovery.Error`, whose reason says which. The responder's own failure
- to join the group is an `Mdns.Error`.
+ La validation, un nom qui ne peut pas être revendiqué, une résolution qui dépasse son délai et un
+ répondeur qui ne peut pas démarrer sont chacun une `Discovery.Error`, dont la raison dit
+ laquelle. L'échec du répondeur lui-même à rejoindre le groupe est une `Mdns.Error`.

- ### Related
+ ### Voir aussi

- - [Sockets](sockets.md) describes the multicast sockets which the responder uses.
- - [Network addresses](network-addresses.md) describes the ports and addresses which a
-   resolution gives.
- - [Daemons](daemons.md) describes the resident processes which are often advertised.
- - [Concurrency](concurrency.md) describes `supervise`, `Promise` and the `Monitor` which
-   unwinds a loan.
- - [Logging](logging.md) receives the `Discovery.Activity` events.
+ - [Sockets](sockets.md) décrit les sockets multicast qu'utilise le répondeur.
+ - [Adresses réseau](network-addresses.md) décrit les ports et les adresses que donne une
+   résolution.
+ - [Démons](daemons.md) décrit les processus résidents qui sont souvent annoncés.
+ - [Concurrence](concurrency.md) décrit `supervise`, `Promise` et le `Monitor` qui défait un
+   prêt.
+ - [Journalisation](logging.md) reçoit les événements `Discovery.Activity`.
