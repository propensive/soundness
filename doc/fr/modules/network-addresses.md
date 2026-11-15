- ## Network Addresses
+ ## Adresses réseau

- ### About
+ ### Présentation

- The identifiers of a network each have a type of their own:
- [URLs](https://en.wikipedia.org/wiki/URL),
- [hostnames](https://en.wikipedia.org/wiki/Hostname),
- [IP addresses](https://en.wikipedia.org/wiki/IP_address),
- [email addresses](https://en.wikipedia.org/wiki/Email_address), ports and
- [MAC addresses](https://en.wikipedia.org/wiki/MAC_address).
+ Les identifiants d'un réseau ont chacun un type qui leur est propre :
+ [URL](https://en.wikipedia.org/wiki/URL),
+ [noms d'hôte](https://en.wikipedia.org/wiki/Hostname),
+ [adresses IP](https://en.wikipedia.org/wiki/IP_address),
+ [adresses e-mail](https://en.wikipedia.org/wiki/Email_address), ports et
+ [adresses MAC](https://en.wikipedia.org/wiki/MAC_address).

- A literal is validated at [compiletime], so a malformed one is a compile error where it is
- written. Text is parsed into the same types at [runtime], and a typed error is reported when
- it does not conform.
+ Un littéral est validé [à la compilation][compilation] ; un littéral mal formé est donc une
+ erreur de compilation à l'endroit où il est écrit. Le texte est analysé dans les mêmes types
+ [à l'exécution][exécution], et une erreur typée est signalée lorsqu'il n'est pas conforme.

- ### On network identifiers
+ ### À propos des identifiants réseau

- In most code, a URL, an IP address and an email address are all just strings. The
- distinctions between them exist only in the programmer's head. A value which is not really a
- well-formed address travels through the program until something tries to use it, and fails.
- The validation is done late, if at all, and far from where the bad value came in.
+ Dans la plupart des programmes, une URL, une adresse IP et une adresse e-mail ne sont que
+ des chaînes. Les distinctions entre elles n'existent que dans la tête du programmeur. Une
+ valeur qui n'est pas réellement une adresse bien formée circule dans le programme jusqu'à ce
+ que quelque chose tente de l'utiliser, et échoue. La validation se fait tard, si elle se
+ fait, et loin de l'endroit où la mauvaise valeur est entrée.

- Here each identifier has a type of its own, and is validated at the earliest possible
- moment: a literal as it compiles, and text at the instant it is decoded. A `Hostname`, an
- `EmailAddress` and a `Port` are known to be well-formed. Their parts are typed too: a URL's
- port is a number, and its host is a hostname or an IP address. An address which cannot be
- constructed invalid is [safety by construction](../../philosophy/safety-by-construction.md) in
- its simplest form.
+ Ici, chaque identifiant a son propre type, et est validé au plus tôt : un littéral pendant
+ sa compilation, et un texte à l'instant où il est décodé. Un `Hostname`, une
+ `EmailAddress` et un `Port` sont connus pour être bien formés. Leurs parties sont aussi
+ typées : le port d'une URL est un nombre, et son hôte est un nom d'hôte ou une adresse IP.
+ Une adresse qui ne peut pas être construite invalide est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) sous sa forme la plus
+ simple.

- Everything comes from the `soundness` package. The samples throw any error as an exception,
- and the last import lets them print their results:
+ Tout provient du package `soundness`. Les exemples lèvent toute erreur sous forme
+ d'exception, et le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### URLs
+ ### URL

- The `url"…"` [interpolator] writes a URL, and checks it at compiletime:
+ L'[interpolateur] `url"…"` écrit une URL, et la vérifie à la compilation :

```scala
val address = url"https://example.com:8080/path?query=1#top"
```

- A URL's parts are typed members: its scheme, host, port, path, query and fragment. A
- substitution puts a value in the right place. A number becomes the port, and a text value is
- URL-encoded into the path.
+ Les parties d'une URL sont des membres typés : son schéma, son hôte, son port, son chemin, sa
+ requête et son fragment. Une substitution place une valeur au bon endroit. Un nombre devient
+ le port, et une valeur texte est encodée en URL dans le chemin.

- A malformed URL does not compile:
+ Une URL mal formée ne compile pas :

```scala fuel
compile
errors show
##
url"https://example.com:port/"
```

- Text is decoded to an `HttpUrl` at runtime, with `as`:
+ Un texte est décodé en `HttpUrl` à l'exécution, avec `as` :

```scala fuel
run
##
Out.println(t"https://example.com/".as[HttpUrl].show)
```

- The [HTTP client](http-client.md) sends requests to URLs, and [Paths](paths.md) describes
- the hierarchy which a URL's path shares with filesystem paths.
+ Le [client HTTP](http-client.md) envoie des requêtes vers des URL, et [Chemins](paths.md)
+ décrit la hiérarchie que le chemin d'une URL partage avec les chemins du système de
+ fichiers.

- ### Hostnames and IP addresses
+ ### Noms d'hôte et adresses IP

- A hostname is written with `host"…"`, and an IPv4 or IPv6 address with `ip"…"`. Each is
- validated as it compiles:
+ Un nom d'hôte s'écrit avec `host"…"`, et une adresse IPv4 ou IPv6 avec `ip"…"`. Chacun est
+ validé pendant sa compilation :

```scala
val host = host"www.example.com"
val ipv4 = ip"192.168.0.1"
val ipv6 = ip"2001:db8::1"
```

- An address which is not valid does not compile:
+ Une adresse invalide ne compile pas :

```scala fuel
compile
errors show
##
ip"192.168.0.0.0.1"
```

- The `ip"…"` interpolator serves both families of address. It takes its type, `Ipv4` or
- `Ipv6`, from what it is given, and the checks are the full ones for each. The prefix of a
- subnet is checked against its family's range too, so an impossible mask is caught with the
- address:
+ L'interpolateur `ip"…"` sert les deux familles d'adresses. Il tire son type, `Ipv4` ou
+ `Ipv6`, de ce qu'on lui donne, et les vérifications sont complètes pour chacune. Le préfixe
+ d'un sous-réseau est lui aussi vérifié par rapport à la plage de sa famille ; un masque
+ impossible est donc détecté avec l'adresse :

```tabular
- column  Rule
- column  IPv4
- column  IPv6
- row  groups  exactly four, separated by dots  at most eight, separated by colons
- row  each group  a number in the range 0–255  one to four hexadecimal characters
- row  abbreviation  none; nothing but digits and dots  at most one `::`
- row  subnet prefix  0–32  0–128
+ column  Règle
+ column  IPv4
+ column  IPv6
+ row  groupes  exactement quatre, séparés par des points  au plus huit, séparés par des deux-points
+ row  chaque groupe  un nombre compris entre 0 et 255  un à quatre caractères hexadécimaux
+ row  abréviation  aucune ; rien d'autre que des chiffres et des points  au plus un `::`
+ row  préfixe de sous-réseau  0–32  0–128
```

- ### Subnets
+ ### Sous-réseaux

- An IP address gives a subnet, which is written out in the usual slash notation, and
- `subnet"…"` writes one directly, with its prefix length, checked at compiletime. The host bits
- below the prefix are masked away, so a subnet which is written carelessly is corrected, not
- misinterpreted:
+ Une adresse IP donne un sous-réseau, qui s'écrit dans la notation habituelle avec barre
+ oblique, et `subnet"…"` en écrit un directement, avec la longueur de son préfixe, vérifiée à
+ la compilation. Les bits d'hôte sous le préfixe sont masqués ; un sous-réseau écrit sans
+ soin est donc corrigé, et non mal interprété :

```scala fuel
run
##
Out.println(ip"255.123.143.0".subnet(12).show)
Out.println(subnet"255.123.143.0/12".show)
Out.println(subnet"192.168.0.0/24".show)
Out.println(subnet"2001:db8::/32".show)
```

- A subnet answers whether an address falls within it, which is what an access rule or a
- routing decision needs. It is a value, not a pair of strings to compare by hand.
+ Un sous-réseau indique si une adresse en fait partie, ce dont ont besoin une règle d'accès ou
+ une décision de routage. C'est une valeur, et non une paire de chaînes à comparer à la main.

- ### Email addresses
+ ### Adresses e-mail

- An email address is written with `email"…"`, and is parsed from text with `as`. Both validate
- it against the rules for a well-formed address:
+ Une adresse e-mail s'écrit avec `email"…"`, et s'analyse à partir de texte avec `as`. Les
+ deux la valident selon les règles d'une adresse bien formée :

```scala
val sender = email"test@example.com"
```

```scala fuel
run
##
Out.println(t"simple@example.com".as[EmailAddress].show)
```

```scala fuel
run
errors show
##
t"not an address".as[EmailAddress]
```

- [Email](email.md) sends messages to such addresses.
+ [E-mail](email.md) envoie des messages à de telles adresses.

- ### Ports
+ ### Ports

- A port is written with `tcp"…"` or `udp"…"`. Its type records the transport and, for a
- literal, the number. A port may be named by its
- [IANA service name](https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml)
- instead of its number, and the name is resolved at compiletime:
+ Un port s'écrit avec `tcp"…"` ou `udp"…"`. Son type enregistre le transport et, pour un littéral,
+ le numéro. Un port peut être désigné par son [nom de service
+ IANA](https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml)
+ au lieu de son numéro, et le nom est résolu à la compilation :

```scala fuel
run
##
Out.println(tcp"smtp".inspect)
Out.println(tcp"443".inspect)
```

- The names are checked against the registry of services, including which transport each is
- registered for. Asking for a service over the wrong transport does not compile:
+ Les noms sont vérifiés par rapport au registre des services, y compris le transport pour
+ lequel chacun est enregistré. Demander un service sur le mauvais transport ne compile pas :

```scala fuel
compile
errors show
##
udp"docker"
```

- `Port[Tcp]()`, with no number, asks the operating system for a port which is not in use. That
- is what a test server needs, or a service which binds a port dynamically. The port which it
- returns is one which can actually be bound, not a guess which may race with another process.
- [Sockets](sockets.md) binds and connects to ports.
+ `Port[Tcp]()`, sans numéro, demande au système d'exploitation un port inutilisé. C'est ce
+ dont a besoin un serveur de test, ou un service qui lie un port dynamiquement. Le port qu'il
+ renvoie peut réellement être lié, et n'est pas une supposition qui pourrait entrer en
+ concurrence avec un autre processus. [Sockets](sockets.md) lie des ports et s'y connecte.

- ### Interfaces
+ ### Interfaces

- The machine's own network interfaces are listed as typed values. Each reports whether it is a
- loopback, and each can be addressed by name:
+ Les interfaces réseau de la machine elle-même sont listées sous forme de valeurs typées.
+ Chacune indique s'il s'agit d'une interface de bouclage, et chacune peut être désignée par
+ son nom :

```scala fuel
typecheck
##
val interfaces = NetworkInterface.all()
val loopback: Boolean = NetworkInterface.all().exists(_.loopback)
```

- ### MAC addresses
+ ### Adresses MAC

- A MAC address is written with `mac"…"`, or decoded from text. It is validated as six
- hexadecimal groups:
+ Une adresse MAC s'écrit avec `mac"…"`, ou se décode à partir de texte. Elle est validée sous
+ forme de six groupes hexadécimaux :

```scala
val card = mac"01-23-45-ab-cd-ef"
```

```scala fuel
compile
errors show
##
mac"01-23-45-ab-cd"
```

- ### DNS names and messages
+ ### Noms et messages DNS

- The DNS has names of its own. Any label may hold any UTF-8, including dots and spaces, which
- is what the instance names of DNS-SD services, such as `Hamlet's Printer._ipp._tcp.local`, rely
- on. A `Dns.Name` is written with `dns"…"`, and checked at compiletime. Equality ignores the
- case of ASCII letters, as the DNS itself compares names, while the labels keep their
- spelling:
+ Le DNS a ses propres noms. Toute étiquette peut contenir n'importe quel UTF-8, y compris des
+ points et des espaces, ce sur quoi reposent les noms d'instance des services DNS-SD, comme
+ `Hamlet's Printer._ipp._tcp.local`. Un `Dns.Name` s'écrit avec `dns"…"`, et est vérifié à la
+ compilation. L'égalité ignore la casse des lettres ASCII, comme le DNS lui-même compare les
+ noms, tandis que les étiquettes conservent leur graphie :

```scala fuel
run
##
Out.println((dns"Example.COM" == dns"example.com").inspect)
Out.println(dns"_fury._tcp.local".labels.inspect)
```

- The messages of RFC 1035 are questions, and records which hold typed data over the addresses
- above. They are encoded to `Data` and decoded from it with the usual `in` and `as`. Names are
- compressed on the way out, and strictly validated on the way in. A record's data is one of the
- recognized types, or an `Unknown` which holds its bytes:
+ Les messages de la RFC 1035 sont des questions, et des enregistrements qui contiennent des
+ données typées sur les adresses ci-dessus. Ils sont encodés en `Data` et décodés dans
+ l'autre sens avec les habituels `in` et `as`. Les noms sont compressés à la sortie, et
+ rigoureusement validés à l'entrée. Les données d'un enregistrement sont de l'un des types
+ reconnus, ou un `Unknown` qui contient ses octets :

```scala
val record = Dns.Record(dns"example.com", 3600, Dns.Rdata.A(ip"93.184.216.34"))
val query = Dns.Message.query(1, List(Dns.Question(dns"example.com", Dns.Type.A))).in[Data]
```

- mDNS adds two bits: a question which accepts a unicast response, and a record which flushes a
- receiver's cache. They are plain fields, `unicast` and `flush`, which default to off.
- `Dns.resolve` reaches the platform's resolver, and
- [Service discovery](service-discovery.md) builds on these messages.
+ mDNS ajoute deux bits : une question qui accepte une réponse unicast, et un enregistrement
+ qui vide le cache d'un receveur. Ce sont de simples champs, `unicast` et `flush`, désactivés
+ par défaut. `Dns.resolve` atteint le résolveur de la plateforme, et la
+ [Découverte de services](service-discovery.md) s'appuie sur ces messages.

- ### Parsing at runtime
+ ### Analyse à l'exécution

- Every identifier which has a literal form is also decoded from text with
- [`as`](decoding.md#decoding-a-value), naming the target type. A value which does not conform
- raises a typed error, which names precisely what was wrong. A program which validates a user's
- input can therefore report the fault, and need not merely reject the value:
+ Chaque identifiant qui a une forme littérale se décode aussi à partir de texte avec
+ [`as`](decoding.md#decoding-a-value), en nommant le type cible. Une valeur non conforme lève
+ une erreur typée, qui indique précisément ce qui n'allait pas. Un programme qui valide la
+ saisie d'un utilisateur peut donc signaler la faute, et pas seulement rejeter la valeur :

```tabular
- column  Identifier
- column  Literal
- column  Type
- column  Error
- row  URL  `url"…"`  `Url`, or `HttpUrl` for HTTP  `Url.Error`
- row  hostname  `host"…"`  `Hostname`  `Hostname.Error`
- row  IP address  `ip"…"`  `Ipv4` or `Ipv6`  `IpAddress.Error`
- row  subnet  `subnet"…"`  `Ipv4Subnet` or `Ipv6Subnet`  `IpAddress.Error`
- row  email address  `email"…"`  `EmailAddress`  `EmailAddress.Error`
- row  port  `tcp"…"`, `udp"…"`  `Port`  `Port.Error`
- row  MAC address  `mac"…"`  `MacAddress`  `MacAddress.Error`
- row  DNS name  `dns"…"`  `Dns.Name`  `Dns.Error`
+ column  Identifiant
+ column  Littéral
+ column  Type
+ column  Erreur
+ row  URL  `url"…"`  `Url`, ou `HttpUrl` pour HTTP  `Url.Error`
+ row  nom d'hôte  `host"…"`  `Hostname`  `Hostname.Error`
+ row  adresse IP  `ip"…"`  `Ipv4` ou `Ipv6`  `IpAddress.Error`
+ row  sous-réseau  `subnet"…"`  `Ipv4Subnet` ou `Ipv6Subnet`  `IpAddress.Error`
+ row  adresse e-mail  `email"…"`  `EmailAddress`  `EmailAddress.Error`
+ row  port  `tcp"…"`, `udp"…"`  `Port`  `Port.Error`
+ row  adresse MAC  `mac"…"`  `MacAddress`  `MacAddress.Error`
+ row  nom DNS  `dns"…"`  `Dns.Name`  `Dns.Error`
```

```scala fuel
run
errors show
##
t"256.1.1.1".as[Ipv4]
```

- A field of one of these types in a case class is decoded in the same way, so a `Hostname` in a
- configuration file is validated as the [JSON](json.md) or [YAML](yaml.md) is read.
+ Un champ de l'un de ces types dans une case class se décode de la même manière ; un
+ `Hostname` dans un fichier de configuration est donc validé au moment où le [JSON](json.md)
+ ou le [YAML](yaml.md) est lu.

- ### Related
+ ### Voir aussi

- - [Forms](forms.md), [geography](geography.md), [HTML](html.md) and [interpolation](interpolation.md) check and carry URLs and addresses.
+ - [Formulaires](forms.md), [géographie](geography.md), [HTML](html.md) et
+   [interpolation](interpolation.md) vérifient et transportent des URL et des adresses.
