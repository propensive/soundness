- ## Cryptography
+ ## Cryptographie

- ### About
+ ### Présentation

- Encryption, signing and message authentication are typed operations. Their *choices* are
- types too: the algorithm, the size of the key, the mode of a block cipher and its padding are
- all checked where the code is written. Encrypting with AES-256, in CBC mode, with PKCS7
- padding, names all four decisions. A combination which the algorithms do not permit, such as a
- stream mode with a padding, does not compile. The weak algorithms, which are DES, small RSA
- keys and unauthenticated modes, need an explicit permission in scope before they can be used
- at all.
+ Le chiffrement, la signature et l'authentification de messages sont des opérations typées.
+ Leurs *choix* sont aussi des types : l'algorithme, la taille de la clé, le mode d'un
+ chiffrement par bloc et son remplissage sont tous vérifiés là où le code est écrit. Chiffrer
+ en AES-256, en mode CBC, avec un remplissage PKCS7, nomme les quatre décisions. Une
+ combinaison que les algorithmes ne permettent pas, comme un mode de flux avec un remplissage,
+ ne compile pas. Les algorithmes faibles, à savoir DES, les petites clés RSA et les modes non
+ authentifiés, exigent une autorisation explicite dans la portée avant de pouvoir être
+ utilisés.

- A key is typed by its cipher. The bytes of a private key can be reached only inside an
- `uncloak` block, so the places where secret material is used are visible in the structure of
- the code. Where that material is *stored*, while it is not in use, is an explicit choice as
- well.
+ Une clé est typée par son chiffrement. Les octets d'une clé privée ne sont accessibles qu'à
+ l'intérieur d'un bloc `uncloak` ; les endroits où des données secrètes sont utilisées sont
+ donc visibles dans la structure du code. L'endroit où ces données sont *stockées*, lorsqu'elles
+ ne sont pas utilisées, est lui aussi un choix explicite.

- ### On cryptography
+ ### À propos de la cryptographie

- Cryptographic APIs traditionally take their parameters as strings, as in
- `Cipher.getInstance("AES/CBC/PKCS5Padding")`. A misspelled algorithm, or an unsupported
- combination, is then discovered only at [runtime]. Worse, such an API is neutral about
- strength. DES and AES are equally easy to reach, and nothing marks the code which quietly uses
- a broken primitive.
+ Les API cryptographiques prennent traditionnellement leurs paramètres sous forme de chaînes,
+ comme dans `Cipher.getInstance("AES/CBC/PKCS5Padding")`. Un algorithme mal orthographié, ou
+ une combinaison non prise en charge, n'est alors découvert qu'[à l'exécution][exécution].
+ Pire, une telle API est neutre quant à la robustesse. DES et AES sont aussi faciles à
+ atteindre l'un que l'autre, et rien ne signale le code qui utilise discrètement une primitive
+ cassée.

- Here the choices are typed, and the dangerous ones are gated:
+ Ici, les choix sont typés, et les choix dangereux sont soumis à autorisation :

- - Modes and paddings are types, and only their legal combinations compile.
- - A weak algorithm demands a *permit*, so a codebase's acceptance of legacy cryptography is
-   one import, which can be searched for.
- - Secret material is a [capability], lent to a block.
+ - Les modes et les remplissages sont des types, et seules leurs combinaisons légales
+   compilent.
+ - Un algorithme faible exige un *permis* ; l'acceptation de la cryptographie héritée par une
+   base de code se résume donc à un import, que l'on peut rechercher.
+ - Les données secrètes sont une [capacité], prêtée à un bloc.

- Everything comes from the `soundness` package. A provider of algorithms is in scope, and the
- last import lets the samples print their results:
+ Tout provient du package `soundness`. Un fournisseur d'algorithmes est dans la portée, et le
+ dernier import permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import soundness.textDecodable

import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import cloaks.heapCloak
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import strategies.throwUnsafely
import alphabets.hexLowerCase
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Where secrets are kept
+ ### Où sont gardés les secrets

- There is deliberately no default `Cloak`. Constructing any secret value, whether a password, a
- symmetric key or a private key, needs one in scope, so the decision about storage is always
- written down. The package `cloaks` offers four. They differ in where the material rests
- between uses, and in whether it rests encrypted:
+ Il n'y a délibérément pas de `Cloak` par défaut. Construire une valeur secrète quelconque,
+ qu'il s'agisse d'un mot de passe, d'une clé symétrique ou d'une clé privée, en exige un dans
+ la portée ; la décision relative au stockage est donc toujours écrite. Le package `cloaks` en
+ propose quatre. Ils diffèrent par l'endroit où les données reposent entre deux utilisations,
+ et par le fait qu'elles y reposent chiffrées ou non :

```tabular
- column  Cloak
- column  At rest, the secret is
- column  Ephemeral key
- row  `heapCloak`  cleartext, in a private byte array on the heap
- row  `offHeapCloak`  cleartext, outside the Java heap
- row  `veiledHeapCloak`  ciphertext, on the heap  off the heap
- row  `veiledOffHeapCloak`  ciphertext, outside the Java heap  on the heap
+ column  Cloak
+ column  Au repos, le secret est
+ column  Clé éphémère
+ row  `heapCloak`  en clair, dans un tableau d'octets privé sur le tas
+ row  `offHeapCloak`  en clair, hors du tas Java
+ row  `veiledHeapCloak`  chiffré, sur le tas  hors du tas
+ row  `veiledOffHeapCloak`  chiffré, hors du tas Java  sur le tas
```

- Only `heapCloak` is available on Scala.js and Scala Native. No cloak defends against a
- debugger in the same process. What the stronger ones shrink is the window during which
- cleartext can be reached from a heap dump or a core file.
+ Seul `heapCloak` est disponible sur Scala.js et Scala Native. Aucun cloak ne protège contre
+ un débogueur dans le même processus. Ce que les plus robustes réduisent, c'est la fenêtre
+ pendant laquelle le texte en clair est accessible depuis un vidage du tas ou un fichier core.

- ### Symmetric encryption
+ ### Chiffrement symétrique

- A symmetric key is generated for its cipher. The algorithm and the size of the key are in the
- type, and so, optionally, are the mode and the padding. Encryption happens inside the key's
- `uncloak` block, and the
- [initialization vector](https://en.wikipedia.org/wiki/Initialization_vector) is supplied
- explicitly:
+ Une clé symétrique est générée pour son chiffrement. L'algorithme et la taille de la clé
+ figurent dans le type, ainsi que, facultativement, le mode et le remplissage. Le chiffrement
+ a lieu dans le bloc `uncloak` de la clé, et le
+ [vecteur d'initialisation](https://en.wikipedia.org/wiki/Initialization_vector) est fourni
+ explicitement :

```scala
import blockCipherModes.cbc
import blockCipherPaddings.pkcs7
import cryptoPermits.permitUnauthenticatedCrypto

val key = SymmetricKey.generate[Aes[256]]()

val decrypted = key.uncloak:
  t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
```

```scala fuel
run
##
Out.println(decrypted)
```

- `uncloak` is a [delimited scope](../../philosophy/delimited-scopes.md). It lends the key to its
- block, as an `Encryptor` and a `Decryptor` capability, and capture checking confines both.
- Neither the capability, nor a closure which would use it later, can escape the block. The
- compiler, not a convention, keeps the use of the key inside its scope. Outside the block there
- is nothing to encrypt with:
+ `uncloak` est une [portée délimitée](../../philosophy/delimited-scopes.md). Il prête la clé à
+ son bloc, sous forme de capacités `Encryptor` et `Decryptor`, et le contrôle des captures les
+ confine toutes deux. Ni la capacité, ni une fermeture qui l'utiliserait plus tard, ne peuvent
+ s'échapper du bloc. C'est le compilateur, et non une convention, qui maintient l'utilisation
+ de la clé dans sa portée. Hors du bloc, il n'y a rien avec quoi chiffrer :

```scala fuel
compile
errors show
##
t"Hello world".encrypt(InitializationVector.random)
```

- A [stream](streams.md) is encrypted a chunk at a time, so large data is never assembled in
- memory. A stream which was encrypted in pieces decrypts as a whole value, and the reverse:
+ Un [flux](streams.md) est chiffré morceau par morceau ; de grandes données ne sont donc jamais
+ assemblées en mémoire. Un flux chiffré par morceaux se déchiffre comme une valeur entière, et
+ inversement :

```scala
val streamed = key.uncloak:
  t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize.decrypt.as[Text]
```

- Decrypting with the wrong key raises a `Crypto.Error` which names the failure. The mode and
- padding of AES may also be fixed in the key's type, as in
- `Aes[256] over BlockCipher.Cbc against BlockCipher.Pkcs7`, and a pairing which the
- specification forbids does not compile.
+ Déchiffrer avec la mauvaise clé lève une `Crypto.Error` qui nomme l'échec. Le mode et le
+ remplissage d'AES peuvent aussi être fixés dans le type de la clé, comme dans
+ `Aes[256] over BlockCipher.Cbc against BlockCipher.Pkcs7`, et une association que la
+ spécification interdit ne compile pas.

- ### Passwords
+ ### Mots de passe

- A `Password` is a cloaked secret, like a key, but with no cipher attached. Its cleartext can
- be reached only inside `uncloak`, as a `Cleartext` capability, and its `show` form never
- reveals it:
+ Un `Password` est un secret masqué, comme une clé, mais sans chiffrement associé. Son texte
+ en clair n'est accessible qu'à l'intérieur d'`uncloak`, sous forme de capacité `Cleartext`,
+ et sa forme `show` ne le révèle jamais :

```scala
val password = Password(t"hunter2")
```

```scala fuel
run
##
Out.println(password.show)
Out.println(password.uncloak(String(cleartext.chars).tt))
```

- ### Public-key encryption
+ ### Chiffrement à clé publique

- An RSA key pair encrypts toward the public key, and decrypts with the private key:
+ Une paire de clés RSA chiffre vers la clé publique, et déchiffre avec la clé privée :

```scala
val privateKey = PrivateKey.generate[Rsa[2048]]()

val message = privateKey.public.uncloak:
  t"secret".encrypt(InitializationVector.random)
```

```scala fuel
run
##
Out.println(privateKey.uncloak(message.decrypt.as[Text]))
```

- ### Signing
+ ### Signature

- Signing runs the other way. The private key signs, and anyone who holds the public key
- verifies. RSA, DSA, ECDSA and ML-DSA all sign in the same way. ML-DSA is the lattice-based
- scheme standardized as FIPS 204, for a signature which resists quantum computers. The digest
- which an RSA or ECDSA signature is computed over is chosen from the package
- `signatureDigests`:
+ La signature fonctionne dans l'autre sens. La clé privée signe, et quiconque détient la clé
+ publique vérifie. RSA, DSA, ECDSA et ML-DSA signent tous de la même façon. ML-DSA est le
+ schéma à base de réseaux euclidiens normalisé sous le nom FIPS 204, pour une signature qui
+ résiste aux ordinateurs quantiques. Le condensé sur lequel est calculée une signature RSA ou
+ ECDSA se choisit dans le package `signatureDigests` :

```scala
import signatureDigests.sha256Signature

val document = t"an important document"

val signer = PrivateKey.generate[Ecdsa[256]]()
val signature = signer.sign(document)
```

```scala fuel
run
##
Out.println(signer.public.verify(document, signature).inspect)
Out.println(signer.public.verify(t"a forged document", signature).inspect)
```

- ### HMAC
+ ### HMAC

- `hmac` authenticates a message under a shared secret, over any of the
- [hash algorithms](hashing.md#hashing-a-value). The result is written through the usual
- [base encodings](base-encoding.md):
+ `hmac` authentifie un message au moyen d'un secret partagé, avec n'importe lequel des
+ [algorithmes de hachage](hashing.md#hashing-a-value). Le résultat s'écrit au moyen des
+ [encodages en base](base-encoding.md) habituels :

```scala
val secret = t"shared secret".in[Data]
```

```scala fuel
run
##
Out.println(document.hmac[Sha2[256]](secret).serialize[Hex])
```

- ### Key agreement and authenticated encryption
+ ### Accord de clés et chiffrement authentifié

- Two parties who hold each other's public keys can agree on a shared secret, with X25519 or
- ECDH. The secret is lent to a block, and zeroed afterwards, since a protocol only ever hashes
- it into session keys:
+ Deux parties qui détiennent chacune la clé publique de l'autre peuvent s'accorder sur un
+ secret partagé, avec X25519 ou ECDH. Le secret est prêté à un bloc, puis mis à zéro, puisqu'un
+ protocole ne fait jamais que le hacher pour en tirer des clés de session :

```scala
val romeo = PrivateKey.generate[X25519]()
val juliet = PrivateKey.generate[X25519]()
```

```scala fuel
run
##
Out.println(romeo.agree(juliet.public) { secret => secret.length }.show)
```

- What the key agreement produces is then used with an authenticated cipher. AES-GCM seals a
- message under a *nonce*, which must never repeat under one key, together with associated data
- which is authenticated but not encrypted. It refuses to open anything which has been tampered
- with:
+ Ce que produit l'accord de clés est ensuite utilisé avec un chiffrement authentifié. AES-GCM
+ scelle un message sous un *nonce*, qui ne doit jamais se répéter pour une même clé,
+ accompagné de données associées authentifiées mais non chiffrées. Il refuse d'ouvrir tout ce
+ qui a été altéré :

```scala
val sessionKey = SymmetricKey.generate[AesGcm[256]]()
val nonce: Data = Array.fill[Byte](12)(0)
val box = sessionKey.seal(nonce, t"header".bytestream, t"message".bytestream)
```

```scala fuel
run
##
Out.println(sessionKey.unseal(nonce, t"header".bytestream, box).read[Text])
```

```scala fuel
run
errors show
##
sessionKey.unseal(nonce, t"another header".bytestream, box).read[Text]
```

- ### PEM
+ ### PEM

- Keys travel in [PEM](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail) form. A public key
- is exported freely. Exporting a *private* key demands the marker `Divulgence`, which is one
- more place where handling secret material is deliberate. A `Pem` value has a label and its
- bytes, and serializes to the familiar armored text:
+ Les clés voyagent sous forme [PEM](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail). Une
+ clé publique s'exporte librement. Exporter une clé *privée* exige le marqueur `Divulgence`,
+ un endroit de plus où la manipulation de données secrètes est délibérée. Une valeur `Pem` a
+ une étiquette et ses octets, et se sérialise dans le texte blindé familier :

```scala
val pemText = privateKey.public.pem.serialize
val privatePem = privateKey.pem(Divulgence)
```

```scala fuel
run
##
Out.println(pemText.cut(t"\n").head)
Out.println(pemText.read[Pem].label.inspect)
```

- PEM is parsed incrementally. A source which holds many blocks, such as a certificate chain or
- a bundle of keys, gives them one at a time, lazily, and not as one parsed document:
+ Le PEM s'analyse de façon incrémentale. Une source qui contient de nombreux blocs, comme une
+ chaîne de certificats ou un ensemble de clés, les donne un par un, paresseusement, et non
+ sous forme d'un unique document analysé :

```scala
val chainText = t"$pemText\n$pemText"
```

```scala fuel
run
##
Out.println(chainText.read[Chain[Pem]].map(_.label).to[List].inspect)
```

- Text which is not part of a block, such as the `subject=…` lines which OpenSSL writes between
- certificates, is skipped. A source with no blocks at all reads as an empty sequence, not as an
- error.
+ Le texte qui ne fait pas partie d'un bloc, comme les lignes `subject=…` qu'OpenSSL écrit
+ entre les certificats, est ignoré. Une source sans aucun bloc se lit comme une séquence vide,
+ et non comme une erreur.

- ### Reading keys
+ ### Lire des clés

- A key which already exists, in `~/.ssh` or written by `openssl`, is read into a `PrivateKey`
- of the cipher named, from any of the forms a key file takes:
+ Une clé qui existe déjà, dans `~/.ssh` ou écrite par `openssl`, se lit en une `PrivateKey` du
+ chiffrement nommé, depuis n'importe laquelle des formes que prend un fichier de clé :

```tabular
- column  Form
- column  PEM label
- column  Label value
- row  PKCS#8  `PRIVATE KEY`  `Pem.Label.PrivateKey`
- row  PKCS#1  `RSA PRIVATE KEY`  `Pem.Label.RsaPrivateKey`
- row  SEC1  `EC PRIVATE KEY`  `Pem.Label.EcPrivateKey`
- row  OpenSSH's own container  `OPENSSH PRIVATE KEY`  `Pem.Label.OpensshPrivateKey`
+ column  Forme
+ column  Étiquette PEM
+ column  Valeur d'étiquette
+ row  PKCS#8  `PRIVATE KEY`  `Pem.Label.PrivateKey`
+ row  PKCS#1  `RSA PRIVATE KEY`  `Pem.Label.RsaPrivateKey`
+ row  SEC1  `EC PRIVATE KEY`  `Pem.Label.EcPrivateKey`
+ row  Le conteneur propre à OpenSSH  `OPENSSH PRIVATE KEY`  `Pem.Label.OpensshPrivateKey`
```

- A `Cloak` must be in scope, as for any secret:
+ Un `Cloak` doit être dans la portée, comme pour tout secret :

```scala fuel
substitute b3Blbn..bXBsZQE=  ...
##
val keyText = t"""-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUgAAAJB+g2KdfoNi
nQAAAAtzc2gtZWQyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUg
AAAEDTzzy05nIC9r9aKtXYNJMgY1F/g5ZiPNyksGUvj8yR6xGH6R8uL9Aww7+jnLISnQu3
kiglJKYyOcKlvCJ7v+xSAAAADHRlc3RAZXhhbXBsZQE=
-----END OPENSSH PRIVATE KEY-----"""

val sshKey = keyText.read[PrivateKey[Ed25519] in Pem]
```

```scala fuel
run
##
Out.println(sshKey.public.verify(t"message", sshKey.sign(t"message")).inspect)
```

- The type is checked against the document, and is not assumed from it. The algorithm's
- identifier, the curve and, for RSA, the size of the modulus must be those of the cipher named,
- so a 4096-bit key can never be read as a `PrivateKey[Rsa[2048]]`. A key which does not match
- is a `PrivateKey.Error`, whose reason, `Mismatch`, names what was found:
+ Le type est vérifié par rapport au document, et non présumé à partir de lui. L'identifiant de
+ l'algorithme, la courbe et, pour RSA, la taille du module doivent être ceux du chiffrement
+ nommé ; une clé de 4096 bits ne peut donc jamais être lue comme une `PrivateKey[Rsa[2048]]`.
+ Une clé qui ne correspond pas est une `PrivateKey.Error`, dont la raison, `Mismatch`, nomme
+ ce qui a été trouvé :

```scala fuel
run
errors show
##
keyText.read[PrivateKey[Rsa[2048]] in Pem]
```

- A key which is protected by a passphrase is reported as `Encrypted`. Reading one is not yet
- supported.
+ Une clé protégée par une phrase secrète est signalée comme `Encrypted`. La lecture d'une
+ telle clé n'est pas encore prise en charge.

- Public keys are read in the same way from `PUBLIC KEY` blocks. They are also read from the
- single-line form which OpenSSH keeps in `id_*.pub`, `authorized_keys` and `known_hosts`, and
- `OpenSsh.line` writes that form back:
+ Les clés publiques se lisent de la même façon depuis des blocs `PUBLIC KEY`. Elles se lisent
+ aussi depuis la forme sur une seule ligne qu'OpenSSH conserve dans `id_*.pub`,
+ `authorized_keys` et `known_hosts`, et `OpenSsh.line` réécrit cette forme :

```scala fuel
substitute AAAAC3..v+xS  ...
##
val line = t"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS test@example"
val sshPublicKey = OpenSsh.publicKey[Ed25519](line)
```

```scala fuel
run
##
Out.println((OpenSsh.line(sshPublicKey, t"test@example") == line).inspect)
```

- [SSH](ssh.md) uses these keys to authenticate a connection.
+ [SSH](ssh.md) utilise ces clés pour authentifier une connexion.

- ### ASN.1 and DER
+ ### ASN.1 et DER

- The structures beneath certificates, keys and signatures are
- [ASN.1](https://en.wikipedia.org/wiki/ASN.1) values, carried in
- [DER](https://en.wikipedia.org/wiki/X.690#DER_encoding). An `Asn1` value models that structure
- directly, as an enumeration of the universal types which certificates use, and `Der` is the
- encoded form:
+ Les structures sous-jacentes aux certificats, aux clés et aux signatures sont des valeurs
+ [ASN.1](https://en.wikipedia.org/wiki/ASN.1), transportées en
+ [DER](https://en.wikipedia.org/wiki/X.690#DER_encoding). Une valeur `Asn1` modélise
+ directement cette structure, sous forme d'une énumération des types universels qu'utilisent
+ les certificats, et `Der` est la forme encodée :

```scala
val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"hello")))

val bytes = value.in[Der]
```

```scala fuel
run
##
Out.println((bytes.as[Asn1] == value).inspect)
```

- DER is canonical, so a document has exactly one valid encoding, and the codec enforces that in
- both directions. Each departure from it raises an `Asn1.Error`, whose reason names the fault,
- and the offset of the byte at which it was found. Among them:
+ DER est canonique : un document a exactement un encodage valide, et le codec l'impose dans les
+ deux sens. Chaque écart lève une `Asn1.Error`, dont la raison nomme le défaut, ainsi que la
+ position de l'octet auquel il a été trouvé. Parmi eux :

```tabular
- column  Fault
- column  Reason
- row  an overlong length  `NonMinimalLength`
- row  a non-minimal integer  `NonMinimalInteger`
- row  an unsorted `SET`  `UnsortedSet`
- row  an indefinite length  `IndefiniteLength`
- row  trailing bytes  `Trailing`
+ column  Défaut
+ column  Raison
+ row  une longueur trop longue  `NonMinimalLength`
+ row  un entier non minimal  `NonMinimalInteger`
+ row  un `SET` non trié  `UnsortedSet`
+ row  une longueur indéfinie  `IndefiniteLength`
+ row  des octets en trop à la fin  `Trailing`
```

- Decoding never fails for want of a type. A tag which this layer does not model, such as
- `T61String`, `BMPString`, `ENUMERATED` or an implicitly tagged value, decodes to
- `Asn1.Unknown`, which holds its content bytes verbatim. Decoding and encoding again therefore
- reproduce the original bytes exactly. Verifying a signature over a certificate's
- `TBSCertificate` depends on that property: a structure can be taken apart, inspected and put
- back together without disturbing the bytes which the signature covers.
+ Le décodage n'échoue jamais faute de type. Une étiquette que cette couche ne modélise pas,
+ comme `T61String`, `BMPString`, `ENUMERATED` ou une valeur étiquetée implicitement, se
+ décode en `Asn1.Unknown`, qui contient ses octets de contenu tels quels. Décoder puis
+ réencoder reproduit donc exactement les octets d'origine. La vérification d'une signature
+ sur le `TBSCertificate` d'un certificat dépend de cette propriété : une structure peut être
+ démontée, inspectée et remontée sans perturber les octets que couvre la signature.

- ### Certificates
+ ### Certificats

- An [X.509](https://en.wikipedia.org/wiki/X.509) certificate binds a public key to a
- *distinguished name*, for a period of validity, and is signed by its issuer. A self-signed
- certificate is the kind which a development server, or a private certificate authority,
- needs. It is generated from a subject, a private key, a [period](time.md) of validity and a
- serial number, with the `dNSName` alternatives which a TLS client will check:
+ Un certificat [X.509](https://en.wikipedia.org/wiki/X.509) lie une clé publique à un *nom
+ distinctif*, pour une période de validité, et est signé par son émetteur. Un certificat
+ autosigné est le genre dont a besoin un serveur de développement, ou une autorité de
+ certification privée. Il se génère à partir d'un sujet, d'une clé privée, d'une
+ [période](time.md) de validité et d'un numéro de série, avec les noms alternatifs `dNSName`
+ que vérifiera un client TLS :

```scala
import chronometries.unixChronometry

val subject = Distinguished(commonName = t"example.com", organization = t"Example", country = t"GB")
val validity = Instant(1_700_000_000_000L) ~ Instant(1_900_000_000_000L)

val certificate =
  Certificate.selfSigned(subject, privateKey, validity, BigInt(1), alternatives = List(t"example.com"))
```

```scala fuel
run
##
Out.println(certificate.pem.serialize.cut(t"\n").head)
Out.println(certificate.verify(privateKey.public).inspect)
```

- A certificate is an `Asn1` value underneath, so its fields can be read through the same
- structure. `authority = true` marks a certificate as one which may sign others. `verify`
- checks the signature against the issuer's public key, which for a self-signed certificate is
- its own.
+ Un certificat est au fond une valeur `Asn1` ; ses champs se lisent donc au travers de la
+ même structure. `authority = true` marque un certificat comme pouvant en signer d'autres.
+ `verify` vérifie la signature par rapport à la clé publique de l'émetteur, qui, pour un
+ certificat autosigné, est la sienne.

- An ML-DSA key signs a certificate in the same way, with the algorithm identifiers of
- FIPS 204. But a TLS implementation may not be able to complete a handshake with a
- post-quantum key, and the JDK's cannot. A service whose identity is an ML-DSA key therefore
- has that key *issue* a certificate over a classical key which it can present. It presents
- that certificate together with the ML-DSA key's own self-signed certificate, and a peer which
- knows the ML-DSA key verifies the leaf against it:
+ Une clé ML-DSA signe un certificat de la même façon, avec les identifiants d'algorithme de
+ FIPS 204. Mais une implémentation de TLS peut ne pas être capable de mener à bien une poignée
+ de main avec une clé post-quantique, et celle du JDK ne le peut pas. Un service dont
+ l'identité est une clé ML-DSA fait donc *émettre* par cette clé un certificat portant sur une
+ clé classique qu'il peut présenter. Il présente ce certificat accompagné du certificat
+ autosigné propre à la clé ML-DSA, et un pair qui connaît la clé ML-DSA vérifie la feuille par
+ rapport à elle :

```scala
val swarmKey = PrivateKey.generate[MlDsa[65]]()
val tlsKey = PrivateKey.generate[Ecdsa[256]]()
val identity = Distinguished(commonName = t"swarm.example.com")

val root = Certificate.selfSigned(identity, swarmKey, validity, BigInt(1), authority = true)

val leaf =
  Certificate.issued(subject, tlsKey.public, identity, swarmKey, validity, BigInt(2), alternatives = List(t"example.com"))
```

```scala fuel
run
##
Out.println(leaf.verify(swarmKey.public).inspect)
Out.println(root.verify(swarmKey.public).inspect)
Out.println(leaf.verify(privateKey.public).inspect)
```

- An issued certificate carries an authority key identifier, which names the key that signed
- it. That is how a verifier finds the root in a chain. An
- [HTTP server](http-server.md) presents such a certificate over TLS.
+ Un certificat émis porte un identifiant de clé d'autorité, qui nomme la clé qui l'a signé.
+ C'est ainsi qu'un vérificateur trouve la racine dans une chaîne. Un
+ [serveur HTTP](http-server.md) présente un tel certificat sur TLS.

- ### Keystores
+ ### Magasins de clés

- A [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) keystore is opened for the duration of a
- scope. The password is given as an opaque `Password`, so its cleartext is reached only
- through the scoped `Cleartext` capability, and the character array which the platform needs
- is zeroed once the store is loaded. Inside the scope, `keystore` is the open store:
+ Un magasin de clés [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) est ouvert pour la durée
+ d'une portée. Le mot de passe est donné sous forme d'un `Password` opaque ; son texte en clair
+ n'est donc accessible que par la capacité délimitée `Cleartext`, et le tableau de caractères
+ dont a besoin la plateforme est mis à zéro une fois le magasin chargé. Dans la portée,
+ `keystore` est le magasin ouvert :

```scala
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val storePath = temporaryDirectory[Path on Linux] / "cryptography" / "store.p12"

def readAliases(): List[Text] =
  storePath.open[Keystore](Password(t"sesame")):
    val server = keystore.certificate(t"server")
    keystore.aliases
```

- An alias which the store does not hold gives `Unset`. A wrong password and a corrupt store
- are deliberately indistinguishable: both are `Unreadable` in the `Keystore.Error`. Telling
- them apart would tell someone who was guessing which of the two had gone wrong.
+ Un alias que le magasin ne contient pas donne `Unset`. Un mauvais mot de passe et un magasin
+ corrompu sont délibérément indiscernables : tous deux sont `Unreadable` dans la
+ `Keystore.Error`. Les distinguer révélerait à quelqu'un qui procède par devinettes laquelle
+ des deux choses a échoué.

- ### Weak algorithms
+ ### Algorithmes faibles

- Encrypting with DES, RC2, 1024-bit RSA, ECB mode, or any unauthenticated block cipher, needs a
- permission from the package `cryptoPermits` in scope. These are the same
- [permits](hashing.md#weak-algorithms) which gate MD5 and SHA-1. Accepting legacy cryptography
- is thus an explicit decision which can be audited, not a default. The permits nest, and
- `permitDisallowedCrypto` includes every other:
+ Chiffrer avec DES, RC2, RSA 1024 bits, le mode ECB, ou tout chiffrement par bloc non
+ authentifié, exige une autorisation du package `cryptoPermits` dans la portée. Ce sont les
+ mêmes [permis](hashing.md#weak-algorithms) qui soumettent MD5 et SHA-1 à autorisation.
+ Accepter la cryptographie héritée est donc une décision explicite et auditable, et non un
+ comportement par défaut. Les permis s'emboîtent, et `permitDisallowedCrypto` inclut tous les
+ autres :

```tabular
- column  Algorithm
- column  Permitted by
- row  any block cipher, which is unauthenticated  `permitUnauthenticatedCrypto`
- row  Triple-DES  `permitDeprecatedCrypto`; to decrypt or verify only, `permitLegacyCrypto`
- row  DSA  `permitDisallowedCrypto`; to verify only, `permitLegacyCrypto`
- row  DES, RC2, Blowfish  `permitDisallowedCrypto`
- row  1024-bit RSA  `permitDisallowedCrypto`
- row  ECB mode  `permitDisallowedCrypto`
+ column  Algorithme
+ column  Autorisé par
+ row  tout chiffrement par bloc, qui n'est pas authentifié  `permitUnauthenticatedCrypto`
+ row  Triple-DES  `permitDeprecatedCrypto` ; pour déchiffrer ou vérifier, `permitLegacyCrypto`
+ row  DSA  `permitDisallowedCrypto` ; pour vérifier seulement, `permitLegacyCrypto`
+ row  DES, RC2, Blowfish  `permitDisallowedCrypto`
+ row  RSA 1024 bits  `permitDisallowedCrypto`
+ row  Mode ECB  `permitDisallowedCrypto`
```

- Across the algorithms, these are the operations which each supports, and the permit it needs:
+ Pour l'ensemble des algorithmes, voici les opérations que chacun prend en charge, et le
+ permis dont il a besoin :

```tabular
- column  Algorithm
- column  Operations
- column  Permit
- row  `Aes[128 | 192 | 256]`  encrypt, decrypt  `permitUnauthenticatedCrypto`
- row  `TripleDes[112 | 168]`  encrypt, decrypt  `permitDeprecatedCrypto`
- row  `Des`, `Rc2[40 | 64 | 128]`, `Blowfish[128 | 256 | 448]`  encrypt, decrypt  `permitDisallowedCrypto`
- row  `AesGcm[128 | 256]`, `ChaCha20Poly1305`  seal, unseal
- row  `Rsa[2048 | 3072 | 4096]`  encrypt, decrypt, sign, verify
- row  `Rsa[1024]`  encrypt, decrypt, sign, verify  `permitDisallowedCrypto`
- row  `Dsa[512 | 1024 | 2048 | 3072]`  sign, verify  `permitDisallowedCrypto`
- row  `Ecdsa[256 | 384 | 521]`, `Ed25519`, `MlDsa[44 | 65 | 87]`  sign, verify
- row  `X25519`, `Ecdh[256 | 384 | 521]`  agree
+ column  Algorithme
+ column  Opérations
+ column  Permis
+ row  `Aes[128 | 192 | 256]`  chiffrer, déchiffrer  `permitUnauthenticatedCrypto`
+ row  `TripleDes[112 | 168]`  chiffrer, déchiffrer  `permitDeprecatedCrypto`
+ row  `Des`, `Rc2[40 | 64 | 128]`, `Blowfish[128 | 256 | 448]`  chiffrer, déchiffrer  `permitDisallowedCrypto`
+ row  `AesGcm[128 | 256]`, `ChaCha20Poly1305`  sceller, desceller
+ row  `Rsa[2048 | 3072 | 4096]`  chiffrer, déchiffrer, signer, vérifier
+ row  `Rsa[1024]`  chiffrer, déchiffrer, signer, vérifier  `permitDisallowedCrypto`
+ row  `Dsa[512 | 1024 | 2048 | 3072]`  signer, vérifier  `permitDisallowedCrypto`
+ row  `Ecdsa[256 | 384 | 521]`, `Ed25519`, `MlDsa[44 | 65 | 87]`  signer, vérifier
+ row  `X25519`, `Ecdh[256 | 384 | 521]`  s'accorder
```

- ### Related
+ ### Voir aussi

- - [Daemons](daemons.md), [Git](git.md), [OAuth](oauth.md), [PDF](pdf.md), [randomness](randomness.md) and [sockets](sockets.md) sign, encrypt and secure with these keys.
+ - [Démons](daemons.md), [Git](git.md), [OAuth](oauth.md), [PDF](pdf.md),
+   [aléatoire](randomness.md) et [sockets](sockets.md) signent, chiffrent et sécurisent avec ces
+   clés.
