                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package sigillate

import adversaria.*
import anticipation.*
import contingency.*
import corpuscular.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import vacuous.*

// A JSON Web Key (RFC 7517): a key as a JSON object, with the members RFC 7518 §6 defines for
// RSA (`RSA`), elliptic-curve (`EC`) and symmetric (`oct`) keys, and RFC 8037 for the Edwards
// and Montgomery curves (`OKP`). `Jwk(key)` writes a public key; `Jwk(key, Divulgence)` writes
// a private or symmetric one, the token being the same one that lets a `PrivateKey` out as PEM.
// `jwk.publicKey[cipher]`, `privateKey[cipher]` and `symmetricKey[cipher]` read it back as a
// key of a named cipher, which the key's own type and size must agree with.
//
// A JWK does not say which of a key's possible uses it is for in a way a type can see: the same
// `RSA` object is an `Rsa[2048]`, an `RsaPss[2048]` or an `RsaOaep[2048, Sha1]` key. The cipher
// asked for decides, as it does for a key file.
object Jwk:
  private def unsigned(value: BigInt): Text =
    val bytes = Array.unsafeFrozen(value.toByteArray)
    Jose.encode(if bytes.length > 1 && value.bitLength%8 == 0 then bytes.skip(1) else bytes)

  private def number(text: Text)(using Tactic[Jose.Error], Diagnostics): BigInt =
    BigInt(new java.math.BigInteger(1, Array.unsafeJvm(Jose.decode(text))))

  private def absent(name: Text)(using Tactic[Jose.Error], Diagnostics): Nothing =
    abort(Jose.Error(Jose.Error.Reason.BadKey(t"the key has no $name member")))

  object Form:
    // RSA keys, whichever padding the cipher selects.
    private def rsa[cipher <: Cipher]: cipher is Form = new Form:
      type Self = cipher

      def published(key: Data): Optional[Jwk] = Spki.rsa(key).let: (modulus, exponent) =>
        Jwk(t"RSA", modulus = unsigned(modulus), exponent = unsigned(exponent))

      def divulged(key: Data, public: Data): Optional[Jwk] = Pkcs8.rsa(key).let: numbers =>
        Jwk
          ( t"RSA",
            modulus     = unsigned(numbers.modulus),
            exponent    = unsigned(numbers.publicExponent),
            d           = unsigned(numbers.privateExponent),
            prime1      = unsigned(numbers.prime1),
            prime2      = unsigned(numbers.prime2),
            exponent1   = unsigned(numbers.exponent1),
            exponent2   = unsigned(numbers.exponent2),
            coefficient = unsigned(numbers.coefficient) )

      def publicKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
        jwk.expect(t"RSA")
        Spki.rsa(number(jwk.modulus.or(absent(t"n"))), number(jwk.exponent.or(absent(t"e"))))

      def privateKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
        jwk.expect(t"RSA")

        Pkcs8.rsa:
          Pkcs8.Rsa
            ( number(jwk.modulus.or(absent(t"n"))),
              number(jwk.exponent.or(absent(t"e"))),
              number(jwk.d.or(absent(t"d"))),
              number(jwk.prime1.or(absent(t"p"))),
              number(jwk.prime2.or(absent(t"q"))),
              number(jwk.exponent1.or(absent(t"dp"))),
              number(jwk.exponent2.or(absent(t"dq"))),
              number(jwk.coefficient.or(absent(t"qi"))) )

    given rsaForm: [bits <: 2048 | 3072 | 4096] => Rsa[bits] is Form = rsa[Rsa[bits]]
    given rsaPssForm: [bits <: 2048 | 3072 | 4096] => RsaPss[bits] is Form = rsa[RsaPss[bits]]

    given rsaOaepForm: [bits <: 2048 | 3072 | 4096, hash <: Algorithm]
    =>  RsaOaep[bits, hash] is Form =
      rsa[RsaOaep[bits, hash]]

    // Keys on a NIST prime curve: the point as its two coordinates, each padded to the
    // curve's width, and the private scalar likewise.
    private def ec[cipher <: Cipher](bits: Int): cipher is Form = new Form:
      type Self = cipher
      private val size: Int = (bits + 7)/8
      private val curve: Text = t"P-$bits"

      private def point(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
        jwk.expect(t"EC")

        if jwk.curve.or(absent(t"crv")) != curve
        then abort(Jose.Error(Jose.Error.Reason.BadKey(t"the key is not on the curve $curve")))

        val x = Jose.decode(jwk.x.or(absent(t"x")))
        val y = Jose.decode(jwk.y.or(absent(t"y")))

        if x.length != size || y.length != size
        then abort(Jose.Error(Jose.Error.Reason.BadKey(t"a coordinate is of the wrong length")))

        val prefix: Data = Array.fill[Byte](1)(4)
        Array.concat(prefix, x, y)

      private def coordinates(point: Data): Optional[Jwk] =
        if point.length != 2*size + 1 then Unset else
          Jwk
            ( t"EC",
              curve = curve,
              x     = Jose.encode(point.skip(1).keep(size)),
              y     = Jose.encode(point.skip(1 + size)) )

      def published(key: Data): Optional[Jwk] = PublicKey[cipher](key).raw.let(coordinates(_))

      // The scalar is written at the curve's full width, as RFC 7518 §6.2.2.1 requires.
      def divulged(key: Data, public: Data): Optional[Jwk] = Pkcs8.scalar(key).let: scalar =>
        val padding: Data = Array.fill[Byte]((size - scalar.length).max(0))(0)
        published(public).let(_.copy(d = Jose.encode(Array.concat(padding, scalar))))

      def publicKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
        Spki.ec(bits, point(jwk))

      def privateKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
        Pkcs8.ec(bits, Jose.decode(jwk.d.or(absent(t"d"))), point(jwk))

    given ecdsaForm: [bits <: 256 | 384 | 521: ValueOf] => Ecdsa[bits] is Form =
      ec[Ecdsa[bits]](valueOf[bits])

    given ecdhForm: [bits <: 256 | 384 | 521: ValueOf] => Ecdh[bits] is Form =
      ec[Ecdh[bits]](valueOf[bits])

    // Keys on RFC 8410's curves: the public key and the private seed, each 32 bytes.
    private def okp[cipher <: Cipher](curve: Text, identifier: List[Int]): cipher is Form =
      new Form:
        type Self = cipher

        private def x(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
          jwk.expect(t"OKP")

          if jwk.curve.or(absent(t"crv")) != curve
          then abort(Jose.Error(Jose.Error.Reason.BadKey(t"the key is not on the curve $curve")))

          Jose.decode(jwk.x.or(absent(t"x")))

        def published(key: Data): Optional[Jwk] = PublicKey[cipher](key).raw.let: bits =>
          Jwk(t"OKP", curve = curve, x = Jose.encode(bits))

        def divulged(key: Data, public: Data): Optional[Jwk] = Pkcs8.seed(key).let: seed =>
          published(public).let(_.copy(d = Jose.encode(seed)))

        def publicKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
          Spki.wrap(Asn1.Sequence(List(Asn1.ObjectId(identifier))), x(jwk))

        def privateKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data =
          Pkcs8.seeded(identifier, Jose.decode(jwk.d.or(absent(t"d"))), x(jwk))

    given ed25519Form: Ed25519 is Form = okp[Ed25519](t"Ed25519", KeyAlgorithm.Ed25519)
    given x25519Form: X25519 is Form = okp[X25519](t"X25519", KeyAlgorithm.X25519)

  // How the keys of a cipher are written as a JWK: between the provider's documents — a
  // `SubjectPublicKeyInfo` and a PKCS#8 `PrivateKeyInfo` — and the JWK's members. Reading is
  // partial in the document (`Unset` for one that is not this kind of key) and raises for a
  // JWK of the wrong type or with members missing.
  trait Form extends Typeclass:
    type Self <: Cipher
    def published(key: Data): Optional[Jwk]
    def divulged(key: Data, public: Data): Optional[Jwk]
    def publicKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data
    def privateKey(jwk: Jwk)(using Tactic[Jose.Error], Diagnostics): Data

  // A public key as a JWK.
  def apply[cipher <: Cipher](key: PublicKey[cipher])(using form: cipher is Form): Jwk =
    form.published(key.bytes).or(panic(m"the key is not a key of its own cipher"))

  // A private key as a JWK, with its private members: the token is what lets them out.
  def apply[cipher <: Cipher](key: PrivateKey[cipher]^, reveal: Divulgence.type)
    ( using form: cipher is Form, cipher: cipher )
  :   Jwk =

    form.divulged(key.pem(reveal).data, key.public.bytes).or:
      panic(m"the key is not a key of its own cipher")

  // A symmetric key as an `oct` JWK.
  def apply[cipher <: Cipher & Symmetric](key: SymmetricKey[cipher]^, reveal: Divulgence.type)
  :   Jwk =

    Jwk(t"oct", k = Jose.encode(key.data(reveal)))

// The members are named as the RFCs name them where the name is a word (`x`, `y`, `d`, `k`) and
// spelt out where it is an abbreviation; each is written under its registered name.
case class Jwk
  ( @name[Json](t"kty")      keyType:          Text,
    @name[Json](t"use")      use:              Optional[Text]       = Unset,
    @name[Json](t"key_ops")  operations:       Optional[List[Text]] = Unset,
    @name[Json](t"alg")      algorithm:        Optional[Text]       = Unset,
    @name[Json](t"kid")      keyId:            Optional[Text]       = Unset,
    @name[Json](t"x5u")      certificateUrl:   Optional[Text]       = Unset,
    @name[Json](t"x5c")      certificates:     Optional[List[Text]] = Unset,
    @name[Json](t"x5t")      certificateThumbprint: Optional[Text]  = Unset,
    @name[Json](t"x5t#S256") certificateSha256:     Optional[Text] = Unset,
    @name[Json](t"crv")      curve:            Optional[Text]       = Unset,
    x:                                          Optional[Text]       = Unset,
    y:                                          Optional[Text]       = Unset,
    d:                                          Optional[Text]       = Unset,
    @name[Json](t"n")        modulus:          Optional[Text]       = Unset,
    @name[Json](t"e")        exponent:         Optional[Text]       = Unset,
    @name[Json](t"p")        prime1:           Optional[Text]       = Unset,
    @name[Json](t"q")        prime2:           Optional[Text]       = Unset,
    @name[Json](t"dp")       exponent1:        Optional[Text]       = Unset,
    @name[Json](t"dq")       exponent2:        Optional[Text]       = Unset,
    @name[Json](t"qi")       coefficient:      Optional[Text]       = Unset,
    k:                                          Optional[Text]       = Unset ):

  private[sigillate] def expect(expected: Text)(using Tactic[Jose.Error], Diagnostics): Unit =
    if keyType != expected then
      val detail = t"a key of type $expected was expected but it is of type $keyType"
      abort(Jose.Error(Jose.Error.Reason.BadKey(detail)))

  // The key with its private members removed: what may be published.
  def public: Jwk =
    copy
      ( d           = Unset,
        prime1      = Unset,
        prime2      = Unset,
        exponent1   = Unset,
        exponent2   = Unset,
        coefficient = Unset,
        k           = Unset )

  // The key as a public key of the named cipher.
  def publicKey[cipher <: Cipher](using form: cipher is Jwk.Form, algorithm: cipher is KeyAlgorithm)
    ( using Tactic[Jose.Error], Diagnostics )
  :   PublicKey[cipher] =

    val document = form.publicKey(this)

    mitigate:
      case PublicKey.Error(reason) =>
        Jose.Error(Jose.Error.Reason.BadKey(t"it is not a key of the cipher asked for"))

    . protect(Der(document).as[PublicKey[cipher]])

  // The key as a private key of the named cipher, held by the cloak in scope.
  def privateKey[cipher <: Cipher]
    ( using form: cipher is Jwk.Form, algorithm: cipher is KeyAlgorithm, cloak: Cloak^ )
    ( using Tactic[Jose.Error], Diagnostics )
  :   PrivateKey[cipher]^{cloak} =

    val document = form.privateKey(this)

    mitigate:
      case PrivateKey.Error(reason) =>
        Jose.Error(Jose.Error.Reason.BadKey(t"it is not a key of the cipher asked for"))

    // The given is named because `as` would take it at the pure type `PrivateKey[cipher]`,
    // and the key captures the cloak that holds it.
    . protect(PrivateKey.derDecodable[cipher].decoded(Der(document)))

  // An `oct` key as a symmetric key of the named cipher. Its length is the cipher's to check,
  // when the key is used.
  def symmetricKey[cipher <: Cipher & Symmetric](using cloak: Cloak^)
    ( using Tactic[Jose.Error], Diagnostics )
  :   SymmetricKey[cipher]^{cloak} =

    expect(t"oct")

    val material = Jose.decode:
      k.or(abort(Jose.Error(Jose.Error.Reason.BadKey(t"the key has no k member"))))

    SymmetricKey[cipher](material)

  // The key's thumbprint (RFC 7638): the digest of its required members alone, in a canonical
  // form, as base64url. Two JWKs for one key have one thumbprint, whatever else they carry,
  // which makes it a natural key identifier.
  def thumbprint[hash <: Algorithm]
    ( using hash: Hash in hash, erased weakness: Permit[HashWeakness[hash]] )
    ( using Tactic[Jose.Error], Diagnostics )
  :   Text =

    def member(name: Text, value: Optional[Text]): Text =
      val text = value.or(Jwk.absent(name))
      t""""$name":"$text""""

    // RFC 7638 §3.2: the required members only, in lexicographic order, with no white space.
    val canonical: Text =
      if keyType == t"RSA" then
        t"""{${member(t"e", exponent)},"kty":"RSA",${member(t"n", modulus)}}"""
      else if keyType == t"EC" then
        t"""{${member(t"crv", curve)},"kty":"EC",${member(t"x", x)},${member(t"y", y)}}"""
      else if keyType == t"OKP" then
        t"""{${member(t"crv", curve)},"kty":"OKP",${member(t"x", x)}}"""
      else if keyType == t"oct" then
        t"""{${member(t"k", k)},"kty":"oct"}"""
      else
        abort(Jose.Error(Jose.Error.Reason.BadKey(t"keys of type $keyType have no thumbprint")))

    Jose.encode(Jose.ascii(canonical).digest[hash].data)

// A JWK Set (RFC 7517 §5): the keys a party publishes, among which a token's `kid` selects.
case class Jwks(keys: List[Jwk]):
  def apply(keyId: Text): Optional[Jwk] = keys.find(_.keyId == keyId)

  // The set's key, if it has exactly one.
  def sole: Optional[Jwk] = keys match
    case List(key) => key
    case _         => Unset
