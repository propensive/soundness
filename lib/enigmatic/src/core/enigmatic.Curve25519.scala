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
package enigmatic

import scala.math.BigInt

import anticipation.*
import contingency.*
import distillate.*
import gastronomy.*
import vacuous.*

// Curve25519 for the `SoundnessCrypto` provider: X25519 key agreement (RFC 7748) over the
// Montgomery form, and Ed25519 signatures (RFC 8032) over the twisted Edwards form, both with the
// field arithmetic done by `BigInt`. That keeps the code short and obviously faithful to the RFCs,
// at two costs the caveat on `SoundnessCrypto` states: it is slow by the standards of a native
// implementation (a signature takes milliseconds, not microseconds), and `BigInt`'s operations
// take time that depends on their operands, so the scalar multiplications are not constant-time.
//
// Keys use the JCE encodings the `Crypto` contract fixes — PKCS#8 (RFC 8410's `CurvePrivateKey`,
// an OCTET STRING of the 32-byte secret inside the `PrivateKeyInfo`'s OCTET STRING) and X.509
// `SubjectPublicKeyInfo` — and, like the JDK provider, a generated private key carries its public
// half in a `[1] EXPLICIT BIT STRING`. Unlike the JDK, this provider can also derive the public key
// from the secret, so it accepts a private key without one.
private[enigmatic] object Curve25519:
  private val P: BigInt = (BigInt(1) << 255) - 19
  private val L: BigInt = (BigInt(1) << 252) + BigInt("27742317777372353535851937790883648493")
  private val D: BigInt = (P - 121665)*BigInt(121666).modInverse(P) mod P
  private val SquareRootOfMinusOne: BigInt = BigInt(2).modPow((P - 1)/4, P)
  private val A24: BigInt = BigInt(121665)
  private val Mask255: BigInt = (BigInt(1) << 255) - 1

  private def decode(bytes: scala.Array[Byte], offset: Int, count: Int): BigInt =
    def recur(index: Int, value: BigInt): BigInt =
      if index < offset then value else recur(index - 1, (value << 8) + (bytes(index) & 0xff))

    recur(offset + count - 1, BigInt(0))

  private def encode(value: BigInt, target: scala.Array[Byte]^, offset: Int, count: Int): Unit =
    def recur(index: Int, value: BigInt): Unit =
      if index < count then
        target(offset + index) = (value & 0xff).toByte
        recur(index + 1, value >> 8)

    recur(0, value)

  private def encoded(value: BigInt): Array[Byte]^ =
    val target = Array.allocate[Byte](32)
    encode(value, target.raw, 0, 32)
    target

  // SHA-512 over one, two or three parts, through gastronomy's pure digestion.
  private def sha512(a: scala.Array[Byte]): scala.Array[Byte] =
    val digestion = SoundnessHashing.sha2(512).digestion()
    // [java-boundary] fresh copies of the parts, retained by nobody else; the digest likewise
    digestion.append(Array.unsafeFrozen(a.clone()))
    Array.unsafeJvm(digestion.digest())

  private def sha512(a: scala.Array[Byte], b: scala.Array[Byte]): scala.Array[Byte] =
    val digestion = SoundnessHashing.sha2(512).digestion()
    // [java-boundary] fresh copies of the parts, retained by nobody else; the digest likewise
    digestion.append(Array.unsafeFrozen(a.clone()))
    digestion.append(Array.unsafeFrozen(b.clone()))
    Array.unsafeJvm(digestion.digest())

  private def sha512(a: scala.Array[Byte], b: scala.Array[Byte], c: scala.Array[Byte])
  :   scala.Array[Byte] =

    val digestion = SoundnessHashing.sha2(512).digestion()
    // [java-boundary] fresh copies of the parts, retained by nobody else; the digest likewise
    digestion.append(Array.unsafeFrozen(a.clone()))
    digestion.append(Array.unsafeFrozen(b.clone()))
    digestion.append(Array.unsafeFrozen(c.clone()))
    Array.unsafeJvm(digestion.digest())

  // ──────────────────────────────────────────── X25519 ───────────────────────────────────────

  // The scalar is clamped as RFC 7748 §5 prescribes; the point's top bit is ignored.
  private def clamp(secret: scala.Array[Byte]): BigInt =
    val k: scala.Array[Byte]^ = Array.scratch[Byte](32)
    java.lang.System.arraycopy(secret, 0, k, 0, 32)
    k(0) = (k(0) & 248).toByte
    k(31) = (k(31) & 127).toByte
    k(31) = (k(31) | 64).toByte
    decode(k, 0, 32)

  // The Montgomery ladder of RFC 7748 §5, with the conditional swaps written as selections.
  private def ladder(k: BigInt, u: BigInt): BigInt =
    def recur(t: Int, x2: BigInt, z2: BigInt, x3: BigInt, z3: BigInt, swap: Int): BigInt =
      if t < 0 then
        val (fx2, fz2) = if swap == 1 then (x3, z3) else (x2, z2)
        fx2*fz2.modPow(P - 2, P) mod P
      else
        val kt = ((k >> t) & 1).toInt
        val (sx2, sz2, sx3, sz3) = if (swap ^ kt) == 1 then (x3, z3, x2, z2) else (x2, z2, x3, z3)
        val a = (sx2 + sz2) mod P
        val aa = a*a mod P
        val b = (sx2 - sz2) mod P
        val bb = b*b mod P
        val e = (aa - bb) mod P
        val c = (sx3 + sz3) mod P
        val d = (sx3 - sz3) mod P
        val da = d*a mod P
        val cb = c*b mod P
        val nx3 = (da + cb).modPow(2, P)
        val nz3 = u*(da - cb).modPow(2, P) mod P
        val nx2 = aa*bb mod P
        val nz2 = e*(aa + A24*e) mod P
        recur(t - 1, nx2, nz2, nx3, nz3, kt)

    recur(254, BigInt(1), BigInt(0), u, BigInt(1), 0)

  private def x25519(secret: scala.Array[Byte], point: scala.Array[Byte]): Array[Byte]^ =
    encoded(ladder(clamp(secret), decode(point, 0, 32) & Mask255))

  // The Montgomery u-coordinate of the base point, 9.
  private val BasePoint: Data =
    val bytes = Array.allocate[Byte](32)
    bytes(0) = 9
    Array.freeze(bytes)

  // ──────────────────────────────────────────── Ed25519 ──────────────────────────────────────

  private object Point:
    val Identity: Point = Point(BigInt(0), BigInt(1), BigInt(1), BigInt(0))

    def affine(x: BigInt, y: BigInt): Point = Point(x, y, BigInt(1), x*y mod P)

    // RFC 8032 §5.1.3: the candidate root of `x² = (y² - 1)/(dy² + 1)`, corrected by √-1 when
    // it squares to `-u/v`, then chosen by the sign bit.
    def decoded(bytes: scala.Array[Byte]): Optional[Point] =
      val raw = decode(bytes, 0, 32)
      val y = raw & Mask255
      val sign = raw.testBit(255)

      if y >= P then Unset else
        val y2 = y*y mod P
        val u = (y2 - 1) mod P
        val v = (D*y2 + 1) mod P
        val candidate = u*v.modPow(3, P)*((u*v.modPow(7, P)) mod P).modPow((P - 5)/8, P) mod P
        val check = v*candidate*candidate mod P

        val x: Optional[BigInt] =
          if check == u then candidate
          else if check == ((P - u) mod P) then candidate*SquareRootOfMinusOne mod P
          else Unset

        x.let: x =>
          if x == 0 && sign then Unset else affine(if x.testBit(0) != sign then P - x else x, y)

  // A point in extended twisted Edwards coordinates, `x = X/Z`, `y = Y/Z`, `T = XY/Z`.
  private final case class Point(x: BigInt, y: BigInt, z: BigInt, t: BigInt):
    // RFC 8032 §5.1.4, with `a = -1` and `k = 2d`.
    def + (that: Point): Point =
      val a = (y - x)*(that.y - that.x) mod P
      val b = (y + x)*(that.y + that.x) mod P
      val c = t*2*D*that.t mod P
      val d = z*2*that.z mod P
      val e = (b - a) mod P
      val f = (d - c) mod P
      val g = (d + c) mod P
      val h = (b + a) mod P
      Point(e*f mod P, g*h mod P, f*g mod P, e*h mod P)

    def doubled: Point =
      val a = x*x mod P
      val b = y*y mod P
      val c = 2*z*z mod P
      val h = (a + b) mod P
      val e = (h - (x + y).modPow(2, P)) mod P
      val g = (a - b) mod P
      val f = (c + g) mod P
      Point(e*f mod P, g*h mod P, f*g mod P, e*h mod P)

    def * (scalar: BigInt): Point =
      def recur(bit: Int, acc: Point): Point =
        if bit < 0 then acc else
          val twice = acc.doubled
          recur(bit - 1, if scalar.testBit(bit) then twice + this else twice)

      recur(scalar.bitLength - 1, Point.Identity)

    def same(that: Point): Boolean =
      ((x*that.z - that.x*z) mod P) == 0 && ((y*that.z - that.y*z) mod P) == 0

    def encoded: Array[Byte]^ =
      val inverse = z.modPow(P - 2, P)
      val ax = x*inverse mod P
      val ay = y*inverse mod P
      val target = Array.allocate[Byte](32)
      encode(ay, target.raw, 0, 32)
      if ax.testBit(0) then target(31) = (target(31) | 0x80).toByte
      target

  private val Base: Point =
    val y = BigInt(4)*BigInt(5).modInverse(P) mod P
    // The base point's x is the even root.
    val bytes: scala.Array[Byte]^ = Array.scratch[Byte](32)
    encode(y, bytes, 0, 32)
    Point.decoded(bytes).or(Point.Identity)

  // The secret scalar and the prefix (RFC 8032 §5.1.5) from the 32-byte seed.
  private def expand(seed: scala.Array[Byte]): (BigInt, scala.Array[Byte]) =
    val h = sha512(seed)
    val prefix = Array.scratch[Byte](32)
    java.lang.System.arraycopy(h, 32, prefix, 0, 32)
    (clamp(h), prefix)

  private def publicKey(seed: scala.Array[Byte]): Array[Byte]^ = (Base*expand(seed)(0)).encoded

  private def sign(message: scala.Array[Byte], seed: scala.Array[Byte]): Array[Byte]^ =
    val (s, prefix) = expand(seed)
    val a = (Base*s).encoded
    val r = decode(sha512(prefix, message), 0, 64) mod L
    val bigR = (Base*r).encoded
    val k = decode(sha512(bigR.raw, a.raw, message), 0, 64) mod L
    val bigS = (r + k*s) mod L
    val signature = Array.allocate[Byte](64)
    java.lang.System.arraycopy(bigR.raw, 0, signature.raw, 0, 32)
    encode(bigS, signature.raw, 32, 32)
    signature

  private def verify
    ( message: scala.Array[Byte], signature: scala.Array[Byte], publicKey: scala.Array[Byte] )
  :   Boolean =

    signature.length == 64 && publicKey.length == 32 && {
      val encodedR = Array.scratch[Byte](32)
      java.lang.System.arraycopy(signature, 0, encodedR, 0, 32)
      val s = decode(signature, 32, 32)

      s < L && Point.decoded(encodedR).lay(false): r =>
        Point.decoded(publicKey).lay(false): a =>
          val k = decode(sha512(encodedR, publicKey, message), 0, 64) mod L
          (Base*s).same(r + a*k)
    }

  // ──────────────────────────────────────── Key encodings ────────────────────────────────────

  private def asn1(data: Data): Asn1 =
    safely(Der(data).as[Asn1]).or:
      CryptoFailure.invalidKey("the key is not valid DER")

  private def pkcs8(oid: List[Int], secret: Data, public: Data): Data =
    val algorithm = Asn1.Sequence(List(Asn1.ObjectId(oid)))
    val curvePrivateKey: Asn1 = Asn1.OctetString(secret)

    val info: Asn1 =
      Asn1.Sequence
        ( List
            ( Asn1.Integer(BigInt(0)), algorithm, Asn1.OctetString(curvePrivateKey.in[Der].data),
              Asn1.Tagged(1, true, Asn1.BitString(public, 0)) ) )

    info.in[Der].data

  private def spki(oid: List[Int], public: Data): Data =
    val algorithm = Asn1.Sequence(List(Asn1.ObjectId(oid)))
    val info: Asn1 = Asn1.Sequence(List(algorithm, Asn1.BitString(public, 0)))
    info.in[Der].data

  // The 32-byte secret of a PKCS#8 key, whatever else the `PrivateKeyInfo` carries.
  private def secretOf(privateKey: Data): scala.Array[Byte] =
    val secret = asn1(privateKey) match
      case Asn1.Sequence(_ :: _ :: Asn1.OctetString(inner) :: _) => asn1(inner) match
        case Asn1.OctetString(secret) => secret
        case _                        => CryptoFailure.invalidKey("not a CurvePrivateKey")

      case _ => CryptoFailure.invalidKey("the private key is not a PrivateKeyInfo")

    if secret.length != 32 then CryptoFailure.invalidKey("the secret is not 32 bytes")
    // [java-boundary] a read-only view of the decoded secret, which nothing else retains
    Array.unsafeJvm(secret)

  private def publicOf(publicKey: Data): scala.Array[Byte] = asn1(publicKey) match
    case Asn1.Sequence(List(_, Asn1.BitString(bytes, _))) =>
      if bytes.length != 32 then CryptoFailure.invalidKey("the public key is not 32 bytes")

      // [java-boundary] a read-only view of the decoded key, which nothing else retains
      Array.unsafeJvm(bytes)

    case _ =>
      CryptoFailure.invalidKey("the public key is not a SubjectPublicKeyInfo")

  val x25519Agreement: Crypto.KeyAgreement = new Crypto.KeyAgreement:
    def generateKeyPair(bits: Int): Data =
      val secret = Entropy.bytes(32)
      // [java-boundary] a read-only view of the fresh secret
      // [java-boundary] a read-only view of the base point
      val public = Array.freeze(x25519(Array.unsafeJvm(secret), Array.unsafeJvm(BasePoint)))
      pkcs8(KeyAlgorithm.X25519, secret, public)

    def privateToPublic(privateKey: Data): Data =
      // [java-boundary] a read-only view of the base point
      val public = Array.freeze(x25519(secretOf(privateKey), Array.unsafeJvm(BasePoint)))
      spki(KeyAlgorithm.X25519, public)

    def agree(privateKey: Data, publicKey: Data): Data =
      Array.freeze(x25519(secretOf(privateKey), publicOf(publicKey)))

  val ed25519Scheme: Crypto.SignatureScheme = new Crypto.SignatureScheme:
    def generateKeyPair(bits: Int): Data =
      val seed = Entropy.bytes(32)
      // [java-boundary] a read-only view of the fresh seed
      pkcs8(KeyAlgorithm.Ed25519, seed, Array.freeze(Curve25519.publicKey(Array.unsafeJvm(seed))))

    def privateToPublic(privateKey: Data): Data =
      spki(KeyAlgorithm.Ed25519, Array.freeze(Curve25519.publicKey(secretOf(privateKey))))

    def sign(data: Data, privateKey: Data): Data =
      // [java-boundary] a read-only view of the message
      Array.freeze(Curve25519.sign(Array.unsafeJvm(data), secretOf(privateKey)))

    def verify(data: Data, signature: Data, publicKey: Data): Boolean =
      // [java-boundary] read-only views of the message and signature
      Curve25519.verify(Array.unsafeJvm(data), Array.unsafeJvm(signature), publicOf(publicKey))
