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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package facsimile

import java.nio as jn
import java.nio.charset as jnc
import java.util as ju
import javax.crypto as jc
import javax.crypto.spec as jcs

import anticipation.*
import contingency.*
import enigmatic.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

import enigmatic.cloaks.cloakHeap
import gastronomy.crypto.permitDisallowedCrypto
import gastronomy.providers.javaStdlibProvider

// The standard security handler (ISO 32000-2 §7.6.4), revisions 2–6. The file key is derived
// from the user password and the `/Encrypt` dictionary and validated against `/U` at open;
// per-object keys are derived from it (with the object number and generation for revisions
// ≤4, or used directly for revision 6). Digests come from gastronomy and AES-CBC from
// enigmatic; the algorithm is a precise sequence of digest and block-cipher rounds. RC4,
// which those libraries do not offer, is local.
private[facsimile] object Guard:
  enum Method:
    case Rc4
    case Aes128
    case Aes256
    case Identity

  // The 32-byte padding string prepended to short passwords (algorithm 2).
  private val padding: Array[Byte] = Array
    ( 0x28, 0xbf, 0x4e, 0x5e, 0x4e, 0x75, 0x8a, 0x41, 0x64, 0x00, 0x4e, 0x56, 0xff, 0xfa,
      0x01, 0x08, 0x2e, 0x2e, 0x00, 0xb6, 0xd0, 0x68, 0x3e, 0x80, 0x2f, 0x0c, 0xa9, 0xfe,
      0x64, 0x53, 0x69, 0x7a )
    . map(_.toByte)

  // MD5 (weak, hence the permit) and SHA-2 digests through gastronomy's cross-platform
  // hashing. The PDF security algorithms hash the concatenation of their parts, which is
  // exactly one `.digest` over the joined bytes.
  private def md5(data: Data): Data = data.digest[Md5].data

  private def sha(bits: Int, data: Data): Data = bits match
    case 384 => data.digest[Sha2[384]].data
    case 512 => data.digest[Sha2[512]].data
    case _   => data.digest[Sha2[256]].data

  // The password arrives as a mutable char array — as lent by `Password.uncloak` — never as
  // an immutable string, so the transient encodings derived from it here can all be zeroed.
  def apply(encrypt: Map[Text, Cos], id: Data, password: Array[Char])(using pdf: Pdf)
  :   Guard raises PdfError =

    val filter = encrypt.at(t"Filter").let(pdf.resolved(_).name).or(t"")
    if filter != t"Standard" then abort(PdfError(PdfError.Reason.UnsupportedEncryption(0)))

    val version = encrypt.at(t"V").let(pdf.resolved(_).long).or(0L).toInt
    val revision = encrypt.at(t"R").let(pdf.resolved(_).long).or(0L).toInt
    val length = encrypt.at(t"Length").let(pdf.resolved(_).long).or(40L).toInt
    val permissions = encrypt.at(t"P").let(pdf.resolved(_).long).or(0L).toInt
    val owner = encrypt.at(t"O").let(pdf.resolved(_).chars).or(IArray.empty[Byte])
    val user = encrypt.at(t"U").let(pdf.resolved(_).chars).or(IArray.empty[Byte])

    val encryptMetadata =
      encrypt.at(t"EncryptMetadata").let(pdf.resolved(_).truth).or(true)

    // The stream and string crypt-filter methods: revisions ≤4 apply one method throughout;
    // revision 4+ names filters in `/CF` selected by `/StmF` and `/StrF`.
    val (streamMethod, stringMethod) =
      if version >= 4 then
        val filters = pdf.resolved(encrypt.at(t"CF").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())

        def method(selector: Text): Method =
          encrypt.at(selector).let(pdf.resolved(_).name).or(t"Identity") match
            case t"Identity" => Method.Identity
            case name =>
              val cfm = pdf.resolved(filters.at(name).or(Cos.Nil))(t"CFM").let(_.name).or(t"")
              cfm match
                case t"V2"    => Method.Rc4
                case t"AESV2" => Method.Aes128
                case t"AESV3" => Method.Aes256
                case _        => Method.Identity

        (method(t"StmF"), method(t"StrF"))
      else (Method.Rc4, Method.Rc4)

    if revision >= 5 then
      // Revisions 5–6 (AES-256): the file key is unwrapped from `/UE` with a key derived
      // from the password, and neither object number nor generation enters the per-object
      // key.
      val ue = encrypt.at(t"UE").let(pdf.resolved(_).chars).or(IArray.empty[Byte])

      val fileKey = unwrap6(password, user, ue)
        . or(abort(PdfError(PdfError.Reason.BadPassword)))

      new Guard(fileKey, revision, Method.Aes256, Method.Aes256, encryptMetadata)
    else
      val keyBytes = if version <= 1 then 5 else length/8

      val fileKey =
        deriveKey(password, owner, permissions, id, keyBytes, revision, encryptMetadata)

      if !validate(fileKey, user, id, revision, keyBytes)
      then abort(PdfError(PdfError.Reason.BadPassword))

      new Guard(fileKey, revision, streamMethod, stringMethod, encryptMetadata)

  // Algorithm 2: the file encryption key for revisions 2–4.
  private def deriveKey
    ( password: Array[Char], owner: Data, permissions: Int, id: Data, keyBytes: Int,
      revision: Int, encryptMetadata: Boolean )
  :   Data =

    val permissionsBytes: Data = IArray((permissions & 0xff).toByte,
        ((permissions >> 8) & 0xff).toByte, ((permissions >> 16) & 0xff).toByte,
        ((permissions >> 24) & 0xff).toByte)

    val metadataBytes: Data =
      if revision >= 4 && !encryptMetadata
      then IArray(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
      else IArray.empty[Byte]

    var hash: Data =
      md5(padded(password).immutable(using Unsafe) ++ owner.take(32.min(owner.length)) ++
        permissionsBytes ++ id ++ metadataBytes)

    // Revision 3+: 50 further MD5 rounds over the first `keyBytes` bytes.
    if revision >= 3 then
      var i = 0

      while i < 50 do
        hash = md5(hash.take(keyBytes))
        i += 1

    hash.take(keyBytes)

  // Algorithm 6: validate the user password by reconstructing `/U`.
  private def validate(fileKey: Data, user: Data, id: Data, revision: Int, keyBytes: Int)
  :   Boolean =

    if revision == 2 then Rc4(fileKey, padding.immutable(using Unsafe)).to(List) == user.to(List)
    else
      var value: Data = md5(padding.immutable(using Unsafe) ++ id)

      value = Rc4(fileKey, value)

      var i = 1

      while i <= 19 do
        val roundKey = fileKey.map(byte => (byte ^ i).toByte)
        value = Rc4(roundKey, value)
        i += 1

      // Only the first 16 bytes are meaningful; the rest of `/U` is arbitrary padding.
      value.take(16).to(List) == user.take(16).to(List)

  // Algorithms 2.A/8 (revision 6): validate the user password against `/U`, then unwrap the
  // file key from `/UE` with the intermediate key, using AES-256-CBC with a zero IV and no
  // padding.
  private def unwrap6(password: Array[Char], user: Data, ue: Data): Optional[Data] =
    if user.length < 48 || ue.length < 32 then Unset else
      val passwordBytes = encoded(password, jnc.StandardCharsets.UTF_8.nn)
      val salt = user.slice(32, 40)
      val keySalt = user.slice(40, 48)

      try
        if hash6(passwordBytes, salt, IArray.empty[Byte]).to(List) != user.take(32).to(List)
        then Unset
        else
          val intermediate = hash6(passwordBytes, keySalt, IArray.empty[Byte])

          // AES-256-CBC with a zero IV and no padding. This stays on the JDK cipher:
          // enigmatic's `NoPadding` given captures a `Tactic[CryptoError]`, and the only
          // available strategy (`throwUnsafely`) yields a root `{any}` capability that
          // capture checking cannot confine here — unlike the `Pkcs7` object ciphers below.
          // See the module notes.
          try
            val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn

            cipher.init(jc.Cipher.DECRYPT_MODE,
                jcs.SecretKeySpec(intermediate.mutable(using Unsafe), "AES"),
                jcs.IvParameterSpec(new Array[Byte](16)))

            cipher.doFinal(ue.take(32).mutable(using Unsafe)).nn.immutable(using Unsafe)
          catch case _: Exception => Unset
      finally ju.Arrays.fill(passwordBytes, 0.toByte)

  // Revision-6 hash (algorithm 2.B): SHA-256 seeded, then rounds mixing SHA-256/384/512
  // selected by the running hash, until the 64th-plus round whose last byte is ≤ round−32.
  private def hash6(password: Array[Byte], salt: Data, extra: Data): Data =
    var k: Data = sha(256, password.immutable(using Unsafe) ++ salt ++ extra)

    var round = 0
    var done = false

    while !done do
      val block = Array.newBuilder[Byte]
      var i = 0

      while i < 64 do
        block.addAll(password)
        block.addAll(k.mutable(using Unsafe))
        if extra.length > 0 then block.addAll(extra.mutable(using Unsafe))
        i += 1

      val input = block.result()
      val key = k.take(16).mutable(using Unsafe)
      val iv = k.slice(16, 32).mutable(using Unsafe)

      // AES-128-CBC with no padding on block-aligned input; on the JDK cipher for the same
      // capture-checking reason as `unwrap6`.
      val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn
      cipher.init(jc.Cipher.ENCRYPT_MODE, jcs.SecretKeySpec(key, "AES"), jcs.IvParameterSpec(iv))
      val e = cipher.doFinal(input).nn

      var sum = 0
      i = 0
      while i < 16 do
        sum += e(i) & 0xff
        i += 1

      val bits = sum%3 match
        case 0 => 256
        case 1 => 384
        case _ => 512

      k = sha(bits, e.immutable(using Unsafe))
      round += 1

      if round >= 64 && (e(e.length - 1) & 0xff) <= round - 32 then done = true

    k.take(32)

  // Encodes the password chars to a mutable byte array, zeroing the encoder's intermediate
  // buffer; callers zero the result once the derived key is computed.
  private def encoded(password: Array[Char], charset: jnc.Charset): Array[Byte] =
    val buffer = charset.encode(jn.CharBuffer.wrap(password)).nn
    val bytes = new Array[Byte](buffer.remaining)
    buffer.get(bytes)
    if buffer.hasArray then ju.Arrays.fill(buffer.array.nn, 0.toByte)
    bytes

  private def padded(password: Array[Char]): Array[Byte] =
    val bytes = encoded(password, jnc.StandardCharsets.ISO_8859_1.nn)
    val out = new Array[Byte](32)
    val count = 32.min(bytes.length)
    System.arraycopy(bytes, 0, out, 0, count)
    System.arraycopy(padding, 0, out, count, 32 - count)
    ju.Arrays.fill(bytes, 0.toByte)
    out

private[facsimile] class Guard
  ( fileKey: Data, revision: Int, streamMethod: Guard.Method, stringMethod: Guard.Method,
    val encryptMetadata: Boolean ):

  import Guard.Method

  // Algorithm 1: the per-object key. For revision 6 the file key is used directly.
  private def objectKey(number: Int, generation: Int, aes: Boolean): Data =
    if revision >= 5 then fileKey else
      val numbering: Data = IArray((number & 0xff).toByte, ((number >> 8) & 0xff).toByte,
          ((number >> 16) & 0xff).toByte, (generation & 0xff).toByte,
          ((generation >> 8) & 0xff).toByte)

      // the "sAlT" constant
      val salt: Data = if aes then IArray[Byte](0x73, 0x41, 0x6c, 0x54) else IArray.empty[Byte]

      Guard.md5(fileKey ++ numbering ++ salt).take((fileKey.length + 5).min(16))

  def string(bytes: Data, number: Int, generation: Int): Data =
    decrypt(bytes, number, generation, stringMethod)

  def stream(bytes: Data, number: Int, generation: Int, cryptFilter: Optional[Method]): Data =
    decrypt(bytes, number, generation, cryptFilter.or(streamMethod))

  private def decrypt(bytes: Data, number: Int, generation: Int, method: Method): Data =
    method match
      case Method.Identity =>
        bytes

      case Method.Rc4 =>
        Rc4(objectKey(number, generation, false), bytes)

      case Method.Aes128 =>
        aesCbc[128](objectKey(number, generation, true), bytes)

      case Method.Aes256 =>
        aesCbc[256](fileKey, bytes)

  // The inverse operations, for writing new or edited objects into an encrypted document's
  // incremental update. RC4 is symmetric, so it reuses `decrypt`; AES prepends a fresh
  // random initialization vector and PKCS#7-pads.
  def encryptString(bytes: Data, number: Int, generation: Int): Data =
    encrypt(bytes, number, generation, stringMethod)

  def encryptStream(bytes: Data, number: Int, generation: Int): Data =
    encrypt(bytes, number, generation, streamMethod)

  private def encrypt(bytes: Data, number: Int, generation: Int, method: Method): Data =
    method match
      case Method.Identity => bytes
      case Method.Rc4      => Rc4(objectKey(number, generation, false), bytes)
      case Method.Aes128   => aesCbcEncrypt[128](objectKey(number, generation, true), bytes)
      case Method.Aes256   => aesCbcEncrypt[256](fileKey, bytes)

  // A fresh random IV is prepended and PKCS#7 padding applied — both handled by enigmatic's
  // `Cbc against Pkcs7`, whose whole-value output is exactly `iv ++ ciphertext`.
  private def aesCbcEncrypt[bits <: 128 | 256: ValueOf](key: Data, bytes: Data): Data =
    val symmetricKey = SymmetricKey[Aes[bits] over Cbc against Pkcs7](key)

    symmetricKey.uncloak:
      bytes.encrypt[Aes[bits] over Cbc against Pkcs7](InitializationVector.random)

  // AESV2/V3 layout: a 16-byte initialization vector prefixes the ciphertext, which enigmatic
  // reads back off the front; `Pkcs7` strips the padding. Any failure yields empty bytes.
  private def aesCbc[bits <: 128 | 256: ValueOf](key: Data, bytes: Data): Data =
    if bytes.length <= 16 then IArray.empty[Byte] else
      val symmetricKey = SymmetricKey[Aes[bits] over Cbc against Pkcs7](key)

      safely(symmetricKey.uncloak(bytes.decrypt[Data, Aes[bits] over Cbc against Pkcs7]))
      . or(IArray.empty[Byte])
