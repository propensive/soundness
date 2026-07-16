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

import java.security as js
import javax.crypto as jc
import javax.crypto.spec as jcs

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The standard security handler (ISO 32000-2 §7.6.4), revisions 2–6. The file key is derived
// from the user password and the `/Encrypt` dictionary and validated against `/U` at open;
// per-object keys are derived from it (with the object number and generation for revisions
// ≤4, or used directly for revision 6). Cryptographic primitives come from the JDK directly:
// the algorithm is a precise sequence of digest and block-cipher rounds that maps onto
// `MessageDigest`/`Cipher` more faithfully than onto a higher-level wrapper. RC4 is local.
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

  def apply(encrypt: Map[Text, Cos], id: Data, password: Text)(using pdf: Pdf)
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
    ( password: Text, owner: Data, permissions: Int, id: Data, keyBytes: Int, revision: Int,
      encryptMetadata: Boolean )
  :   Data =

    val md5 = js.MessageDigest.getInstance("MD5").nn
    md5.update(padded(password))
    md5.update(owner.mutable(using Unsafe), 0, 32.min(owner.length))

    md5.update(Array((permissions & 0xff).toByte, ((permissions >> 8) & 0xff).toByte,
        ((permissions >> 16) & 0xff).toByte, ((permissions >> 24) & 0xff).toByte))

    md5.update(id.mutable(using Unsafe))

    if revision >= 4 && !encryptMetadata then md5.update(Array(0xff.toByte, 0xff.toByte,
        0xff.toByte, 0xff.toByte))

    var hash = md5.digest().nn

    // Revision 3+: 50 further MD5 rounds over the first `keyBytes` bytes.
    if revision >= 3 then
      var i = 0

      while i < 50 do
        val next = js.MessageDigest.getInstance("MD5").nn
        next.update(hash, 0, keyBytes)
        hash = next.digest().nn
        i += 1

    hash.take(keyBytes).immutable(using Unsafe)

  // Algorithm 6: validate the user password by reconstructing `/U`.
  private def validate(fileKey: Data, user: Data, id: Data, revision: Int, keyBytes: Int)
  :   Boolean =

    if revision == 2 then Rc4(fileKey, padding.immutable(using Unsafe)).to(List) == user.to(List)
    else
      val md5 = js.MessageDigest.getInstance("MD5").nn
      md5.update(padding)
      md5.update(id.mutable(using Unsafe))
      var value = md5.digest().nn.immutable(using Unsafe)

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
  private def unwrap6(password: Text, user: Data, ue: Data): Optional[Data] =
    if user.length < 48 || ue.length < 32 then Unset else
      val passwordBytes = password.s.getBytes("UTF-8").nn
      val salt = user.slice(32, 40)
      val keySalt = user.slice(40, 48)

      if hash6(passwordBytes, salt, IArray.empty[Byte]).to(List) != user.take(32).to(List)
      then Unset
      else
        val intermediate = hash6(passwordBytes, keySalt, IArray.empty[Byte])

        try
          val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn

          cipher.init(jc.Cipher.DECRYPT_MODE,
              jcs.SecretKeySpec(intermediate.mutable(using Unsafe), "AES"),
              jcs.IvParameterSpec(new Array[Byte](16)))

          cipher.doFinal(ue.take(32).mutable(using Unsafe)).nn.immutable(using Unsafe)
        catch case _: Exception => Unset

  // Revision-6 hash (algorithm 2.B): SHA-256 seeded, then rounds mixing SHA-256/384/512
  // selected by the running hash, until the 64th-plus round whose last byte is ≤ round−32.
  private def hash6(password: Array[Byte], salt: Data, extra: Data): Data =
    val sha256 = js.MessageDigest.getInstance("SHA-256").nn
    sha256.update(password)
    sha256.update(salt.mutable(using Unsafe))
    if extra.length > 0 then sha256.update(extra.mutable(using Unsafe))
    var k = sha256.digest().nn.immutable(using Unsafe)

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

      val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn
      cipher.init(jc.Cipher.ENCRYPT_MODE, jcs.SecretKeySpec(key, "AES"), jcs.IvParameterSpec(iv))
      val e = cipher.doFinal(input).nn

      var sum = 0
      i = 0
      while i < 16 do
        sum += e(i) & 0xff
        i += 1

      val algorithm = sum%3 match
        case 0 => "SHA-256"
        case 1 => "SHA-384"
        case _ => "SHA-512"

      k = js.MessageDigest.getInstance(algorithm).nn.digest(e).nn.immutable(using Unsafe)
      round += 1

      if round >= 64 && (e(e.length - 1) & 0xff) <= round - 32 then done = true

    k.take(32)

  private def padded(password: Text): Array[Byte] =
    val bytes = password.s.getBytes("ISO-8859-1").nn
    val out = new Array[Byte](32)
    val count = 32.min(bytes.length)
    System.arraycopy(bytes, 0, out, 0, count)
    System.arraycopy(padding, 0, out, count, 32 - count)
    out

private[facsimile] class Guard
  ( fileKey: Data, revision: Int, streamMethod: Guard.Method, stringMethod: Guard.Method,
    val encryptMetadata: Boolean ):

  import Guard.Method

  // Algorithm 1: the per-object key. For revision 6 the file key is used directly.
  private def objectKey(number: Int, generation: Int, aes: Boolean): Data =
    if revision >= 5 then fileKey else
      val md5 = js.MessageDigest.getInstance("MD5").nn
      md5.update(fileKey.mutable(using Unsafe))

      md5.update(Array((number & 0xff).toByte, ((number >> 8) & 0xff).toByte,
          ((number >> 16) & 0xff).toByte, (generation & 0xff).toByte,
          ((generation >> 8) & 0xff).toByte))

      if aes then md5.update(Array[Byte](0x73, 0x41, 0x6c, 0x54)) // the "sAlT" constant
      md5.digest().nn.take((fileKey.length + 5).min(16)).immutable(using Unsafe)

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
        aesCbc(objectKey(number, generation, true), bytes)

      case Method.Aes256 =>
        aesCbc(fileKey, bytes)

  // AESV2/V3 layout: a 16-byte initialization vector prefixes the ciphertext.
  private def aesCbc(key: Data, bytes: Data): Data =
    if bytes.length <= 16 then IArray.empty[Byte] else
      val iv = bytes.slice(0, 16).mutable(using Unsafe)
      val body = bytes.slice(16, bytes.length).mutable(using Unsafe)

      try
        val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn

        cipher.init(jc.Cipher.DECRYPT_MODE, jcs.SecretKeySpec(key.mutable(using Unsafe), "AES"),
            jcs.IvParameterSpec(iv))

        val decrypted = cipher.doFinal(body).nn
        // Strip PKCS#7 padding.
        val pad = if decrypted.length > 0 then decrypted(decrypted.length - 1) & 0xff else 0
        val end = if pad >= 1 && pad <= 16 then decrypted.length - pad else decrypted.length
        decrypted.take(end).immutable(using Unsafe)
      catch case _: Exception => IArray.empty[Byte]
