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
package enigmatic

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import vacuous.*
import xenophile.*

// A `Crypto` provider backed by OpenSSL's `libcrypto`, called through xenophile's typed C
// navigation: the prototypes in `/enigmatic/openssl.h` are parsed by `CHeaderDialect` at compile
// time, and each fully-applied navigation's `invoke` materializes as a Panama downcall on the JVM
// and a direct C call on Scala Native — this one source serves both platforms, with buffers
// passed through the platform-twinned `ForeignBuffer` and opaque handles as `Pointer`s.
//
// Native today: `random` (RAND_bytes), `hmac` (one-shot HMAC), and `aes` (the EVP cipher API,
// for PKCS#7 and no-padding; ISO-10126 is a JCE-only scheme and is not offered). `rsa` delegates
// to the JDK provider (which panics on Scala Native) — native EVP_PKEY RSA (DER key parsing +
// keygen) is future work.
//
// Select it with `import providers.opensslProvider`.
object OpensslCrypto extends Crypto:
  // On the JVM, `libcrypto` must be loaded for symbol resolution; on Scala Native, registration
  // is a no-op — the library is statically linked instead (`-lcrypto`).
  ForeignLibrary.register
    ( t"/opt/homebrew/opt/openssl@3/lib/libcrypto.dylib",
      t"/opt/homebrew/lib/libcrypto.dylib",
      t"/usr/local/opt/openssl@3/lib/libcrypto.dylib",
      t"libcrypto.so.3",
      t"libcrypto.so",
      t"libcrypto.dylib" )

  private given interface: (Interface in Native at "/enigmatic/openssl.h") =
    Interface[Native]("/enigmatic/openssl.h")

  def random: Crypto.Random = new Crypto.Random:
    def bytes(size: Int): Data =
      val output = ForeignBuffer(size)

      try
        Foreign["library", Native].RAND_bytes(output.pointer, size).invoke[Int]
        output.data(size)
      finally output.free()

  def hmac(algorithm: Text): Crypto.Mac = new Crypto.Mac:
    def mac(key: Data, data: Data): Data =
      val md = digest(algorithm)
      val keyBuffer = ForeignBuffer(key)
      val dataBuffer = ForeignBuffer(data)
      val output = ForeignBuffer(64)            // EVP_MAX_MD_SIZE
      val outputLength = ForeignBuffer(4)

      try
        Foreign["library", Native].HMAC
          ( md, keyBuffer.pointer, key.length, dataBuffer.pointer, data.length.toLong,
            output.pointer, outputLength.pointer )
        . invoke[Pointer]

        output.data(outputLength.int)

      finally
        keyBuffer.free()
        dataBuffer.free()
        output.free()
        outputLength.free()

  def aes: Crypto.SymmetricCipher = symmetric(t"AES")

  // RSA is not yet implemented natively (it needs EVP_PKEY DER parsing and keygen); delegate to
  // the JDK provider so this remains a complete `Crypto` (on Scala Native, where that provider
  // is a panicking stub, `rsa` is simply unavailable).
  def rsa: Crypto.PublicKeyCipher = JavaStdlibCrypto.rsa

  private def digest(algorithm: Text): Pointer = algorithm match
    case t"HmacSHA256" => Foreign["library", Native].EVP_sha256().invoke[Pointer]
    case t"HmacSHA384" => Foreign["library", Native].EVP_sha384().invoke[Pointer]
    case t"HmacSHA512" => Foreign["library", Native].EVP_sha512().invoke[Pointer]
    case t"HmacSHA1"   => Foreign["library", Native].EVP_sha1().invoke[Pointer]
    case t"HmacMD5"    => Foreign["library", Native].EVP_md5().invoke[Pointer]
    case other         => panic(m"unsupported HMAC algorithm: $other")

  // Maps a JCE-style cipher name (`AES`, `CBC`, key length) to OpenSSL's name, e.g.
  // `aes-256-cbc`. Only AES is offered; other block ciphers would need their own key-length
  // handling.
  private def opensslCipher(algorithm: Text, mode: Text, keyLength: Int): Text = algorithm match
    case t"AES" => t"aes-${keyLength*8}-${mode.lower}"
    case other  => panic(m"unsupported OpenSSL cipher: $other")

  private def cipher(name: Text): Pointer =
    val result = Foreign["library", Native].EVP_get_cipherbyname(name).invoke[Pointer]
    if result.isNull then panic(m"unknown OpenSSL cipher: $name") else result

  private def newContext(): Pointer =
    Foreign["library", Native].EVP_CIPHER_CTX_new().invoke[Pointer]

  private def freeContext(context: Pointer): Unit =
    Foreign["library", Native].EVP_CIPHER_CTX_free(context).invoke[Unit]

  // Initialises `context` for one direction of one transformation: cipher selection, key and
  // (optional) IV, and padding. The two EVP directions are distinct C entry points, so each
  // branch is its own static navigation.
  private def initialise
    ( context: Pointer, transformation: Text, key: Data, iv: Optional[Data], encrypting: Boolean )
  :   Unit =

    val parts = transformation.cut(t"/").stdlib
    val cipher0 = cipher(opensslCipher(parts(0), parts(1), key.length))
    val keyBuffer = ForeignBuffer(key)
    val ivBuffer = iv.let(ForeignBuffer(_))
    val ivPointer = ivBuffer.lay(Pointer.Null)(_.pointer)

    try
      if encrypting then
        Foreign["library", Native]
        . EVP_EncryptInit_ex(context, cipher0, Pointer.Null, keyBuffer.pointer, ivPointer)
        . invoke[Int]
      else
        Foreign["library", Native]
        . EVP_DecryptInit_ex(context, cipher0, Pointer.Null, keyBuffer.pointer, ivPointer)
        . invoke[Int]

      if parts(2) == t"NoPadding" then
        Foreign["library", Native].EVP_CIPHER_CTX_set_padding(context, 0).invoke[Int]

    finally
      keyBuffer.free()
      ivBuffer.let(_.free())

  // One EVP update step: feeds `chunk` through `context` into a fresh output buffer (sized for
  // the worst case of one extra block) and returns the bytes produced.
  private def update(context: Pointer, chunk: Data, block: Int, encrypting: Boolean): Data =
    val output = ForeignBuffer(chunk.length + block)
    val length = ForeignBuffer(4)
    val input = ForeignBuffer(chunk)

    try
      if encrypting then
        Foreign["library", Native]
        . EVP_EncryptUpdate(context, output.pointer, length.pointer, input.pointer, chunk.length)
        . invoke[Int]
      else
        Foreign["library", Native]
        . EVP_DecryptUpdate(context, output.pointer, length.pointer, input.pointer, chunk.length)
        . invoke[Int]

      output.data(length.int)

    finally
      output.free()
      length.free()
      input.free()

  // The final EVP step: flushes the last block (applying or checking padding) and returns it.
  private def finish(context: Pointer, block: Int, encrypting: Boolean): Data =
    val output = ForeignBuffer(block)
    val length = ForeignBuffer(4)

    try
      if encrypting
      then
        Foreign["library", Native]
        . EVP_EncryptFinal_ex(context, output.pointer, length.pointer)
        . invoke[Int]
      else
        Foreign["library", Native]
        . EVP_DecryptFinal_ex(context, output.pointer, length.pointer)
        . invoke[Int]

      output.data(length.int)

    finally
      output.free()
      length.free()

  private def symmetric(algorithm: Text): Crypto.SymmetricCipher = new Crypto.SymmetricCipher:
    def blockSize(transformation: Text): Int = if algorithm == t"AES" then 16 else 8
    def generateKey(bits: Int): Data = random.bytes(bits/8)

    def encrypt(transformation: Text, key: Data, iv: Optional[Data], data: Data): Data =
      val body = oneShot(encrypting = true, transformation, key, iv, data)
      iv.lay(body)(_ ++ body)

    def decrypt(transformation: Text, key: Data, ivSize: Optional[Int], data: Data): Data =
      ivSize.lay(oneShot(encrypting = false, transformation, key, Unset, data)): size =>
        oneShot(encrypting = false, transformation, key, data.take(size), data.drop(size))

    def stream(transformation: Text, key: Data, iv: Optional[Data]): CipherSession =
      session(transformation, key, iv, encrypting = true)

    def decryptStream(transformation: Text, key: Data, iv: Optional[Data]): CipherSession =
      session(transformation, key, iv, encrypting = false)

    private def session
      ( transformation: Text, key: Data, iv: Optional[Data], encrypting: Boolean )
    :   CipherSession =

      val context = newContext()
      initialise(context, transformation, key, iv, encrypting)
      val parts = transformation.cut(t"/").stdlib
      val block = if parts(0) == t"AES" then 16 else 8

      new CipherSession:
        def update(chunk: Data): Data = OpensslCrypto.update(context, chunk, block, encrypting)

        def finish(): Data =
          try OpensslCrypto.finish(context, block, encrypting) finally freeContext(context)

  // One-shot EVP encrypt/decrypt (init → update → final), returning the raw output without IV
  // framing (the caller prepends/strips the IV).
  private def oneShot
    ( encrypting: Boolean, transformation: Text, key: Data, iv: Optional[Data], data: Data )
  :   Data =

    val context = newContext()

    try
      initialise(context, transformation, key, iv, encrypting)
      val parts = transformation.cut(t"/").stdlib
      val block = if parts(0) == t"AES" then 16 else 8
      update(context, data, block, encrypting) ++ finish(context, block, encrypting)
    finally freeContext(context)
