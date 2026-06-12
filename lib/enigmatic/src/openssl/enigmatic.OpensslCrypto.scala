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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.lang.foreign.*

import anticipation.*
import fulminate.*
import gossamer.*
import vacuous.*
import xenophile.*


// A `Crypto` provider backed by OpenSSL's `libcrypto`, called through the
// xenophile native FFM layer (`ForeignLibrary`): the C prototypes below are parsed
// by `CHeaderDialect` and each call becomes a `java.lang.foreign` downcall.
//
// Native today: `random` (RAND_bytes), `hmac` (one-shot HMAC), and `aes` (the EVP
// cipher API, for PKCS#7 and no-padding; ISO-10126 is a JCE-only scheme and is not
// offered). `rsa` currently delegates to the JDK provider — native EVP_PKEY RSA
// (DER key parsing + keygen) is future work.
//
// Select it with `import cryptoProviders.opensslCrypto`.
object OpensslCrypto extends Crypto:
  private val header: Text = t"""
    int RAND_bytes(unsigned char* buf, int num);

    const EVP_MD* EVP_sha1(void);
    const EVP_MD* EVP_md5(void);
    const EVP_MD* EVP_sha256(void);
    const EVP_MD* EVP_sha384(void);
    const EVP_MD* EVP_sha512(void);
    unsigned char* HMAC(const EVP_MD* md, const void* key, int key_len, const unsigned char* d,
        size_t n, unsigned char* out, unsigned int* out_len);

    const EVP_CIPHER* EVP_get_cipherbyname(const char* name);
    EVP_CIPHER_CTX* EVP_CIPHER_CTX_new(void);
    void EVP_CIPHER_CTX_free(EVP_CIPHER_CTX* ctx);
    int EVP_CIPHER_CTX_set_padding(EVP_CIPHER_CTX* ctx, int pad);
    int EVP_EncryptInit_ex(EVP_CIPHER_CTX* ctx, const EVP_CIPHER* type, void* impl,
        const unsigned char* key, const unsigned char* iv);
    int EVP_EncryptUpdate(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl,
        const unsigned char* in, int inl);
    int EVP_EncryptFinal_ex(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl);
    int EVP_DecryptInit_ex(EVP_CIPHER_CTX* ctx, const EVP_CIPHER* type, void* impl,
        const unsigned char* key, const unsigned char* iv);
    int EVP_DecryptUpdate(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl,
        const unsigned char* in, int inl);
    int EVP_DecryptFinal_ex(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl);
    """

  private val paths: List[Text] = List
    ( t"/opt/homebrew/opt/openssl@3/lib/libcrypto.dylib",
      t"/opt/homebrew/lib/libcrypto.dylib",
      t"/usr/local/opt/openssl@3/lib/libcrypto.dylib",
      t"libcrypto.so.3",
      t"libcrypto.so",
      t"libcrypto.dylib" )

  // The library outlives every call, so it is bound to the global arena.
  private lazy val lib: ForeignLibrary = ForeignLibrary(header, paths)(using Arena.global().nn)

  private val int: ValueLayout.OfInt = ValueLayout.JAVA_INT.nn

  def random: Crypto.Random = new Crypto.Random:
    def bytes(size: Int): Data =
      val arena = Arena.ofConfined().nn

      try
        val buffer = arena.allocate(size.toLong).nn
        lib.handle(t"RAND_bytes").invokeWithArguments(buffer, Integer.valueOf(size))
        ForeignLibrary.bytes(buffer, size)
      finally arena.close()

  def hmac(algorithm: Text): Crypto.Mac = new Crypto.Mac:
    def mac(key: Data, data: Data): Data =
      val arena = Arena.ofConfined().nn

      try
        val md = digest(algorithm)
        val keySegment = ForeignLibrary.segment(key)(using arena)
        val dataSegment = ForeignLibrary.segment(data)(using arena)
        val output = arena.allocate(64L).nn          // EVP_MAX_MD_SIZE
        val outputLength = arena.allocate(int).nn

        lib.handle(t"HMAC").invokeWithArguments(md, keySegment, Integer.valueOf(key.length),
            dataSegment, java.lang.Long.valueOf(data.length.toLong), output, outputLength)

        ForeignLibrary.bytes(output, outputLength.get(int, 0L))
      finally arena.close()

  def aes: Crypto.SymmetricCipher = symmetric(t"AES")

  // RSA is not yet implemented natively (it needs EVP_PKEY DER parsing and keygen);
  // delegate to the JDK provider so this remains a complete `Crypto`.
  def rsa: Crypto.PublicKeyCipher = JavaStdlibCrypto.rsa

  private def digest(algorithm: Text): MemorySegment =
    val function = algorithm match
      case t"HmacSHA256" => t"EVP_sha256"
      case t"HmacSHA384" => t"EVP_sha384"
      case t"HmacSHA512" => t"EVP_sha512"
      case t"HmacSHA1"   => t"EVP_sha1"
      case t"HmacMD5"    => t"EVP_md5"
      case other         => panic(m"unsupported HMAC algorithm: $other")

    lib.handle(function).invokeWithArguments().nn.asInstanceOf[MemorySegment]

  // Maps a JCE-style cipher name (`AES`, `CBC`, key length) to OpenSSL's name, e.g.
  // `aes-256-cbc`. Only AES is offered; other block ciphers would need their own
  // key-length handling.
  private def opensslCipher(algorithm: Text, mode: Text, keyLength: Int): Text = algorithm match
    case t"AES" => t"aes-${keyLength*8}-${mode.lower}"
    case other  => panic(m"unsupported OpenSSL cipher: $other")

  private def cipher(name: Text)(using arena: Arena): MemorySegment =
    val nameSegment = arena.allocateFrom(name.s).nn
    val handle = lib.handle(t"EVP_get_cipherbyname").invokeWithArguments(nameSegment).nn
    val result = handle.asInstanceOf[MemorySegment]
    if result.address == 0L then panic(m"unknown OpenSSL cipher: $name") else result

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
      val arena = Arena.ofShared().nn
      val parts = transformation.cut(t"/")
      val context = newContext()
      val cipher0 = cipher(opensslCipher(parts(0), parts(1), key.length))(using arena)
      val keySegment = ForeignLibrary.segment(key)(using arena)
      val ivSegment = iv.lay(MemorySegment.NULL)(ForeignLibrary.segment(_)(using arena))

      lib.handle(t"EVP_EncryptInit_ex").invokeWithArguments
        ( context, cipher0, MemorySegment.NULL, keySegment, ivSegment )

      if parts(2) == t"NoPadding" then
        lib.handle(t"EVP_CIPHER_CTX_set_padding").invokeWithArguments(context, Integer.valueOf(0))

      val block = if parts(0) == t"AES" then 16 else 8

      new CipherSession:
        def update(chunk: Data): Data =
          val output = arena.allocate((chunk.length + block).toLong).nn
          val length = arena.allocate(int).nn
          val input = ForeignLibrary.segment(chunk)(using arena)

          lib.handle(t"EVP_EncryptUpdate").invokeWithArguments
            ( context, output, length, input, Integer.valueOf(chunk.length) )

          ForeignLibrary.bytes(output, length.get(int, 0L))

        def finish(): Data =
          val output = arena.allocate(block.toLong).nn
          val length = arena.allocate(int).nn
          lib.handle(t"EVP_EncryptFinal_ex").invokeWithArguments(context, output, length)
          val result = ForeignLibrary.bytes(output, length.get(int, 0L))
          lib.handle(t"EVP_CIPHER_CTX_free").invokeWithArguments(context)
          arena.close()
          result

  private def newContext(): MemorySegment =
    lib.handle(t"EVP_CIPHER_CTX_new").invokeWithArguments().nn.asInstanceOf[MemorySegment]

  // One-shot EVP encrypt/decrypt (init → update → final), returning the raw output
  // without IV framing (the caller prepends/strips the IV).
  private def oneShot
    ( encrypting: Boolean, transformation: Text, key: Data, iv: Optional[Data], data: Data)
  :   Data =
    val arena = Arena.ofConfined().nn

    try
      val parts = transformation.cut(t"/")
      val cipher0 = cipher(opensslCipher(parts(0), parts(1), key.length))(using arena)
      val context = newContext()

      try
        val keySegment = ForeignLibrary.segment(key)(using arena)
        val ivSegment = iv.lay(MemorySegment.NULL)(ForeignLibrary.segment(_)(using arena))
        val initFunction = if encrypting then t"EVP_EncryptInit_ex" else t"EVP_DecryptInit_ex"

        lib.handle(initFunction).invokeWithArguments
          ( context, cipher0, MemorySegment.NULL, keySegment, ivSegment )

        if parts(2) == t"NoPadding" then
          lib.handle(t"EVP_CIPHER_CTX_set_padding").invokeWithArguments(context, Integer.valueOf(0))

        val block = if parts(0) == t"AES" then 16 else 8
        val output = arena.allocate((data.length + block).toLong).nn
        val input = ForeignLibrary.segment(data)(using arena)
        val length1 = arena.allocate(int).nn
        val updateFunction = if encrypting then t"EVP_EncryptUpdate" else t"EVP_DecryptUpdate"

        lib.handle(updateFunction).invokeWithArguments
          ( context, output, length1, input, Integer.valueOf(data.length) )

        val count1 = length1.get(int, 0L)
        val length2 = arena.allocate(int).nn
        val finalFunction = if encrypting then t"EVP_EncryptFinal_ex" else t"EVP_DecryptFinal_ex"

        lib.handle(finalFunction)
        . invokeWithArguments(context, output.asSlice(count1.toLong).nn, length2)

        ForeignLibrary.bytes(output, count1 + length2.get(int, 0L))
      finally lib.handle(t"EVP_CIPHER_CTX_free").invokeWithArguments(context)
    finally arena.close()
