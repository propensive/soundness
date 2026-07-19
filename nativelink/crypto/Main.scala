// A native binary whose ONLY foreign-library user is enigmatic's `OpensslCrypto` — deliberately
// NOT depending on coaxial, whose hand-written `@extern @link("crypto")` TLS backend would
// otherwise statically link libcrypto and mask a missing link. This is the regression guard for
// xenophile's runtime library loading (model A): `OpensslCrypto`'s object initializer
// `ForeignLibrary.register`s libcrypto by path (a real `dlopen` on native), and every FFI symbol
// resolves from that handle. If that path regressed to a `dlopen(null)`-only lookup, this binary
// would still LINK but crash at run time — so its passing run, with no coaxial in the graph, is
// exactly the proof the smoke binary cannot give.
package nativecrypto

import soundness.*
import gastronomy.*

object Main:
  def main(args: Array[String]): Unit =
    val out = java.lang.System.out.nn
    import charEncoders.utf8Encoder

    // HMAC-SHA256 against a known vector — the same assertion the JVM suite makes, here proving
    // libcrypto was loaded and its symbols resolved at run time with nothing else linking it.
    val mac = enigmatic.OpensslCrypto.hmac(t"HmacSHA256").mac(t"key".in[Data], t"message".in[Data])
    val hex = mac.to(List).map(b => String.format("%02x", Int.box(b & 255))).mkString
    val expected = "6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a"
    out.println("crypto-only: hmac verified = "+(hex == expected))

    // AES-256-CBC round-trip through the EVP cipher API — a second, independent libcrypto surface.
    val aes = enigmatic.OpensslCrypto.aes
    val key = aes.generateKey(256)
    val iv = enigmatic.OpensslCrypto.random.bytes(16)
    val secret = t"attack at dawn!!".in[Data]
    val ciphertext = aes.encrypt(t"AES/CBC/PKCS7", key, iv, secret)
    val plaintext = aes.decrypt(t"AES/CBC/PKCS7", key, 16, ciphertext)
    out.println("crypto-only: aes round-trip = "+(plaintext.to(List) == secret.to(List)))
