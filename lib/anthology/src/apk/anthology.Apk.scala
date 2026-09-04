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
package anthology

import scala.math

import java.io as ji
import java.security as js
import java.security.cert as jsc

import anticipation.*
import gastronomy.*
import gossamer.*
import rudiments.*
import vacuous.*

import gastronomy.providers.javaBaseProvider
import denominative.*
import symbolism.*
import denominative.dysasymptotics.linearSize

// A complete, installable Android application package—dexed code, a binary manifest, aligned
// and signed—bound to the Android runtime: the application node reached from `Classfile`
// through `Dex`.
object Apk extends Format.Application:
  def id: Text = t"apk"

  // ApkConfiguration → Apk.Configuration
  object Configuration:
    // The conventional Android debug keystore (shared with Android Studio), a PKCS#12 store with a
    // well-known alias and password. The defaults produce a debuggable, emulator-installable APK.
    def default: Apk.Configuration =
      val home = java.lang.System.getProperty("user.home").nn.tt

      Apk.Configuration
        ( minApi      = 26,
          targetApi   = 34,
          packageName = t"dev.soundness.app",
          versionCode = 1,
          versionName = t"1.0",
          label       = t"Soundness App",
          permissions = Nil,
          keystore    = t"$home/.android/debug.keystore",
          storePass   = t"android",
          alias       = t"androiddebugkey",
          keyPass     = t"android" )

  // What an Android application package needs beyond its compiled code: the Android API level it
  // targets, the package name and human label, the requested runtime permissions, and the keystore
  // to sign with. The launcher activity is the compilation's entry point.
  case class Configuration
    ( minApi:       Int,
      targetApi:    Int,
      packageName:  Text,
      versionCode:  Int,
      versionName:  Text,
      label:        Text,
      permissions:  List[Text],
      keystore:     Text,
      storePass:    Text,
      alias:        Text,
      keyPass:      Text )

  // ApkManifest → Apk.Manifest
  // Builds the `Axml.Element` tree for an Android manifest. The resource IDs below are the stable,
  // public `android.R.attr.*` constants (verified against `aapt`'s own output); public resource
  // IDs never change across platform versions, so hardcoding the handful a manifest needs avoids
  // depending on the platform `android.jar`'s resource table at all.
  object Manifest:
    private val versionCode:      Int = 0x0101021b
    private val versionName:      Int = 0x0101021c
    private val minSdkVersion:    Int = 0x0101020c
    private val targetSdkVersion: Int = 0x01010270
    private val nameAttr:         Int = 0x01010003
    private val labelAttr:        Int = 0x01010001
    private val exportedAttr:     Int = 0x01010010

    private def android(name: Text, id: Int, value: Axml.Value): Axml.Attribute =
      Axml.Attribute(Axml.androidUri, name, id, value)

    def apply
      ( packageName:  Text,
        versionCode:  Int,
        versionName:  Text,
        minSdk:       Int,
        targetSdk:    Int,
        label:        Text,
        activity:     Text,
        permissions:  List[Text] )
    :   Axml.Element =

      // Each requested runtime permission is a `<uses-permission android:name="…"/>` element.
      val permissionElements: List[Axml.Element] =
        permissions.map: permission =>
          Axml.Element
            ( t"uses-permission",
              List(android(t"name", nameAttr, Axml.Value.Str(permission))),
              Nil )

      val launcher =
        Axml.Element
          ( t"intent-filter",
            Nil,
            List
              ( Axml.Element
                  ( t"action",
                    List(android(t"name", nameAttr,
                        Axml.Value.Str(t"android.intent.action.MAIN"))),
                    Nil ),
                Axml.Element
                  ( t"category",
                    List(android(t"name", nameAttr,
                        Axml.Value.Str(t"android.intent.category.LAUNCHER"))),
                    Nil ) ) )

      val activityElement =
        Axml.Element
          ( t"activity",
            List
              ( android(t"name", nameAttr, Axml.Value.Str(activity)),
                android(t"exported", exportedAttr, Axml.Value.Bool(true)) ),
            List(launcher) )

      val application =
        Axml.Element
          ( t"application",
            List(android(t"label", labelAttr, Axml.Value.Str(label))),
            List(activityElement) )

      val usesSdk =
        Axml.Element
          ( t"uses-sdk",
            List
              ( android(t"minSdkVersion", minSdkVersion, Axml.Value.Num(minSdk)),
                android(t"targetSdkVersion", targetSdkVersion, Axml.Value.Num(targetSdk)) ),
            Nil )

      Axml.Element
        ( t"manifest",
          List
            ( Axml.Attribute(Unset, t"package", Unset, Axml.Value.Str(packageName)),
              android(t"versionCode", Apk.Manifest.versionCode, Axml.Value.Num(versionCode)),
              android(t"versionName", Apk.Manifest.versionName, Axml.Value.Str(versionName)) ),
          usesSdk :: permissionElements + List(application) )

  // ApkSigner → Apk.Signer
  // APK Signature Scheme v2: signs a finished (aligned, unsigned) ZIP by inserting an "APK Signing
  // Block" between its entries and its central directory. The signature covers a digest of the
  // whole file computed in 1 MiB chunks, so any later byte change invalidates it — which is why
  // signing is the final step. The digests are computed with gastronomy (SHA-256, streaming); the
  // RSA signature, the keystore, and the certificate use `java.security` directly, since
  // enigmatic's RSA is encryption-only and its keystore surfaces no private keys. No Android SDK
  // tool is involved: this replaces `apksigner`.
  object Signer:
    private val chunkSize:         Int  = 1048576
    private val signAlgorithm:     Long = 0x0103L        // RSASSA-PKCS1-v1.5 with SHA2-256
    private val v2BlockId:         Long = 0x7109871aL
    private val magic:             Text = t"APK Sig Block 42"

    private def u32(value: Long): Data =
      Array((value & 0xff).toByte, ((value >> 8) & 0xff).toByte, ((value >> 16) & 0xff).toByte,
          ((value >> 24) & 0xff).toByte)

    private def u64(value: Long): Data =
      Array.range(0, 8).remap: i =>
        ((value >> (i*8)) & 0xff).toByte

    private def concat(parts: Data*): Data =
      val total = parts.map(_.length).sum
      val buffer = Array.allocate[Byte](total)
      var offset = 0

      parts.foreach: part =>
        buffer.copyFrom(part, 0, offset, part.length)
        offset += part.length

      Array.freeze(buffer)

    private def sha256(data: Data): Data = data.digest[Sha2[256]].data

    // Reads the little-endian u32 at `offset`.
    private def readU32(data: Data, offset: Int): Long =
      def byte(index: Int): Long = data.readable(offset + index).toLong & 0xff
      byte(0) | (byte(1) << 8) | (byte(2) << 16) | (byte(3) << 24)

    // The end-of-central-directory record is the archive's final 22 bytes: the writer emits no
    // ZIP comment, and the package is far too small for the ZIP64 end record.
    private def endOfCentralDirectory(data: Data): Int = data.length - 22

    // A length-prefixed (u32) sequence of length-prefixed (u32) elements.
    private def lengthPrefixedSequence(elements: List[Data]): Data =
      val body = concat(elements.map { element => concat(u32(element.length.toLong), element) }*)
      concat(u32(body.length.toLong), body)

    // Splits a byte range into 1 MiB chunks and returns each chunk's content digest, prefixed
    // per the v2 scheme (0xa5, then the chunk length).
    private def chunkDigests(data: Data, from: Int, until: Int): List[Data] =
      val builder = scala.collection.immutable.List.newBuilder[Data]
      var offset = from

      while offset < until do
        val end = math.min(offset + chunkSize, until)
        val length = end - offset
        val chunk = Array.allocate[Byte](length)
        chunk.copyFrom(data, offset, 0, length)
        val prefixed = concat(Array(0xa5.toByte), u32(length.toLong), Array.freeze(chunk))
        builder += sha256(prefixed)
        offset = end

      builder.result().to(List)

    // Signs the finished, unsigned APK bytes with the RSA key and certificate loaded from the
    // keystore, returning the signed APK.
    def sign
      ( unsigned: Data, keystore: Text, storePass: Text, alias: Text, keyPass: Text )
    :   Data =

      val store = js.KeyStore.getInstance("PKCS12").nn

      val stream = ji.FileInputStream(keystore.s)
      try store.load(stream, storePass.s.toCharArray) finally stream.close()

      val privateKey = store.getKey(alias.s, keyPass.s.toCharArray).nn.asInstanceOf[js.PrivateKey]
      val certificate = store.getCertificate(alias.s).nn.asInstanceOf[jsc.X509Certificate]
      val certificateDer = Array.unsafeFrozen(certificate.getEncoded.nn)
      val publicKeyDer = Array.unsafeFrozen(certificate.getPublicKey.nn.getEncoded.nn)

      val eocdOffset = endOfCentralDirectory(unsigned)
      val cdOffset = readU32(unsigned, eocdOffset + 16).toInt

      // The three digested sections: the ZIP entries, the central directory, and the
      // end-of-central-directory record. The unsigned EOCD already points at `cdOffset` — which is
      // exactly where the signing block will begin — so it is digested as-is.
      val digests =
        chunkDigests(unsigned, 0, cdOffset) +
          chunkDigests(unsigned, cdOffset, eocdOffset) +
          chunkDigests(unsigned, eocdOffset, unsigned.length)

      val count = digests.size
      val topLevel = sha256(concat(Array(0x5a.toByte), u32(count.toLong), concat(digests*)))

      // signed data: digests, certificates, additional attributes (none).
      val digestRecord = concat(u32(signAlgorithm), u32(topLevel.length.toLong), topLevel)
      val digestBlock = lengthPrefixedSequence(List(digestRecord))
      val certBlock = lengthPrefixedSequence(List(certificateDer))
      val attributes = concat(u32(0))
      val signedData = concat(digestBlock, certBlock, attributes)

      val signature = js.Signature.getInstance("SHA256withRSA").nn
      signature.initSign(privateKey)
      signature.update(Array.unsafeJvm(signedData))
      val signatureData = Array.unsafeFrozen(signature.sign.nn)

      // signer: signed data, signatures, public key.
      val signatureRecord =
        concat(u32(signAlgorithm), u32(signatureData.length.toLong), signatureData)

      val signer =
        concat
          ( concat(u32(signedData.length.toLong), signedData),
            lengthPrefixedSequence(List(signatureRecord)),
            concat(u32(publicKeyDer.length.toLong), publicKeyDer) )

      val v2Block = lengthPrefixedSequence(List(signer))

      // The ID-value pair, then the signing block framing (size, pair, size again, magic).
      val pair = concat(u64((4 + v2Block.length).toLong), u32(v2BlockId), v2Block)
      val blockLength = pair.length + 8 + 16
      val magicBytes = Array.unsafeFrozen(magic.s.getBytes("US-ASCII").nn)
      val signingBlock = concat(u64(blockLength.toLong), pair, u64(blockLength.toLong), magicBytes)

      // The final file: entries, signing block, central directory, and the EOCD with its
      // central-directory offset advanced past the inserted block.
      val section1 = slice(unsigned, 0, cdOffset)
      val centralDirectory = slice(unsigned, cdOffset, eocdOffset)
      val newCdOffset = cdOffset + signingBlock.length
      val patch = u32(newCdOffset.toLong)

      // The EOCD is copied out so its central-directory offset can be patched, so the buffer is
      // built exclusively here rather than laundered out of a frozen `slice`.
      val eocd = Array.allocate[Byte](unsigned.length - eocdOffset)
      eocd.copyFrom(unsigned, eocdOffset, 0, eocd.length)
      for i <- 0 until 4 do eocd(16 + i) = patch.readable(i)

      concat(section1, signingBlock, centralDirectory, Array.freeze(eocd))

    private def slice(data: Data, from: Int, until: Int): Data =
      val length = until - from
      val buffer = Array.allocate[Byte](length)
      buffer.copyFrom(data, from, 0, length)
      Array.freeze(buffer)
