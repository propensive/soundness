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
package ethereal

import java.lang as jl
import java.nio as jnio

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zeppelin.*

import errorDiagnostics.emptyDiagnostics

import filesystemOptions.createNonexistentParents
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.overwritePreexisting

import filesystemBackends.javaBaseFilesystem

// Produces a self-contained per-platform executable by patching a bare ethereal
// runner binary's ETHRCFG block (build id, Java version policy, ML-DSA-44 public
// key) and appending the application JAR at EOF. Shared by the
// `-Dbuild.executable` CLI build path and the ziggurat packager, which supplies
// a runner downloaded from a URL rather than read from the classpath.
object Assembler:
  // v2 ETHRCFG layout — keep in sync with lib/ethereal/src/runner/src/config.rs.
  @scala.caps.unsafe.untrackedCaptures
  val MagicMarker: scala.Array[Byte] =
    scala.Array[Byte]('E'.toByte, 'T'.toByte, 'H'.toByte, 'R'.toByte,
                'C'.toByte, 'F'.toByte, 'G'.toByte, 2.toByte)

  val PublicKeyLength: Int = 1312   // ML-DSA-44 public key size

  // Patches a bare runner's ETHRCFG block (build id, Java version policy, ML-DSA-44
  // public key) and returns the patched bytes, without appending a JAR. Used both by
  // `assemble` (which then appends the JAR) and by the offline polyglot packager, which
  // embeds the patched stub and lets the launcher append the JAR at unpack time.
  def patch
    ( runner:        Data, // bare runner binary
      buildId:       Long,
      javaMinimum:   Int,
      javaPreferred: Int,
      jdk:           Boolean,
      publicKey:     Data )           // 1312 raw bytes (all-zero disables upgrades)
    ( using Tactic[Assembler.Error] )
  :   Data =

    val bytes: scala.Array[Byte] = runner.readable.toArray

    val magicOffset: Int =
      var found: Int = -1
      var i = 0

      while found < 0 && i <= bytes.length - MagicMarker.length do
        var matches = true
        var j = 0

        while matches && j < MagicMarker.length do
          if bytes(i + j) != MagicMarker(j) then matches = false
          j += 1

        if matches then found = i
        i += 1

      if found < 0
      then abort(Assembler.Error(m"The runner binary does not contain the ETHRCFG marker"))

      found

    val configOffset: Int = magicOffset + MagicMarker.length
    val keyBytes: scala.Array[Byte] = publicKey.readable.toArray

    // Write the 24-byte metadata region.
    val metaBuf = jnio.ByteBuffer.wrap(bytes, configOffset, 24).nn
    metaBuf.order(jnio.ByteOrder.LITTLE_ENDIAN).nn
    metaBuf.putLong(buildId)
    metaBuf.putShort(javaMinimum.toShort)
    metaBuf.putShort(javaPreferred.toShort)
    metaBuf.put((if jdk then 1 else 0).toByte)
    while metaBuf.hasRemaining do metaBuf.put(0.toByte)

    // Write the 1312-byte public-key region at offset 32 within the ETHRCFG block (8
    // magic + 24 metadata). The signature slot at offset 1344 stays zero — populated
    // later by the signer when shipped as an upgrade.
    jl.System.arraycopy(keyBytes, 0, bytes, configOffset + 24, PublicKeyLength)

    Array.from(bytes.iterator): Array[Byte]^{}


  def assemble
    ( runner:        Data, // bare runner binary
      jarFile:       Path on Linux, // application JAR appended at EOF
      output:        Path on Linux,
      platformLabel: Text,
      buildId:       Long,
      javaMinimum:   Int,
      javaPreferred: Int,
      jdk:           Boolean,
      publicKey:     Data )           // 1312 raw bytes (all-zero disables upgrades)
    ( using WorkingDirectory )
    // Explicit `using` evidence instead of stacked `raises` sugar: the handle-loan lambdas
    // in the body cannot cross the nested context-function results the sugar desugars to
    // (the stacked-raises convention; see rep/DECISIONS.md).
    ( using Tactic[Assembler.Error], Tactic[Io.Error], Tactic[Truncation.Error] )
  :   Unit =

    val isWindows: Boolean = platformLabel.starts(t"windows")
    val patched: Data = patch(runner, buildId, javaMinimum, javaPreferred, jdk, publicKey)

    // Assemble into a hidden sibling and rename it over the output at the end. The
    // output may be a live launcher (a `-Dbuild.executable` rebuild names the installed
    // path directly), and mutating it in place both corrupts the zip a resident daemon
    // is running from and — on macOS, whose code-signing cache is per-vnode — poisons
    // the inode so that later execs are SIGKILLed. The rename yields a fresh inode and
    // an always-complete file at the output path.
    val temporary: Path on Linux = unsafely:
      val directory: Path on Linux = output.parent.assume
      t"${directory.encode}/.${output.name}.tmp".as[Path on Linux]

    temporary.open[File](Write, OpenFlag.Create, OpenFlag.Truncate)
      ( file.write(Chain(patched)) )

    if platformLabel.starts(t"macos") then
      if !isWindows then temporary.executable() = true
      safely(mute[Exec.Event](sh"codesign --sign - --force $temporary".exec[Exit]()))

    // Measured from the file rather than taken as `patched.length`: on macOS `codesign` has just
    // rewritten the stub, adding a signature blob, so the prefix the JAR is about to sit behind is
    // longer than the bytes `patch` returned.
    val prefixSize: Bytes = temporary.filesize()

    // Two sequential opens rather than one nested inside the other's lambda (the inner
    // open's evidence would mint fresh roots inside the outer handle's loan that cannot
    // unify with it), and a direct append-mode open rather than `Eof` (whose two-evidence
    // dependent `Result` chain has the same root problem). The read is strict (`to(List)`)
    // so nothing reads the closed handle.
    val chunks: List[Data] = jarFile.open[File]()(file.reader().to[List])

    temporary.open[File](Write, OpenFlag.Create, OpenFlag.Append)
      ( file.write(chunks.to[Chain]) )

    // Appending the JAR to the stub shifts every byte of it by the stub's length. ZIP offsets are
    // relative, so a reader recovers that shift by itself — except for the ZIP64 locator's pointer,
    // which is physical, and which only exists at all once the JAR holds more than 0xffff entries.
    // Left stale, the JVM declines to open the executable and the daemon dies mute (#1680).
    mitigate:
      case Zip.Error(_) =>
        Assembler.Error(m"The appended JAR's ZIP64 metadata could not be rebased")

    . protect:
        Zipfile.rebase(temporary, prefixSize.long)

    if !isWindows then temporary.executable() = true

    import filesystemOptions.moveAtomically
    import filesystemOptions.deleteOnlyEmpty
    temporary.moveTo(output)
    ()

  // AssemblyError → Assembler.Error
  case class Error(detail: Message)(using Diagnostics) extends fulminate.Error(detail)
