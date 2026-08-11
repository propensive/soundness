package wasme2e

import java.nio.file as jnf

import anticipation.*
import embarcadero.*
import gossamer.*
import rudiments.*
import turbulence.*
import vacuous.*
import xenophile.*
import zephyrine.*

// Wraps a linked `.wasm` component in a Wasm OCI Artifact and writes it as an `oci-archive` tar.
// Driven by the `wasm.image`/`wasm.httpImage` mill tasks; a standalone driver for the same reason
// the linker is one — it needs Soundness modules on its classpath, which the build itself does not
// have. `anthology.oci` models this properly as a toolchain edge, following the component link;
// this driver starts from an already-linked component, so the e2e can package exactly the bytes
// the scenarios ran.
//
// The `component` metadata is read from the WIT world with `Wit.Dialect.worlds`, so the artifact
// states which host capabilities the workload needs without anything disassembling the component.
//
// Arguments: <component.wasm> <wit-dir> <world> <out.tar>
object Package:
  private val incomingHandler = t"wasi:http/incoming-handler@0.2.0"
  private val proxy = t"wasi:http/proxy@0.2.0"

  def main(args: scala.Array[String]): Unit =
    val componentPath = jnf.Paths.get(args(0)).nn
    val witDir = jnf.Paths.get(args(1)).nn
    val world = args(2).tt
    val out = jnf.Paths.get(args(3)).nn

    val component: Data = proscenium.Array.unsafeFrozen(jnf.Files.readAllBytes(componentPath).nn)

    // Only the top level of the WIT directory is searched: `deps/` holds the packages the world
    // draws on, and none of them declares the world being linked.
    val files = witDir.toFile.nn.listFiles.nn

    def search(index: Int): Optional[Wit.Dialect.World] =
      if index >= files.length then Unset else
        val file = files(index).nn

        if !file.getName.nn.endsWith(".wit") then search(index + 1) else
          val text = String(jnf.Files.readAllBytes(file.toPath).nn, "UTF-8").tt
          Wit.Dialect.worlds(text).stdlib.get(world).optional.or(search(index + 1))

    val found = search(0).or:
      System.err.nn.println(s"wasm-e2e: no world `${world.s}` under ${witDir}")
      sys.exit(1)

    val image =
      Image.wasm
        ( component,
          exports = found.exports,
          imports = found.imports,
          target  = if found.exports.has(incomingHandler) then proxy else Unset )

    val archive = image.archive.source[Data].memoize
    jnf.Files.write(out, archive.mutable(using Unsafe))

    val layer = image.manifest.layers.stdlib.head

    // `t"…"` rather than `s"…"`: a `MediaType` renders through its `Showable`, where `toString`
    // would print the parsed case-class structure.
    def line(text: Text): Unit = println(text.s)

    line(t"wasm-e2e: packaged ${out.toString.tt} (${archive.length} bytes)")

    line(t"wasm-e2e:   config    ${image.manifest.config.mediaType} ${image.configDescriptor.digest}")

    line(t"wasm-e2e:   layer     ${layer.mediaType} ${layer.digest} (${layer.size} bytes)")

    val counts = t"${found.imports.stdlib.size} imports, ${found.exports.stdlib.size} exports"
    line(t"wasm-e2e:   world     ${world} ($counts)")
