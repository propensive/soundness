package wasme2e

import org.scalajs.linker.*
import org.scalajs.linker.interface.*
import org.scalajs.logging.*

import java.nio.file.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

// Links `.sjsir` classpath entries into a Wasm component with the scala-wasm fork linker.
// Driven by the `wasm.component` mill task; a standalone driver because mill's Scala.js worker
// cannot express the fork-only `WasmFeatures` (WIT directory/world) configuration.
//
// Arguments: <output-dir> <wit-dir> <wit-world> <classpath entries...>
object Link:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get(args(0)).nn
    val witDir = args(1)
    val witWorld = args(2)
    val classpath: Seq[Path] = args.drop(3).toSeq.map(Paths.get(_).nn)

    Files.createDirectories(outDir)

    val config = StandardConfig()
      .withModuleKind(ModuleKind.WasmComponent)
      .withExperimentalUseWebAssembly(true)
      .withESFeatures(_.withESVersion(ESVersion.ES2022))
      .withWasmFeatures(_.withWitDirectory(Some(witDir)).withWitWorld(Some(witWorld)))
      .withCheckIR(true)
      .withOptimizer(true)

    val linker = StandardImpl.linker(config)
    val cache = StandardImpl.irFileCache().newCache
    val (containers, _) = Await.result(PathIRContainer.fromClasspath(classpath), 300.seconds)
    val irFiles = Await.result(cache.cached(containers), 300.seconds)

    val output = PathOutputDirectory(outDir)
    val logger = new ScalaConsoleLogger(Level.Info)
    Await.result(linker.link(irFiles, Nil, output, logger), 1800.seconds)
    println(s"wasm-e2e: linked $outDir/main.wasm")
