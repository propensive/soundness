// The out-of-band Scala Native link driver, run in its own JVM by `soundness.nativelink.binary`
// (mirroring `tests/wasm/linker`, which links the Wasm component). It depends on the Scala Native
// `tools` library — the reachability/optimisation/LLVM/clang pipeline — which mill's Scala toolchain
// does not model, so it is driven directly here. Arguments: the base directory, the fully-qualified
// main class, and the classpath (NIR + native runtime) as a single path-separated string.
package nativelink

import scala.scalanative.build.*
import scala.scalanative.util.Scope

import java.io.File
import java.nio.file.{Files, Paths}
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

@main def link(baseDir: String, mainClass: String, classpath: String): Unit =
  given ExecutionContext = ExecutionContext.global
  given Scope = Scope.forever

  val entries =
    classpath.split(File.pathSeparator).nn.toSeq.map(p => Paths.get(p).nn).filter(Files.exists(_))

  // coaxial's TLS backend declares `@link("ssl")`/`@link("crypto")`, so the final clang link gets
  // `-lssl -lcrypto` whenever those externs are reachable; on macOS the Homebrew OpenSSL keg is
  // not on the default search path, so add it when present (Linux finds the system libssl as-is).
  val librarySearchPaths =
    Seq("/opt/homebrew/opt/openssl@3/lib", "/usr/local/opt/openssl@3/lib")
      .filter(path => Files.exists(Paths.get(path)))
      .map("-L" + _)

  val config = Config.empty
    .withBaseDir(Paths.get(baseDir).nn.toAbsolutePath.nn)
    .withMainClass(Some(mainClass))
    .withClassPath(entries)
    .withModuleName("soundness-native")
    .withCompilerConfig(
      NativeConfig.empty
        .withClang(Discover.clang())
        .withClangPP(Discover.clangpp())
        .withGC(GC.immix)
        .withMode(Mode.debug)
        .withLTO(LTO.none)
        .withLinkingOptions(librarySearchPaths)
        .withBaseName("soundness-native"))

  val artifact = Await.result(Build.build(config), Duration.Inf)
  System.out.nn.println(s"BINARY:$artifact")
