package zeppelin

import soundness.*

import strategies.throwUnsafely
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import logging.silentLogging
import systems.javaSystem

// ── capturing-raises ───────────────────────────────────────────────────────────────────────────
// `Zipfile.write` is `… raises ZipError` = `Tactic[ZipError]^ ?=> …` (contingency). Calling it inside
// a helper `def`, with the requirement satisfied by the ambient `strategies.throwUnsafely`, mints a
// `ThrowTactic[Exception,_]^{any}` whose capability lives in the enclosing function and cannot flow
// into the method's fresh `raises` existential `^{any²}`:
//   "capability `any` … cannot flow into capture set {any²} … not visible from any² in type raises".
// WHAT WE WANT: an ambient error strategy should satisfy the `raises ZipError` requirement.
// NOTE (tested): NOT fixable at the call site — explicit `raises`, `inline`, `unsafely{}` all fail;
// removing the `^` from `raises` in contingency-core breaks turbulence's async `await`/`safely`.
// Compile with `rep/compile.sh capturing-raises` (dotc). WHERE (7 suites): zeppelin, cordillera,
// burdock, perihelion, surveillance, ziggurat, contingency.

object CapturingRaises:
  def writeZip(workDir: Path on Linux, name: Text, entries: List[Zip.Entry]): Path on Linux =
    val path = workDir/name
    Zipfile.write(path)(entries)
    path
