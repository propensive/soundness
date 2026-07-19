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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package anthology

object Artifact:
  // Classfile universe: an executable JAR, bound to the JDK.
  sealed trait Jar extends Artifact

  // Classfile universe: Dalvik executable bytecode, bound to the Android runtime. Nameable but
  // not yet producible: no `Linkage` exists for it.
  sealed trait Dex extends Artifact

  object Js:
    // How a JavaScript host consumes the artifact: an ECMAScript module, a CommonJS module, or
    // a plain script. The module system is part of the artifact's binding contract, so it is
    // part of its type.
    type Modules = "es" | "commonjs" | "script"

  // Sjsir universe: JavaScript, bound to a JavaScript host (a browser's DOM or a runtime such
  // as Node) through the given module system.
  sealed trait Js[+module <: Js.Modules] extends Artifact

  // Sjsir universe: a core WebAssembly module with JavaScript glue, bound to a JavaScript host.
  sealed trait Wasm extends Artifact

  object Wasi:
    // WASI interface versions: 0.1 (preview 1) is a flat, libc-style syscall ABI on core
    // modules; 0.2 is the component model, with imports and exports described by WIT; 0.3 adds
    // native asynchrony. Versions determine the artifact's ABI, so they are part of its type.
    type Versions = 0.1 | 0.2 | 0.3

  // A standalone WebAssembly artifact bound to a version of the WASI system interface.
  sealed trait Wasi[+version <: Wasi.Versions] extends Artifact

  // Nir universe: a machine-code executable, bound to an operating system's C library; the
  // platform and architecture are selected at link time as a `Triple`.
  sealed trait Binary extends Artifact

  // The artifacts linked from `.sjsir` by the Scala.js linker, sharing one options family.
  type Sjs = Js[Js.Modules] | Wasm | Wasi[Wasi.Versions]

// A linked product of a compilation: the tier users choose from. Each artifact is producible
// from exactly one universe, witnessed by `Provenance`.
sealed trait Artifact
