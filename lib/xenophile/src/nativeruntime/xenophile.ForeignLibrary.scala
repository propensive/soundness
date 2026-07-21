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
package xenophile

import scala.scalanative.posix.dlfcn.*
import scala.scalanative.unsafe.*

import anticipation.*
import fulminate.*
import gossamer.*

// The Scala Native twin of the JVM (Panama) `ForeignLibrary`. Both now load libraries by path at
// *runtime* (`dlopen`) and resolve symbols from the loaded set, so a native FFI binary has no
// link-time dependency on the foreign library — exactly like the JVM, whose `SymbolLookup` also
// loads by path. This is what makes the identical FFI source safe on both platforms: a library
// missing at its declared paths fails loudly at `register` (a clear panic naming the paths tried),
// not with an undefined symbol at link time or — worse — a NULL-pointer crash on first call, which
// is what a `dlopen(null)`-only resolution silently risked when nothing else statically linked it.
//
// `NativeInvoke` routes every symbol through `resolve`.
object ForeignLibrary:
  // Handles opened by `register`, newest first; searched before the process-default lookup.
  // Registration happens in a provider's object initializer (before any worker thread that would
  // call `resolve`), so a plain `var` needs no synchronisation.
  private var handles: List[CVoidPtr] = Nil

  // The default lookup (`dlopen(null)`): the main image and everything statically linked or already
  // loaded — where libc/libm symbols (`getpid`, `malloc`, …) resolve without any registration.
  private lazy val self: CVoidPtr = dlopen(null.asInstanceOf[CString], RTLD_NOW)

  // Loads the first of `paths` that `dlopen` resolves and keeps the handle for the life of the
  // process (foreign libraries are never unloaded); panics if none load — the native counterpart of
  // the JVM `SymbolLookup.libraryLookup` failure.
  def register(paths: Text*): Unit =
    def attempt(remaining: List[Text]): CVoidPtr = remaining match
      case path :: rest =>
        val handle = Zone.acquire: zone =>
          dlopen(toCString(path.s)(using zone), RTLD_NOW)

        if handle == null then attempt(rest) else handle

      case Nil =>
        val tried = paths.map(_.s).mkString(", ").tt
        panic(m"xenophile: no native library could be loaded from $tried")

    handles = attempt(paths.to(List)) :: handles

  // Resolves a C symbol from the registered handles, then the default lookup; panics if unbound —
  // a symbol whose library was neither `register`ed nor statically linked (the failure that a
  // `dlopen(null)`-only resolution would have turned into a NULL-pointer call).
  def resolve(symbol: CString): CVoidPtr =
    def search(remaining: List[CVoidPtr]): CVoidPtr = remaining match
      case handle :: rest =>
        val pointer = dlsym(handle, symbol)
        if pointer == null then search(rest) else pointer

      case Nil =>
        val pointer = dlsym(self, symbol)

        if pointer == null then
          val name = fromCString(symbol).tt
          panic(m"xenophile: unresolved native symbol $name (is its library registered?)")
        else pointer

    search(handles)
