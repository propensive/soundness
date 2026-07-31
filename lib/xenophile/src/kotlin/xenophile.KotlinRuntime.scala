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

import java.lang.invoke as jli
import java.util.concurrent as juc

// The one reflective edge of the Kotlin ecosystem: Kotlin's synthetic `$default` bridges (the
// carriers of default-argument values) are marked ACC_SYNTHETIC, which the compiler's classfile
// reader omits from the symbol table, so no direct call to one can be emitted. Instead, the
// bridge is resolved once per call shape into a cached `MethodHandle` and invoked through it —
// the same linkage-time trade the Panama backend makes for `dlsym`.
object KotlinRuntime:
  private val handles: juc.ConcurrentHashMap[String, jli.MethodHandle] =
    juc.ConcurrentHashMap()

  // Unwraps a facade returned from an adapted lambda, so Kotlin/Java receives the raw value.
  def dispatch(result: Any | Null): Object | Null = result match
    case facade: Facade => facade.raw.asInstanceOf[Object | Null]
    case other          => other.asInstanceOf[Object | Null]

  // The invocation handler behind a functional-interface proxy: `equals`/`hashCode`/`toString`
  // answer locally; everything else (the single abstract method) forwards to the lambda.
  def forwarder(handler: (scala.Array[Object | Null] | Null) => Object | Null)
  :   java.lang.reflect.InvocationHandler^{handler} =

    (proxy, method, arguments) =>
      method.nn.getName match
        case "equals"   => java.lang.Boolean.valueOf(proxy.nn eq arguments.nn(0))
        case "hashCode" => java.lang.Integer.valueOf(java.lang.System.identityHashCode(proxy))
        case "toString" => s"<function proxy>"
        case _          => handler(arguments)

  def invokeDefault(owner: Class[?], name: String, arguments: scala.Array[Any | Null]): Any | Null =
    val key = s"${owner.getName}#$name#${arguments.length}"

    val handle = handles.computeIfAbsent(key, _ =>
      owner.getMethods.nn.find: method =>
        method.nn.getName == name && method.nn.getParameterCount == arguments.length

      . map: method => jli.MethodHandles.lookup.nn.unreflect(method.nn).nn
      . getOrElse(throw IllegalStateException(s"xenophile: no $name bridge on ${owner.getName}")))

    // The `java.util.List` overload, not the varargs one: an array splice cannot flow into
    // the pure varargs formal under separation checking.
    val argumentList = java.util.ArrayList[AnyRef]()
    arguments.foreach { argument => argumentList.add(argument.asInstanceOf[AnyRef]); () }
    handle.nn.invokeWithArguments(argumentList)
