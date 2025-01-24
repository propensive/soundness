/*
    Anthology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anthology

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import hellenism.*
import rudiments.*
import spectacular.*

import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.CompilerConfiguration

case class Kotlinc
   [CompilerType <: KotlinVersions]
   (sources: Map[Text, Text], classpath: LocalClasspath, out: Path):
  def apply()(using SystemProperties): List[Diagnostic] raises KotlinError =
    val compiler = K2JVMCompiler()
    val configuration = CompilerConfiguration().apply: _ =>
      put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, MessageCollector.NONE)

    compiler.exec
     (MessageCollector.NONE, configuration, kotlin.collections.CollectionsKt.listOf(code))
