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
┃    Soundness, version 0.49.0.                                                                    ┃
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

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import dotty.tools.dotc as dtd
import dotty.tools.dotc.reporting.*

import language.adhocExtensions

object scalacOptions:
  val newSyntax = Scalac.Option[Scalac.Versions](t"-new-syntax")
  def sourceFuture = Scalac.Option[Scalac.Versions](t"-source", t"future")
  val experimental = Scalac.Option[3.4 | 3.5 | 3.6 | 3.7](t"-experimental")

  object warnings:
    val feature = Scalac.Option[Scalac.Versions](t"-feature")
    val deprecation = Scalac.Option[Scalac.Versions](t"-deprecation")
    val implausiblePatterns = Scalac.Option[3.3 | 3.4 | 3.5 | 3.6](t"-Wimplausible-patterns")
    val enumCommentDiscard = Scalac.Option[3.4 | 3.5 | 3.6](t"-Wenum-comment-discard")
    val unstableInlineAccessors = Scalac.Option[3.4 | 3.5 | 3.6](t"-WunstableInlineAccessors")
    val nonUnitStatement = Scalac.Option[3.4 | 3.5 | 3.6](t"-Wnonunit-statement")
    val valueDiscard = Scalac.Option[3.4 | 3.5 | 3.6](t"-Wvalue-discard")

    def unused[version <: Scalac.Versions](selection: Unused[version]) =
      val option = selection.absolve match
        case Unused.All              => t"-Wunused:all"
        case Unused.None             => t"-Wunused:none"
        case Unused.Subset(features) => features.map(_.name).join(t"-Wunused:", t",", t"")

      Scalac.Option[version](option)

    object lint:
      val privateShadow = Scalac.Option[3.4 | 3.5 | 3.6](t"-Wshadow:private-shadow")
      val typeParameterShadow = Scalac.Option[3.4 | 3.5 | 3.6](t"-Wshadow:type-parameter-shadow")

  object internal:
    val requireTargetName = Scalac.Option[Scalac.Versions](t"-Yrequire-targetName")
    val safeInit = Scalac.Option[Scalac.Versions](t"-Ysafe-init")
    val explicitNulls = Scalac.Option[Scalac.Versions](t"-Yexplicit-nulls")
    val checkPatterns = Scalac.Option[Scalac.Versions](t"-Ycheck-all-patmat")
    val ccNew = Scalac.Option[3.4 | 3.5 | 3.6](t"-Ycc-new")
    val ccDebug = Scalac.Option[3.5 | 3.6](t"-Ycc-debug")
    val ccLog = Scalac.Option[3.5 | 3.6](t"-Ycc-log")

  object advanced:
    def maxInlines(n: Int): Scalac.Option[Scalac.Versions] = Scalac.Option(t"-Xmax-inlines", n.show)

  object language:
    object experimental:
      val clauseInterleaving =
        Scalac.Option[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.clauseInterleaving")

      val givenLoopPrevention =
        Scalac.Option[3.4 | 3.5 | 3.6](t"-language:experimental.givenLoopPrevention")

      val fewerBraces =
        Scalac.Option[3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.fewerBraces")

      val into = Scalac.Option[3.4 | 3.5 | 3.6](t"-language:experimental.into")

      val relaxedExtensionImports =
        Scalac.Option[3.3](t"-language:experimental.relaxedExtensionImports")

      val erasedDefinitions = Scalac.Option[Scalac.Versions]
                               (t"-language:experimental.erasedDefinitions")

      val saferExceptions =
        Scalac.Option[3.2 | 3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.saferExceptions")

      val namedTypeArguments =
        Scalac.Option[Scalac.Versions](t"-language:experimental.namedTypeArguments")

      val pureFunctions =
        Scalac.Option[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.pureFunctions")

      val captureChecking =
        Scalac.Option[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.captureChecking")

      val modularity = Scalac.Option[3.5 | 3.6](t"-language:experimental.modularity")
      val namedTuples = Scalac.Option[3.5 | 3.6](t"-language:experimental.namedTuples")

      val genericNumberLiterals =
        Scalac.Option[Scalac.Versions](t"-language:experimental.genericNumberLiterals")

      val betterMatchTypeExtractors =
        Scalac.Option[3.5 | 3.6](t"-language:experimental.betterMatchTypeExtractors")

      val quotedPatternsWithPolymorphicFunctions =
        Scalac.Option[3.6](t"-language:experimental.quotedPatternsWithPolymorphicFunctions")

      val betterFors = Scalac.Option[3.6](t"-language:experimental.betterFors")


extension (companion: Notice.type)
  def apply(diagnostic: Diagnostic): Notice =
    val importance: Importance = Importance.fromOrdinal(diagnostic.level)
    val file: Text = diagnostic.position.map(_.nn.source.nn.name.nn.tt).nn.orElse(t"unknown").nn
    val message: Text = diagnostic.message.tt

    diagnostic.position.map: position =>
      position.nn.pipe: position =>
        val codeRange =
          CodeRange
           (position.startLine.nn,
            position.startColumn.nn,
            position.endLine.nn,
            position.endColumn.nn)

        Notice(importance, file, message, codeRange)

    . nn
    . orElse(Notice(importance, file, message, Unset))
    . nn
