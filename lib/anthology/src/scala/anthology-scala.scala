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
┃    Soundness, version 0.32.0.                                                                    ┃
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

package scalacOptions:
  val newSyntax = ScalacOption[Scalac.All](t"-new-syntax")
  def sourceFuture = ScalacOption[Scalac.All](t"-source", t"future")
  val experimental = ScalacOption[3.4 | 3.5 | 3.6](t"-experimental")

  package warnings:
    val feature = ScalacOption[Scalac.All](t"-feature")
    val deprecation = ScalacOption[Scalac.All](t"-deprecation")
    val implausiblePatterns = ScalacOption[3.3 | 3.4 | 3.5 | 3.6](t"-Wimplausible-patterns")
    val enumCommentDiscard = ScalacOption[3.4 | 3.5 | 3.6](t"-Wenum-comment-discard")
    val unstableInlineAccessors = ScalacOption[3.4 | 3.5 | 3.6](t"-WunstableInlineAccessors")
    val nonUnitStatement = ScalacOption[3.4 | 3.5 | 3.6](t"-Wnonunit-statement")
    val valueDiscard = ScalacOption[3.4 | 3.5 | 3.6](t"-Wvalue-discard")

    def unused[version <: Scalac.All](selection: Unused[version]) =
      val option = selection.absolve match
        case Unused.All              => t"-Wunused:all"
        case Unused.None             => t"-Wunused:none"
        case Unused.Subset(features) => features.map(_.name).join(t"-Wunused:", t",", t"")

      ScalacOption[version](option)

    package lint:
      val privateShadow = ScalacOption[3.4 | 3.5 | 3.6](t"-Wshadow:private-shadow")
      val typeParameterShadow = ScalacOption[3.4 | 3.5 | 3.6](t"-Wshadow:type-parameter-shadow")

  package internal:
    val requireTargetName = ScalacOption[Scalac.All](t"-Yrequire-targetName")
    val safeInit = ScalacOption[Scalac.All](t"-Ysafe-init")
    val explicitNulls = ScalacOption[Scalac.All](t"-Yexplicit-nulls")
    val checkPatterns = ScalacOption[Scalac.All](t"-Ycheck-all-patmat")
    val ccNew = ScalacOption[3.4 | 3.5 | 3.6](t"-Ycc-new")
    val ccDebug = ScalacOption[3.5 | 3.6](t"-Ycc-debug")
    val ccLog = ScalacOption[3.5 | 3.6](t"-Ycc-log")

  package advanced:
    def maxInlines(n: Int): ScalacOption[Scalac.All] = ScalacOption(t"-Xmax-inlines", n.show)

  package language:
    package experimental:
      val clauseInterleaving =
        ScalacOption[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.clauseInterleaving")

      val givenLoopPrevention =
        ScalacOption[3.4 | 3.5 | 3.6](t"-language:experimental.givenLoopPrevention")

      val fewerBraces =
        ScalacOption[3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.fewerBraces")

      val into = ScalacOption[3.4 | 3.5 | 3.6](t"-language:experimental.into")

      val relaxedExtensionImports =
        ScalacOption[3.3](t"-language:experimental.relaxedExtensionImports")

      val erasedDefinitions = ScalacOption[Scalac.All](t"-language:experimental.erasedDefinitions")

      val saferExceptions =
        ScalacOption[3.2 | 3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.saferExceptions")

      val namedTypeArguments =
        ScalacOption[Scalac.All](t"-language:experimental.namedTypeArguments")

      val pureFunctions =
        ScalacOption[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.pureFunctions")

      val captureChecking =
        ScalacOption[3.3 | 3.4 | 3.5 | 3.6](t"-language:experimental.captureChecking")

      val modularity = ScalacOption[3.5 | 3.6](t"-language:experimental.modularity")
      val namedTuples = ScalacOption[3.5 | 3.6](t"-language:experimental.namedTuples")

      val genericNumberLiterals =
        ScalacOption[Scalac.All](t"-language:experimental.genericNumberLiterals")

      val betterMatchTypeExtractors =
        ScalacOption[3.5 | 3.6](t"-language:experimental.betterMatchTypeExtractors")

      val quotedPatternsWithPolymorphicFunctions =
        ScalacOption[3.6](t"-language:experimental.quotedPatternsWithPolymorphicFunctions")

      val betterFors = ScalacOption[3.6](t"-language:experimental.betterFors")


extension (companion: Notice.type)
  def apply(diagnostic: Diagnostic): Notice =
    val importance: Importance = Importance.fromOrdinal(diagnostic.level)
    val file: Text = diagnostic.position.map(_.nn.source.nn.name.nn.tt).nn.orElse(t"unknown").nn
    val message: Text = diagnostic.message.tt

    diagnostic.position.map: position =>
      position.nn.pipe: position =>
        val codeRange =
          CodeRange
            ( position.startLine.nn,
              position.startColumn.nn,
              position.endLine.nn,
              position.endColumn.nn )

        Notice(importance, file, message, codeRange)

    . nn
    . orElse(Notice(importance, file, message, Unset))
    . nn
