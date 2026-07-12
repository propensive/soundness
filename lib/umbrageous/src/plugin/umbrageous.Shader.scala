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
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package umbrageous

import dotty.tools.dotc.*, ast.*, core.*, Contexts.*, Decorators.*, plugins.*

class Shader(options: List[String]) extends PluginPhase:
  val phaseName: String = "shade"
  override val runsAfter: Set[String] = Set("parser")
  override val runsBefore: Set[String] = Set("typer")

  override def transformUnit(tree: tpd.Tree)(using context: Context): tpd.Tree =
    import untpd.*

    val unit = context.compilationUnit

    val prefixes: List[(String, String)] =
      options.flatMap: opt =>
        opt.split(":").nn.to(List).map(_.nn) match
          case List(from, to) => List((from, to))

          case other =>
            report.warning(s"umbrageous: the option '$opt' is not a valid shading mapping; "+
              "please specify a mapping of the form, '<package>:<new-prefix>'"); Nil

    object transformer extends UntypedTreeMap:
      def nameToSegments(tree: Tree): List[String] = tree match
        case Ident(name)        => List(name.as.toString)
        case Select(qual, name) => nameToSegments(qual) :+ name.as.toString
        case _                  => Nil

      def peelPackageDefs(name: Tree, stats: List[Tree]): (List[String], List[Tree]) =
        stats match
          case List(inner: PackageDef) =>
            val (rest, innermostStats) = peelPackageDefs(inner.pid, inner.stats)
            (nameToSegments(name) ++ rest, innermostStats)

          case _ =>
            (nameToSegments(name), stats)

      def splitDots(prefix: String): List[String] =
        prefix.split("\\.").nn.map(_.nn).toList

      def segmentsToTree(segments: List[String]): RefTree = segments match
        case Nil          => Ident(StdNames.nme.EMPTY_PACKAGE)

        case head :: tail =>
          tail.foldLeft(Ident(head.toTermName): RefTree): (acc, segment) =>
            Select(acc, segment.toTermName)

      def matchPrefix(segments: List[String]): Option[(String, String)] =
        val fqn = segments.mkString(".")

        prefixes
        . filter: (k, _) => fqn == k || fqn.startsWith(k+".")
        . sortBy(_(0).length)
        . lastOption

      def rewriteRootRef(tree: Tree): Option[Tree] =
        def collect(t: Tree, acc: List[String]): Option[List[String]] = t match
          case Ident(name) if name == StdNames.nme.ROOTPKG =>
            Some(acc)

          case Select(qual, name) =>
            collect(qual, name.as.toString :: acc)

          case _ =>
            None

        collect(tree, Nil).flatMap: segments =>
          if segments.isEmpty then None else matchPrefix(segments).map: (_, shadePrefix) =>
            (splitDots(shadePrefix) ++ segments).foldLeft(Ident(StdNames.nme.ROOTPKG): Tree):
              (acc, segment) => Select(acc, segment.toTermName)

      override def transform(tree: Tree)(using Context): Tree = tree match
        case PackageDef(name: (Ident | Select), defs) =>
          val (segments, innerStats) = peelPackageDefs(name, defs)
          val matched = matchPrefix(segments)

          val nameTree =
            matched.fold(segmentsToTree(segments)): (_, v) =>
              segmentsToTree(splitDots(v) ++ segments)

          val imports =
            matched.fold(prefixes) { (k, _) => prefixes.filter(_(0) != k) }.map: (_, prefix) =>
              Import
                ( segmentsToTree(splitDots(prefix)),
                  List(ImportSelector(Ident(StdNames.nme.WILDCARD))) )

          PackageDef(nameTree, imports ::: innerStats.map(transform))

        case _ =>
          rewriteRootRef(tree) match
            case Some(rewritten) => rewritten
            case None            => super.transform(tree)

    unit.untpdTree = transformer.transform(unit.untpdTree)
    super.transformUnit(tree)
