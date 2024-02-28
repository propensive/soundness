/*
    Umbrageous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package umbrageous

import dotty.tools.dotc.*, ast.*, core.*, Contexts.*, Decorators.*, Names.*, plugins.*

class UmbrageousPlugin() extends StandardPlugin:
  val name: String = "umbrageous"
  override val description: String = "shades packages during compilation"
  def init(options: List[String]): List[PluginPhase] = List(UmbrageousTransformer(options))

class UmbrageousTransformer(options: List[String]) extends PluginPhase:
  val phaseName: String = "shade"
  override val runsAfter: Set[String] = Set("parser")
  override val runsBefore: Set[String] = Set("typer")

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree =
    import untpd.*
    val untpdTree = ctx.compilationUnit.untpdTree
    
    val prefixes: List[(String, String)] =
      options.flatMap: opt =>
        opt.split(":").nn.to(List).map(_.nn) match
          case List(from, to) => List((from, to))
          case other =>
            report.warning(s"umbrageous: the option '$opt' is not a valid shading mapping; "+
                "please specify a mapping of the form, '<package>:<new-prefix>'"); Nil

    object transformer extends UntypedTreeMap:
      private def rewritePackage
          (tree: Ident | Select, fqn: String, defs: List[Tree], select: Select => Select): PackageDef =
        tree match
          case Ident(name) =>
            val pkg = name.decode.toString+"."+fqn
            val prefixes2 = prefixes.filter { (k, v) => pkg == k || pkg.startsWith(k+".") }.sortBy(_(0).length)
              
            val ident = prefixes2.lastOption.fold(tree): (k, v) =>
              select(Select(Ident(v.toTermName), name))
            
            val imports = prefixes2.lastOption.fold(prefixes) { (k, v) => prefixes.filter(_(0) != k) }.map:
              case (_, prefix) =>
                Import(Ident(prefix.toTermName), List(ImportSelector(Ident(StdNames.nme.WILDCARD))))
            
            PackageDef(ident, imports ::: defs)
          
          case Select(pkg: (Ident | Select), name) =>
            rewritePackage(pkg, s"${name.decode}.$fqn", defs, Select(_, name))
          
          case _ => ???

      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case PackageDef(name: (Ident | Select), defs) => rewritePackage(name, "", defs, identity)
          case _                                        => super.transform(tree)
    
    ctx.compilationUnit.untpdTree = transformer.transform(untpdTree)
    super.transformUnit(tree)
