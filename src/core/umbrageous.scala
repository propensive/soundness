package umbrageous

import dotty.tools.dotc.*, util.*, ast.*, core.Contexts.*, core.Decorators.*, core.Symbols.*, plugins.*

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
        opt.split(":").toList match
          case List(from, to) => List((from, to))
          case other          => report.warning(s"umbrageous: the option '$opt' is not a valid shading mapping; please specify a mapping of the form, '<package>:<new-prefix>'"); Nil

    object transformer extends UntypedTreeMap:
      private def rewritePackage(tree: Ident | Select, fqn: String = ""): Ident | Select = tree match
        case Ident(name) =>
          val pkgName = name.decode.toString+"."+fqn
          prefixes
            .filter: (k, v) =>
              pkgName == k || pkgName.startsWith(k+".")
            .sortBy(_(0).length)
            .lastOption
            .fold(tree): (k, v) =>
              Select(Ident(v.toTermName), name)
        
        case Select(pkg: (Ident | Select), name) =>
          Select(rewritePackage(pkg, name.decode.toString+"."+fqn), name)

      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case PackageDef(name: (Ident | Select), defs) => PackageDef(rewritePackage(name), defs)
          case _                                        => super.transform(tree)
    
    ctx.compilationUnit.untpdTree = transformer.transform(untpdTree)
    super.transformUnit(tree)
