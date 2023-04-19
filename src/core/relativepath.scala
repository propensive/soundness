package serpentine

import gossamer.*
import rudiments.*

import language.experimental.captureChecking

object Relative:
  @targetName("RelativeRoot")
  object `?` extends Relative(0, Nil)
  
  given [RelativeType <: Relative]: PathBounds[RelativeType] = isRoot => ()
  
  given Show[Relative] = rel =>
    if rel == ? then t"."
    else (List.fill(rel.ascent)(t"..") ::: rel.elements.reverse.map(_.show)).join(t"", t"/", t"")

  given hierarchy: Hierarchy[Relative] with
    type ForbiddenType = '/' | ".." | ""
    
    def separator: Text = t"/"
    def prefix(root: Relative): Text = t"./"
    def root(path: Relative): Relative = ?
    def elements(path: Relative): List[PathElement[ForbiddenType]] = path.elements.map(PathElement(_))
    
    def child(base: Relative, element: PathElement[ForbiddenType]): Relative =
      base.copy(elements = element.show :: base.elements)
    
    def parent(path: Relative): Relative =
      if path.elements.isEmpty then Relative(path.ascent + 1, Nil)
      else Relative(path.ascent, path.elements.tail)

  def parse(text: Text): Relative throws PathError =
    def recur(text: Text, ascent: Int): Relative =
      if text == t"." then ?
      else if text == t".." then Relative(ascent + 1, Nil)
      else if text.starts(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/").filter(_ != t"")*).reverse)
    
    recur(text, 0)

case class Relative(ascent: Int, elements: List[Text])

export Relative.`?`