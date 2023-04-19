package serpentine

import rudiments.*
import gossamer.*

import language.experimental.captureChecking

object GenericPath:
  @targetName("GeneralPathRoot")
  object `^` extends GenericPath(Nil)

  given Show[GenericPath] = Hierarchy.show[GenericPath]

  given hierarchy: Hierarchy[GenericPath] with
    type ForbiddenType = '/' | ".." | ""

    def separator: Text = t"/"
    def prefix(root: GenericPath): Text = t"/"
    def root(path: GenericPath): `^`.type = ^
    def elements(path: GenericPath): List[PathElement['/' | ".." | ""]] = path.elements
    def child(base: GenericPath, child: PathElement['/' | ".." | ""]): GenericPath =
      base.copy(elements = child :: base.elements)
    
    def parent(path: GenericPath): GenericPath = path.copy(elements = path.elements.tail)

case class GenericPath(elements: List[SerpentineInternals.PathElement['/' | ".." | ""]])

export GenericPath.`^`