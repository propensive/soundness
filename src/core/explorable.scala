package serpentine

trait Explorable[PathType] extends Hierarchy[PathType]:
  def children(path: PathType): List[PathElement[ForbiddenType]]

extension [PathType, PathType2 >: PathType](path: PathType)(using explorable: Explorable[PathType2])
  def childNames: List[PathElement[explorable.ForbiddenType]] = explorable.children(path)
  def children: List[PathType2] = childNames.map(explorable.child(path, _))