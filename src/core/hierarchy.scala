/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import rudiments.*
import digression.*
import spectacular.*
import kaleidoscope.*
import gossamer.*

import scala.quoted.*

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidPrefix(prefix: Text)
    case InvalidSuffix(suffix: Text)
    case InvalidName(name: Text)
    case ParentOfRoot
    case NotRooted

  given Show[Reason] =
    case Reason.InvalidChar(char)     => t"the character $char may not appear in a path name"
    case Reason.InvalidPrefix(prefix) => t"the path name cannot begin with $prefix"
    case Reason.InvalidSuffix(suffix) => t"the path name cannot end with $suffix"
    case Reason.InvalidName(name)     => t"the name $name is not valid"
    case Reason.ParentOfRoot          => t"the root has no parent"
    case Reason.NotRooted             => t"the path is not rooted"

object SerpentineOpaques:
  opaque type PathName[ForbiddenType <: Label] = String

  object PathName:
    inline def apply[ForbiddenType <: Label](text: Text): PathName[ForbiddenType] =
      ${SerpentineMacros.runtimeParse[ForbiddenType]('text)}

  extension [ForbiddenType <: Label](pathName: PathName[ForbiddenType])
    def text: Text = Text(pathName)

export SerpentineOpaques.*

case class PathError(reason: PathError.Reason)
extends Error(err"the path is invalid because ${reason.show}")

@targetName("unixRoot")
def `%`: UnixPath = UnixPath(Nil)

@targetName("relative")
def ?
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
    : RelativePathType =
  hierarchy.relativePath(0, Nil)

@targetName("relativeParent")
def ?^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
    : RelativePathType =
  hierarchy.relativePath(1, Nil)

@targetName("relativeParent2")
def ?^^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
    : RelativePathType =
  hierarchy.relativePath(2, Nil)

@targetName("relativeParent3")
def ?^^^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
    : RelativePathType =
  hierarchy.relativePath(3, Nil)

// case class System
//     [RootType, NameType <: Label]
//     (hierarchies: Hierarchy[RootType, NameType]*)


trait Hierarchy[AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]:
  def relativePath(ascent: Int, ancestry: List[PathName[NameType]]): RelativePathType
  def absolutePath(root: RootType, ancestry: List[PathName[NameType]]): AbsolutePathType

  def root(path: AbsolutePathType): RootType
  def ascent(path: RelativePathType): Int
  
extension
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (left: RelativePathType)
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
  def ascent: Int = hierarchy.ascent(left)

  @targetName("relativeKeep")
  def keep(n: Int)(using pathlike: Pathlike[RelativePathType, NameType]): RelativePathType =
    hierarchy.relativePath(ascent, left.ancestry.takeRight(n))

extension
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, RootType, NameType <: Label]
    (left: AbsolutePathType)
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType, RootType, NameType])
  
  def root: RootType = hierarchy.root(left)
  
  def relativeTo(right: AbsolutePathType)(using Pathlike[AbsolutePathType, NameType]): RelativePathType =
    val common = left.conjunction(right).depth
    hierarchy.relativePath(left.depth - common, right.ancestry.dropRight(common))
  
  @targetName("absoluteKeep")
  def keep(n: Int)(using Pathlike[AbsolutePathType, NameType]): AbsolutePathType =
    hierarchy.absolutePath(left.root, left.ancestry.takeRight(n))
    
  def conjunction(right: AbsolutePathType)(using Pathlike[AbsolutePathType, NameType]): AbsolutePathType =
    lazy val leftElements: IArray[Text] = IArray.from(left.ancestry.reverse.map(_.text))
    lazy val rightElements: IArray[Text] = IArray.from(right.ancestry.reverse.map(_.text))
    
    @tailrec
    def count(n: Int): Int =
      if leftElements.length > n && rightElements.length > n && leftElements(n) == rightElements(n)
      then count(n + 1)
      else n
    
    hierarchy.absolutePath(left.root, left.ancestry.takeRight(count(0)))
 
  def precedes(path: AbsolutePathType)(using Pathlike[AbsolutePathType, NameType]): Boolean =
    left.conjunction(path).ancestry == left.ancestry

  @targetName("plus")
  def ++
      (relative: RelativePathType)
      (using absolutePathlike: AbsolutePathlike[AbsolutePathType, NameType, ?])
      (using relativePathlike: RelativePathlike[RelativePathType, NameType])
      : AbsolutePathType throws PathError =

    if relative.ascent > left.depth then throw PathError(PathError.Reason.ParentOfRoot)
    else
      val common: AbsolutePathType = absolutePathlike.ancestor(left, relative.ascent).avow
      val ancestry = absolutePathlike.ancestry(common)
      hierarchy.absolutePath(left.root, relative.ancestry ::: ancestry)

type UnixForbidden =
  ".*<.*" | ".*>.*" | ".*:.*" | ".*\".*" | ".*\\\\.*" | ".*\\|.*" | ".*\\?.*" | ".*\\*.*" | ".*/.*"

type WindowsForbidden =
  "con(\\..*)?" | "prn(\\..*)?" | "aux(\\..*)?" | "nul(\\..*)?" | "com1(\\..*)?" | "com2(\\..*)?" |
      "com3(\\..*)?" | "com4(\\..*)?" | "com5(\\..*)?" | "com6(\\..*)?" | "com7(\\..*)?" |
      "com8(\\..*)?" | "com9(\\..*)?" | "lpt1(\\..*)?" | "lpt2(\\..*)?" | "lpt3(\\..*)?" |
      "lpt4(\\..*)?" | "lpt5(\\..*)?" | "lpt6(\\..*)?" | "lpt7(\\..*)?" | "lpt8(\\..*)?" |
      "lpt9(\\..*)?" | ".* " | ".*\\."

object UnixPath:

  def parse(text: Text): UnixPath throws PathError = pathlike.parse(text)

  given pathlike: AbsolutePathlike[UnixPath, UnixForbidden, hierarchies.unix.type] with
    val pathSeparator: Text = t"/"
    def root(path: UnixPath): hierarchies.unix.type = hierarchies.unix
    def prefix(root: hierarchies.unix.type): Text = t"/"
    def child(path: UnixPath, name: PathName[UnixForbidden]): UnixPath =
      UnixPath(name :: path.ancestry)
    
    def make(root: hierarchies.unix.type, ancestry: List[PathName[UnixForbidden]]): UnixPath =
      UnixPath(ancestry)

    def parseRoot(text: Text): (hierarchies.unix.type, Text) throws PathError =
      if text.starts(t"/") then (hierarchies.unix, text.drop(1))
      else throw PathError(PathError.Reason.NotRooted)
    
    def ancestry(path: UnixPath): List[PathName[UnixForbidden]] = path.ancestry
    
    def parent(path: UnixPath): Maybe[UnixPath] =
      if path.ancestry == Nil then Unset else UnixPath(path.ancestry.tail)
    
    def ancestor(path: UnixPath, n: Int): Maybe[UnixPath] =
      if path.ancestry.length < n then Unset else UnixPath(path.ancestry.drop(n))
    

case class UnixPath(ancestry: List[PathName[UnixForbidden]])

object WindowsPath:
  def parse(text: Text): WindowsPath throws PathError = pathlike.parse(text)
  
  given pathlike: AbsolutePathlike[WindowsPath, WindowsForbidden, WindowsDrive] with
    val pathSeparator: Text = t"\\"
    def root(path: WindowsPath): WindowsDrive = path.drive
    def prefix(drive: WindowsDrive): Text = t"${drive.letter}:\\"
    
    def make(drive: WindowsDrive, ancestry: List[PathName[WindowsForbidden]]): WindowsPath =
      WindowsPath(drive, ancestry)
    
    def parseRoot(text: Text): (WindowsDrive, Text) throws PathError = text match
      case r"$letter([A-Za-z]):\\.*" => (WindowsDrive(unsafely(letter(0).toUpper)), text.drop(3))
      case _                         => throw PathError(PathError.Reason.NotRooted)

    def child(path: WindowsPath, name: PathName[WindowsForbidden]): WindowsPath =
      WindowsPath(path.drive, name :: path.ancestry)
    
    def ancestry(path: WindowsPath): List[PathName[WindowsForbidden]] = path.ancestry
    
    def parent(path: WindowsPath): Maybe[WindowsPath] =
      if path.ancestry == Nil then Unset else WindowsPath(path.drive, path.ancestry.tail)
    
    def ancestor(path: WindowsPath, n: Int): Maybe[WindowsPath] =
      if path.ancestry.length < n then Unset else WindowsPath(path.drive, path.ancestry.drop(n))
    

case class WindowsPath(drive: WindowsDrive, ancestry: List[PathName[WindowsForbidden]])

object RelativeUnixPath:
  def parse(text: Text): RelativeUnixPath throws PathError = pathlike.parse(text)
  
  given pathlike: RelativePathlike[RelativeUnixPath, UnixForbidden] with
    val pathSeparator: Text = t"/"
    val parentRef: Text = t".."
    val selfRef: Text = t"."

    def ascent(path: RelativeUnixPath): Int = path.ascent

    def make(ascent: Int, ancestry: List[PathName[UnixForbidden]]): RelativeUnixPath =
      RelativeUnixPath(ascent, ancestry)

    def child(path: RelativeUnixPath, name: PathName[UnixForbidden]): RelativeUnixPath =
      RelativeUnixPath(path.ascent, name :: path.ancestry)
    
    def ancestry(path: RelativeUnixPath): List[PathName[UnixForbidden]] = path.ancestry
    
    def parent(path: RelativeUnixPath): RelativeUnixPath =
      if path.ancestry == Nil then RelativeUnixPath(path.ascent + 1, Nil)
      else RelativeUnixPath(path.ascent, path.ancestry.tail)
    
object RelativeWindowsPath:
  def parse(text: Text): RelativeWindowsPath throws PathError = pathlike.parse(text)
  
  given pathlike: RelativePathlike[RelativeWindowsPath, WindowsForbidden] with
    val pathSeparator: Text = t"\\"
    val parentRef: Text = t".."
    val selfRef: Text = t"."
    
    def ascent(path: RelativeWindowsPath): Int = path.ascent
    
    def make(ascent: Int, ancestry: List[PathName[WindowsForbidden]]): RelativeWindowsPath =
      RelativeWindowsPath(ascent, ancestry)
    
    def child(path: RelativeWindowsPath, name: PathName[WindowsForbidden]): RelativeWindowsPath =
      RelativeWindowsPath(path.ascent, name :: path.ancestry)
    
    def ancestry(path: RelativeWindowsPath): List[PathName[WindowsForbidden]] = path.ancestry
    
    def parent(path: RelativeWindowsPath): RelativeWindowsPath =
      if path.ancestry == Nil then RelativeWindowsPath(path.ascent + 1, Nil)
      else RelativeWindowsPath(path.ascent, path.ancestry.tail)
    
case class RelativeUnixPath(ascent: Int, ancestry: List[PathName[UnixForbidden]])
case class RelativeWindowsPath(ascent: Int, ancestry: List[PathName[WindowsForbidden]])

trait Pathlike[PathType <: Matchable, NameType <: Label]:
  def pathSeparator: Text
  def child(path: PathType, name: PathName[NameType]): PathType
  def ancestry(path: PathType): List[PathName[NameType]]
  def text(path: PathType): Text
  inline def parse(text: Text): PathType throws PathError

trait AbsolutePathlike[PathType <: Matchable, NameType <: Label, RootType]
extends Pathlike[PathType, NameType]:
  def prefix(root: RootType): Text
  def root(path: PathType): RootType
  def parent(path: PathType): Maybe[PathType]
  def ancestor(path: PathType, n: Int): Maybe[PathType]
  def make(root: RootType, ancestry: List[PathName[NameType]]): PathType
  def parseRoot(text: Text): (RootType, Text) throws PathError
  
  def text(path: PathType): Text =
    t"${prefix(root(path))}${ancestry(path).reverse.map(_.text).join(pathSeparator)}"
  
  inline def parse(text: Text): PathType throws PathError =
    val (root, rest) = parseRoot(text)
    
    val names = rest.cut(pathSeparator).reverse match
      case t"" :: tail => tail
      case names       => names

    make(root, names.map(PathName(_)))

  
trait RelativePathlike[PathType <: Matchable, NameType <: Label]
extends Pathlike[PathType, NameType]:
  def parentRef: Text
  def selfRef: Text
  def parent(path: PathType): PathType
  def ascent(path: PathType): Int
  
  def ancestor(path: PathType, n: Int): PathType =
    val depth = ancestry(path).length
    val ancestry2 = ancestry(path).drop(n)
    make(ascent(path) + (if n > depth then n - depth else 0), ancestry2)

  def make(ascent: Int, ancestry: List[PathName[NameType]]): PathType
  
  def text(path: PathType): Text =
    if ancestry(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef$pathSeparator"*(ascent(path) - 1)}$parentRef"
    else
      val elements: IArray[Text] = IArray.from(ancestry(path).reverse.map(_.text))
      t"${t"$parentRef$pathSeparator"*ascent(path)}${elements.join(pathSeparator)}"

  inline def parse(text: Text): PathType throws PathError =
    val ascentPrefix: Text = t"$parentRef$pathSeparator"
    
    def recur(text: Text, ascent: Int = 0): PathType =
      if text.starts(ascentPrefix) then recur(text.drop(ascentPrefix.length), ascent + 1)
      else if text == parentRef then make(ascent + 1, Nil)
      else
        val names = text.cut(pathSeparator).reverse match
          case t"" :: tail => tail
          case names       => names
        
        make(ascent, names.map(PathName(_)))
    
    if text == selfRef then make(0, Nil) else recur(text)
  
extension
    [PathType <: Matchable, NameType <: Label]
    (path: PathType)
    (using pathlike: Pathlike[PathType, NameType])
  
  @targetName("child")
  infix def /(name: PathName[NameType]): PathType = pathlike.child(path, name)
  
  def ancestry: List[PathName[NameType]] = pathlike.ancestry(path)
  def depth: Int = ancestry.length
  def text: Text = pathlike.text(path)

  transparent inline def parent: Maybe[PathType] = compiletime.summonFrom:
    case pathlike: AbsolutePathlike[PathType, NameType, ?] => pathlike.parent(path)
    case pathlike: RelativePathlike[PathType, NameType] => pathlike.parent(path)
  
  transparent inline def ancestor(n: Int): Maybe[PathType] = compiletime.summonFrom:
    case pathlike: AbsolutePathlike[PathType, NameType, ?] => pathlike.ancestor(path, n)
    case pathlike: RelativePathlike[PathType, NameType] => pathlike.ancestor(path, n)

case class WindowsDrive(letter: Char):
  def apply(): WindowsPath = WindowsPath(this, Nil)
  
  @targetName("child")
  infix def /(name: PathName[WindowsForbidden]): WindowsPath = WindowsPath(this, List(name))

package hierarchies:
  given unix: Hierarchy[UnixPath, RelativeUnixPath, hierarchies.unix.type, UnixForbidden] with
    def apply(): UnixPath = absolutePath(hierarchies.unix, Nil)

    def relativePath(ascent: Int, ancestry: List[PathName[UnixForbidden]]): RelativeUnixPath =
      RelativeUnixPath(ascent, ancestry)
    
    def absolutePath(root: hierarchies.unix.type, ancestry: List[PathName[UnixForbidden]]): UnixPath = UnixPath(ancestry)

    def relativeAncestry(path: RelativeUnixPath): List[PathName[UnixForbidden]] = path.ancestry
    def absoluteAncestry(path: UnixPath): List[PathName[UnixForbidden]] = path.ancestry
    def root(path: UnixPath): hierarchies.unix.type = hierarchies.unix
    def ascent(path: RelativeUnixPath): Int = path.ascent

  given windows: Hierarchy[WindowsPath, RelativeWindowsPath, WindowsDrive, WindowsForbidden] with
    val pathSeparator: Text = t"\\"
    val parentRef: Text = t".."
    val selfRef: Text = t"."
    
    def relativePath(ascent: Int, ancestry: List[PathName[WindowsForbidden]]): RelativeWindowsPath =
      RelativeWindowsPath(ascent, ancestry)
    
    def absolutePath(drive: WindowsDrive, ancestry: List[PathName[WindowsForbidden]]): WindowsPath =
      WindowsPath(drive, ancestry)
    
    def relativeAncestry(path: RelativeWindowsPath): List[PathName[WindowsForbidden]] = path.ancestry
    def absoluteAncestry(path: WindowsPath): List[PathName[WindowsForbidden]] = path.ancestry
    def root(path: WindowsPath): WindowsDrive = path.drive
    def ascent(path: RelativeWindowsPath): Int = path.ascent

extension (inline context: StringContext)
  inline def p[ForbiddenType <: Label](): PathName[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('context)}