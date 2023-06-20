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
import gossamer.*
import kaleidoscope.*
import digression.*

object UnixRoot:
  @targetName("Root")
  final val `%`: UnixPath = UnixPath(Nil)
    
    @targetName("child")
    infix def /(name: PathName[UnixForbidden]): UnixPath = UnixPath(List(name))
    
    @targetName("child2")
    infix def /(name: Text): UnixPath throws PathError = UnixPath(List(PathName(name)))

export UnixRoot.%

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

  given pathlike: AbsolutePathlike[UnixPath, UnixForbidden, %.type](t"/") with
    def root(path: UnixPath): %.type = %
    def prefix(root: %.type): Text = t"/"
    def child(path: UnixPath, name: PathName[UnixForbidden]): UnixPath =
      UnixPath(name :: path.ancestry)
    
    def make(root: %.type, ancestry: List[PathName[UnixForbidden]]): UnixPath =
      UnixPath(ancestry)

    def parseRoot(text: Text): (%.type, Text) throws PathError =
      if text.starts(t"/") then (%, text.drop(1))
      else throw PathError(PathError.Reason.NotRooted)
    
    def ancestry(path: UnixPath): List[PathName[UnixForbidden]] = path.ancestry
    
    def parent(path: UnixPath): Maybe[UnixPath] =
      if path.ancestry == Nil then Unset else UnixPath(path.ancestry.tail)
    
    def ancestor(path: UnixPath, n: Int): Maybe[UnixPath] =
      if path.ancestry.length < n then Unset else UnixPath(path.ancestry.drop(n))

case class UnixPath(ancestry: List[PathName[UnixForbidden]])

object WindowsPath:
  def parse(text: Text): WindowsPath throws PathError = pathlike.parse(text)
  
  given pathlike: AbsolutePathlike[WindowsPath, WindowsForbidden, WindowsDrive](t"\\") with
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
  
  given pathlike: RelativePathlike[RelativeUnixPath, UnixForbidden](t"/", t"..", t".") with
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
  
  given pathlike: RelativePathlike[RelativeWindowsPath, WindowsForbidden](t"\\", t"..", t".") with
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

case class WindowsDrive(letter: Char):
  def apply(): WindowsPath = WindowsPath(this, Nil)
  
  @targetName("child")
  infix def /(name: PathName[WindowsForbidden]): WindowsPath = WindowsPath(this, List(name))

package hierarchies:
  erased given unix: Hierarchy[UnixPath, RelativeUnixPath, %.type, UnixForbidden] = ###

  erased given windows
      : Hierarchy[WindowsPath, RelativeWindowsPath, WindowsDrive, WindowsForbidden] =
    ###
