/*
    Diuretic, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package diuretic

import anticipation.*

import java.time as jt
import java.io as ji
import java.nio.file as jnf
import java.net as jn
import java.util as ju

object JavaTime extends GenericInstant[jt.Instant], GenericDuration[Long]:
  def readInstant(value: jt.Instant): Long = value.toEpochMilli
  def makeInstant(long: Long): jt.Instant = jt.Instant.ofEpochMilli(long).nn
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaLongTime extends GenericInstant[Long], GenericDuration[Long]:
  def readInstant(long: Long): Long = long
  def makeInstant(value: Long): Long = value
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaUtilTime extends GenericInstant[ju.Date], GenericDuration[Long]:
  def makeInstant(long: Long): ju.Date = ju.Date(long)
  def readInstant(value: ju.Date): Long = value.getTime
  def readDuration(long: Long): Long = long
  def makeDuration(value: Long): Long = value

object JavaNioFile
extends GenericFileMaker[jnf.Path], GenericDirectoryMaker[jnf.Path], GenericPathMaker[jnf.Path],
    GenericPathReader[jnf.Path], GenericDirectoryReader[jnf.Path], GenericFileReader[jnf.Path]:

  def makePath(path: Text): jnf.Path = jnf.Paths.get(path.s).nn

  def makeFile(path: Text): jnf.Path = makePath(path)
  def makeDirectory(path: Text): jnf.Path = makePath(path)

  def fromPath(value: jnf.Path): Text = value.toAbsolutePath.nn.toString.tt
  def fromFile(value: jnf.Path): Text = fromPath(value)
  def fromDirectory(value: jnf.Path): Text = fromPath(value)

object JavaIoFile
extends GenericFileMaker[ji.File], GenericDirectoryMaker[ji.File], GenericPathMaker[ji.File],
    GenericPathReader[ji.File], GenericDirectoryReader[ji.File], GenericFileReader[ji.File]:

  def makePath(path: Text): ji.File = ji.File(path.s).nn

  def makeFile(path: Text): ji.File = makePath(path)
  def makeDirectory(path: Text): ji.File = makePath(path)

  def fromPath(value: ji.File): Text = value.getAbsolutePath.nn.toString.tt
  def fromFile(value: ji.File): Text = fromPath(value)
  def fromDirectory(value: ji.File): Text = fromPath(value)

object JavaNetUrl extends GenericUrl[jn.URL], SpecificUrl[jn.URL]:
  def text(value: jn.URL): Text = value.toString.tt
  def url(text: Text): jn.URL = jn.URI(text.s).nn.toURL().nn

extension (stream: LazyList[IArray[Byte]])
  def inputStream: ji.InputStream = new ji.InputStream:
    private var lazyList: LazyList[IArray[Byte]] = stream
    private var total: Int = 0
    private var index: Int = -1
    private var current: IArray[Byte] = IArray[Byte]()

    def read(): Int =
      index += 1
      if index < current.length then current(index)
      else lazyList match
        case head #:: tail =>
          lazyList = tail
          current = head
          total += index
          index = -1
          read()
        
        case _ =>
          -1

