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

import language.experimental.captureChecking

object JavaTime
extends GenericInstant[jt.Instant], GenericDuration[Long], SpecificInstant[jt.Instant], SpecificDuration[Long]:
  
  def millisecondsSinceEpoch(value: jt.Instant): Long = value.toEpochMilli
  def instant(long: Long): jt.Instant = jt.Instant.ofEpochMilli(long).nn
  def milliseconds(long: Long): Long = long
  def duration(value: Long): Long = value

object JavaLongTime
extends GenericInstant[Long], GenericDuration[Long], SpecificInstant[Long], SpecificDuration[Long]:
  
  def millisecondsSinceEpoch(long: Long): Long = long
  def instant(value: Long): Long = value
  def milliseconds(long: Long): Long = long
  def duration(value: Long): Long = value

object JavaUtilTime
extends GenericInstant[ju.Date], GenericDuration[Long], SpecificInstant[ju.Date], SpecificDuration[Long]:
  def instant(long: Long): ju.Date = ju.Date(long)
  def millisecondsSinceEpoch(value: ju.Date): Long = value.getTime
  def milliseconds(long: Long): Long = long
  def duration(value: Long): Long = value

object JavaNioFile
extends SpecificFile[jnf.Path], SpecificDirectory[jnf.Path], SpecificPath[jnf.Path],
    GenericPath[jnf.Path], GenericDirectory[jnf.Path], GenericFile[jnf.Path]:

  def path(path: Text): jnf.Path = jnf.Paths.get(path.s).nn
  def file(file: Text): jnf.Path = path(file)
  def directory(directory: Text): jnf.Path = path(directory)

  def pathText(value: jnf.Path): Text = value.toAbsolutePath.nn.toString.tt
  def fileText(value: jnf.Path): Text = pathText(value)
  def directoryText(value: jnf.Path): Text = pathText(value)

object JavaIoFile
extends SpecificFile[ji.File], SpecificDirectory[ji.File], SpecificPath[ji.File],
    GenericPath[ji.File], GenericDirectory[ji.File], GenericFile[ji.File]:

  def path(path: Text): ji.File = ji.File(path.s).nn
  def file(file: Text): ji.File = path(file)
  def directory(directory: Text): ji.File = path(directory)

  def pathText(value: ji.File): Text = value.getAbsolutePath.nn.toString.tt
  def fileText(value: ji.File): Text = pathText(value)
  def directoryText(value: ji.File): Text = pathText(value)

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
      if index < current.asInstanceOf[Array[Byte]].length then current.asInstanceOf[Array[Byte]](index)
      else lazyList match
        case head #:: tail =>
          lazyList = tail
          current = head
          total += index
          index = -1
          read()
        
        case _ =>
          -1

