/*
    Diuretic, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

object JavaTimeInstant extends GenericInstant, SpecificInstant:
  type Self = jt.Instant
  
  def millisecondsSinceEpoch(value: jt.Instant): Long = value.toEpochMilli
  def instant(long: Long): jt.Instant = jt.Instant.ofEpochMilli(long).nn

object JavaLongDuration extends GenericDuration, SpecificDuration:
  type Self = Long
  
  def milliseconds(long: Long): Long = long
  def duration(value: Long): Long = value

object JavaLongInstant extends GenericInstant, SpecificInstant:
  type Self = Long

  def millisecondsSinceEpoch(long: Long): Long = long
  def instant(value: Long): Long = value

object JavaUtilDate extends GenericInstant, SpecificInstant:
  type Self = ju.Date
  def instant(long: Long): ju.Date = ju.Date(long)
  def millisecondsSinceEpoch(value: ju.Date): Long = value.getTime

object JavaNioFile
extends SpecificFile, SpecificDirectory, SpecificPath, GenericPath, GenericDirectory, GenericFile:

  type Self = jnf.Path

  def path(path: Text): jnf.Path = jnf.Paths.get(path.s).nn
  def file(file: Text): jnf.Path = path(file)
  def directory(directory: Text): jnf.Path = path(directory)

  def pathText(value: jnf.Path): Text = value.toAbsolutePath.nn.toString.tt
  def fileText(value: jnf.Path): Text = pathText(value)
  def directoryText(value: jnf.Path): Text = pathText(value)

object JavaIoFile
extends SpecificFile, SpecificDirectory, SpecificPath, GenericPath, GenericDirectory, GenericFile:

  type Self = ji.File

  def path(path: Text): ji.File = ji.File(path.s).nn
  def file(file: Text): ji.File = path(file)
  def directory(directory: Text): ji.File = path(directory)

  def pathText(value: ji.File): Text = value.getAbsolutePath.nn.toString.tt
  def fileText(value: ji.File): Text = pathText(value)
  def directoryText(value: ji.File): Text = pathText(value)

object JavaNetUrl extends GenericUrl, SpecificUrl:
  type Self = jn.URL
  def text(value: jn.URL): Text = value.toString.tt
  def url(text: Text): jn.URL = jn.URI(text.s).nn.toURL().nn

