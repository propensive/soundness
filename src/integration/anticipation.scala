/*
    Galilei, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation.integration

import anticipation.*
import rudiments.*
import _root_.galilei as gal

import language.experimental.captureChecking

given galileiPath[Fs <: gal.Filesystem](using fs: Fs): (PathProvider[gal.DiskPath[Fs]] & DirectoryProvider[gal.Directory[Fs]] & FileProvider[gal.File[Fs]] & PathInterpreter[gal.DiskPath[Fs]] & FileInterpreter[gal.File[Fs]] & DirectoryInterpreter[gal.Directory[Fs]]) =
  new PathProvider[gal.DiskPath[Fs]] with DirectoryProvider[gal.Directory[Fs]] with FileProvider[gal.File[Fs]] with PathInterpreter[gal.DiskPath[Fs]] with FileInterpreter[gal.File[Fs]] with DirectoryInterpreter[gal.Directory[Fs]]:
    def makePath(str: String, readOnly: Boolean = false): Option[gal.DiskPath[Fs]] =
      safely(fs.parse(Text(str))).option
    
    def makeDirectory(str: String, readOnly: Boolean = false): Option[gal.Directory[Fs]] =
      safely(fs.parse(Text(str)).directory(gal.Expect)).option
    
    def makeFile(str: String, readOnly: Boolean = false): Option[gal.File[Fs]] =
      safely(fs.parse(Text(str)).file(gal.Expect)).option

    def getPath(value: gal.DiskPath[Fs]): String = value.fullname.s
    def directoryPath(value: gal.Directory[Fs]): String = value.path.fullname.s
    def filePath(value: gal.File[Fs]): String = value.path.fullname.s
