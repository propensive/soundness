/*
    Joviality, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

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
import _root_.joviality as jov

import language.experimental.captureChecking

given jovialityPath[Fs <: jov.Filesystem](using fs: Fs): (PathProvider[jov.DiskPath[Fs]] & DirectoryProvider[jov.Directory[Fs]] & FileProvider[jov.File[Fs]] & PathInterpreter[jov.DiskPath[Fs]] & FileInterpreter[jov.File[Fs]] & DirectoryInterpreter[jov.Directory[Fs]]) =
  new PathProvider[jov.DiskPath[Fs]] with DirectoryProvider[jov.Directory[Fs]] with FileProvider[jov.File[Fs]] with PathInterpreter[jov.DiskPath[Fs]] with FileInterpreter[jov.File[Fs]] with DirectoryInterpreter[jov.Directory[Fs]]:
    def makePath(str: String, readOnly: Boolean = false): Option[jov.DiskPath[Fs]] =
      safely(fs.parse(Text(str))).option
    
    def makeDirectory(str: String, readOnly: Boolean = false): Option[jov.Directory[Fs]] =
      safely(fs.parse(Text(str)).directory(jov.Expect)).option
    
    def makeFile(str: String, readOnly: Boolean = false): Option[jov.File[Fs]] =
      safely(fs.parse(Text(str)).file(jov.Expect)).option

    def getPath(value: jov.DiskPath[Fs]): String = value.fullname.s
    def directoryPath(value: jov.Directory[Fs]): String = value.path.fullname.s
    def filePath(value: jov.File[Fs]): String = value.path.fullname.s