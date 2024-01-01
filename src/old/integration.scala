/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import rudiments.*
import digression.*
import galilei.*

import language.experimental.captureChecking

package fileApi:
  given galileiApi
      (using fs: Filesystem)
      : (GenericPathMaker[DiskPath] & GenericDirectoryMaker[Directory] & GenericFileMaker[File] &
          GenericPathReader[DiskPath] & GenericFileReader[File] & GenericDirectoryReader[Directory]) =
    new GenericPathMaker[DiskPath] with GenericDirectoryMaker[Directory] with GenericFileMaker[File]
        with GenericPathReader[DiskPath] with GenericFileReader[File] with GenericDirectoryReader[Directory]:
      def makePath(str: String, readOnly: Boolean = false): Option[DiskPath] =
        safely(fs.parse(Text(str))).option
      
      def makeDirectory(str: String, readOnly: Boolean = false): Option[Directory] =
        safely(fs.parse(Text(str)).directory(Expect)).option
      
      def makeFile(str: String, readOnly: Boolean = false): Option[File] =
        safely(fs.parse(Text(str)).file(Expect)).option
  
      def getPath(value: DiskPath): String = value.fullname.s
      def directoryPath(value: Directory): String = value.path.fullname.s
      def filePath(value: File): String = value.path.fullname.s
