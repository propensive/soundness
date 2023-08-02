/*
    Galilei, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import serpentine.*
import spectacular.*
import galilei.*

// import language.experimental.captureChecking

package fileApi:
  given galileiApi
      [PathType <: Path]
      (using hierarchy: Hierarchy[PathType, ?])
      (using Decoder[PathType], PathResolver[File, PathType], PathResolver[Directory, PathType])
      : (GenericPathMaker[PathType] & GenericFileMaker[File] & GenericDirectoryMaker[Directory] &
          GenericPathReader[PathType] & GenericFileReader[File] &
          GenericDirectoryReader[Directory]) =
    
    new GenericPathMaker[PathType] with GenericFileMaker[File]
        with GenericDirectoryMaker[Directory] with GenericPathReader[PathType]
        with GenericFileReader[File] with GenericDirectoryReader[Directory]:

      def makePath(name: Text): PathType = name.decodeAs[PathType]
      def makeFile(name: Text): File = makePath(name).as[File]
      def makeDirectory(name: Text): Directory = makePath(name).as[Directory]
      def fromPath(path: PathType): Text = path.fullname
      def fromFile(file: File): Text = file.path.fullname
      def fromDirectory(directory: Directory): Text = directory.path.fullname
      