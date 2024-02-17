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

import serpentine.*
import spectacular.*
import galilei.*

// import language.experimental.captureChecking

package filesystemInterfaces:
  given galileiApi
      [PathType <: Path]
      (using hierarchy: Hierarchy[PathType, ?])
      (using Decoder[PathType], PathResolver[File, PathType], PathResolver[Directory, PathType])
      : (SpecificPath[PathType] & SpecificFile[File] & SpecificDirectory[Directory] &
          GenericPath[PathType] & GenericFile[File] & GenericDirectory[Directory]) =
    
    new SpecificPath[PathType] with SpecificFile[File]
        with SpecificDirectory[Directory] with GenericPath[PathType]
        with GenericFile[File] with GenericDirectory[Directory]:

      def path(name: Text): PathType = name.decodeAs[PathType]
      def file(name: Text): File = path(name).as[File]
      def directory(name: Text): Directory = path(name).as[Directory]
      def pathText(path: PathType): Text = path.fullname
      def fileText(file: File): Text = file.path.fullname
      def directoryText(directory: Directory): Text = directory.path.fullname
      
