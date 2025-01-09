/*
    Diuretic, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import diuretic.*

import language.experimental.captureChecking

package instantApi:
  given javaTimeInstant: JavaTimeInstant.type = JavaTimeInstant
  given javaLong: JavaLongInstant.type = JavaLongInstant
  given javaUtilDate: JavaUtilDate.type = JavaUtilDate

package durationApi:
  given javaLong: JavaLongDuration.type = JavaLongDuration

package filesystemApi:
  given javaNioPath: JavaNioPath.type = JavaNioPath
  given javaIoFile: JavaIoFile.type = JavaIoFile

package urlApi:
  given javaNetUrl: JavaNetUrl.type = JavaNetUrl

