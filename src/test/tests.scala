/*
    Rudiments, version 0.4.0. Copyright 2020-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import probably.*
import eucalyptus.*
import gossamer.*

import unsafeExceptions.canThrowAny
given Log(Everything |-> Stdout)

object Tests extends Suite(t"Rudiments tests"):
  def run(using Runner): Unit =
    
    val array = (0 until 65536).to(Array).map(_.toByte)
    
    test(t"read Java `InputStream`, chunked") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 4.kb).map(_.to(Vector)).reduce(_ ++ _)
    }.assert(_ == array.to(Vector))
    
    test(t"read Java `InputStream`, single chunk") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 64.kb).map(_.to(Vector)).head
    }.assert(_ == array.to(Vector))
    
    test(t"read Java `InputStream`, two chunks") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 32.kb).map(_.to(Vector)).length
    }.assert(_ == 2)

    test(t"initialize array") {
      val iarray: IArray[Text] = IArray.init(3) { arr =>
        arr(0) = t"zero"
        arr(1) = t"one"
        arr(2) = t"two"
      }
      iarray.to(List)
    }.assert(_ == List(t"zero", t"one", t"two"))
