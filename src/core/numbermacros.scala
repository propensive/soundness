/*
    Cardinality, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cardinality

import fulminate.*

import scala.quoted.*
import scala.compiletime.*

object Cardinality:
  given Realm = realm"cardinality"
  
  def apply[LeftDoubleType <: Double: Type, RightDoubleType <: Double: Type](digits: Expr[String])(using Quotes)
        : Expr[LeftDoubleType ~ RightDoubleType] =

    import quotes.reflect.*
    
    digits.value match
      case Some(string) =>
        TypeRepr.of[LeftDoubleType].asMatchable match
          case ConstantType(DoubleConstant(lowerBound)) =>
            TypeRepr.of[RightDoubleType].asMatchable match
              case ConstantType(DoubleConstant(upperBound)) =>
                val value = string.toDouble
            
                if value < lowerBound
                then fail(msg"""the value $string is less than the lower bound for this value,
                    ${lowerBound.toString}""")
            
                if value > upperBound
                then fail(msg"""the value $string is greater than the upper bound for this value,
                    ${upperBound.toString}""")
  
                '{${Expr(value)}.asInstanceOf[LeftDoubleType ~ RightDoubleType]}
          
              case _ =>
                fail(msg"the upper bound must be a Double singleton literal types")
          case _ =>
            fail(msg"the lower bound must be a Double singleton literal types")
      case None =>
        '{NumericRange($digits.toDouble)}
