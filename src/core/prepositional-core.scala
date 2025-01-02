/*
    Prepositional, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package prepositional

infix type across [Type <: { type Domain },     DomainType]     = Type { type Domain     = DomainType }
infix type by     [Type <: { type Operand },    OperandType]    = Type { type Operand    = OperandType }
infix type from   [Type <: { type Source },     SourceType]     = Type { type Source     = SourceType }
infix type in     [Type <: { type Format },     FormatType]     = Type { type Format     = FormatType }
infix type into   [Type <: { type Result },     ResultType]     = Type { type Result     = ResultType }
infix type of     [Type <: { type Subject },    SubjectType]    = Type { type Subject    = SubjectType }
infix type on     [Type <: { type Platform },   PlatformType]   = Type { type Platform   = PlatformType }
infix type onto   [Type <: { type Target },     TargetType]     = Type { type Target     = TargetType }
infix type over   [Type <: { type Carrier },    CarrierType]    = Type { type Carrier    = CarrierType }
infix type under  [Type <: { type Constraint }, ConstraintType] = Type { type Constraint = ConstraintType }
