                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package vivisection

import anticipation.*
import fulminate.*
import proscenium.*

// The failures a debug session can raise: a transport-level fault local to the debugger, or a JDWP
// error code returned by the debuggee VM in reply to a command.
object Debugger:
  object Error:
    object Reason:
      // Hand-written rather than derived: an unrecognized code becomes `Other`, so decoding a
      // reply's error code is total and the number the VM sent is never lost. `Disconnected`
      // carries −1 because it originates here, not from the VM.
      def apply(code: Int): Reason = code match
        case 10    => InvalidThread
        case 11    => InvalidThreadGroup
        case 13    => ThreadNotSuspended
        case 14    => ThreadSuspended
        case 20    => InvalidObject
        case 21    => InvalidClass
        case 22    => ClassNotPrepared
        case 23    => InvalidMethod
        case 24    => InvalidLocation
        case 25    => InvalidField
        case 30    => InvalidFrame
        case 31    => NoMoreFrames
        case 32    => OpaqueFrame
        case 34    => TypeMismatch
        case 35    => InvalidSlot
        case 41    => NotFound
        case 99    => NotImplemented
        case 100   => NullPointer
        case 101   => AbsentInformation
        case 102   => InvalidEventType
        case 103   => IllegalArgument
        case 112   => VmDead
        case 113   => Internal
        case 502   => AlreadyInvoking
        case -1    => Disconnected
        case code0 => Other(code0)

    given communicable: Reason is Communicable =
      case Reason.InvalidThread      => m"the thread identifier was invalid"
      case Reason.InvalidThreadGroup => m"the thread-group identifier was invalid"
      case Reason.ThreadNotSuspended => m"the thread was not suspended"
      case Reason.ThreadSuspended    => m"the thread was already suspended"
      case Reason.InvalidObject      => m"the object identifier was invalid"
      case Reason.InvalidClass       => m"the class identifier was invalid"
      case Reason.ClassNotPrepared   => m"the class had not been prepared"
      case Reason.InvalidMethod      => m"the method identifier was invalid"
      case Reason.InvalidLocation    => m"the location was invalid"
      case Reason.InvalidField       => m"the field identifier was invalid"
      case Reason.InvalidFrame       => m"the frame identifier was invalid"
      case Reason.NoMoreFrames       => m"there were no more frames on the stack"
      case Reason.OpaqueFrame        => m"the frame was opaque (native)"
      case Reason.TypeMismatch       => m"the value did not match the expected type"
      case Reason.InvalidSlot        => m"the local-variable slot was invalid"
      case Reason.NotFound           => m"the requested item was not found"
      case Reason.NotImplemented     => m"the VM does not implement this command"
      case Reason.NullPointer        => m"a null pointer was encountered"
      case Reason.AbsentInformation  => m"the debug information was absent"
      case Reason.InvalidEventType   => m"the event type was invalid"
      case Reason.IllegalArgument    => m"an argument was illegal"
      case Reason.VmDead             => m"the virtual machine had died"
      case Reason.Internal           => m"the virtual machine reported an internal error"
      case Reason.AlreadyInvoking    => m"a method was already being invoked on the thread"
      case Reason.Disconnected       => m"the connection to the debuggee was lost"
      case Reason.Other(code0)       => m"the VM reported error code $code0"

    enum Reason(val number: Int) extends Clarification:
      case InvalidThread extends Reason(10)
      case InvalidThreadGroup extends Reason(11)
      case ThreadNotSuspended extends Reason(13)
      case ThreadSuspended extends Reason(14)
      case InvalidObject extends Reason(20)
      case InvalidClass extends Reason(21)
      case ClassNotPrepared extends Reason(22)
      case InvalidMethod extends Reason(23)
      case InvalidLocation extends Reason(24)
      case InvalidField extends Reason(25)
      case InvalidFrame extends Reason(30)
      case NoMoreFrames extends Reason(31)
      case OpaqueFrame extends Reason(32)
      case TypeMismatch extends Reason(34)
      case InvalidSlot extends Reason(35)
      case NotFound extends Reason(41)
      case NotImplemented extends Reason(99)
      case NullPointer extends Reason(100)
      case AbsentInformation extends Reason(101)
      case InvalidEventType extends Reason(102)
      case IllegalArgument extends Reason(103)
      case VmDead extends Reason(112)
      case Internal extends Reason(113)
      case AlreadyInvoking extends Reason(502)
      case Disconnected extends Reason(-1)
      case Other(code0: Int) extends Reason(code0)

  case class Error(reason: Error.Reason, detail: Text)(using Diagnostics)
  extends fulminate.Error(601, reason.number)(m"the JDWP action failed because $reason: $detail")
