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
package tarantula

import anticipation.*
import fulminate.*
import gossamer.*

object WebDriverError:
  object Reason:
    // Hand-written rather than derived from `values`: Scala generates `values`/`valueOf` only
    // for an enum whose cases are all singletons, and `Other` is parameterized. An
    // unrecognized code becomes `Other`, so decoding is total, and the code a driver sent is
    // never lost.
    def apply(code: Text): Reason = code match
      case t"no such element"            => NoSuchElement
      case t"stale element reference"    => StaleElement
      case t"no such window"             => NoSuchWindow
      case t"javascript error"           => JavascriptError
      case t"script timeout"             => ScriptTimeout
      case t"timeout"                    => Timeout
      case t"unknown command"            => UnknownCommand
      case t"element click intercepted"  => ElementClickIntercepted
      case t"element not interactable"   => ElementNotInteractable
      case t"insecure certificate"       => InsecureCertificate
      case t"invalid argument"           => InvalidArgument
      case t"invalid cookie domain"      => InvalidCookieDomain
      case t"invalid element state"      => InvalidElementState
      case t"invalid selector"           => InvalidSelector
      case t"invalid session id"         => InvalidSessionId
      case t"move target out of bounds"  => MoveTargetOutOfBounds
      case t"no such alert"              => NoSuchAlert
      case t"no such cookie"             => NoSuchCookie
      case t"no such frame"              => NoSuchFrame
      case t"no such shadow root"        => NoSuchShadowRoot
      case t"detached shadow root"       => DetachedShadowRoot
      case t"session not created"        => SessionNotCreated
      case t"unable to set cookie"       => UnableToSetCookie
      case t"unable to capture screen"   => UnableToCaptureScreen
      case t"unexpected alert open"      => UnexpectedAlertOpen
      case t"unknown error"              => UnknownError
      case t"unknown method"             => UnknownMethod
      case t"unsupported operation"      => UnsupportedOperation
      case other                         => Other(other)

  // The error codes a WebDriver may report, as the driver spells them in the `error` field of
  // the error object. The numbers are the `SN-589.e` subcodes, and are frozen: 1–8 keep the
  // meanings they were first published with — including `Other` at 8, out of alphabetical
  // order — and every code added since appends from 9.
  enum Reason(val number: Int, val code: Text) extends Clarification:
    case NoSuchElement           extends Reason(1, t"no such element")
    case StaleElement            extends Reason(2, t"stale element reference")
    case NoSuchWindow            extends Reason(3, t"no such window")
    case JavascriptError         extends Reason(4, t"javascript error")
    case ScriptTimeout           extends Reason(5, t"script timeout")
    case Timeout                 extends Reason(6, t"timeout")
    case UnknownCommand          extends Reason(7, t"unknown command")
    case Other(code0: Text)      extends Reason(8, code0)
    case ElementClickIntercepted extends Reason(9, t"element click intercepted")
    case ElementNotInteractable  extends Reason(10, t"element not interactable")
    case InsecureCertificate     extends Reason(11, t"insecure certificate")
    case InvalidArgument         extends Reason(12, t"invalid argument")
    case InvalidCookieDomain     extends Reason(13, t"invalid cookie domain")
    case InvalidElementState     extends Reason(14, t"invalid element state")
    case InvalidSelector         extends Reason(15, t"invalid selector")
    case InvalidSessionId        extends Reason(16, t"invalid session id")
    case MoveTargetOutOfBounds   extends Reason(17, t"move target out of bounds")
    case NoSuchAlert             extends Reason(18, t"no such alert")
    case NoSuchCookie            extends Reason(19, t"no such cookie")
    case NoSuchFrame             extends Reason(20, t"no such frame")
    case NoSuchShadowRoot        extends Reason(21, t"no such shadow root")
    case DetachedShadowRoot      extends Reason(22, t"detached shadow root")
    case SessionNotCreated       extends Reason(23, t"session not created")
    case UnableToSetCookie       extends Reason(24, t"unable to set cookie")
    case UnableToCaptureScreen   extends Reason(25, t"unable to capture screen")
    case UnexpectedAlertOpen     extends Reason(26, t"unexpected alert open")
    case UnknownError            extends Reason(27, t"unknown error")
    case UnknownMethod           extends Reason(28, t"unknown method")
    case UnsupportedOperation    extends Reason(29, t"unsupported operation")

  given communicable: Reason is Communicable =
    case Reason.NoSuchElement           => m"the requested element was not found"
    case Reason.StaleElement            => m"the element is no longer attached to the page"
    case Reason.NoSuchWindow            => m"the requested window was not found"
    case Reason.JavascriptError         => m"a JavaScript error occurred while evaluating it"
    case Reason.ScriptTimeout           => m"the asynchronous script did not complete in time"
    case Reason.Timeout                 => m"the operation timed out"
    case Reason.UnknownCommand          => m"the WebDriver does not recognise the command"
    case Reason.Other(code)             => m"the WebDriver reported the error code $code"
    case Reason.ElementClickIntercepted => m"the click was intercepted by another element"
    case Reason.ElementNotInteractable  => m"the element cannot be interacted with"
    case Reason.InsecureCertificate     => m"the page was served with an untrusted certificate"
    case Reason.InvalidArgument         => m"an argument to the command was not acceptable"
    case Reason.InvalidCookieDomain     => m"the cookie's domain does not match the current page"
    case Reason.InvalidElementState     => m"the element is in a state which forbids the action"
    case Reason.InvalidSelector         => m"the selector was not a valid one"
    case Reason.InvalidSessionId        => m"the session is no longer active"
    case Reason.MoveTargetOutOfBounds   => m"the pointer target lies outside the viewport"
    case Reason.NoSuchAlert             => m"no alert is currently open"
    case Reason.NoSuchCookie            => m"the requested cookie was not found"
    case Reason.NoSuchFrame             => m"the requested frame was not found"
    case Reason.NoSuchShadowRoot        => m"the element has no shadow root"
    case Reason.DetachedShadowRoot      => m"the shadow root is no longer attached to the page"
    case Reason.SessionNotCreated       => m"a new session could not be created"
    case Reason.UnableToSetCookie       => m"the cookie could not be set"
    case Reason.UnableToCaptureScreen   => m"the screenshot could not be taken"
    case Reason.UnexpectedAlertOpen     => m"an unexpected alert is open"
    case Reason.UnknownError            => m"the WebDriver encountered an unknown error"
    case Reason.UnknownMethod           => m"the command does not support the HTTP method used"
    case Reason.UnsupportedOperation    => m"the WebDriver does not support the operation"

case class WebDriverError
  ( reason: WebDriverError.Reason, detail: Text, browserStacktrace: List[Text] )
  ( using Diagnostics )
extends Error(589, reason.number)(m"the WebDriver action failed because $reason: $detail")
