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
import vacuous.*

// The session in scope, for code that reads better as `browser.title()` than `session.title()`.
// A *named* using parameter, not a `summon`, which would mint a fresh root instead of referring
// to the session the enclosing block owns.
def browser(using session: WebDriver.Session^): WebDriver.Session^{session} = session

// Sugar over the session's element methods, so that `element.click()` reads as it did when
// `WebDriver.Element` was an inner class of the session. The session is a *named* using
// parameter: a `summon[WebDriver.Session^]` would mint a fresh root rather than refer to the one
// in scope.
extension (element: WebDriver.Element)
  def click()(using session: WebDriver.Session^): Unit = session.click(element)
  def clear()(using session: WebDriver.Session^): Unit = session.clear(element)
  def innerText()(using session: WebDriver.Session^): Text = session.innerText(element)
  def tagName()(using session: WebDriver.Session^): Text = session.tagName(element)
  def enabled()(using session: WebDriver.Session^): Boolean = session.enabled(element)
  def selected()(using session: WebDriver.Session^): Boolean = session.selected(element)
  def displayed()(using session: WebDriver.Session^): Boolean = session.displayed(element)
  def role()(using session: WebDriver.Session^): Text = session.role(element)
  def label()(using session: WebDriver.Session^): Text = session.label(element)
  def css(name: Text)(using session: WebDriver.Session^): Text = session.css(element, name)
  def shadowRoot()(using session: WebDriver.Session^): WebDriver.ShadowRoot =
    session.shadowRoot(element)

  def rect()(using session: WebDriver.Session^): WebDriver.Session.Rect = session.rect(element)

  def value(text: Text)(using session: WebDriver.Session^): Unit =
    session.value(element, text)

  def screenshotData()(using session: WebDriver.Session^): Data =
    session.screenshotData(element)

  def attribute(name: Text)(using session: WebDriver.Session^): Optional[Text] =
    session.attribute(element, name)

  def property(name: Text)(using session: WebDriver.Session^): Optional[Text] =
    session.property(element, name)

  def element[focus: Focusable](value: focus)(using session: WebDriver.Session^)
  :   WebDriver.Element =

    session.element(element, value)

  @targetName("at")
  infix def / [focus: Focusable](value: focus)(using session: WebDriver.Session^)
  :   List[WebDriver.Element] =

    session.elements(element, value)

extension (root: WebDriver.ShadowRoot)
  def element[focus: Focusable](value: focus)(using session: WebDriver.Session^)
  :   WebDriver.Element =

    session.element(root, value)

  @targetName("at")
  infix def / [focus: Focusable](value: focus)(using session: WebDriver.Session^)
  :   List[WebDriver.Element] =

    session.elements(root, value)

// The chained form the documentation has always shown — `browser / id"menu" / Li / cls"item"` —
// which selects from every element found so far, deduplicating nothing: the protocol has no
// batch find, so this is one request per element, as any WebDriver client must do.
extension (elements: List[WebDriver.Element])
  @targetName("at")
  infix def / [focus: Focusable](value: focus)(using session: WebDriver.Session^)
  :   List[WebDriver.Element] =

    List.of(elements.stdlib.flatMap(session.elements(_, value).stdlib))
