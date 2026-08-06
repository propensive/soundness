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
package soundness

// `Region` and `Slate` are deliberately absent: their lenders and combinators are
// dependent-typed inline extensions, which synthesized export forwarders break (see the
// hand-written `via`/`accepting` forwarders below for the same problem). Consumers import
// them from `zephyrine` directly.
export zephyrine.{Addressable, Buffering, Conduit, Credit, Cursor, Datum,
    Duct, Ductile, Expanse, Malleable, Pace,
    Format, Formatting, Intake, Lineation, ParseError, PositionTracking, Producer, Positionable,
    Records, records, Regulation, Spring, Stream, Substrate, chunks,
    discard, gather, locate,
    locateKey, memoize, pump, stream, streamOf, sweep, toProgression, truncate,
    viaDuct, acceptingDuct}

// Hand-written forwarders: the synthesized export forwarders for these dependent-typed
// extensions lose the `ductile.Result`/`ductile.Operand` path refinements under capture
// checking and fail to retypecheck.
extension [in, transport](consume stream: (Stream[in] over transport)^)
  def via[stage](consume stage: stage^)
    ( using ductile: (stage is Ductile by in) { type Upstream = transport },
            buffering: Buffering )
  :   (Stream[ductile.Result] over ductile.Transport)^ =
    // The call's dependent result widens to a `Ductile{...}#Result` projection rather than
    // narrowing back to this forwarder's `ductile.Result`; the value is returned unchanged,
    // so the cast only restores the dependent typing the export forwarder would have lost.
    zephyrine.via[in, transport](stream)[stage](stage)(using ductile, buffering)
    . asInstanceOf[(Stream[ductile.Result] over ductile.Transport)^]

extension [out, transport](consume intake: (Intake[out] over transport)^)
  def accepting[stage](consume stage: stage^)
    ( using ductile: (stage is Ductile to out) { type Transport = transport },
            buffering: Buffering )
  :   (Intake[ductile.Operand] over ductile.Upstream)^ =
    // See `via` above.
    zephyrine.accepting[out, transport](intake)[stage](stage)(using ductile, buffering)
    . asInstanceOf[(Intake[ductile.Operand] over ductile.Upstream)^]

package parsing:
  export zephyrine.parsing.trackPositions
