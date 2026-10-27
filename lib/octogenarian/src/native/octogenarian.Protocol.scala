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
package octogenarian

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import symbolism.*
import vacuous.*

// A connection to a remote repository's `git-upload-pack` or `git-receive-pack`, however it is
// carried: the transport supplies the server's opening advertisement for a service and a way to
// send a request and read what comes back.
trait Channel:
  def advertise(service: Text)(using Tactic[Git.Error]): Pktline.Reader
  def exchange(service: Text, request: Data)(using Tactic[Git.Error]): Pktline.Reader
  def close(): Unit

// The client side of git's wire protocol over a `Channel`: protocol version 2 for listing refs
// and fetching, and the version 0 `receive-pack` exchange for pushing, as git itself does.
private[octogenarian] class Protocol(channel: Channel^) extends Peer:
  import Git.Error.Reason as Reason

  private val uploadPack: Text = t"git-upload-pack"
  private val receivePack: Text = t"git-receive-pack"
  private val agent: Text = t"agent=soundness-octogenarian"

  private def ensureV2()(using Tactic[Git.Error]): Unit =
    val reader = channel.advertise(uploadPack)

    reader.section().map(_.text.trim) match
      case t"version 2" :: _ => ()

      case _ =>
        abort(Git.Error(Reason.Unsupported(t"a server not offering protocol version 2")))

  // The listing is fetched once per connection; `null` until then (`Unset` is itself `null` at
  // runtime, so the slot is spelled as a nullable JDK reference).
  private type Listing = List[(Text, Git.Hash, Optional[Text], Optional[Git.Hash])]

  private val listed: java.util.concurrent.atomic.AtomicReference[Listing | Null] =
    java.util.concurrent.atomic.AtomicReference(null)

  // `ls-refs` with symrefs and peeled tags: name, hash, symref target and peeled hash.
  private def lsRefs()
    ( using Tactic[Git.Error] )
  :   List[(Text, Git.Hash, Optional[Text], Optional[Git.Hash])] =

    val cached: List[(Text, Git.Hash, Optional[Text], Optional[Git.Hash])] | Null = listed.get

    cached match
      case list: List[(Text, Git.Hash, Optional[Text], Optional[Git.Hash])] @unchecked => list

      case _ =>
        ensureV2()

        val request =
          Pktline.concatenate
            ( List
                ( List(Pktline.line(t"command=ls-refs\n"), Pktline.line(t"$agent\n")),
                  List(Pktline.delim),
                  List(Pktline.line(t"peel\n"), Pktline.line(t"symrefs\n"), Pktline.flush) ) )

        val reader = channel.exchange(uploadPack, request)

        def parse(line: Pktline.Packet.Line): (Text, Git.Hash, Optional[Text], Optional[Git.Hash]) =
          val fields = line.text.trim.cut(t" ")
          val hash = Git.Hash.unsafe(Refstore.nth(fields, 0).or(Git.Hash.zero.text))
          val name = Refstore.nth(fields, 1).or(t"")
          val symref = fields.filter(_.starts(t"symref-target:")).head.let(_.skip(14))

          val peeled =
            fields.filter(_.starts(t"peeled:")).head.let(_.skip(7)).let(Git.Hash.unsafe(_))

          (name, hash, symref, peeled)

        val result = reader.section().map(parse(_))
        listed.set(result)
        result

  private def peeledEntry
    ( name: Text, peeled: Git.Hash )
  :   List[(Text, Git.Hash)] =

    List((t"$name^{}", peeled))

  def refs()(using Tactic[Git.Error]): List[(Text, Git.Hash)] =
    def expand(entry: (Text, Git.Hash, Optional[Text], Optional[Git.Hash]))
    :   List[(Text, Git.Hash)] =

      val (name, hash, _, peeled) = entry
      List((name, hash)) + peeled.let(peeledEntry(name, _)).or(Nil)

    lsRefs().bind(expand(_))

  def symbolicHead()(using Tactic[Git.Error]): Optional[Text] =
    lsRefs().filter(_(0) == t"HEAD").head.let(_(2)).or(Unset)

  // Reads a sideband-multiplexed packfile section: band 1 is pack data, band 2 progress text,
  // band 3 a fatal error.
  private def sideband(reader: Pktline.Reader, activity: Git.Activity^)
    ( using Tactic[Git.Error] )
  :   Data =

    val output = java.io.ByteArrayOutputStream()

    def recur(): Unit = reader.next() match
      case Pktline.Packet.Line(data) =>
        val array = data.asInstanceOf[scala.Array[Byte]]

        if array.length > 0 then
          (array(0) & 0xff) match
            case 1 =>
              output.write(array, 1, array.length - 1)
              activity.transferred((array.length - 1).toLong)

            case 2 =>
              progress(Objects.text(array.slice(1, array.length).asInstanceOf[Data]), activity)

            case 3 =>
              val message = Objects.text(array.slice(1, array.length).asInstanceOf[Data]).trim
              abort(Git.Error(Reason.RemoteRejected(t"", message)))

            case _ => ()

        recur()

      case Pktline.Packet.Flush => ()
      case Pktline.Packet.End   => ()
      case Pktline.Packet.Delim => recur()
      case _                    => ()

    recur()
    output.toByteArray.nn.asInstanceOf[Data]

  // Progress as the server prints it: `Counting objects:  50% (1/2)` and the like.
  private def progress(text: Text, activity: Git.Activity^): Unit =
    val line = text.cut(t"\r").reverse.head.or(t"").trim
    val open = line.s.indexOf('(')
    val slash = line.s.indexOf('/', open.max(0))
    val close = line.s.indexOf(')', slash.max(0))

    if open >= 0 && slash > open && close > slash then
      val done = safely(line.s.substring(open + 1, slash).nn.tt.trim.s.toLong).or(-1L)
      val total = safely(line.s.substring(slash + 1, close).nn.tt.trim.s.toLong).or(-1L)
      if done >= 0 && total >= 0 then activity.progress(done, total)

  def download
    ( wants: List[Git.Hash], haves: List[Git.Hash], has: Git.Hash => Boolean, depth: Optional[Int],
      store: (Git.Object.Kind, Data) => Git.Hash, activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash] =

    if wants == Nil then Nil else
      ensureV2()
      activity.begin(Git.Activity.Phase.Receiving, true)

      def wantLine(hash: Git.Hash): Data = Pktline.line(t"want ${hash.text}\n")
      def haveLine(hash: Git.Hash): Data = Pktline.line(t"have ${hash.text}\n")
      def deepenLines(depth: Int): List[Data] = List(Pktline.line(t"deepen $depth\n"))
      val deepen: List[Data] = depth.let(deepenLines(_)).or(Nil)

      val request =
        Pktline.concatenate
          ( List
              ( List(Pktline.line(t"command=fetch\n"), Pktline.line(t"$agent\n")),
                List(Pktline.delim, Pktline.line(t"ofs-delta\n")),
                wants.distinct.map(wantLine(_)),
                haves.distinct.map(haveLine(_)),
                deepen,
                List(Pktline.line(t"done\n"), Pktline.flush) ) )

      val reader = channel.exchange(uploadPack, request)

      // Sections: optional `acknowledgments`, optional `shallow-info`, then `packfile`.
      def sections(shallow: List[Git.Hash]): (List[Git.Hash], Data) = reader.next() match
        case Pktline.Packet.Line(data) =>
          Objects.text(data).trim match
            case t"packfile"        => (shallow, sideband(reader, activity))
            case t"shallow-info"    => sections(shallowLines(shallow))
            case t"acknowledgments" => reader.section(); sections(shallow)
            case other              => sections(shallow)

        case Pktline.Packet.Delim => sections(shallow)
        case Pktline.Packet.Flush => (shallow, Data())
        case _                    => (shallow, Data())

      def shallowLines(acc: List[Git.Hash]): List[Git.Hash] = reader.next() match
        case Pktline.Packet.Line(data) =>
          val text = Objects.text(data).trim

          if text.starts(t"shallow ") then shallowLines(Git.Hash.unsafe(text.skip(8)) :: acc)
          else shallowLines(acc)

        case _ => acc.reverse

      val (shallow, pack) = sections(Nil)
      if pack.length > 0 then Packs.read(pack, _ => Unset, store, activity)
      shallow

  def upload
    ( objects: List[Git.Hash], read: Git.Hash => (Git.Object.Kind, Data), updates: List[RefUpdate],
      activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   List[(Text, Optional[Text])] =

    if updates == Nil then Nil else
      val reader = channel.advertise(receivePack)
      val advertised = reader.section()

      // The first line carries the server's capabilities after a NUL.
      val nul: Text = "\u0000".tt

      def beforeNul(line: Pktline.Packet.Line): Text = line.text.cut(nul).head.trim
      def afterNul(line: Pktline.Packet.Line): Text = Refstore.nth(line.text.cut(nul), 1).or(t"")

      val serverCapabilities = advertised.head.let(afterNul(_)).or(t"").trim.cut(t" ")

      def refEntry(line: Pktline.Packet.Line): List[(Text, Git.Hash)] =
        val fields = beforeNul(line).cut(t" ")

        Refstore.nth(fields, 0) match
          case hash: Text => Refstore.nth(fields, 1) match
            case name: Text => List((name, Git.Hash.unsafe(hash)))
            case _          => Nil

          case _ => Nil

      val current: List[(Text, Git.Hash)] = advertised.bind(refEntry(_))

      def old(update: RefUpdate): Text = update.expected match
        case Git.Backend.Expectation.Exactly(hash) => hash.text
        case Git.Backend.Expectation.Absent        => Git.Hash.zero.text

        case Git.Backend.Expectation.Unconditional =>
          current.filter(_(0) == update.ref).head.let(_(1).text).or(Git.Hash.zero.text)

      val wanted =
        List(t"report-status", t"delete-refs", t"ofs-delta").filter(serverCapabilities.has(_))

      val capabilities = (wanted + List(agent)).join(t" ")

      def command(update: RefUpdate, first: Boolean): Data =
        val body = t"${old(update)} ${update.to.lay(Git.Hash.zero.text)(_.text)} ${update.ref}"
        if first then Pktline.line(t"$body$nul$capabilities\n") else Pktline.line(t"$body\n")

      val commands = updates match
        case first :: rest => command(first, true) :: rest.map(command(_, false))
        case _             => Nil

      activity.begin(Git.Activity.Phase.Writing, true)
      activity.measure(Transfer.count(objects))
      def hasTarget(update: RefUpdate): Boolean = update.to.present

      val pack =
        if updates.exists(hasTarget(_)) then Packs.write(objects, read, activity) else Data()

      val body = Pktline.concatenate(List(commands, List(Pktline.flush), List(pack)))
      val response = channel.exchange(receivePack, body)

      // `unpack ok` then `ok <ref>` or `ng <ref> <reason>` per command.
      val report = response.section().map(_.text.trim)
      val unpack = report.filter(_.starts(t"unpack ")).head.let(_.skip(7)).or(t"ok")

      def reportLine(update: RefUpdate): Optional[Text] =
        val okPrefix = t"ok ${update.ref}"
        val ngPrefix = t"ng ${update.ref}"
        def matches(line: Text): Boolean = line == okPrefix || line.starts(t"$ngPrefix ")
        report.filter(matches(_)).head

      def result(update: RefUpdate): (Text, Optional[Text]) =
        val rejection: Optional[Text] = reportLine(update) match
          case line: Text if line.starts(t"ng ") => line.skip(4 + update.ref.length).trim
          case line: Text                        => Unset

          case _ =>
            if unpack == t"ok" then t"no report from the server" else t"unpack $unpack"

        (update.ref, rejection)

      updates.map(result(_))
