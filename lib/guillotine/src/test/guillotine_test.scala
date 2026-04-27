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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package guillotine

import ambience.*
import ambience.workingDirectories.default
import anticipation.*
import anticipation.abstractables.durationIsAbstractable
import contingency.*, strategies.throwUnsafely
import distillate.*
import fulminate.*
import gossamer.*
import nomenclature.*
import parasite.*
import prepositional.*
import probably.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import errorDiagnostics.empty

given silentExecEvent: ExecEvent is Loggable =
  new Loggable:
    type Self = ExecEvent
    def log(level: Level, realm: Realm, timestamp: Long, event: ExecEvent): Unit = ()


object Tests extends Suite(m"Guillotine tests"):
  def run(): Unit =
    suite(m"Parsing"):
      test(m"parse simple command"):
        sh"ls -la"
      . assert(_ == Command(t"ls", t"-la"))

      test(m"parse a substitution"):
        val flags = t"-la"
        sh"ls $flags"
      . assert(_ == Command(t"ls", t"-la"))

      test(m"parse two substitutions"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls $flags $file"
      . assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"parse irregular spacing"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags     $file"
      . assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"parse irregular spacing 2"):
        val flags = t"-la"
        val file = t"filename"
        sh"ls  $flags $file"
      . assert(_ == Command(t"ls", t"-la", t"filename"))

      test(m"adjacent substitutions"):
        val a = t"a"
        val b = t"b"
        sh"ls $a$b"
      . assert(_ == Command(t"ls", t"ab"))

      test(m"substitute a list"):
        val a = List(t"a", t"b")
        sh"ls $a"
      . assert(_ == Command(t"ls", t"a", t"b"))

      test(m"substitute a single-quoted list"):
        val a = List(t"a", t"b")
        sh"ls '$a'"
      . assert(_ == Command(t"ls", t"a b"))

      test(m"substitute in a double-quoted list"):
        val a = List(t"a", t"b")
        sh"""ls "$a""""
      . assert(_ == Command(t"ls", t"a b"))

      test(m"insertion after arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}x"""
      . assert(_ == Command(t"ls", t"a", t"bx"))

      test(m"insertion before arg"):
        val a = List(t"a", t"b")
        sh"""ls x${a}"""
      . assert(_ == Command(t"ls", t"xa", t"b"))

      test(m"insertion before quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}'x'"""
      . assert(_ == Command(t"ls", t"a", t"bx"))

      test(m"insertion after quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls 'x'${a}"""
      . assert(_ == Command(t"ls", t"xa", t"b"))

      test(m"empty list insertion unquoted"):
        val a = List()
        sh"""ls ${a}"""
      . assert(_ == Command(t"ls"))

      test(m"empty list insertion quoted"):
        val a = List()
        sh"""ls '${a}'"""
      . assert(_ == Command(t"ls", t""))

      test(m"empty parameters"):
        sh"""ls '' ''"""
      . assert(_ == Command(t"ls", t"", t""))

      test(m"three empty parameters"):
        sh"""ls '' '' ''"""
      . assert(_ == Command(t"ls", t"", t"", t""))

      test(m"one empty parameter, specified twice"):
        sh"""ls ''''"""
      . assert(_ == Command(t"ls", t""))

      test(m"single quote inside double quotes"):
        sh"""ls "'" """
      . assert(_ == Command(t"ls", t"'"))

      test(m"double quote inside single quotes"):
        sh"""ls '"' """
      . assert(_ == Command(t"ls", t"\""))

      test(m"escaped double quote"):
        sh"""ls \" """
      . assert(_ == Command(t"ls", t"\""))

      test(m"escaped single quote"):
        sh"""ls \' """
      . assert(_ == Command(t"ls", t"'"))

      test(m"escape inside double quotes"):
        sh"""ls "a\"b" """
      . assert(_ == Command(t"ls", t"a\"b"))

      test(m"backslash inside single quotes is literal"):
        sh"""ls 'a\b'"""
      . assert(_ == Command(t"ls", t"a\\b"))

      test(m"substitute a Pid"):
        val pid = Pid(42L)
        sh"echo $pid"
      . assert(_ == Command(t"echo", t"↯42"))

      test(m"substitute a Path on Linux"):
        val path: Path on Linux = (% / "etc" / "hosts").on[Linux]
        sh"cat $path"
      . assert(_ == Command(t"cat", t"/etc/hosts"))

    suite(m"Showable, Inspectable, equality"):
      test(m"render simple command"):
        (sh"echo Hello World": Command).inspect
      . check(_ == t"""sh"echo Hello World"""")

      test(m"render command with quoted space"):
        (sh"echo 'Hello World'": Command).inspect
      . check(_ == t"""sh"echo 'Hello World'"""")

      test(m"render command with quote and space"):
        Command(t"echo", t"Don't stop").inspect
      . check(_ == t"sh\"\"\"echo \"Don't stop\"\"\"\"")

      test(m"render command with single and double quote"):
        Command(t"echo", t"single ' and double \" quotes").inspect
      . check(_ == t"sh\"\"\"echo \"single ' and double \\\" quotes\"\"\"\"")

      test(m"render command with tab"):
        Command(t"echo", t"a\tb").inspect
      . check(_ == t"""sh"echo 'a\tb'"""")

      test(m"render command with backslash"):
        Command(t"echo", t"back\\slash").inspect
      . check(_ == t"""sh"echo 'back\\slash'"""")

      test(m"render pipeline of two commands"):
        (sh"echo Hello" | sh"sed s/e/a/g").inspect
      . check(_ == t"""sh"echo Hello" | sh"sed s/e/a/g"""")

      test(m"render pipeline of three commands"):
        (sh"echo Hello" | sh"sed s/e/a/g" | sh"wc -c").inspect
      . check(_ == t"""sh"echo Hello" | sh"sed s/e/a/g" | sh"wc -c"""")

      test(m"command Showable produces unquoted form"):
        val cmd: Command = sh"echo hi"
        cmd.show
      . assert(_ == t"echo hi")

      test(m"pipeline Showable joins with pipe"):
        val pipe: Pipeline = sh"echo hi" | sh"cat"
        pipe.show
      . assert(_ == t"echo hi | cat")

      test(m"two commands written differently are equivalent"):
        sh"echo 'hello world'"
      . assert(_ == sh"""echo "hello world"""")

      test(m"commands with different whitespace are equal"):
        sh"one two   three"
      . assert(_ == sh"one   two three")

    suite(m"Pipeline construction"):
      test(m"pipe of two commands produces Pipeline of length 2"):
        (sh"echo a" | sh"cat").commands.length
      . assert(_ == 2)

      test(m"pipe of three commands flattens"):
        (sh"echo a" | sh"cat" | sh"wc").commands.length
      . assert(_ == 3)

      test(m"piping a Pipeline into a Command flattens"):
        ((sh"a" | sh"b")(sh"c")).commands.length
      . assert(_ == 3)

      test(m"piping a Command into a Pipeline flattens"):
        (sh"a"(sh"b" | sh"c")).commands.length
      . assert(_ == 3)

      test(m"piping two pipelines flattens to 4"):
        ((sh"a" | sh"b")(sh"c" | sh"d")).commands.length
      . assert(_ == 4)

      test(m"`|` and `apply` produce equal pipelines"):
        (sh"echo a" | sh"cat") == sh"cat"(sh"echo a")
      . assert(_ == true)

    suite(m"Parameterizable substitution"):
      test(m"Pid substitutes with no extra quoting"):
        val pid = Pid(42L)
        sh"kill $pid"
      . assert(_ == Command(t"kill", t"↯42"))

      test(m"Path substitutes encoded form"):
        val path: Path on Linux = (% / "tmp" / "x").on[Linux]
        sh"rm $path"
      . assert(_ == Command(t"rm", t"/tmp/x"))

      test(m"Custom Parameterizable via contramap"):
        case class Tag(value: Int)
        given Tag is Parameterizable = summon[Text is Parameterizable].contramap(t => t"<${t.value}>")
        sh"echo ${Tag(7)}"
      . assert(_ == Command(t"echo", t"<7>"))

    suite(m"Execution — result shapes"):
      test(m"echo string"):
        sh"echo hello".exec[Text]().trim
      . assert(_ == t"hello")

      test(m"substitute string into echo"):
        val text = t"Hello world!"
        sh"echo $text".exec[Text]().trim
      . assert(_ == t"Hello world!")

      test(m"pipe output through two commands"):
        (sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]().trim
      . assert(_ == t"Hallo world")

      test(m"pipe output through three commands"):
        (sh"echo 'a b c'" | sh"tr ' ' '\n'" | sh"wc -l").exec[Text]().trim
      . assert(_ == t"3")

      test(m"read stream of strings"):
        sh"echo 'Hello world'".exec[Stream[Text]]().to(List)
      . assert(_ == List(t"Hello world"))

      test(m"read list of strings"):
        sh"printf 'a\nb\nc\n'".exec[List[Text]]()
      . assert(_ == List(t"a", t"b", t"c"))

      test(m"read stream of bytes"):
        sh"echo 'Hello world'".exec[Stream[Data]]().read[Data].to(List)
      . assert(_ == Data(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10).to(List))

      test(m"read as String"):
        sh"echo hi".exec[String]().trim
      . assert(_ == "hi")

      test(m"read Stderr"):
        sh"sh -c 'echo oops 1>&2'".exec[Stderr]().text.trim
      . assert(_ == t"oops")

      test(m"exec[Unit] succeeds"):
        sh"true".exec[Unit]()
      . assert(_ == ())

      test(m"successful Exit"):
        sh"true".exec[Exit]()
      . assert(_ == Exit.Ok)

      test(m"failed Exit"):
        sh"false".exec[Exit]()
      . assert(_ == Exit.Fail(1))

      test(m"non-default Exit code"):
        sh"sh -c 'exit 7'".exec[Exit]()
      . assert(_ == Exit.Fail(7))

    suite(m"Process lifecycle"):
      test(m"fork sleeping process is non-blocking"):
        val t0 = java.lang.System.currentTimeMillis
        sh"sleep 0.2".fork[Unit]()
        java.lang.System.currentTimeMillis - t0
      . assert(_ <= 100L)

      test(m"exec sleeping process blocks"):
        val t0 = java.lang.System.currentTimeMillis
        sh"sleep 0.2".exec[Unit]()
        java.lang.System.currentTimeMillis - t0
      . assert(_ >= 200L)

      test(m"fork and await"):
        val t0 = java.lang.System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.await()
        java.lang.System.currentTimeMillis - t0
      . assert(_ >= 200L)

      test(m"fork and abort"):
        val t0 = java.lang.System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.abort()
        java.lang.System.currentTimeMillis - t0
      . assert(_ <= 100L)

      test(m"fork and kill"):
        val t0 = java.lang.System.currentTimeMillis
        val proc = sh"sleep 0.2".fork[Unit]()
        proc.kill()
        java.lang.System.currentTimeMillis - t0
      . assert(_ <= 100L)

      test(m"alive flag is true after fork"):
        val proc = sh"sleep 0.5".fork[Unit]()
        val a = proc.alive
        proc.kill()
        a
      . assert(_ == true)

      test(m"alive flag is false after attend"):
        val proc = sh"sh -c 'exit 0'".fork[Unit]()
        proc.attend()
        proc.alive
      . assert(_ == false)

      test(m"exitStatus reflects exit code"):
        val proc = sh"sh -c 'exit 5'".fork[Unit]()
        proc.exitStatus()
      . assert(_ == Exit.Fail(5))

      test(m"await with timeout fires AsyncError"):
        val proc = sh"sleep 1".fork[Unit]()
        val outcome = capture[AsyncError](proc.await(50_000_000L))
        proc.kill()
        outcome
      . assert(_.reason == AsyncError.Reason.Timeout)

      test(m"await with sufficient duration returns"):
        val proc = sh"sleep 0.05".fork[Unit]()
        proc.await(2_000_000_000L)
      . assert(_ == ())

    suite(m"Stdin and stderr"):
      test(m"pipe Stream[Data] into stdin"):
        val proc = sh"cat".fork[Text]()
        proc.stdin(Stream(Data(104, 105, 10)))
        proc.await().trim
      . assert(_ == t"hi")

      test(m"read stderr from a forked job"):
        val proc = sh"sh -c 'echo err 1>&2; sleep 0.05'".fork[Unit]()
        val bytes = proc.stderr().read[Data]
        proc.await()
        String(bytes.mutable(using Unsafe), "UTF-8").nn.trim
      . assert(_ == "err")

    suite(m"Pid"):
      test(m"Pid show formats with arrow"):
        Pid(123L).show
      . assert(_ == t"↯123")

      test(m"Pid encode formats with arrow"):
        Pid(123L).encode
      . assert(_ == t"↯123")

      test(m"decode Pid from numeric Text"):
        t"42".decode[Pid]
      . assert(_ == Pid(42L))

      test(m"decode Pid from non-numeric Text raises NumberError"):
        capture[NumberError](t"abc".decode[Pid])
      . assert(_.text == t"abc")

    suite(m"OS Process"):
      test(m"Process() returns the current process"):
        Process().pid.value
      . assert(_ == ProcessHandle.current.nn.pid)

      test(m"Process(pid) constructs from a valid pid"):
        val cur = Process()
        Process(cur.pid).pid
      . assert(_ == Process().pid)

      test(m"Process(invalid pid) raises PidError"):
        capture[PidError](Process(Pid(Long.MaxValue)))
      . assert(_.pid == Pid(Long.MaxValue))

      test(m"current process has a parent"):
        Process().parent.let(_.pid).or(Pid(0L))
      . assert(_ != Pid(0L))

    suite(m"Job-as-Process"):
      test(m"Job exposes its OS process via .process"):
        val job = sh"sleep 0.3".fork[Unit]()
        val osProc = job.process
        val matched = osProc.pid == job.pid
        job.kill()
        matched
      . assert(_ == true)

    suite(m"Implied return type via Intelligible"):
      test(m"echo() returns Text"):
        sh"echo hi"().trim
      . assert(_ == t"hi")

      test(m"head() returns Stream[Text]"):
        sh"head -n 1 /dev/null"().to(List)
      . assert(_ == Nil)

      test(m"sleep() returns Exit"):
        sh"sleep 0.01"()
      . assert(_ == Exit.Ok)

    suite(m"ExecError"):
      test(m"running a missing binary raises ExecError"):
        capture[ExecError](sh"definitely-not-a-binary-xyz".exec[Text]())
      . assert(_.command.arguments.head == t"definitely-not-a-binary-xyz")

      test(m"ExecError reports the failing command"):
        val err = capture[ExecError](sh"definitely-not-a-binary-xyz".exec[Text]())
        err.command.arguments.length
      . assert(_ == 1)

    suite(m"Nested commands"):
      test(m"nested Command in sh -c"):
        val cmd = sh"echo 'Hello world'"
        sh"sh -c '$cmd'".exec[Text]().trim
      . assert(_ == t"Hello world")

      test(m"Command#escape wraps each argument in single quotes"):
        Command(t"echo", t"a b").escape
      . assert(_ == t"'echo' 'a b'")
