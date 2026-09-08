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
package guillotine

import soundness.*

import workingDirectories.javaBaseWorkingDirectory
import abstractables.millisecondsAbstractable
import strategies.throwUnsafely
import errorDiagnostics.emptyDiagnostics

given silentExecEvent: Exec.Event is Loggable =
  new Loggable:
    type Self = Exec.Event
    def log(level: Level, timestamp: Long, event: => Exec.Event): Unit = ()


object Tests extends Suite(m"Guillotine tests"):
  def run(): Unit =
    suite(m"Parsing"):
      test(m"parse simple command"):
        sh"ls -la"
      . assert(_ == Command("ls", "-la"))

      test(m"parse a substitution"):
        val flags = "-la"
        sh"ls $flags"
      . assert(_ == Command("ls", "-la"))

      test(m"parse two substitutions"):
        val flags = "-la"
        val file = "filename"
        sh"ls $flags $file"
      . assert(_ == Command("ls", "-la", "filename"))

      test(m"parse irregular spacing"):
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags     $file"
      . assert(_ == Command("ls", "-la", "filename"))

      test(m"parse irregular spacing 2"):
        val flags = "-la"
        val file = "filename"
        sh"ls  $flags $file"
      . assert(_ == Command("ls", "-la", "filename"))

      test(m"adjacent substitutions"):
        val a = "a"
        val b = "b"
        sh"ls $a$b"
      . assert(_ == Command("ls", "ab"))

      test(m"substitute a list"):
        val a = List(t"a", t"b")
        sh"ls $a"
      . assert(_ == Command("ls", "a", "b"))

      test(m"substitute a single-quoted list"):
        val a = List(t"a", t"b")
        sh"ls '$a'"
      . assert(_ == Command("ls", "a b"))

      test(m"substitute in a double-quoted list"):
        val a = List(t"a", t"b")
        sh"""ls "$a""""
      . assert(_ == Command("ls", "a b"))

      test(m"insertion after arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}x"""
      . assert(_ == Command("ls", "a", "bx"))

      test(m"insertion before arg"):
        val a = List(t"a", t"b")
        sh"""ls x${a}"""
      . assert(_ == Command("ls", "xa", "b"))

      test(m"insertion before quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls ${a}'x'"""
      . assert(_ == Command("ls", "a", "bx"))

      test(m"insertion after quoted arg"):
        val a = List(t"a", t"b")
        sh"""ls 'x'${a}"""
      . assert(_ == Command("ls", "xa", "b"))

      test(m"empty list insertion unquoted"):
        val a = List()
        sh"""ls ${a}"""
      . assert(_ == Command("ls"))

      test(m"empty list insertion quoted"):
        val a = List()
        sh"""ls '${a}'"""
      . assert(_ == Command("ls", ""))

      test(m"empty parameters"):
        sh"""ls '' ''"""
      . assert(_ == Command("ls", "", ""))

      test(m"three empty parameters"):
        sh"""ls '' '' ''"""
      . assert(_ == Command("ls", "", "", ""))

      test(m"one empty parameter, specified twice"):
        sh"""ls ''''"""
      . assert(_ == Command("ls", ""))

      test(m"single quote inside double quotes"):
        sh"""ls "'" """
      . assert(_ == Command("ls", "'"))

      test(m"double quote inside single quotes"):
        sh"""ls '"' """
      . assert(_ == Command("ls", "\""))

      test(m"escaped double quote"):
        sh"""ls \" """
      . assert(_ == Command("ls", "\""))

      test(m"escaped single quote"):
        sh"""ls \' """
      . assert(_ == Command("ls", "'"))

      test(m"escape inside double quotes"):
        sh"""ls "a\"b" """
      . assert(_ == Command("ls", "a\"b"))

      test(m"backslash inside single quotes is literal"):
        sh"""ls 'a\b'"""
      . assert(_ == Command("ls", "a\\b"))

      test(m"substitute a Pid"):
        val pid = Pid(42L)
        sh"echo $pid"
      . assert(_ == Command("echo", "↯42"))

      test(m"substitute a Path on Linux"):
        val path: Path on Linux = (% / "etc" / "hosts").on[Linux]
        sh"cat $path"
      . assert(_ == Command("cat", "/etc/hosts"))

    suite(m"Compile-time checking"):
      test(m"unterminated single quote is a compile error"):
        demilitarize:
          sh"echo 'Hello world"
        . map(_.message)
      . assert(_.headOption.exists(_.contains("single quote")))

      test(m"unterminated single quote focus is the opening quote"):
        demilitarize:
          sh"echo 'Hello world"
        . map(_.focus)
      . assert(_ == List("'"))

      test(m"unterminated double quote is a compile error"):
        demilitarize:
          sh"""echo "Hello"""
        . map(_.message)
      . assert(_.headOption.exists(_.contains("double quote")))

      test(m"unterminated double quote focus is the opening quote"):
        demilitarize:
          sh"""echo "Hello"""
        . map(_.focus)
      . assert(_ == List("\""))

      test(m"trailing escape character is a compile error"):
        demilitarize:
          sh"""echo \"""
        . map(_.message)
      . assert(_.headOption.exists(_.contains("escape character")))

      test(m"trailing escape character focus is the backslash"):
        demilitarize:
          sh"""echo \"""
        . map(_.focus)
      . assert(_ == List("\\"))

      test(m"escape character before a substitution is a compile error"):
        val file = "file"
        demilitarize:
          sh"""ls \$file"""
        . map(_.message)
      . assert(_.headOption.exists(_.contains("substitution")))

      test(m"quote opened after a substitution is positioned correctly"):
        val flags = "-la"
        demilitarize:
          sh"ls $flags 'unclosed"
        . map(_.focus)
      . assert(_ == List("'"))

      test(m"a well-formed command produces no compile errors"):
        demilitarize:
          sh"""echo 'Hello world' "a b c" plain"""
      . assert(_ == Nil)

    suite(m"Showable, Inspectable, equality"):
      test(m"render simple command"):
        (sh"echo Hello World": Command).inspect
      . check(_ == """sh"echo Hello World"""")

      test(m"render command with quoted space"):
        (sh"echo 'Hello World'": Command).inspect
      . check(_ == """sh"echo 'Hello World'"""")

      test(m"render command with quote and space"):
        Command("echo", "Don't stop").inspect
      . check(_ == "sh\"\"\"echo \"Don't stop\"\"\"\"")

      test(m"render command with single and double quote"):
        Command("echo", "single ' and double \" quotes").inspect
      . check(_ == "sh\"\"\"echo \"single ' and double \\\" quotes\"\"\"\"")

      test(m"render command with tab"):
        Command("echo", "a\tb").inspect
      . check(_ == t"""sh"echo 'a\tb'"""")

      test(m"render command with backslash"):
        Command("echo", "back\\slash").inspect
      . check(_ == t"""sh"echo 'back\\slash'"""")

      test(m"render pipeline of two commands"):
        (sh"echo Hello" | sh"sed s/e/a/g").inspect
      . check(_ == """sh"echo Hello" | sh"sed s/e/a/g"""")

      test(m"render pipeline of three commands"):
        (sh"echo Hello" | sh"sed s/e/a/g" | sh"wc -c").inspect
      . check(_ == """sh"echo Hello" | sh"sed s/e/a/g" | sh"wc -c"""")

      test(m"command Showable produces unquoted form"):
        val cmd: Command = sh"echo hi"
        cmd.show
      . assert(_ == "echo hi")

      test(m"pipeline Showable joins with pipe"):
        val pipe: Pipeline = sh"echo hi" | sh"cat"
        pipe.show
      . assert(_ == "echo hi | cat")

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
      . assert(_ == Command("kill", "↯42"))

      test(m"Path substitutes encoded form"):
        val path: Path on Linux = (% / "tmp" / "x").on[Linux]
        sh"rm $path"
      . assert(_ == Command("rm", "/tmp/x"))

      test(m"Custom Parameterizable via contramap"):
        case class Tag(value: Int)
        given Tag is Parameterizable = summon[Text is Parameterizable].contramap(t => t"<${t.value}>")
        sh"echo ${Tag(7)}"
      . assert(_ == Command("echo", "<7>"))

    suite(m"Execution — result shapes"):
      test(m"echo string"):
        sh"echo hello".exec[Text]().trim
      . assert(_ == "hello")

      test(m"substitute string into echo"):
        val text = "Hello world!"
        sh"echo $text".exec[Text]().trim
      . assert(_ == "Hello world!")

      test(m"pipe output through two commands"):
        (sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]().trim
      . assert(_ == "Hallo world")

      test(m"pipe output through three commands"):
        (sh"echo 'a b c'" | sh"tr ' ' '\n'" | sh"wc -l").exec[Text]().trim
      . assert(_ == "3")

      test(m"read stream of strings"):
        sh"echo 'Hello world'".exec[Iterator[Text]]().to(List)
      . assert(_ == List(t"Hello world"))

      test(m"read list of strings"):
        sh"printf 'a\nb\nc\n'".exec[List[Text]]()
      . assert(_ == List(t"a", t"b", t"c"))

      test(m"read all bytes"):
        sh"echo 'Hello world'".exec[Data]().to[List]
      . assert(_ == Data(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10).to[List])

      test(m"read as String"):
        sh"echo hi".exec[String]().trim
      . assert(_ == "hi")

      test(m"read Stderr"):
        sh"sh -c 'echo oops 1>&2'".exec[Stderr]().text.trim
      . assert(_ == "oops")

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

      test(m"await with timeout fires Async.Error"):
        val proc = sh"sleep 1".fork[Unit]()
        val outcome = capture[Async.Error](proc.await(50L))
        proc.kill()
        outcome
      . assert(_.reason == Async.Error.Reason.Timeout)

      test(m"await with sufficient duration returns"):
        val proc = sh"sleep 0.05".fork[Unit]()
        proc.await(2000L)
      . assert(_ == ())

    suite(m"Stdin and stderr"):
      test(m"pipe Chain[Data] into stdin"):
        val proc = sh"cat".fork[Text]()
        proc.stdin(Stream(Data(104, 105, 10)))
        proc.await().trim
      . assert(_ == "hi")

      test(m"drive a live process incrementally through its intake"):
        val proc = sh"cat".fork[Text]()
        val intake = proc.intake
        intake.put(Data(104, 105, 10))
        intake.flush()
        // The pipe is still open, so `cat` is still running and echoing.
        val alive = proc.alive
        intake.put(Data(98, 121, 101, 10))
        intake.finish()
        (alive, proc.await().trim)
      . assert(_ == (true, "hi\nbye"))

      test(m"pipe Chain[Data] into the head of a pipeline"):
        val proc = (sh"cat" | sh"tr a-z A-Z").fork[Text]()
        proc.stdin(Stream(Data(104, 105, 10)))
        proc.await().trim
      . assert(_ == "HI")

      test(m"drive a live pipeline incrementally through its intake"):
        val proc = (sh"cat" | sh"tr a-z A-Z").fork[Text]()
        val intake = proc.intake
        intake.put(Data(104, 105, 10))
        intake.flush()
        val alive = proc.alive
        intake.put(Data(98, 121, 101, 10))
        intake.finish()
        (alive, proc.await().trim)
      . assert(_ == (true, "HI\nBYE"))

      test(m"read stderr from a forked job"):
        val proc = sh"sh -c 'echo err 1>&2; sleep 0.05'".fork[Unit]()
        val bytes = proc.stderr().memoize
        proc.await()
        bytes.utf8.s.trim
      . assert(_ == "err")

    suite(m"Pid"):
      test(m"Pid show formats with arrow"):
        Pid(123L).show
      . assert(_ == "↯123")

      test(m"Pid encode formats with arrow"):
        Pid(123L).encode
      . assert(_ == "↯123")

      test(m"decode Pid from numeric Text"):
        "42".as[Pid]
      . assert(_ == Pid(42L))

      test(m"decode Pid from non-numeric Text raises Number.Error"):
        capture[Number.Error]("abc".as[Pid])
      . assert(_.text == "abc")

    suite(m"OS Process"):
      test(m"Process() returns the current process"):
        Process().pid.value
      . assert(_ == ProcessHandle.current.nn.pid)

      test(m"Process(pid) constructs from a valid pid"):
        val cur = Process()
        Process(cur.pid).pid
      . assert(_ == Process().pid)

      test(m"Process(invalid pid) raises Pid.Error"):
        // `.pid` rather than the `Process` itself: the fresh capability may not leak into
        // `capture`'s result.
        capture[Pid.Error](Process(Pid(Long.MaxValue)).pid)
      . assert(_.pid == Pid(Long.MaxValue))

      test(m"current process has a parent"):
        // A direct match rather than `let`/`or`: the `Optionality` evidence cannot span the
        // capability-typed element.
        Process().parent match
          case parent: Process => parent.pid
          case _               => Pid(0L)
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
      . assert(_ == "hi")

      test(m"head() returns Chain[Text]"):
        sh"head -n 1 /dev/null"().to(List)
      . assert(_ == Nil)

      test(m"sleep() returns Exit"):
        sh"sleep 0.01"()
      . assert(_ == Exit.Ok)

    suite(m"Exec.Error"):
      test(m"running a missing binary raises Exec.Error"):
        capture[Exec.Error](sh"definitely-not-a-binary-xyz".exec[Text]())
      . assert(_.command.arguments.head == "definitely-not-a-binary-xyz")

      test(m"Exec.Error reports the failing command"):
        val err = capture[Exec.Error](sh"definitely-not-a-binary-xyz".exec[Text]())
        err.command.arguments.length
      . assert(_ == 1)

    suite(m"Nested commands"):
      test(m"nested Command in sh -c"):
        val cmd = sh"echo 'Hello world'"
        sh"sh -c '$cmd'".exec[Text]().trim
      . assert(_ == "Hello world")

      test(m"Command#escape wraps each argument in single quotes"):
        Command("echo", "a b").escape
      . assert(_ == "'echo' 'a b'")

    suite(m"Native-rendering coverage"):
      test(m"guillotine's types inspect natively"):
        Inspectable.fallbacks(Pid(1234L).inspect, sh"ls -la".inspect)
      . assert(_ == Nil)

      test(m"A PID shows its number, marked as a process"):
        Pid(1234L).inspect
      . assert(_ == "↯1234")
