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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package galilei

import soundness.*

import filesystemBackends.virtualMachine

object Tests extends Suite(m"Galilei tests"):
  def run(): Unit =
    import charEncoders.utf8Encoder
    import charDecoders.utf8Decoder
    import textSanitizers.skipSanitizer

    suite(m"Direct read and write"):
      val leafName: Text = Uuid().show
      val dest: Path on Linux = unsafely((% / "tmp" / leafName).on[Linux])

      test(m"Writing then reading a file round-trips its content"):
        unsafely:
          dest.write(t"Hello world")
          dest.read[Text]
      . assert(_ == t"Hello world")

    suite(m"Opening files"):
      val openLeaf: Text = Uuid().show
      val dest: Path on Linux = unsafely((% / "tmp" / openLeaf).on[Linux])

      test(m"A fresh path does not exist"):
        dest
      . assert(!_.exists())

      test(m"A file opened for writing can be written and read back"):
        unsafely:
          dest.open[File](Write, OpenFlag.Create): handle ?=>
            handle.write(LazyList(t"Hello world".in[Data]))

          dest.read[Text]
      . assert(_ == t"Hello world")

      test(m"The path exists after writing"):
        dest
      . assert(_.exists())

      test(m"Opening an Eof appends to the file"):
        unsafely:
          Eof(dest).open(Write): handle ?=>
            handle.write(LazyList(t"!".in[Data]))

          dest.read[Text]
      . assert(_ == t"Hello world!")

      test(m"The file accessor reaches the contextual handle"):
        unsafely:
          dest.open[File]()(file.stream.read[Data]).utf8
      . assert(_ == t"Hello world!")

    suite(m"Opening directories"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.enabled

      val dirLeaf: Text = Uuid().show
      val root: Path on Linux = unsafely((% / "tmp" / dirLeaf).on[Linux])
      unsafely(root.create[Directory]())

      test(m"An opened directory can write and read back an entry"):
        unsafely:
          root.open[Directory](Read & Write): dir ?=>
            val target = dir / "greeting.txt"
            target.overwrite(t"Hello directory")
            target.contents[Text]
      . assert(_ == t"Hello directory")

      test(m"An entry is extant after writing, and a missing one is not"):
        unsafely:
          root.open[Directory](): dir ?=>
            ((dir / "greeting.txt").extant(), (dir / "missing.txt").extant())
      . assert(_ == (true, false))

      test(m"The entries of the directory root are listed"):
        unsafely:
          root.open[Directory](): dir ?=>
            dir.base.entries.to(List).map(_.name)
      . assert(_ == List(t"greeting.txt"))

      test(m"A removed entry is no longer extant"):
        unsafely:
          root.open[Directory](Read & Write): dir ?=>
            val doomed = dir / "doomed.txt"
            doomed.overwrite(t"temporary")
            doomed.remove()
            doomed.extant()
      . assert(_ == false)

      test(m"Opening a non-directory raises IoError"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          val plainFile: Path on Linux = root / "plain.txt"
          plainFile.write(t"not a directory")
          capture[IoError](plainFile.open[Directory]() { () }).reason
      . assert(_ == IoError.Reason.IsNotDirectory)

      test(m"A write operation without the Write grant does not compile"):
        demilitarize:
          root.open[Directory](): dir ?=>
            (dir / "nope.txt").overwrite(t"nope")
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"A dot-dot path element does not compile"):
        demilitarize:
          root.open[Directory](): dir ?=>
            dir / ".."
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"A path from one directory cannot be written under another"):
        demilitarize:
          root.open[Directory](): first ?=>
            root.open[Directory](Read & Write): second ?=>
              (first / "stolen.txt").overwrite(t"nope")
        . map(_.message)
      . assert(_.nonEmpty)

    suite(m"Creating entries"):
      import errorDiagnostics.emptyDiagnostics

      val createLeaf: Text = Uuid().show
      val base: Path on Linux = unsafely((% / "tmp" / createLeaf).on[Linux])
      unsafely(base.create[Directory]())

      test(m"Creating a file without Replace fails if it already exists"):
        unsafely:
          val target: Path on Linux = base / "once.txt"
          target.create[File]()
          capture[IoError](target.create[File]()).reason
      . assert(_ == IoError.Reason.AlreadyExists)

      test(m"Replace permits re-creation"):
        unsafely:
          val target: Path on Linux = base / "twice.txt"
          target.create[File]()
          target.create[File](CreateFlag.Replace)
          target.exists()
      . assert(_ == true)

      test(m"Creating beneath a missing parent fails without Parents"):
        unsafely:
          val target: Path on Linux = base / "no" / "such" / "deep"
          capture[IoError](target.create[Directory]()).reason
      . assert(_ == IoError.Reason.Nonexistent)

      test(m"Parents creates missing ancestors"):
        unsafely:
          val target: Path on Linux = base / "a" / "b" / "c"
          target.create[Directory](CreateFlag.Parents)
          target.exists()
      . assert(_ == true)

      test(m"Directory authoring provides a fresh-plane handle over the new directory"):
        unsafely:
          val target: Path on Linux = base / "authored"

          target.create[Directory](): dir ?=>
            (dir / "inner.txt").overwrite(t"hello")

          val inner: Path on Linux = target / "inner.txt"
          inner.read[Text]
      . assert(_ == t"hello")

      test(m"A failed directory authoring scope leaves nothing behind"):
        unsafely:
          val target: Path on Linux = base / "doomed-dir"

          capture[IoError]:
            target.create[Directory](): dir ?=>
              (dir / "x.txt").overwrite(t"data")
              abort(IoError(target, IoError.Operation.Write, IoError.Reason.Unsupported))

          target.exists()
      . assert(_ == false)

      test(m"File authoring commits the staged content on success"):
        unsafely:
          val target: Path on Linux = base / "staged.txt"

          target.create[File](): handle ?=>
            handle.write(LazyList(t"payload".in[Data]))

          target.read[Text]
      . assert(_ == t"payload")

      test(m"A failed file authoring scope leaves nothing behind"):
        unsafely:
          val target: Path on Linux = base / "doomed.txt"

          capture[IoError]:
            target.create[File](): handle ?=>
              handle.write(LazyList(t"data".in[Data]))
              abort(IoError(target, IoError.Operation.Write, IoError.Reason.Unsupported))

          target.exists()
      . assert(_ == false)

    suite(m"Scratch directories"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.enabled

      val scratchLeaf: Text = Uuid().show
      val base: Path on Linux = unsafely((% / "tmp" / scratchLeaf).on[Linux])
      unsafely(base.create[Directory]())

      test(m"A scratch directory works within its scope and vanishes afterwards"):
        unsafely:
          val (written, stem) = base.open[Scratch](Read & Write): scratch ?=>
            (scratch / "file.txt").overwrite(t"data")
            ((scratch / "file.txt").extant(), scratch.stem)

          (written, stem.exists())
      . assert(_ == (true, false))

      test(m"A scratch directory is removed even when the scope fails"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          var stem: Optional[Path on Linux] = Unset

          capture[IoError]:
            base.open[Scratch](Read & Write): scratch ?=>
              stem = scratch.stem
              abort(IoError(base, IoError.Operation.Write, IoError.Reason.Unsupported))

          stem.let(_.exists()).or(true)
      . assert(_ == false)

    suite(m"Memory-mapped access"):
      val ramLeaf: Text = Uuid().show
      val ramFile: Path on Linux = unsafely((% / "tmp" / ramLeaf).on[Linux])
      unsafely(ramFile.write(t"0123456789"))

      test(m"A mapped file serves positional reads"):
        unsafely:
          ramFile.open[Ram](): ram ?=>
            ram(2, 3).utf8
      . assert(_ == t"234")

      test(m"A mapped file accepts positional writes and persists them"):
        unsafely:
          ramFile.open[Ram](Read & Write): ram ?=>
            ram(3L) = t"XYZ".in[Data]

          ramFile.read[Text]
      . assert(_ == t"012XYZ6789")

      test(m"The expanse view reads consistently"):
        unsafely:
          ramFile.open[Ram](): ram ?=>
            val source = ram.expanse
            (source.size, source.read(0, 3).utf8)
      . assert(_ == (10L, t"012"))

      test(m"A positional write without the Write grant does not compile"):
        demilitarize:
          ramFile.open[Ram](): ram ?=>
            ram(0L) = t"no".in[Data]
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"Growing a mapping extends the file for positional writes past the old end"):
        val growLeaf: Text = Uuid().show
        val growFile: Path on Linux = unsafely((% / "tmp" / growLeaf).on[Linux])
        unsafely(growFile.write(t"0123456789"))
        unsafely:
          growFile.open[Ram](Read & Write): ram ?=>
            ram.grow(13L)
            ram(10L) = t"abc".in[Data]

          growFile.read[Text]
      . assert(_ == t"0123456789abc")

      test(m"Growing does not shrink when passed a smaller size"):
        unsafely:
          ramFile.open[Ram](Read & Write): ram ?=>
            ram.grow(4L)
            ram.size
      . assert(_ == 10L)

      test(m"Growing without the Write grant does not compile"):
        demilitarize:
          ramFile.open[Ram](): ram ?=>
            ram.grow(20L)
        . map(_.message)
      . assert(_.nonEmpty)

    suite(m"Creating mapped files"):
      import errorDiagnostics.emptyDiagnostics

      val ramCreateLeaf: Text = Uuid().show
      val ramBase: Path on Linux = unsafely((% / "tmp" / ramCreateLeaf).on[Linux])
      unsafely(ramBase.create[Directory]())

      test(m"A created mapping is sized, writable, and persists"):
        unsafely:
          val target: Path on Linux = ramBase / "fresh.bin"

          target.create[Ram](RamFlag.Size(16L)): ram ?=>
            ram(0L) = t"ABCD".in[Data]

          (target.read[Data].length, target.read[Data].slice(0, 4).utf8)
      . assert(_ == (16, t"ABCD"))

      test(m"Creating a mapping without Size is refused"):
        unsafely:
          val target: Path on Linux = ramBase / "unsized.bin"
          capture[IoError](target.create[Ram]() { () }).reason
      . assert(_ == IoError.Reason.Unsupported)

    suite(m"The access register"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.enabled

      val registerLeaf: Text = Uuid().show
      val outer: Path on Linux = unsafely((% / "tmp" / registerLeaf).on[Linux])
      val inner: Path on Linux = unsafely(outer / "nested")
      val siblingLeaf: Text = t"$registerLeaf-sibling"
      val sibling: Path on Linux = unsafely((% / "tmp" / siblingLeaf).on[Linux])
      unsafely(inner.create[Directory](CreateFlag.Parents))
      unsafely(sibling.create[Directory]())

      test(m"Overlapping Read opens coexist"):
        unsafely:
          outer.open[Directory](): a ?=>
            inner.open[Directory](): b ?=>
              true
      . assert(_ == true)

      test(m"An Exclusive open conflicts with an overlapping open"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          capture[IoError]:
            outer.open[Directory](): a ?=>
              inner.open[Directory](Read & Exclusive) { () }
          . reason
      . assert(_ == IoError.Reason.Busy)

      test(m"An open under an Exclusive open conflicts"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          capture[IoError]:
            outer.open[Directory](Read & Exclusive): a ?=>
              inner.open[Directory]() { () }
          . reason
      . assert(_ == IoError.Reason.Busy)

      test(m"An Exclusive open of a sibling does not conflict"):
        unsafely:
          outer.open[Directory](Read & Exclusive): a ?=>
            sibling.open[Directory](Read & Exclusive): b ?=>
              true
      . assert(_ == true)

      test(m"An Exclusive scope is released when it ends"):
        unsafely:
          outer.open[Directory](Read & Exclusive) { () }
          outer.open[Directory](Read & Exclusive) { true }
      . assert(_ == true)
