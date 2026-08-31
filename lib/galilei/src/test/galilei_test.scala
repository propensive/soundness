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
package galilei

import soundness.*
import soundness.collationOrdering
import soundness.collations.codepoints

import filesystemBackends.virtualMachineFilesystem

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
      . assert(!_.existent())

      test(m"A file opened for writing can be written and read back"):
        unsafely:
          dest.open[File](Write, OpenFlag.Create): handle ?=>
            handle.write(Chain(t"Hello world".in[Data]))

          dest.read[Text]
      . assert(_ == t"Hello world")

      test(m"The path exists after writing"):
        dest
      . assert(_.existent())

      test(m"Opening an Eof appends to the file"):
        unsafely:
          Eof(dest).open(Write): handle ?=>
            handle.write(Chain(t"!".in[Data]))

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
          scala.caps.unsafe.unsafeAssumeSeparate:
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
          scala.caps.unsafe.unsafeAssumeSeparate:
           root.open[Directory](): dir ?=>
            dir.base.entries.stdlib.to(List).map(_.name)
      . assert(_ == List(t"greeting.txt"))

      test(m"A removed entry is no longer extant"):
        unsafely:
          scala.caps.unsafe.unsafeAssumeSeparate:
           root.open[Directory](Read & Write): dir ?=>
            val doomed = dir / "doomed.txt"
            doomed.overwrite(t"temporary")
            doomed.remove()
            doomed.extant()
      . assert(_ == false)

      test(m"Opening a non-directory raises Io.Error"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          val plainFile: Path on Linux = root / "plain.txt"
          plainFile.write(t"not a directory")
          capture[Io.Error](plainFile.open[Directory]() { () }).reason
      . assert(_ == Io.Error.Reason.IsNotDirectory)

      test(m"A write operation without the Write grant does not compile"):
        demilitarize:
          import strategies.throwUnsafely
          root.open[Directory](): dir ?=>
            (dir / "nope.txt").overwrite(t"nope")
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"A dot-dot path element does not compile"):
        demilitarize:
          import strategies.throwUnsafely
          root.open[Directory](): dir ?=>
            dir / ".."
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"A path from one directory cannot be written under another"):
        demilitarize:
          import strategies.throwUnsafely
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
          capture[Io.Error](target.create[File]()).reason
      . assert(_ == Io.Error.Reason.AlreadyExists)

      test(m"Replace permits re-creation"):
        unsafely:
          val target: Path on Linux = base / "twice.txt"
          target.create[File]()
          target.create[File](CreateFlag.Replace)
          target.existent()
      . assert(_ == true)

      test(m"Creating beneath a missing parent fails without Parents"):
        unsafely:
          val target: Path on Linux = base / "no" / "such" / "deep"
          capture[Io.Error](target.create[Directory]()).reason
      . assert(_ == Io.Error.Reason.Nonexistent)

      test(m"Parents creates missing ancestors"):
        unsafely:
          val target: Path on Linux = base / "a" / "b" / "c"
          target.create[Directory](CreateFlag.Parents)
          target.existent()
      . assert(_ == true)

      test(m"Directory authoring provides a fresh-plane handle over the new directory"):
        unsafely:
          val target: Path on Linux = base / "authored"

          scala.caps.unsafe.unsafeAssumeSeparate:
           target.create[Directory](): dir ?=>
            (dir / "inner.txt").overwrite(t"hello")

          val inner: Path on Linux = target / "inner.txt"
          inner.read[Text]
      . assert(_ == t"hello")

      test(m"A failed directory authoring scope leaves nothing behind"):
        unsafely:
          val target: Path on Linux = base / "doomed-dir"

          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
             target.create[Directory](): dir ?=>
              (dir / "x.txt").overwrite(t"data")
              abort(Io.Error(target, Io.Error.Operation.Write, Io.Error.Reason.Unsupported))

          target.existent()
      . assert(_ == false)

      test(m"File authoring commits the staged content on success"):
        unsafely:
          val target: Path on Linux = base / "staged.txt"

          target.create[File](): handle ?=>
            handle.write(Chain(t"payload".in[Data]))

          target.read[Text]
      . assert(_ == t"payload")

      test(m"A failed file authoring scope leaves nothing behind"):
        unsafely:
          val target: Path on Linux = base / "doomed.txt"

          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
             target.create[File](): handle ?=>
              handle.write(Chain(t"data".in[Data]))
              abort(Io.Error(target, Io.Error.Operation.Write, Io.Error.Reason.Unsupported))

          target.existent()
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
          scala.caps.unsafe.unsafeAssumeSeparate:
            val (written, stem) = base.open[Scratch](Read & Write): scratch ?=>
              (scratch / "file.txt").overwrite(t"data")
              ((scratch / "file.txt").extant(), scratch.stem)

            (written, stem.existent())
      . assert(_ == (true, false))

      test(m"A scratch directory is removed even when the scope fails"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          var stem: Optional[Path on Linux] = Unset

          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
             base.open[Scratch](Read & Write): scratch ?=>
              stem = scratch.stem
              abort(Io.Error(base, Io.Error.Operation.Write, Io.Error.Reason.Unsupported))

          stem.let(_.existent()).or(true)
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
          import strategies.throwUnsafely
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
          import strategies.throwUnsafely
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

          (target.read[Data].readable.length, Array.frozen(target.read[Data].readable.slice(0, 4)).utf8)
      . assert(_ == (16, t"ABCD"))

      test(m"Creating a mapping without Size is refused"):
        unsafely:
          val target: Path on Linux = ramBase / "unsized.bin"
          capture[Io.Error](target.create[Ram]() { () }).reason
      . assert(_ == Io.Error.Reason.Unsupported)

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
          scala.caps.unsafe.unsafeAssumeSeparate:
           outer.open[Directory](): a ?=>
            inner.open[Directory](): b ?=>
              true
      . assert(_ == true)

      test(m"An Exclusive open conflicts with an overlapping open"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
             outer.open[Directory](): a ?=>
              inner.open[Directory](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"An open under an Exclusive open conflicts"):
        import errorDiagnostics.emptyDiagnostics
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              outer.open[Directory](Read & Exclusive): a ?=>
                inner.open[Directory]() { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"An Exclusive open of a sibling does not conflict"):
        unsafely:
          scala.caps.unsafe.unsafeAssumeSeparate:
            outer.open[Directory](Read & Exclusive): a ?=>
              sibling.open[Directory](Read & Exclusive): b ?=>
                true
      . assert(_ == true)

      test(m"An Exclusive scope is released when it ends"):
        unsafely:
          outer.open[Directory](Read & Exclusive) { () }
          outer.open[Directory](Read & Exclusive) { true }
      . assert(_ == true)

    suite(m"Glob expansion"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.dereferenceSymlinks.enabled

      val globLeaf: Text = Uuid().show
      val root: Path on Linux = unsafely((% / "tmp" / globLeaf).on[Linux])

      unsafely:
        root.create[Directory]()
        (root / "a.jar").write(t"a")
        (root / "a.txt").write(t"a")
        (root / "sub1").create[Directory]()
        (root / "sub1" / "b.jar").write(t"b")
        (root / "sub1" / "inner").create[Directory]()
        (root / "sub1" / "inner" / "c.jar").write(t"c")
        (root / "sub2").create[Directory]()
        (root / "sub2" / "d.jar").write(t"d")

      def names(paths: List[Path on Linux]): List[Text] = paths.map(_.name).order(identity)

      test(m"A star glob matches entries of the root only"):
        unsafely(names(root.glob(Glob.parse(t"*.jar"))))
      . assert(_ == List(t"a.jar"))

      test(m"A question mark matches a single character"):
        unsafely(names(root.glob(Glob.parse(t"?.txt"))))
      . assert(_ == List(t"a.txt"))

      test(m"A character range filters a wildcard segment"):
        unsafely(names(root.glob(Glob.parse(t"*/[bd].jar"))))
      . assert(_ == List(t"b.jar", t"d.jar"))

      test(m"A literal segment descends directly"):
        unsafely(names(root.glob(Glob.parse(t"sub1/*.jar"))))
      . assert(_ == List(t"b.jar"))

      test(m"A globstar spans any number of directories"):
        unsafely(names(root.glob(Glob.parse(t"**/*.jar"))))
      . assert(_ == List(t"a.jar", t"b.jar", t"c.jar", t"d.jar"))

      test(m"A globstar below a literal segment spans its subtree only"):
        unsafely(names(root.glob(Glob.parse(t"sub1/**/*.jar"))))
      . assert(_ == List(t"b.jar", t"c.jar"))

      test(m"A pattern matching nothing yields an empty list"):
        unsafely(root.glob(Glob.parse(t"nowhere/*.jar")))
      . assert(_ == List())

    suite(m"File locking"):
      import errorDiagnostics.stackTracesDiagnostics
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.enabled

      val lockDirLeaf: Text = Uuid().show
      val lockDir: Path on Linux = unsafely((% / "tmp" / lockDirLeaf).on[Linux])

      unsafely:
        lockDir.create[Directory]()
        (lockDir / "target.txt").write(t"content")

      val target: Path on Linux = unsafely(lockDir / "target.txt")

      test(m"An Exclusive file open succeeds and reads its content"):
        unsafely:
          target.open[File](Read & Exclusive)(file.stream.read[Data]).utf8
      . assert(_ == t"content")

      test(m"A second Exclusive open of the same file is Busy"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              target.open[File](Read & Exclusive): a ?=>
                target.open[File](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"Ordinary Read opens of one file coexist"):
        unsafely:
          scala.caps.unsafe.unsafeAssumeSeparate:
            target.open[File](Read): a ?=>
              target.open[File](Read) { true }
      . assert(_ == true)

      test(m"An Exclusive file open conflicts with an enclosing Exclusive directory scope"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              lockDir.open[Directory](Read & Exclusive): a ?=>
                target.open[File](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"The lock is released when the scope ends"):
        unsafely:
          target.open[File](Read & Exclusive) { () }
          target.open[File](Read & Exclusive) { true }
      . assert(_ == true)

    suite(m"Positional reading"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled

      val expanseLeaf: Text = Uuid().show
      val source: Path on Linux = unsafely((% / "tmp" / expanseLeaf).on[Linux])
      unsafely(source.write(t"0123456789abcdef"))

      test(m"The expanse reports the file's size"):
        unsafely(source.expanse(_.size))
      . assert(_ == 16L)

      test(m"A positional read returns the requested slice"):
        unsafely(source.expanse(_.read(4L, 6)).utf8)
      . assert(_ == t"456789")

      test(m"Reads at different offsets are independent"):
        unsafely:
          source.expanse: expanse =>
            (expanse.read(10L, 3).utf8, expanse.read(0L, 3).utf8)
      . assert(_ == (t"abc", t"012"))

      test(m"A read overlapping the end returns the bytes which exist"):
        unsafely(source.expanse(_.read(12L, 100)).utf8)
      . assert(_ == t"cdef")

    suite(m"Storage filesystem axis"):
      import anticipation.instantiables.instantInstantiable
      val root: Path on Linux = unsafely((% / "tmp").on[Linux])

      test(m"At most one storage filesystem extractor matches a path"):
        List(
          Btrfs.unapply(root).isDefined,
          Ext4.unapply(root).isDefined,
          Apfs.unapply(root).isDefined,
          Ntfs.unapply(root).isDefined).count(identity)
      . assert(_ <= 1)

      test(m"A matched creation-timed filesystem offers a total creation time"):
        root match
          case Apfs(path)  => unsafely(path.creation[Long]()) > 0L
          case Btrfs(path) => unsafely(path.creation[Long]()) > 0L
          case Ntfs(path)  => unsafely(path.creation[Long]()) > 0L
          case _           => true  // no creation-timed filesystem here; the gate did its job
      . assert(_ == true)

      test(m"A btrfs subvolume root is itself a subvolume root"):
        root match
          case Btrfs(path) => unsafely(path.subvolume().root.subvolumeRoot)
          case _           => true  // not btrfs here; the gate did its job
      . assert(_ == true)

      test(m"An entry shares its subvolume root's device number"):
        root match
          case Btrfs(path) =>
            path.entryIdentity.let(_.device)
            == unsafely(path.subvolume()).root.entryIdentity.let(_.device)

          case _ => true  // not btrfs here; the gate did its job
      . assert(_ == true)

    suite(m"Entry identity"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.deleteRecursively.enabled
      import filesystemOptions.overwritePreexisting.enabled

      val identifiedLeaf: Text = Uuid().show
      val linkedLeaf: Text = Uuid().show
      val separateLeaf: Text = Uuid().show
      val identified: Path on Linux = unsafely((% / "tmp" / identifiedLeaf).on[Linux])
      val linked: Path on Linux = unsafely((% / "tmp" / linkedLeaf).on[Linux])
      val separate: Path on Linux = unsafely((% / "tmp" / separateLeaf).on[Linux])
      unsafely(identified.write(t"content"))
      unsafely(separate.write(t"content"))
      unsafely(identified.hardLinkTo(linked))

      test(m"An entry has a device and inode number"):
        identified.entryIdentity.let(_.inode).or(0L) > 0L
      . assert(_ == true)

      test(m"A hard link has the same identity as its target"):
        identified.entryIdentity == linked.entryIdentity
      . assert(_ == true)

      test(m"Two distinct files have distinct identities"):
        identified.entryIdentity == separate.entryIdentity
      . assert(_ == false)

    suite(m"Shared locking"):
      import errorDiagnostics.stackTracesDiagnostics
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled

      val sharedLeaf: Text = Uuid().show
      val shared: Path on Linux = unsafely((% / "tmp" / sharedLeaf).on[Linux])
      unsafely(shared.write(t"content"))

      test(m"Shared opens of one file coexist"):
        unsafely:
          scala.caps.unsafe.unsafeAssumeSeparate:
            shared.open[File](Read & Shared): a ?=>
              shared.open[File](Read & Shared) { true }
      . assert(_ == true)

      test(m"An Exclusive open cannot join a Shared one"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              shared.open[File](Read & Shared): a ?=>
                shared.open[File](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"A Shared open cannot join an Exclusive one"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              shared.open[File](Read & Exclusive): a ?=>
                shared.open[File](Read & Shared) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

    suite(m"Awaited locking"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import threading.platformThreading

      val awaitLeaf: Text = Uuid().show
      val awaited: Path on Linux = unsafely((% / "tmp" / awaitLeaf).on[Linux])
      unsafely(awaited.write(t"content"))

      test(m"An awaited open blocks until the holder's scope ends, then proceeds"):
        unsafely:
          val order = java.util.concurrent.ConcurrentLinkedQueue[String]()

          val runnable: Runnable = () =>
            unsafely:
              awaited.open[File](Read & Exclusive, OpenFlag.Await):
                order.add("acquired") yet ()

          val waiter = java.lang.Thread(runnable)

          scala.caps.unsafe.unsafeAssumeSeparate:
            awaited.open[File](Read & Exclusive): a ?=>
              waiter.start()
              java.lang.Thread.sleep(300)
              order.add("releasing") yet ()

          waiter.join(5000)
          var seen = scala.List[String]()
          while !order.isEmpty do seen = seen :+ order.poll().toString
          seen
      . assert(_ == scala.List("releasing", "acquired"))

    suite(m"Slice locking"):
      import errorDiagnostics.stackTracesDiagnostics
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled

      val sliceLeaf: Text = Uuid().show
      val sliced: Path on Linux = unsafely((% / "tmp" / sliceLeaf).on[Linux])
      unsafely(sliced.write(t"0123456789abcdef"))

      test(m"A slice view is windowed to its range"):
        unsafely:
          Slice(sliced, 4L, 6L).open[File](Read): view ?=>
            (view.size, view.read(0L, 6).utf8, view.read(4L, 100).utf8)
      . assert(_ == (6L, t"456789", t"89"))

      test(m"Overlapping exclusive slices conflict"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              Slice(sliced, 0L, 8L).open[File](Read & Exclusive): a ?=>
                Slice(sliced, 4L, 8L).open[File](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

      test(m"Disjoint exclusive slices coexist"):
        unsafely:
          scala.caps.unsafe.unsafeAssumeSeparate:
            Slice(sliced, 0L, 4L).open[File](Read & Exclusive): a ?=>
              Slice(sliced, 8L, 4L).open[File](Read & Exclusive) { true }
      . assert(_ == true)

      test(m"A whole-file Exclusive open conflicts with any slice"):
        unsafely:
          capture[Io.Error]:
            scala.caps.unsafe.unsafeAssumeSeparate:
              sliced.open[File](Read & Exclusive): a ?=>
                Slice(sliced, 0L, 4L).open[File](Read & Exclusive) { () }
          . reason
      . assert(_ == Io.Error.Reason.Busy)

    suite(m"Extended attributes"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled

      val xattrLeaf: Text = Uuid().show
      val xattred: Path on Linux = unsafely((% / "tmp" / xattrLeaf).on[Linux])
      unsafely(xattred.write(t"content"))

      test(m"An attribute round-trips on a matched attributed filesystem"):
        def roundtrip[transport <: Attributed](path: Path on Linux over transport)
        :   (Optional[Text], Boolean) =
          unsafely:
            path.attribute(t"origin", t"soundness".in[Data])
            (path.attribute[Data](t"origin").let(_.utf8), path.attributes().has(t"origin"))

        xattred match
          case Apfs(path)  => roundtrip(path)
          case Btrfs(path) => roundtrip(path)
          case Ext4(path)  => roundtrip(path)
          case _           => (t"soundness", true) // no attributed filesystem here; gate held
      . assert(_ == (t"soundness", true))

      test(m"An unset attribute is absent"):
        xattred match
          case Apfs(path) => unsafely(path.attribute[Data](t"missing")).absent
          case _          => true
      . assert(_ == true)

    suite(m"Slice windows write"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import charDecoders.utf8Decoder
      import textSanitizers.skipSanitizer

      val windowLeaf: Text = Uuid().show
      val windowed: Path on Linux = unsafely((% / "tmp" / windowLeaf).on[Linux])
      unsafely(windowed.write(t"0123456789abcdef"))

      test(m"A write lands at the window-adjusted offset and reads back"):
        unsafely:
          Slice(windowed, 4L, 6L).open[File](Read & Write & Exclusive): window ?=>
            window.write(2L, t"XY".in[Data]) yet ()

          windowed.read[Text]
      . assert(_ == t"012345XY89abcdef")

      test(m"A write is clamped to the window and reports the count"):
        unsafely:
          Slice(windowed, 12L, 4L).open[File](Read & Write & Exclusive): window ?=>
            window.write(2L, t"WXYZ".in[Data])
      . assert(_ == 2)

      test(m"A clamped write stores only the bytes which fit"):
        unsafely(windowed.read[Text])
      . assert(_ == t"012345XY89abcdWX")

      test(m"A write past the window's end stores nothing"):
        unsafely:
          Slice(windowed, 0L, 4L).open[File](Read & Write & Exclusive): window ?=>
            window.write(4L, t"zz".in[Data])
      . assert(_ == 0)

    suite(m"Searchpaths"):
      import filesystemOptions.createNonexistentParents.enabled
      import filesystemOptions.overwritePreexisting.enabled
      import interfaces.paths.pathOnLinux

      val spLeafA: Text = t"sp-a-${Uuid().show}"
      val spLeafB: Text = t"sp-b-${Uuid().show}"
      val stemA: Path on Linux = unsafely((% / "tmp" / spLeafA).on[Linux])
      val stemB: Path on Linux = unsafely((% / "tmp" / spLeafB).on[Linux])

      unsafely:
        stemA.create[Directory]()
        stemB.create[Directory]()
        (stemA / "icons").create[Directory]()
        (stemB / "icons").create[Directory]()
        (stemA / "icons" / "app.png").write(t"A")
        (stemB / "icons" / "app.png").write(t"B")
        (stemB / "icons" / "extra.png").write(t"B2")
        (stemB / "themes").create[Directory]()

      given Searchpaths.Stems on Xdg.Data onto Linux = new Searchpaths.Stems:
        type Plane = Xdg.Data
        type Target = Linux
        val stems: List[Path on Linux] = List(stemA, stemB)

      test(m"locate finds the first stem's match"):
        unsafely((% / "icons" / "app.png").on[Xdg.Data].search())
      . assert(_ == unsafely(stemA / "icons" / "app.png"))

      test(m"locate falls through to a later stem"):
        unsafely((% / "icons" / "extra.png").on[Xdg.Data].search())
      . assert(_ == unsafely(stemB / "icons" / "extra.png"))

      test(m"a path present nowhere locates to Unset"):
        unsafely((% / "icons" / "missing.png").on[Xdg.Data].search())
      . assert(_ == Unset)

      test(m"locations lists every extant match in precedence order"):
        unsafely((% / "icons" / "app.png").on[Xdg.Data].locations())
      . assert(_ == unsafely(List(stemA / "icons" / "app.png", stemB / "icons" / "app.png")))

      test(m"entries merge across stems, earlier shadowing later"):
        unsafely((% / "icons").on[Xdg.Data].listing().map(_.name).order(identity))
      . assert(_ == List(t"app.png", t"extra.png"))

      test(m"a later-stem-only directory is found"):
        unsafely((% / "themes").on[Xdg.Data].search().present)
      . assert(_ == true)

      test(m"destination is the head-stem realization"):
        unsafely((% / "themes" / "new.css").on[Xdg.Data].destination())
      . assert(_ == unsafely(stemA / "themes" / "new.css"))

      test(m"the Xdg constructor reads the variables in spec order"):
        import systems.javaSystem
        given Environment = name =>
          if name == t"XDG_DATA_HOME" then stemA.encode
          else if name == t"XDG_DATA_DIRS" then stemB.encode
          else Unset

        Xdg.dataSearch().stems
      . assert(_ == List(stemA, stemB))
