// A classpath, a source and output locations, which the compilation tutorial's prose assumes
// rather than constructs: the running application's own classpath, a one-object source, and
// fresh directories under the temporary directory.
import soundness.*

import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import stdios.javaLangSystemStdio
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.javaBaseTemporaryDirectory

val classpath: LocalClasspath = Bundler.applicationClasspath
val source: Text = t"package com.example\n\nobject Main:\n  def main(args: Array[String]): Unit = ()"
val outputPath: Path on Linux = temporaryDirectory[Path on Linux] / "compiler" / Uuid().show
val destination: Path on Linux = temporaryDirectory[Path on Linux] / "compiler" / Uuid().show
outputPath.create[Directory](CreateFlag.Parents)

def report(importance: Importance, file: Text, message: Text): Unit =
  Out.println(t"$importance $file: $message")
