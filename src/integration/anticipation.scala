package anticipation.integration

import anticipation.*
import rudiments.*
import _root_.joviality as jov

import language.experimental.captureChecking

given jovialityPath[Fs <: jov.Filesystem](using fs: Fs): (PathProvider[jov.DiskPath[Fs]] & DirectoryProvider[jov.Directory[Fs]] & FileProvider[jov.File[Fs]] & PathInterpreter[jov.DiskPath[Fs]] & FileInterpreter[jov.File[Fs]] & DirectoryInterpreter[jov.Directory[Fs]]) =
  new PathProvider[jov.DiskPath[Fs]] with DirectoryProvider[jov.Directory[Fs]] with FileProvider[jov.File[Fs]] with PathInterpreter[jov.DiskPath[Fs]] with FileInterpreter[jov.File[Fs]] with DirectoryInterpreter[jov.Directory[Fs]]:
    def makePath(str: String, readOnly: Boolean = false): Option[jov.DiskPath[Fs]] =
      safely(fs.parse(Text(str))).option
    
    def makeDirectory(str: String, readOnly: Boolean = false): Option[jov.Directory[Fs]] =
      safely(fs.parse(Text(str)).directory(jov.Expect)).option
    
    def makeFile(str: String, readOnly: Boolean = false): Option[jov.File[Fs]] =
      safely(fs.parse(Text(str)).file(jov.Expect)).option

    def getPath(value: jov.DiskPath[Fs]): String = value.fullname.s
    def directoryPath(value: jov.Directory[Fs]): String = value.path.fullname.s
    def filePath(value: jov.File[Fs]): String = value.path.fullname.s