package anticipation.integration

import anticipation.*
import rudiments.*
import _root_.joviality as jov

given jovialityPath[Fs <: jov.Filesystem](using fs: Fs): (PathProvider[jov.DiskPath[Fs]] & DirectoryProvider[jov.Directory[Fs]] & FileProvider[jov.File[Fs]]) =
  new PathProvider[jov.DiskPath[Fs]] with DirectoryProvider[jov.Directory[Fs]] with FileProvider[jov.File[Fs]]:
    def makePath(str: String, readOnly: Boolean = false): Option[jov.DiskPath[Fs]] =
      safely(fs.parse(Text(str))).option
    
    def makeDirectory(str: String, readOnly: Boolean = false): Option[jov.Directory[Fs]] =
      safely(fs.parse(Text(str)).directory(jov.Expect)).option
    
    def makeFile(str: String, readOnly: Boolean = false): Option[jov.File[Fs]] =
      safely(fs.parse(Text(str)).file(jov.Expect)).option
    