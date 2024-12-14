package galilei

import java.nio.file as jnf

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*

erased trait Filesystem

object Filesystem:
  type Rules = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
      MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
      MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
      MustNotMatch["(?i)(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(\\.[^.]+)?"] & MustNotEqual["."] &
      MustNotEqual[".."] & MustNotEqual[""]

  given [FilesystemType <: Filesystem] => (Path on FilesystemType) is Substantiable =
    path => jnf.Files.exists(path.javaPath)

  given (using Tactic[PathError])
      => Filesystem is Radical from WindowsDrive | Linux.Root | MacOs.Root as radical =
    val os = System.getProperty("os.name").nn.tt

    val delegate =
      if os.starts(t"Windows") then Windows.radical
      else if os.starts(t"Mac") then MacOs.radical else Linux.radical

    (delegate: @unchecked) match
      case radical: (Filesystem is Radical from WindowsDrive | Linux.Root | MacOs.Root) => radical

  given (using Tactic[NameError])
      => Filesystem is Navigable by Name[Filesystem] under Rules as navigable =
    val os = System.getProperty("os.name").nn.tt

    val delegate =
      if os.starts(t"Windows") then Windows.navigable
      else if os.starts(t"Mac") then MacOs.navigable else Linux.navigable

    (delegate: @unchecked) match
      case navigable: (Filesystem is Navigable by Name[Filesystem] under Rules) => navigable
