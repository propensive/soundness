package ambience

import gossamer.*
import digression.*
import anticipation.*
import rudiments.*
import ambience.*

case class Xdg(home: Text):
  def dataHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgDataHome[PathType]).or(makeGenericPath(t"$home/.local/share"))
  
  def configHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(makeGenericPath(t"$home/.config"))
  
  def cacheHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgCacheHome[PathType]).or(makeGenericPath(t"$home/.cache"))
  
  def stateHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgStateHome[PathType]).or(makeGenericPath(t"$home/.local/state"))
  
  def runtimeDir[PathType: GenericPathMaker](using Environment): Maybe[PathType] =
    safely(Environment.xdgRuntimeDir[PathType])
  
  def bin[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(makeGenericPath(t"$home/.local/bin"))
  
  def dataDirs[PathType: GenericPathMaker](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgDataDirs[List[PathType]]).or:
      List(t"/usr/local/share", t"/usr/share").map(makeGenericPath(_))
  
  def configDirs[PathType: GenericPathMaker](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgConfigDirs[List[PathType]]).or:
      List(t"/etc/xdg").map(makeGenericPath(_))