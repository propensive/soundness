package surveillance

import eucalyptus.*
import turbulence.*
import gossamer.*
import rudiments.*
import serpentine.*
import anticipation.*
import joviality.*

import scala.collection.mutable.HashMap

import java.nio.file as jnf
import jnf.StandardWatchEventKinds.*

extension [Fs <: Filesystem](fs: Fs)
  def watch(dirs: List[Directory[Fs]])(using Log): Watcher[Fs] throws InotifyError | IoError =
    val svc: jnf.WatchService = Unix.javaFilesystem.newWatchService().nn
    val watches: HashMap[jnf.WatchKey, Directory[Fs]] = HashMap()
    val directories: HashMap[Directory[Fs], jnf.WatchKey] = HashMap()
    
    dirs.foreach: dir =>
      val watchKey = dir.javaPath.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
      watches(watchKey) = dir
      directories(dir) = watchKey
      Log.info(t"Started watching ${dir.path}")
    
    Watcher(svc, watches, directories)
    
extension [Fs <: Filesystem](dir: Directory[Fs])
  def watch()(using Log): Watcher[Fs] throws InotifyError | IoError =
    dir.path.root.watch(List(dir))
  
case class Watcher[Fs <: Filesystem]
                  (private val svc: jnf.WatchService,
                   private val watches: HashMap[jnf.WatchKey, Directory[Fs]],
                   private val dirs: HashMap[Directory[Fs], jnf.WatchKey]):

  private val funnel = Funnel[Maybe[WatchEvent]]
  private val pumpTask = Task(pump())
  pumpTask()
  
  def stream: LazyList[WatchEvent] = funnel.stream.takeWhile(_ != Unset).sift[WatchEvent]
  def removeAll()(using Log): Unit = watches.values.foreach(remove(_))

  @tailrec
  private def pump(): Unit =
    svc.take().nn match
      case k: jnf.WatchKey =>
        val key = k.nn
        key.pollEvents().nn.iterator.nn.asScala.flatMap(process(key, _)).foreach(funnel.put(_))
        key.reset()
    
    pump()

  private def process(key: jnf.WatchKey, event: jnf.WatchEvent[?]): List[WatchEvent] =
    val keyDir = watches(key)
    
    val diskPath = event.context.nn match
      case path: jnf.Path => unsafely(keyDir.path + Relative.parse(Showable(path).show))
      case _              => throw Mistake("the event context should always be a path")
    
    try event.kind match
      case ENTRY_CREATE =>
        if diskPath.isDirectory
        then List(WatchEvent.NewDirectory(keyDir.show, diskPath.directory(Expect).show))
        else List(WatchEvent.NewFile(keyDir.show, diskPath.file(Expect).show))
      
      case ENTRY_MODIFY =>
        List(WatchEvent.Modify(keyDir.show, diskPath.file(Expect).show))
      
      case ENTRY_DELETE =>
        List(WatchEvent.Delete(keyDir.show, diskPath.show))
      
      case _ =>
        Nil
    
    catch case err: Exception => List()

  def remove(dir: Directory[Fs])(using Log): Unit = synchronized:
    val watchKey = dirs(dir)
    watchKey.cancel()
    dirs.remove(dir)
    watches.remove(watchKey)
    Log.info(t"Stopped watching ${dir.path}")
    if dirs.isEmpty then funnel.put(Unset)
  
  def add(dir: Directory[Fs])(using Log): Unit = synchronized:
    val watchKey = dir.javaPath.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
    watches(watchKey) = dir
    dirs(dir) = watchKey
    Log.info(t"Started watching ${dir.path}")
  
  def directories: Set[Directory[Fs]] = dirs.keySet.to(Set)

sealed trait WatchEvent:
  def path[P](using mkpath: PathProvider[P]): P = this match
    case NewFile(_, file)     => mkpath.make(file.s).get
    case NewDirectory(_, dir) => mkpath.make(dir.s).get
    case Modify(_, file)      => mkpath.make(file.s).get
    case Delete(_, path)      => mkpath.make(path.s).get

object WatchEvent:
  case class NewFile(keyDir: Text, file: Text) extends WatchEvent
  case class NewDirectory(keyDir: Text, directory: Text) extends WatchEvent
  case class Modify(keyDir: Text, file: Text) extends WatchEvent
  case class Delete(keyDir: Text, file: Text) extends WatchEvent

export WatchEvent.*

given Realm(t"surveillance")