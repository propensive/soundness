package rudiments

import anticipation.*

@implicitNotFound("rudiments: a contextual Environment instance is required, for example one of:\n"+
                  "    given Environment = environments.empty       // no environment variables or system properties\n"+
                  "    given Environment = environments.restricted  // access to system properties, but no environment variables\n"+
                  "    given Environment = environments.system      // full access to the JVM's environment")
class Environment(getEnv: Text => Option[Text], getProperty: Text => Option[Text]):
  def apply(variable: Text): Maybe[Text] = getEnv(variable) match
    case None        => Unset
    case Some(value) => value

  def property(variable: Text): Text throws EnvError =
    getProperty(variable).getOrElse(throw EnvError(variable, true))

  def fileSeparator: ('/' | '\\') throws EnvError = property(Text("file.separator")).s match
    case "/"  => '/'
    case "\\" => '\\'
    case _    => throw EnvError(Text("file.separator"), true)

  def pathSeparator: (':' | ';') throws EnvError = property(Text("path.separator")).s match
    case ";" => ';'
    case ":" => ':'
    case _    => throw EnvError(Text("path.separator"), true)

  def javaClassPath[P](using pp: PathProvider[P]): List[P] throws EnvError =
    property(Text("java.class.path")).s.split(pathSeparator).to(List).flatMap(pp.makePath(_))

  def javaHome[P](using pp: PathProvider[P]): P throws EnvError =
    pp.makePath(property(Text("java.home")).s).getOrElse(throw EnvError(Text("java.home"), true))

  def javaVendor: Text throws EnvError = property(Text("java.vendor"))
  def javaVendorUrl: Text throws EnvError = property(Text("java.vendor.url"))
  def javaVersion: Text throws EnvError = property(Text("java.version"))

  def javaSpecificationVersion: Int throws EnvError = property(Text("java.specification.version")) match
    case As[Int](version) => version
    case other            => throw EnvError(Text("java.specification.version"), true)

  def lineSeparator: Text throws EnvError = property(Text("line.separator"))
  def osArch: Text throws EnvError = property(Text("os.arch"))
  def osVersion: Text throws EnvError = property(Text("os.version"))

  def userDir[P](using pp: PathProvider[P]): P throws EnvError =
    pp.makePath(property(Text("user.dir")).s).getOrElse(throw EnvError(Text("user.dir"), true))

  def userHome[P](using pp: PathProvider[P]): P throws EnvError =
    pp.makePath(property(Text("user.home")).s).getOrElse(throw EnvError(Text("user.home"), true))

  def userName: Text throws EnvError = property(Text("user.name"))

  def pwd[P](using pp: PathProvider[P]): P throws EnvError =
    apply(Text("PWD")).or(safely(property(Text("user.dir")))).fm(throw EnvError(Text("user.dir"), true)): path =>
      pp.makePath(path.s).getOrElse(throw EnvError(Text("user.dir"), true))

case class EnvError(variable: Text, property: Boolean)
extends Error(
  if property then err"the system property $variable was not found"
  else err"the environment variable $variable was not found"
)
