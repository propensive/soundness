package hellenism

package classloaders:
  given threadContext: Classloader = Classloader.threadContext
  given system: Classloader = new Classloader(ClassLoader.getSystemClassLoader.nn)
  given platform: Classloader = new Classloader(ClassLoader.getPlatformClassLoader.nn)
  given scala: Classloader = Classloader[List]