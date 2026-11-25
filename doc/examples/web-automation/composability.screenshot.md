# A screenshot is an image

A screenshot of an element is not a file or an array of bytes. It is a `Raster in Png`, the
type which [images](../../soundness/modules/images.md) are read, transformed and written as.

```scala
import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
import threads.virtualThreads, probates.awaitProbate, strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import htmlDoms.whatwg, whatwg.*
```

```scala fuel
compile
frame
  label A screenshot of the page's heading
  highlight Raster..Png  an image, in its format
  code
      def masthead(): Raster in Png = supervise:
        WebDriver.Chrome.headless.session:
          browser.navigateTo(url"https://example.com/")
          browser.element(H1).screenshot()
##
```

The example is compiled, and is not run, since it needs a browser.
[Finding and using elements](../../soundness/modules/web-automation.md#finding-and-using-elements)
has the other operations on an element.
