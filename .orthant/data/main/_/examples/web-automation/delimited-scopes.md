# A browser for the length of a block

`session` launches a browser's driver, opens a session, and runs a block in which `browser` is
that session. When the block ends, by returning or by failing, the session and the driver are
closed.

```scala
import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
import threads.virtualThreads, probates.awaitProbate, strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
```

```scala fuel
compile
frame
  label A session
  focus WebDriver.Chrome.headless.session:
  code
      def title(): Text = supervise:
        WebDriver.Chrome.headless.session:
          t"untitled"
frame
  label The browser, available within it
  highlight browser  the session, in scope for the block
    occurrence 1
  code
      def title(): Text = supervise:
        WebDriver.Chrome.headless.session:
          browser.navigateTo(url"https://example.com/")
          browser.title()
##
```

The example is compiled, and is not run, since it needs a browser.
[Driving a browser](../../soundness/modules/web-automation.md#driving-a-browser) has the other
browsers, and a driver which is already running.
