# A browser session is used only while the browser runs

`session` starts a browser, lends it to a block, and stops it when the block ends. A function
which would drive the browser later holds a session which will by then be closed, and the
compiler does not let it leave. A title, once read, is just text.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which would drive a closed browser
  errors show
  highlight browser.title()  would drive the browser later
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import threads.virtualThreads, probates.awaitProbate, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      def title() = supervise:
        WebDriver.Chrome.headless.session:
          browser.navigateTo(url"https://example.com/")
          () => browser.title()
frame
  label The title, read while it is open
  highlight browser.title()
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import threads.virtualThreads, probates.awaitProbate, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      def title(): Text = supervise:
        WebDriver.Chrome.headless.session:
          browser.navigateTo(url"https://example.com/")
          browser.title()
##
```

The example is compiled, and is not run, since it would start a browser.
[Web automation](../../soundness/modules/web-automation.md) describes what a session can do.
