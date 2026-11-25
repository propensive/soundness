# How long to wait is the caller's choice

A page changes after it loads, so a test must wait for what it expects. `awaitElement` polls
under whichever retry schedule is in scope, and the schedule is an import and not a number of
milliseconds at each call.

```scala
import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
import threads.virtualThreads, probates.awaitProbate, strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
```

```scala fuel
compile
frame
  label Waiting, with no schedule to wait by
  errors show
  code
      def result(): Unit = supervise:
        WebDriver.Chrome.headless.session:
          browser.awaitElement(Name[DomId](t"result"))
frame
  label Ten attempts, further apart each time
  highlight retryTenacities.exponentialTenTimesTenacity  the schedule, chosen once
  code
      import retryTenacities.exponentialTenTimesTenacity

      def result(): Unit = supervise:
        WebDriver.Chrome.headless.session:
          browser.awaitElement(Name[DomId](t"result"))
##
```

The example is compiled, and is not run, since it needs a browser.
[Waiting](../../soundness/modules/web-automation.md#waiting) describes what is polled, and why.
