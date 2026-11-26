# Finding an element by the tag that makes it

An element is located by the values which describe a page elsewhere: the tag which
[HTML](../../soundness/modules/html.md) builds it with, or the validated identifier it was
given. No selector is spelled out in a string.

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
  label The heading, by its tag
  highlight H1  the tag which HTML builds a heading with
  code
      def heading(): Text = supervise:
        WebDriver.Chrome.headless.session:
          browser.element(H1).innerText()
frame
  label A field, by its identifier
  highlight Name[DomId](t"query")  an identifier, checked as one
  code
      def search(): Unit = supervise:
        WebDriver.Chrome.headless.session:
          browser.element(Name[DomId](t"query")).value(t"Illyria")
          browser.element(Button).click()
##
```

The example is compiled, and is not run, since it needs a browser.
[Finding and using elements](../../soundness/modules/web-automation.md#finding-and-using-elements)
lists the locators and what an element offers.
