package soundness

export parasite.{Codicil, Completion, ConcurrencyError, Daemon, Hook, Monitor, Promise, Task,
    ThreadModel, Trace, Transgression, monitor, daemon, async, task, intercept, relent, cancel,
    sleep, snooze, sequence, race, supervise}

package threadModels:
  export parasite.threadModels.{platform, virtual}

package orphanDisposal:
  export parasite.orphanDisposal.{await, cancel, fail}
