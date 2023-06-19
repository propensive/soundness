_Parasite_ provides an implementation of asynchronous tasks, built upon Java threads for use in high-availability applications running on a Loom JVM,
or smaller-scale applications on other JVMs. All tasks form a supervisor hierarchy, where each task is "owned" by a supervising parent task, and
cancelation of tasks cascades through the hierarchy. This makes it easier to avoid thread leaks in complex systems. Scala 3's context functions are
used to track tasks unintrusively, while documenting a thread's blocking nature in its signature.
