_Parasite_ provides asynchronous tasks, built upon Java threads or virtual
threads. Asynchronous tasks form a supervisor hierarchy, where each task is
"owned" by a supervising parent task, and cancelation of tasks cascades through
the hierarchy. Capture checking is used to avoid thread leaks.

