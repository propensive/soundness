# A task's life, line by line

A container is a definition, and a task is a running instance of one. Creating it, starting
it, waiting for it and stopping it are four requests to a daemon, written as four statements
in the order they happen.

```scala fuel
compile
frame
  label Created, with its root filesystem
  highlight createTask  a request over gRPC
  code
      import strategies.throwUnsafely

      def run(client: Containerd)(using Monitor): Unit =
        val rootfs = List(Containerd.Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/globe")))
        client.createTask(t"herald", rootfs)
frame
  label Started
  highlight startTask  gives the process id
  code
      import strategies.throwUnsafely

      def run(client: Containerd)(using Monitor): Unit =
        val rootfs = List(Containerd.Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/globe")))
        client.createTask(t"herald", rootfs)
        val pid = client.startTask(t"herald")
frame
  label Waited for, or stopped
  highlight waitTask  returns when the task has exited
  code
      import strategies.throwUnsafely

      def run(client: Containerd)(using Monitor): Unit =
        val rootfs = List(Containerd.Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/globe")))
        client.createTask(t"herald", rootfs)
        val pid = client.startTask(t"herald")
        client.killTask(t"herald", signal = 15)
        client.waitTask(t"herald")
##
```

The example is compiled, and is not run, since it needs a running daemon.
[Tasks](../../soundness/modules/docker.md#tasks) describes the states of a task, and
[direct style](../../philosophy/direct-style.md) why a remote call reads as a local one.
