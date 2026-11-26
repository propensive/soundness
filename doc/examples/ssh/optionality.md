# A setting which the configuration leaves out

`~/.ssh/config` rarely says everything about a host. Resolving a destination gives a hostname
and a port, which have defaults, and a user, which has none, so the user is optional and the
code which connects decides what its absence means.

```scala fuel
compile
frame
  label A host, resolved
  highlight resolve  the first value found for each setting
  code
      val config = SshConfig(t"""
      Host court
        HostName elsinore.example.com
        Port 2222
      """)

      val court = config.resolve(t"court")
frame
  label The user, which the file does not give
  highlight or(t"hamlet")  the default, chosen here
  code
      val config = SshConfig(t"""
      Host court
        HostName elsinore.example.com
        Port 2222
      """)

      val court = config.resolve(t"court")
      val user: Text = court.user.or(t"hamlet")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(court.hostName)
Out.println(court.port.show)
Out.println(court.user.inspect)
Out.println(user)
```

[Client configuration](../../soundness/modules/ssh.md#client-configuration) describes the
file, and [optionality](../../philosophy/optionality.md) `or`.
