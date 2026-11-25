# A host key gets one of four verdicts

Checking a host key against a `known_hosts` file does not answer yes or no, and does not throw
for the cases which are not yes. It answers with a verdict, and a host which was never seen is
a different verdict from a host whose key has changed.

```scala fuel
compile
frame
  label The verdict on a host and its key
  highlight KnownHosts.Verdict  one of four answers
  code
      import alphabets.base64Standard, providers.javaBaseProvider, strategies.throwUnsafely

      val blob = t"AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl"
      val hosts = KnownHosts(t"elsinore.example.com ssh-ed25519 $blob")

      def verdict(host: Text): KnownHosts.Verdict =
        hosts.verify(host, 22, t"ssh-ed25519", blob.deserialize[Base64])
frame
  label What to do about each
  highlight KnownHosts.Verdict.Unknown  never seen, which is not the same as changed
  code
      import alphabets.base64Standard, providers.javaBaseProvider, strategies.throwUnsafely

      val blob = t"AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl"
      val hosts = KnownHosts(t"elsinore.example.com ssh-ed25519 $blob")

      def verdict(host: Text): KnownHosts.Verdict =
        hosts.verify(host, 22, t"ssh-ed25519", blob.deserialize[Base64])

      def advice(host: Text): Text = verdict(host) match
        case KnownHosts.Verdict.Known   => t"connect"
        case KnownHosts.Verdict.Unknown => t"ask before trusting"
        case KnownHosts.Verdict.Changed => t"refuse: the key has changed"
        case KnownHosts.Verdict.Revoked => t"refuse: the key was revoked"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(advice(t"elsinore.example.com"))
Out.println(advice(t"dunsinane.example.com"))
```

[Host keys](../../soundness/modules/ssh.md#host-keys) says when each verdict is given, and
[total transitions](../../philosophy/total-transitions.md) why none of them is an exception.
