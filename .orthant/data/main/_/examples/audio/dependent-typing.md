# The channels of a layout, known from its type

A channel layout is a type, and `Surround` takes its number of channels as a type argument.
The count is read back from the type, so six-channel sound is a different type from stereo,
and neither holds a number which could disagree with it.

```scala fuel
compile
frame
  label Two channels
  code
      val channels = summon[Stereo is ChannelLayout].channels
frame
  label Six channels
  highlight Surround[6]  the count is part of the type
  code
      val channels = summon[Surround[6] is ChannelLayout].channels
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(channels.show)
```

[Channel layouts](../../soundness/modules/audio.md#channel-layouts) lists the layouts, which a
recording carries in its type as `Audio across Stereo`.
