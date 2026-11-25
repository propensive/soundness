# A recording may fail, and says so

An input device may be unplugged, or busy, or unable to record at the rate asked of it.
`record` names that failure in its type, so a method which records must handle the error or
declare it too.

```scala fuel
compile
frame
  label The error is neither handled nor declared
  errors show
  code
      def rehearsal(feed: Audio.Feed): Recording across Stereo =
        feed.record[Stereo](44100.0*Hertz, bits = 16)
frame
  label The error is declared
  highlight raises..Error  the caller must now decide
  code
      def rehearsal(feed: Audio.Feed): Recording across Stereo raises Audio.Feed.Error =
        feed.record[Stereo](44100.0*Hertz, bits = 16)
##
```

The example is compiled, and is not run, since it needs an audio device.
[Recording](../../soundness/modules/audio.md#recording) describes the feed, and
[error handling](../../philosophy/error-handling.md) what a caller may do.
