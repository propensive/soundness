### Null Safety

The curse of `null`, the billion-dollar mistake is well known by anyone who has ever encountered a `NullPointerException`. In Soundness, `null` is unrepresentable and unnecessary, and Scala's type system enforces it. You won't see a null in Soundness code, and `NullPointerExceptions` rarely ever occur, so you can code without concern for them.
