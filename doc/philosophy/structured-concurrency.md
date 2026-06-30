# Structured Concurrency

Concurrency in Soundness is structured: tasks are spawned within a scope that owns
them, and that scope does not complete until its tasks have. A failure in one task
propagates to its siblings and its parent rather than vanishing unnoticed, and
cancelling a scope cancels everything beneath it. Concurrent lifetimes nest like the
blocks of ordinary code, so parallel work has the same clear beginning and end as
sequential work, and no task outlives the scope that launched it.
