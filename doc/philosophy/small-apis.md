# Small APIs

Soundness keeps its APIs small by giving one operation one name. A single polymorphic
method — `read`, `decode`, `show` — serves every type that supports it, instead of a
family of near-duplicates that differ only in what they apply to. There is one obvious
way to do a thing, so there is less to learn, less to remember, and less chance of
reaching for the wrong variant. The breadth of what an API can do comes from the range
of types it ranges over, not from the number of methods it offers.
