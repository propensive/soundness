All terms and types are defined in the `panopticon` package, which can be imported with,
```scala
import panopticon.*
```

### Creating a Lens

Imagine we have two case classes,
```scala
import anticipation.*

case class User(id: Int, name: Text, birthday: Date)
case class Date(day: Int, month: Int, year: Int)
```
describing users of a hypothetical system, and numerical calendar dates.

We can construct a `Lens` for modifying a user's name with:
```scala
val userName = Lens[User](_.name)
```

Step-by-step, the expression is a reference to the `Lens` factory object, the
type upon which the lens will operate (`User`), and a lambda from that type to
the field the lens will focus on. The `User` type cannot be inferred, but the
type of `name`, which is `Text`, will be inferred.

Similarly, we could construct a lens which accesses the month field of a `Date`
with `Lens[Date](_.month)`. Or we could construct a lens which directly access
the year of birth of a user, like so:
```scala
val userBirthYear = Lens[User](_.birthday.year)
```

### Applying Lenses

If we construct an instance of a `User` with,
```scala
val user = User(38295, t"Bob Mason", Date(12, 7, 1997))
```
then we can always access the user's birth year with `user.birthday.year`, but
we can also use the lens created above, `userBirthYear`, like so:
```scala
val birthYear: Int = userBirthYear.get(user)
```

This is unspectacular. But it becomes more useful for updating a user.

We can change that user's birth year with,
```scala
val user2: User = userBirthYear.set(user, 1995)
```
which compares favorably with the equivalent code written using the case
classes' `copy` methods:
```scala
val user2: User = user.copy(birthday = user.birthday.copy(year = 1995))
```

We can also specify the lens inline in the expression, like so:
```scala
val user2: User = Lens[User](_.birthday.year).set(user, 1995)
```

The saving on syntactic verbosity improves the deeper the nesting.

### Composing Lenses

Although the `userBirthYear` lens above was created in a single expression, it
is also possible to construct it by composing a lens for `User#birthday` and a
lens for `Date#year`, using the `++` operator:
```scala
val userBirthday = Lens[User](_.birthday)
val dateYear = Lens[Date](_.year)
val userBirthYear = userBirthday ++ dateYear
```




