_Cosmopolite_ provides a typesafe representation for working with multilingual strings, `Messages`, with
convenient constructors which guarantee through their types that translations for a specific set of languages
exist in each instance. Furthermore, a `Language` type provides a coproduct representation of a language from
the same set which can safely select one string from the set.
