package exoskeleton

trait Suggestible:
  type Self
  def suggest(values: Self): Suggestion
