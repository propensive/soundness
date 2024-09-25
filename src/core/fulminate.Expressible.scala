package fulminate

trait Expressible:
  type Self
  def express(): Message