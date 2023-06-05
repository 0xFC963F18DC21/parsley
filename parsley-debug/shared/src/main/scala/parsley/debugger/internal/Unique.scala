package parsley.debugger.internal

// Wrapper class that eliminates the equality / hash code overrides for a type.
private [debugger] final class Unique[+A](val item: A) extends AnyVal {
  // Shorthand syntax for extracting the item out of a Unique instance.
  def apply(): A =
    item

  def map[B](f: A => B): Unique[B] =
    Unique(f(item))

  def flatMap[B](f: A => Unique[B]): Unique[B] =
    f(item)
}

private [debugger] object Unique {
  def apply[A](item: A): Unique[A] =
    new Unique(item)
}
